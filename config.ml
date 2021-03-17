open Mirage


(* boilerplate from https://github.com/mirage/ocaml-git.git unikernel/config.ml
   (commit #2220ba7fe749d228a52e52f25f3761575269a98a) *)
type mimic = Mimic

let mimic = typ Mimic

let mimic_count =
  let v = ref (-1) in
  fun () -> incr v ; !v

let mimic_conf () =
  let packages = [ package "mimic" ] in
  impl @@ object
       inherit base_configurable
       method ty = mimic @-> mimic @-> mimic
       method module_name = "Mimic.Merge"
       method! packages = Key.pure packages
       method name = Fmt.str "merge_ctx%02d" (mimic_count ())
       method! connect _ _modname =
         function
         | [ a; b ] -> Fmt.str "Lwt.return (Mimic.merge %s %s)" a b
         | [ x ] -> Fmt.str "%s.ctx" x
         | _ -> Fmt.str "Lwt.return Mimic.empty"
     end

let merge ctx0 ctx1 = mimic_conf () $ ctx0 $ ctx1

let mimic_tcp_conf =
  let packages = [ package "git-mirage" ~sublibs:[ "tcp" ] ] in
  impl @@ object
       inherit base_configurable
       method ty = stackv4 @-> mimic
       method module_name = "Git_mirage_tcp.Make"
       method! packages = Key.pure packages
       method name = "tcp_ctx"
       method! connect _ modname = function
         | [ stack ] ->
           Fmt.str {ocaml|Lwt.return (%s.with_stack %s %s.ctx)|ocaml}
             modname stack modname
         | _ -> assert false
     end

let mimic_tcp_impl stackv4 = mimic_tcp_conf $ stackv4

let mimic_ssh_conf ~kind ~seed ~auth =
  let seed = Key.abstract seed in
  let auth = Key.abstract auth in
  let packages = [ package "git-mirage" ~sublibs:[ "ssh" ] ] in
  impl @@ object
       inherit base_configurable
       method ty = stackv4 @-> mimic @-> mclock @-> mimic
       method! keys = [ seed; auth; ]
       method module_name = "Git_mirage_ssh.Make"
       method! packages = Key.pure packages
       method name = match kind with
         | `Rsa -> "ssh_rsa_ctx"
         | `Ed25519 -> "ssh_ed25519_ctx"
       method! connect _ modname =
         function
         | [ _; tcp_ctx; _ ] ->
             let with_key =
               match kind with
               | `Rsa -> "with_rsa_key"
               | `Ed25519 -> "with_ed25519_key"
             in
             Fmt.str
               {ocaml|let ssh_ctx00 = Mimic.merge %s %s.ctx in
                      let ssh_ctx01 = Option.fold ~none:ssh_ctx00 ~some:(fun v -> %s.%s v ssh_ctx00) %a in
                      let ssh_ctx02 = Option.fold ~none:ssh_ctx01 ~some:(fun v -> %s.with_authenticator v ssh_ctx01) %a in
                      Lwt.return ssh_ctx02|ocaml}
               tcp_ctx modname
               modname with_key Key.serialize_call seed
               modname Key.serialize_call auth
         | _ -> assert false
     end

let mimic_ssh_impl ~kind ~seed ~auth stackv4 mimic_git mclock =
  mimic_ssh_conf ~kind ~seed ~auth
  $ stackv4
  $ mimic_git
  $ mclock

(* TODO(dinosaure): user-defined nameserver and port. *)

let mimic_dns_conf =
  let packages = [ package "git-mirage" ~sublibs:[ "dns" ] ] in
  impl @@ object
       inherit base_configurable
       method ty = random @-> mclock @-> time @-> stackv4 @-> mimic @-> mimic
       method module_name = "Git_mirage_dns.Make"
       method! packages = Key.pure packages
       method name = "dns_ctx"
       method! connect _ modname =
         function
         | [ _; _; _; stack; tcp_ctx ] ->
             Fmt.str
               {ocaml|let dns_ctx00 = Mimic.merge %s %s.ctx in
                      let dns_ctx01 = %s.with_dns %s dns_ctx00 in
                      Lwt.return dns_ctx01|ocaml}
               tcp_ctx modname
               modname stack
         | _ -> assert false
     end

let mimic_dns_impl random mclock time stackv4 mimic_tcp =
  mimic_dns_conf $ random $ mclock $ time $ stackv4 $ mimic_tcp

(* --- end of copied code --- *)

(* Command-line options *)

let push_hook_k =
  let doc = Key.Arg.info ~doc:"GitHub push hook." ["hook"] in
  Key.(create "push_hook" Arg.(opt string "push" doc))

let remote_k =
  let doc = Key.Arg.info ~doc:"Remote repository to fetch content.\
                             \ Use suffix #foo to specify a branch 'foo':\
                             \ https://github.com/user/blog.git#content"
      ["r"; "remote"] in
  Key.(create "remote" Arg.(opt string "https://github.com/Engil/__blog.git" doc))

let port_k =
  let doc = Key.Arg.info ~doc:"Socket port." ["p"; "port"] in
  Key.(create "port" Arg.(opt int 8080 doc))

let tls_port_k =
  let doc = Key.Arg.info ~doc:"Enable TLS (using keys in `tls/`) on given port." ["tls"] in
  Key.(create "tls_port" Arg.(opt (some int) None doc))

let ssh_seed =
  let doc = Key.Arg.info ~doc:"Seed for ssh private key." ["ssh-seed"] in
  Key.(create "ssh_seed" Arg.(opt (some string) None doc))

let ssh_authenticator =
  let doc = Key.Arg.info ~doc:"SSH host key authenticator." ["ssh-authenticator"] in
  Key.(create "ssh_authenticator" Arg.(opt (some string) None doc))

(* Dependencies *)

let packages = [
  package "omd" ;
  package ~min:"4.0.0" "tyxml";
  package "ptime";
  package ~min:"0.5" "decompress";
  package ~min:"2.0.0" "irmin";
  package ~min:"2.0.0" "irmin-mirage";
  package ~min:"2.0.0" "irmin-mirage-git";
  package ~min:"3.3.1" "git-mirage";
  package "git-cohttp-mirage";
  package "cohttp-mirage";
  package "mirage-flow";
  package "tls-mirage";
  package "re";
  package ~min:"0.21.0" "cohttp";
  package ~min:"1.5" "syndic";
  package "magic-mime";
  package "uuidm";
  package "logs";
]


(* Network stack *)
let stack = generic_stackv4 default_network

let mimic_impl ~kind ~seed ~authenticator stackv4 random mclock time =
  let mtcp = mimic_tcp_impl stackv4 in
  let mdns = mimic_dns_impl random mclock time stackv4 mtcp in
  let mssh = mimic_ssh_impl ~kind ~seed ~auth:authenticator stackv4 mtcp mclock in
  merge mssh mdns

let mimic_impl =
  mimic_impl ~kind:`Rsa ~seed:ssh_seed ~authenticator:ssh_authenticator stack
    default_random default_monotonic_clock default_time

let () =
  let keys = Key.([
      abstract push_hook_k;
      abstract remote_k;
      abstract port_k;
      abstract tls_port_k;
    ])
  in
  register "canopy" [
    foreign
      ~keys
      ~packages
      "Canopy_main.Main"
      (stackv4 @-> mimic @-> resolver @-> conduit @-> pclock @-> kv_ro @-> job)
    $ stack
    $ mimic_impl
    $ resolver_dns stack
    $ conduit_direct ~tls:true stack
    $ default_posix_clock
    $ crunch "tls"
  ]
