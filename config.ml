open Mirage

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

let dns_key =
  let doc = Key.Arg.info ~doc:"nsupdate key (name:type:value,...)" ["dns-key"] in
  Key.(create "dns-key" Arg.(required string doc))

let dns_server =
  let doc = Key.Arg.info ~doc:"dns server IP" ["dns-server"] in
  Key.(create "dns-server" Arg.(required ipv4_address doc))

let dns_port =
  let doc = Key.Arg.info ~doc:"dns server port" ["dns-port"] in
  Key.(create "dns-port" Arg.(opt int 53 doc))

let hostname =
  let doc = Key.Arg.info ~doc:"hostname" ["hostname"] in
  Key.(create "hostname" Arg.(required string doc))

let key_seed =
  let doc = Key.Arg.info ~doc:"certificate key seed" ["key-seed"] in
  Key.(create "key-seed" Arg.(required string doc))

(* Dependencies *)

let packages = [
  package "omd" ;
  package ~min:"4.0.0" "tyxml";
  package "ptime";
  package ~min:"0.5" "decompress";
  package ~min:"1.0.0" "irmin";
  package "irmin-mirage";
  package "cohttp-mirage";
  package "mirage-flow";
  package ~sublibs:["mirage"] "tls";
  package "re";
  package ~min:"0.21.0" "cohttp";
  package ~min:"1.5" "syndic";
  package "magic-mime";
  package "uuidm";
  package "logs";
  package ~sublibs:["mirage.certify"] "udns" ;
]


(* Network stack *)
let stack =
  if_impl Key.is_unix
    (socket_stackv4 [Ipaddr.V4.any])
    (static_ipv4_stack ~arp:farp default_network)

let logger =
  syslog_udp ~config:(syslog_config ~truncate:1484 "robur.io") stack

let () =
  let keys = Key.([
      abstract push_hook_k;
      abstract remote_k;
      abstract port_k;
      abstract tls_port_k;
      abstract dns_key ;
      abstract dns_server ;
      abstract dns_port ;
      abstract hostname ;
      abstract key_seed
    ])
  in
  register "canopy" [
    foreign
      ~deps:[ abstract nocrypto ; abstract logger ; abstract app_info ]
      ~keys
      ~packages
      "Canopy_main.Main"
      (random @-> time @-> stackv4 @-> resolver @-> conduit @-> pclock @-> job)
    $ default_random
    $ default_time
    $ stack
    $ resolver_dns stack
    $ conduit_direct ~tls:true stack
    $ default_posix_clock
  ]
