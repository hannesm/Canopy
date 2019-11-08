open Lwt.Infix

module Main (C : Mirage_console.S) (T : Mirage_time.S) (_: sig end) (Http : Cohttp_mirage.Server.S) (CLOCK: Mirage_clock.PCLOCK) (Management : Mirage_stack.V4V6) = struct

  module D  = Canopy_dispatch.Make(Http)

  let src = Logs.Src.create "canopy-main" ~doc:"Canopy main logger"
  module Log = (val Logs.src_log src : Logs.LOG)

  let access kind =
    let s = ref (0, 0) in
    let open Metrics in
    let doc = "connection statistics" in
    let data () =
      Data.v [
        int "active" (fst !s) ;
        int "total" (snd !s) ;
      ] in
    let tags = Tags.string "kind" in
    let src = Src.v ~doc ~tags:Tags.[ tags ] ~data "connections" in
    (fun action ->
       (match action with
        | `Open -> s := (succ (fst !s), succ (snd !s))
        | `Close -> s := (pred (fst !s), snd !s));
       Metrics.add src (fun x -> x kind) (fun d -> d ()))

  let tcp_access = access "tcp"
  let git_access = access "git"

  module Store = Canopy_store
  module Monitoring = Monitoring_experiments.Make(T)(Management)
  module Syslog = Logs_syslog_mirage.Udp(C)(CLOCK)(Management)

  let start c _time ctx http _clock management =
    let hostname = Key_gen.name ()
    and syslog = Key_gen.syslog ()
    and monitor = Key_gen.monitor ()
    in
    (match syslog with
     | None -> Logs.warn (fun m -> m "no syslog specified, dumping on stdout")
     | Some ip -> Logs.set_reporter (Syslog.create c management ip ~hostname ()));
    (match monitor with
     | None -> Logs.warn (fun m -> m "no monitor specified, not outputting statistics")
     | Some ip -> Monitoring.create ~hostname ip management);
    git_access `Open;
    Store.pull ~ctx >>= fun () ->
    Store.base_uuid () >>= fun uuid ->
    Store.fill_cache uuid >>= fun new_cache ->
    git_access `Close; (* TODO otherwise "type cariable that cannot be generalized"*)
    let cache = ref (new_cache) in
    let update_atom, atom =
      Canopy_syndic.atom uuid Store.last_commit_date cache
    in
    let store_ops = {
      Canopy_dispatch.subkeys = Store.get_subkeys ;
      value = Store.get_key ;
      update =
        (fun () ->
           git_access `Open;
           Store.pull ~ctx >>= fun () ->
           Store.fill_cache uuid >>= fun new_cache ->
           git_access `Close;
           cache := new_cache ;
           update_atom ());
      last_commit = Store.last_commit_date ;
    } in
    update_atom () >>= fun () ->
    let disp hdr = `Dispatch (hdr, store_ops, atom, cache) in
    let hdr = Cohttp.Header.init_with
        "Strict-Transport-Security" "max-age=31536000" (* in seconds, roughly a year *)
    in
    let port = Canopy_config.port () in
    Log.info (fun f -> f "HTTP server listening on port %d" port);
    http (`TCP port) (D.create tcp_access (disp hdr))
end
