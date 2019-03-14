open Core
open Async

open Coinbasepro_ws
open Coinbasepro_ws_async

let src = Logs.Src.create "coinbasepro.ws-test"
    ~doc:"Coinbasepro API - WS test application"

let process_user_cmd ?auth w =
  let process s =
    match String.split s ~on:' ' with
    (* | "unsubscribe" :: chanid :: _ ->
     *   let chanid = int_of_string chanid in
     *   Pipe.write w (Unsubscribe { chanid ; reqid = None })
     * | "ping" :: v :: _ ->
     *   Pipe.write w (Ping (int_of_string_opt v))
     * | "ping" :: _ ->
     *   Pipe.write w (Ping None)
     * | "trades" :: pair ->
     *   Pipe.write w (Subscribe { reqid = None ; pair ; sub = Trade })
     * | "books" :: pair ->
     *   Pipe.write w (Subscribe { reqid = None ; pair ; sub = Book 10 })
     * | h :: _ ->
     *   Logs_async.err (fun m -> m "Unknown command %s" h) *)
    | [] ->
      Logs_async.err ~src (fun m -> m "Empty command")
    | "full" :: products ->
      Pipe.write w (Subscribe (None, [full products]))
    | "full_auth" :: products ->
      Pipe.write w (Subscribe (auth, [full products]))
    | _ ->
      Logs_async.err ~src (fun m -> m "Non Empty command")
  in
  let rec loop () = Reader.(read_line @@ Lazy.force stdin) >>= function
    | `Eof -> Deferred.unit
    | `Ok line -> process line >>= loop
  in
  loop ()

let main cfg sandbox =
  let auth =
    Option.map
      (List.Assoc.find cfg "CBPRO" ~equal:String.equal)
      ~f:begin fun cfg ->
        let timestamp = Time_ns.(now () |> to_span_since_epoch |> Span.to_int_ms |> fun a -> a // 1000 |> Float.to_string) in
        auth
          ~timestamp
          ~key:cfg.Bs_devkit.Cfg.key
          ~secret:(Base64.decode_exn cfg.secret)
          ~passphrase:cfg.passphrase
      end in
  with_connection ~sandbox begin fun r w ->
    let log_incoming msg =
      Logs_async.debug ~src (fun m -> m "%a" pp msg) in
    Deferred.all_unit [
      process_user_cmd ?auth w ;
      Pipe.iter r ~f:log_incoming
    ]
  end

let () =
  Command.async ~summary:"Coinbasepro WS client" begin
    let open Command.Let_syntax in
    [%map_open
      let () = Logs_async_reporter.set_level_via_param None
      and sandbox = flag "sandbox" no_arg ~doc:" Use sandbox"
      and cfg = Bs_devkit.Cfg.param () in
      fun () ->
        Logs.set_reporter (Logs_async_reporter.reporter ()) ;
        main cfg sandbox
    ] end |>
  Command.run
