open Core
open Async

open Coinbasepro
open Coinbasepro_ws

let src = Logs.Src.create "coinbasepro.ws-test"
    ~doc:"Coinbasepro API - WS test application"

let process_user_cmd ~sandbox ?auth w =
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
      let products = List.map ~f:Pair.of_string_exn products in
      Pipe.write w (Subscribe (None, [full products]))
    | "full_auth" :: products ->
      let products = List.map ~f:Pair.of_string_exn products in
      Pipe.write w (Subscribe (Option.map ~f:fst auth, [full products]))
    | "orders" :: _ ->
      Fastrest.request ?auth:(Option.map ~f:snd auth)
        (Coinbasepro_rest.Order.get_all ~sandbox ()) >>= fun os ->
      let os = Or_error.ok_exn os in
      Deferred.List.iter os ~f:begin fun o ->
        Logs_async.app ~src (fun m -> m "%a" Sexp.pp (Coinbasepro_rest.Order.sexp_of_t o))
      end
    | "fills" :: pairs ->
      Fastrest.request ?auth:(Option.map ~f:snd auth)
        (Coinbasepro_rest.Fill.get ~sandbox
           (`ProductID (List.map ~f:Pair.of_string_exn pairs))) >>= fun fills ->
      let fills = Or_error.ok_exn fills in
      Deferred.List.iter fills ~f:begin fun fi ->
        Logs_async.app ~src (fun m -> m "%a" Sexp.pp (Coinbasepro_rest.Fill.sexp_of_t fi))
      end
    | "accounts" :: _ ->
      Fastrest.request ?auth:(Option.map ~f:snd auth)
        (Coinbasepro_rest.accounts ~sandbox ()) >>= fun accounts ->
      let accounts = Or_error.ok_exn accounts in
      Deferred.List.iter accounts ~f:begin fun a ->
        Logs_async.app ~src (fun m -> m "%a" Sexp.pp (Coinbasepro_rest.sexp_of_account a))
      end
    | "ledger" :: id :: _ ->
      let id = Option.value_exn (Uuidm.of_string id) in
      Fastrest.request ?auth:(Option.map ~f:snd auth)
        (Coinbasepro_rest.ledger ~sandbox id) >>= fun entries ->
      let entries = Or_error.ok_exn entries in
      Deferred.List.iter entries ~f:begin fun a ->
        Logs_async.app ~src (fun m -> m "%a" Sexp.pp (Coinbasepro_rest.sexp_of_ledger a))
      end
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
        let secret = Base64.decode_exn cfg.Bs_devkit.Cfg.secret in
        auth
          ~timestamp
          ~key:cfg.key
          ~secret
          ~passphrase:cfg.passphrase,
        { Fastrest.key = cfg.key ; secret ; meta = ["passphrase", cfg.passphrase] }
      end in
  Coinbasepro_ws_async.with_connection_exn ~sandbox begin fun r w ->
    let log_incoming msg =
      Logs_async.debug ~src (fun m -> m "%a" pp msg) in
    Deferred.all_unit [
      process_user_cmd ~sandbox ?auth w ;
      Pipe.iter r ~f:log_incoming
    ]
  end

let () =
  Command.async ~summary:"Coinbasepro WS client" begin
    let open Command.Let_syntax in
    [%map_open
      let () = Logs_async_reporter.set_level_via_param []
      and sandbox = flag "sandbox" no_arg ~doc:" Use sandbox"
      and cfg = Bs_devkit.Cfg.param () in
      fun () ->
        Logs.set_reporter (Logs_async_reporter.reporter ()) ;
        main cfg sandbox
    ] end |>
  Command.run
