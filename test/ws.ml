open Core
open Async
open Coinbasepro
open Coinbasepro_ws

let src =
  Logs.Src.create "coinbasepro.ws-test"
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
    | [] -> Logs_async.err ~src (fun m -> m "Empty command")
    | "full" :: products ->
        let products = List.map ~f:Pair.of_string_exn products in
        Pipe.write w (Subscribe (None, [ full products ]))
    | "full_auth" :: products ->
        let products = List.map ~f:Pair.of_string_exn products in
        Pipe.write w (Subscribe (Option.map ~f:fst auth, [ full products ]))
    | "orders" :: _ ->
        Fastrest.request ?auth:(Option.map ~f:snd auth)
          (Coinbasepro_rest.Order.get_all ~sandbox ())
        >>= fun os ->
        Deferred.List.iter os ~f:(fun o ->
            Logs_async.app ~src (fun m ->
                m "%a" Sexp.pp (Coinbasepro_rest.Order.sexp_of_t o)))
    | "fills" :: pairs ->
        Fastrest.request ?auth:(Option.map ~f:snd auth)
          (Coinbasepro_rest.Fill.get ~sandbox
             (`ProductID (List.map ~f:Pair.of_string_exn pairs)))
        >>= fun fills ->
        Deferred.List.iter fills ~f:(fun fi ->
            Logs_async.app ~src (fun m ->
                m "%a" Sexp.pp (Coinbasepro_rest.Fill.sexp_of_t fi)))
    | "accounts" :: _ ->
        Fastrest.request ?auth:(Option.map ~f:snd auth)
          (Coinbasepro_rest.accounts ~sandbox ())
        >>= fun accounts ->
        Deferred.List.iter accounts ~f:(fun a ->
            Logs_async.app ~src (fun m ->
                m "%a" Sexp.pp (Coinbasepro_rest.sexp_of_account a)))
    | "ledger" :: id :: _ ->
        let id = Option.value_exn (Uuidm.of_string id) in
        Fastrest.request ?auth:(Option.map ~f:snd auth)
          (Coinbasepro_rest.ledger ~sandbox id)
        >>= fun entries ->
        Deferred.List.iter entries ~f:(fun a ->
            Logs_async.app ~src (fun m ->
                m "%a" Sexp.pp (Coinbasepro_rest.sexp_of_ledger a)))
    | _ -> Logs_async.err ~src (fun m -> m "Non Empty command")
  in
  let rec loop () =
    Reader.(read_line @@ Lazy.force stdin) >>= function
    | `Eof -> Deferred.unit
    | `Ok line -> process line >>= loop
  in
  loop ()

let key, secret, passphrase =
  match String.split ~on:':' (Sys.getenv_exn "TOKEN_CBPRO") with
  | [ key; secret; passphrase ] -> (key, Base64.decode_exn secret, passphrase)
  | _ -> assert false

let main sandbox =
  let auth =
    let timestamp =
      Time_ns.(
        now () |> to_span_since_epoch |> Span.to_int_ms |> fun a ->
        a // 1000 |> Float.to_string)
    in
    ( auth ~timestamp ~key ~secret ~passphrase,
      { Fastrest.key; secret; meta = [ ("passphrase", passphrase) ] } )
  in
  let url = if sandbox then url_sandbox else url in
  let module Encoding = Json_encoding.Make (Json_repr.Yojson) in
  let buf = Bi_outbuf.create 4096 in
  let of_string s =
    Encoding.destruct encoding (Yojson.Safe.from_string ~buf s)
  in
  let to_string t =
    Yojson.Safe.to_string ~buf (Encoding.construct encoding t)
  in
  Fastws_async.with_connection ~of_string ~to_string url (fun r w ->
      let log_incoming msg = Logs_async.debug ~src (fun m -> m "%a" pp msg) in
      Deferred.all_unit
        [ process_user_cmd ~sandbox ~auth w; Pipe.iter r ~f:log_incoming ])

let () =
  Command.async ~summary:"Coinbasepro WS client"
    (let open Command.Let_syntax in
    [%map_open
      let () = Logs_async_reporter.set_level_via_param []
      and sandbox = flag "sandbox" no_arg ~doc:" Use sandbox" in
      fun () ->
        Logs.set_reporter (Logs_async_reporter.reporter ());
        main sandbox])
  |> Command.run
