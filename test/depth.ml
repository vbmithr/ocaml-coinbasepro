open Core
open Async

open Coinbasepro_rest
open Coinbasepro_ws
open Coinbasepro_ws_async

let src = Logs.Src.create "coinbasepro.depth"
    ~doc:"Coinbasepro API - depth test application"

(* module UuidMap = Map.Make(Coinbasepro.Uuidm) *)

let main symbols =
  with_connection begin fun r w ->
    let obids = ref Float.Map.empty in
    let oasks = ref Float.Map.empty in
    let msgq = Queue.create () in
    let start_init = Ivar.create () in
    let inited = Ivar.create () in
    let update_books = function
      | Error _
      | Heartbeat _
      | Subscribe _
      | Unsubscribe _
      | Subscriptions _
      | L2Snapshot _
      | L2Update _ -> assert false
      | Received _ -> ()
      | Done _ -> ()
      | Open _ -> ()
      | LastMatch _ -> ()
      | Match _ -> ()
      | Change _ -> ()
    in
    let init_books () =
      Ivar.read start_init >>= fun () ->
      Fastrest.request (book (List.hd_exn symbols)) >>= function
      | Error _ -> failwith "could not get orderbook"
      | Ok { sequence ; bids ; asks } ->
        (* Discard msgs from msgq where sequence is leq than snapshot
           sequence *)
        let msgq = Queue.filter msgq ~f:(has_seq_gt sequence) in
        let bids' = List.fold_left bids ~init:Float.Map.empty ~f:begin fun a ({ price ; _ } as data) ->
          Float.Map.add_multi a ~key:price ~data
          end in
        let asks' = List.fold_left asks ~init:Float.Map.empty ~f:begin fun a ({ price ; _ } as data) ->
          Float.Map.add_multi a ~key:price ~data
          end in
        obids := bids' ;
        oasks := asks' ;
        Queue.iter msgq ~f:update_books ;
        Ivar.fill inited () ;
        Deferred.unit in
    let process_msgs = function
      | Subscribe _
      | Unsubscribe _ -> Deferred.unit
      | Subscriptions _ -> init_books ()
      | m ->
        if Ivar.is_empty inited then
          Queue.enqueue msgq m
        else update_books m ;
        Deferred.unit
    in
    Pipe.write w (Subscribe (None, [full symbols])) >>= fun () ->
    Deferred.all_unit [
      Pipe.iter r ~f:process_msgs
    ]
  end

let () =
  Command.async ~summary:"Coinbasepro WS client" begin
    let open Command.Let_syntax in
    [%map_open
      let () = Logs_async_reporter.set_level_via_param None
      and symbols = anon (sequence ("symbols" %: string)) in
      fun () ->
        Logs.set_reporter (Logs_async_reporter.reporter ()) ;
        main symbols
    ] end |>
  Command.run
