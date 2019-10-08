open Core
open Async

open Coinbasepro
open Coinbasepro_ws

let src = Logs.Src.create "coinbasepro.depth2"
    ~doc:"Coinbasepro API - depth2 test application"

let main symbols =
  Coinbasepro_ws_async.with_connection_exn begin fun r w ->
    let obids = ref Float.Map.empty in
    let oasks = ref Float.Map.empty in
    let process_msgs msg =
      Logs_async.debug ~src (fun m -> m "%a" pp msg) >>= fun () ->
      match msg with
      | L2Snapshot { bids; asks; _ } ->
        obids := List.fold_left bids
            ~init:Float.Map.empty ~f:begin fun a { price; size } ->
            Float.Map.set a ~key:price ~data:size
          end ;
        oasks := List.fold_left asks
            ~init:Float.Map.empty ~f:begin fun a { price; size } ->
            Float.Map.set a ~key:price ~data:size
          end ;
        Deferred.unit
      | L2Update { changes ; _ } ->
        List.iter changes ~f:begin function
          | `Buy, { price; size } ->
            obids := Float.Map.set !obids ~key:price ~data:size
          | `Sell, { price; size } ->
            oasks := Float.Map.set !oasks ~key:price ~data:size
        end ;
        Deferred.unit
      | _ -> Deferred.unit
    in
    Pipe.write w (Subscribe (None, [level2 symbols])) >>= fun () ->
    Deferred.all_unit [
      Pipe.iter r ~f:process_msgs
    ]
  end

let () =
  Command.async ~summary:"Coinbasepro depth2 application" begin
    let pair =
      Command.(Arg_type.map Param.string ~f:Pair.of_string_exn) in
    let open Command.Let_syntax in
    [%map_open
      let () = Logs_async_reporter.set_level_via_param []
      and symbols = anon (sequence ("symbols" %: pair)) in
      fun () ->
        Logs.set_reporter (Logs_async_reporter.reporter ()) ;
        main symbols
    ] end |>
  Command.run
