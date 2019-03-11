open Core
open Async

open Coinbasepro
open Coinbasepro_ws

let url = Uri.make ~scheme:"https" ~host:"ws-feed.pro.coinbase.com" ()

let with_connection ?heartbeat f =
  let hb_ns = Option.map heartbeat ~f:Time_ns.Span.to_int63_ns in
  Fastws_async.with_connection_ez ?hb_ns url ~f:begin fun r w ->
    let r = Pipe.map r ~f:(fun msg -> Ezjsonm.from_string msg) in
    let r =
      Pipe.map r ~f:(fun msg -> Ezjsonm_encoding.destruct_safe encoding msg) in
    let ws_read, client_write = Pipe.create () in
    don't_wait_for @@
    Pipe.transfer ws_read w ~f:begin fun cmd ->
      let doc =
        match Ezjsonm_encoding.construct encoding cmd with
        | `A _ | `O _ as a -> Ezjsonm.to_string a
        | _ -> invalid_arg "not a json document" in
      Logs.debug (fun m -> m "-> %s" doc) ;
      doc
    end ;
    f r client_write
  end
