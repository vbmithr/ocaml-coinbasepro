open Core
open Async

open Coinbasepro
open Coinbasepro_ws

let url = Uri.make ~scheme:"https" ~host:"ws-feed.pro.coinbase.com" ()
let sandbox_url = Uri.make ~scheme:"https" ~host:"ws-feed-public.sandbox.pro.coinbase.com" ()

let src = Logs.Src.create "cbpro.ws.async"
module Log = (val Logs.src_log src : Logs.LOG)

module T = struct
  type t = {
    r: Coinbasepro_ws.t Pipe.Reader.t ;
    w: Coinbasepro_ws.t Pipe.Writer.t ;
    cleaned_up: unit Deferred.t ;
  }

  let create r w cleaned_up = { r; w; cleaned_up }

  module Address = struct
    include Uri_sexp
    let equal = Uri.equal
  end

  let is_closed { r; _ } = Pipe.is_closed r
  let close { r; w; cleaned_up } =
    Pipe.close w ; Pipe.close_read r ; cleaned_up
  let close_finished { cleaned_up; _ } = cleaned_up
end
include T

let connect url =
  Deferred.Or_error.map (Fastws_async.EZ.connect url)
    ~f:begin fun { r; w; cleaned_up } ->
      let client_read = Pipe.map r ~f:begin fun msg ->
          Ezjsonm_encoding.destruct_safe encoding (Ezjsonm.from_string msg)
        end in
      let ws_read, client_write = Pipe.create () in
      don't_wait_for
        (Pipe.closed client_write >>| fun () -> Pipe.close w) ;
      don't_wait_for @@
      Pipe.transfer ws_read w ~f:begin fun cmd ->
        let doc =
          match Ezjsonm_encoding.construct encoding cmd with
          | `A _ | `O _ as a -> Ezjsonm.to_string a
          | _ -> invalid_arg "not a json document" in
        Log.debug (fun m -> m "-> %s" doc) ;
        doc
      end ;
      create client_read client_write cleaned_up
    end

module Persistent = struct
  include Persistent_connection_kernel.Make(T)

  let create' ~server_name ?on_event ?retry_delay =
    create ~server_name ?on_event ?retry_delay ~connect
end

let connect_exn url =
  connect url >>= function
  | Error e -> Error.raise e
  | Ok a -> return a

let with_connection ?(sandbox=false) f =
  let url = if sandbox then sandbox_url else url in
  Fastws_async.EZ.with_connection url ~f:begin fun r w ->
    let client_read = Pipe.map r ~f:begin fun msg ->
        Ezjsonm_encoding.destruct_safe encoding (Ezjsonm.from_string msg)
      end in
    let ws_read, client_write = Pipe.create () in
    don't_wait_for @@
    Pipe.transfer ws_read w ~f:begin fun cmd ->
      let doc =
        match Ezjsonm_encoding.construct encoding cmd with
        | `A _ | `O _ as a -> Ezjsonm.to_string a
        | _ -> invalid_arg "not a json document" in
      Log.debug (fun m -> m "-> %s" doc) ;
      doc
    end ;
    f client_read client_write
  end

let with_connection_exn ?sandbox f =
  with_connection ?sandbox f >>= function
  | Error e -> Error.raise e
  | Ok a -> return a
