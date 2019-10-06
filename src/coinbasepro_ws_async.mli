open Core
open Async

val url : Uri.t
val sandbox_url : Uri.t

type t = {
  r: Coinbasepro_ws.t Pipe.Reader.t ;
  w: Coinbasepro_ws.t Pipe.Writer.t ;
  cleaned_up: unit Deferred.t ;
}

val connect : Uri.t -> t Deferred.Or_error.t
val connect_exn : Uri.t -> t Deferred.t

val with_connection : ?sandbox:bool ->
  (Coinbasepro_ws.t Pipe.Reader.t ->
   Coinbasepro_ws.t Pipe.Writer.t -> 'a Deferred.t) ->
  'a Deferred.Or_error.t

val with_connection_exn : ?sandbox:bool ->
  (Coinbasepro_ws.t Pipe.Reader.t ->
   Coinbasepro_ws.t Pipe.Writer.t -> 'a Deferred.t) -> 'a Deferred.t

module Persistent : sig
  include Persistent_connection_kernel.S
    with type address = Uri.t
     and type conn = t

  val create' :
    server_name:string ->
    ?on_event:(Event.t -> unit Deferred.t) ->
    ?retry_delay:(unit -> Time_ns.Span.t) ->
    (unit -> address Or_error.t Deferred.t) -> t
end
