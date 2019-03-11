open Core
open Async

open Coinbasepro_ws

val with_connection :
  ?heartbeat:Time_ns.Span.t ->
  (t Pipe.Reader.t -> t Pipe.Writer.t -> 'a Deferred.t) -> 'a Deferred.t
