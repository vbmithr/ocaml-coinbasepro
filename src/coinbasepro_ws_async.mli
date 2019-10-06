open Async

open Coinbasepro_ws

val url : Uri.t
val sandbox_url : Uri.t

val connect : ?sandbox:bool -> unit ->
  (t Pipe.Reader.t * t Pipe.Writer.t * unit Deferred.t) Deferred.Or_error.t

val connect_exn : ?sandbox:bool -> unit ->
  (t Pipe.Reader.t * t Pipe.Writer.t * unit Deferred.t) Deferred.t

val with_connection : ?sandbox:bool ->
  (t Pipe.Reader.t -> t Pipe.Writer.t -> 'a Deferred.t) ->
  'a Deferred.Or_error.t

val with_connection_exn : ?sandbox:bool ->
  (t Pipe.Reader.t -> t Pipe.Writer.t -> 'a Deferred.t) -> 'a Deferred.t
