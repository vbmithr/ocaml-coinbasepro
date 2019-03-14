open Fastrest

type order = {
  price : float ;
  size : float ;
  order_id : Uuidm.t ;
} [@@deriving sexp]

type book = {
  sequence : int64 ;
  bids : order list ;
  asks : order list ;
} [@@deriving sexp]

val book : ?sandbox:bool -> string -> (get, book, string) service

type account = {
  id : Uuidm.t ;
  currency : string ;
  balance : float ;
  available : float ;
  hold : float ;
  profile_id : Uuidm.t ;
} [@@deriving sexp]

val accounts :
  ?sandbox:bool -> unit -> (get, account list, string) service

