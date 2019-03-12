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

val book : ?sandbox:bool -> string -> (get, book, 'a) service
