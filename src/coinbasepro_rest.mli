open Coinbasepro
open Fastrest

type product = {
  id: string ;
  base_currency: string ;
  quote_currency: string ;
  base_min_size: float ;
  base_max_size: float ;
  base_increment: float ;
  quote_increment: float ;
} [@@deriving sexp]

val pair_of_product : product -> Pair.t

val products :
  ?sandbox:bool -> unit -> (form, product list) service

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

val book : ?sandbox:bool -> Pair.t -> (form, book) service

type account = {
  id : Uuidm.t ;
  currency : string ;
  balance : float ;
  available : float ;
  hold : float ;
  profile_id : Uuidm.t ;
  trading_enabled: bool ;
} [@@deriving sexp]

val accounts :
  ?sandbox:bool -> unit -> (form, account list) service

