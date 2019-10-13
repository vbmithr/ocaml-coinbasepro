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

type ledger = {
  id: int64 ;
  created_at: Ptime.t ;
  amount: float ;
  balance: float ;
  typ: ledger_type;
  details: details ;
}

and ledger_type =
  | Transfer
  | Match
  | Fee
  | Rebate
  | Conversion

and details = {
  order_id: Uuidm.t ;
  trade_id: int64 ;
  product_id: string ;
} [@@deriving sexp]

val ledger_encoding : ledger Json_encoding.encoding

val ledger :
  ?sandbox:bool -> Uuidm.t -> (form, ledger list) service

type hold = {
  id: Uuidm.t ;
  account_id: Uuidm.t ;
  created_at: Ptime.t ;
  updated_at: Ptime.t ;
  amount: float ;
  typ: hold_type;
  ref_id: Uuidm.t ;
}

and hold_type =
  | Order
  | Transfer

val hold :
  ?sandbox:bool -> Uuidm.t -> (form, hold list) service

type stp =
  | DecreaseAndCancel
  | CancelOldest
  | CancelNewest
  | CancelBoth

val stp_encoding : stp Json_encoding.encoding
