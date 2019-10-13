open Core
open Fastrest
open Coinbasepro
open Json_encoding

let base_url =
  Uri.make ~scheme:"https" ~host:"api.pro.coinbase.com" ()

let sandbox_url =
  Uri.make ~scheme:"https" ~host:"api-public.sandbox.pro.coinbase.com" ()

type product = {
  id: string ;
  base_currency: string ;
  quote_currency: string ;
  base_min_size: float ;
  base_max_size: float ;
  base_increment: float ;
  quote_increment: float ;
} [@@deriving sexp]

let pair_of_product { base_currency ; quote_currency ; _ } =
  { Pair.base = base_currency ; quote = quote_currency }

let result_encoding encoding =
  union [
    case
      (obj1 (req "message" string))
      (function Ok _ -> None | Error msg -> Some (Error.to_string_hum msg))
      (fun msg -> Error (Error.of_string msg)) ;
    case encoding
      (function Ok v -> Some v | _ -> None)
      (fun v -> Ok v) ;
  ]

let product_encoding =
  conv
    (fun { id; base_currency; quote_currency; base_min_size; base_max_size;
           base_increment; quote_increment } ->
      (),
      (id, base_currency, quote_currency, base_min_size, base_max_size,
       base_increment, quote_increment))
    (fun ((),
          (id, base_currency, quote_currency, base_min_size, base_max_size,
           base_increment, quote_increment)) ->
      { id; base_currency; quote_currency; base_min_size; base_max_size;
        base_increment; quote_increment })
    (merge_objs unit
       (obj7
          (req "id" string)
          (req "base_currency" string)
          (req "quote_currency" string)
          (req "base_min_size" strfloat)
          (req "base_max_size" strfloat)
          (req "base_increment" strfloat)
          (req "quote_increment" strfloat)))

let products ?(sandbox=false) () =
  let url = if sandbox then sandbox_url else base_url in
  get
    (result_encoding (list product_encoding))
    (Uri.with_path url "products")

type order = {
  price : float ;
  size : float ;
  order_id : Uuidm.t ;
} [@@deriving sexp]

let order_encoding =
  conv
    (fun { price ; size ; order_id } -> price, size, order_id)
    (fun (price, size, order_id) -> { price ; size ; order_id })
    (tup3 strfloat strfloat Uuidm.encoding)

type book = {
  sequence : int64 ;
  bids : order list ;
  asks : order list ;
} [@@deriving sexp]

let int64str = conv Int64.to_string Int64.of_string string

let book_encoding =
  conv
    (fun { sequence ; bids ; asks } -> (sequence, bids, asks))
    (fun (sequence, bids, asks) -> { sequence ; bids ; asks })
    (obj3
       (req "sequence" int53)
       (req "bids" (list order_encoding))
       (req "asks" (list order_encoding)))

let book ?(sandbox=false) pair =
  let url = if sandbox then sandbox_url else base_url in
  get (result_encoding book_encoding)
    (Uri.with_query'
       (Uri.with_path url ("products/" ^ Pair.to_string pair ^ "/book"))
       ["level", "3"])

type account = {
  id : Uuidm.t ;
  currency : string ;
  balance : float ;
  available : float ;
  hold : float ;
  profile_id : Uuidm.t ;
  trading_enabled: bool ;
} [@@deriving sexp]

let account_encoding =
  conv
    (fun { id ; currency ; balance ; available ; hold ; profile_id; trading_enabled } ->
       (id, currency, balance, available, hold, profile_id, trading_enabled))
    (fun (id, currency, balance, available, hold, profile_id, trading_enabled) ->
       { id ; currency ; balance ; available ; hold ; profile_id ; trading_enabled })
    (obj7
       (req "id" Uuidm.encoding)
       (req "currency" string)
       (req "balance" strfloat)
       (req "available" strfloat)
       (req "hold" strfloat)
       (req "profile_id" Uuidm.encoding)
       (req "trading_enabled" bool))

let auth (type a) (srv : (a, _) service) { key ; secret ; meta } =
  let ts =
    Float.to_string @@
    Time_ns.(Span.to_int_ms (to_span_since_epoch (now ())) // 1000) in
  let meth = match srv.meth with
    | `GET -> "GET"
    | #Httpaf.Method.t -> "POST" in
  let path = Uri.path srv.url in
  let body = match body_hdrs_of_service srv with
    | None -> ""
    | Some (_, body) -> body in
  let prehash = ts ^ meth ^ path ^ body in
  let passphrase =
    List.Assoc.find_exn meta "passphrase" ~equal:String.equal in
  let sign = Base64.encode_exn
      Digestif.SHA256.(hmac_string ~key:secret prehash |> to_raw_string) in
  let headers = Httpaf.Headers.of_list [
      "CB-ACCESS-KEY", key ;
      "CB-ACCESS-SIGN", sign ;
      "CB-ACCESS-TIMESTAMP", ts ;
      "CB-ACCESS-PASSPHRASE", passphrase ;
    ] in
  { params = Form [] ; headers }

let accounts ?(sandbox=false) () =
  let url = if sandbox then sandbox_url else base_url in
  get ~auth
    (result_encoding (list account_encoding))
    (Uri.with_path url "accounts")

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

let ledger_type_encoding =
  string_enum [
    "transfer", (Transfer : ledger_type) ;
    "match", Match ;
    "fee", Fee ;
    "rebate", Rebate ;
    "conversion", Conversion
  ]

let details_encoding =
  conv
    (fun { order_id; trade_id; product_id } -> (order_id, trade_id, product_id))
    (fun (order_id, trade_id, product_id) -> { order_id; trade_id; product_id })
    (obj3
       (req "order_id" Uuidm.encoding)
       (req "trade_id" int64str)
       (req "product_id" string))

let ledger_encoding =
  conv
    (fun { id; created_at; amount; balance; typ; details } ->
       (id, created_at, amount, balance, typ, details))
    (fun (id, created_at, amount, balance, typ, details) ->
       { id; created_at; amount; balance; typ; details })
    (obj6
       (req "id" int53)
       (req "created_at" Ptime.encoding)
       (req "amount" strfloat)
       (req "balance" strfloat)
       (req "type" ledger_type_encoding)
       (req "details" details_encoding))

let ledger ?(sandbox=false) account_id =
  let url = if sandbox then sandbox_url else base_url in
  get ~auth
    (result_encoding (list ledger_encoding))
    (Format.kasprintf (Uri.with_path url) "accounts/%a/ledger" Uuidm.pp account_id)

let hold_type_encoding =
  string_enum [
    "transfer", (Transfer : hold_type) ;
    "order", Order ;
  ]

let hold_encoding =
  conv
    (fun { id; account_id; created_at; updated_at; amount; typ; ref_id } ->
       (id, account_id, created_at, updated_at, amount, typ, ref_id))
    (fun (id, account_id, created_at, updated_at, amount, typ, ref_id) ->
       { id; account_id; created_at; updated_at; amount; typ; ref_id })
    (obj7
       (req "id" Uuidm.encoding)
       (req "account_id" Uuidm.encoding)
       (req "created_at" Ptime.encoding)
       (req "updated_at" Ptime.encoding)
       (req "amount" strfloat)
       (req "type" hold_type_encoding)
       (req "ref" Uuidm.encoding))

let hold ?(sandbox=false) account_id =
  let url = if sandbox then sandbox_url else base_url in
  get ~auth
    (result_encoding (list hold_encoding))
    (Format.kasprintf (Uri.with_path url) "accounts/%a/holds" Uuidm.pp account_id)

type stp =
  | DecreaseAndCancel
  | CancelOldest
  | CancelNewest
  | CancelBoth

let stp_encoding =
  string_enum [
    "dc", DecreaseAndCancel ;
    "co", CancelOldest ;
    "cn", CancelNewest ;
    "cb", CancelBoth ;
  ]
