open Sexplib.Std
open Coinbasepro

type channel =
  | Ticker
  | Level2
  | User
  | Matches
  | Full
[@@deriving sexp]

let channel_encoding =
  let open Json_encoding in
  string_enum [
    "ticker", Ticker ;
    "level2", Level2 ;
    "user", User ;
    "matches", Matches ;
    "full", Full ;
  ]

type channel_full = {
  chan: channel ;
  product_ids : string list ;
} [@@deriving sexp]

let full product_ids =
  { chan = Full ; product_ids }

let level2 product_ids =
  { chan = Level2 ; product_ids }

let channel_full_encoding =
  let open Json_encoding in
  conv
    (fun { chan ; product_ids } -> chan, product_ids)
    (fun (chan, product_ids) -> { chan ; product_ids })
    (obj2
       (req "name" channel_encoding)
       (req "product_ids" (list string)))

let channel_full_encoding =
  let open Json_encoding in
  union [
    case channel_full_encoding
      (fun c -> Some c) (fun c -> c) ;
    case channel_encoding
      (fun { chan ; _ } -> Some chan)
      (fun chan -> { chan ; product_ids = [] })
  ]

type auth = {
  key : string ;
  passphrase : string [@opaque] ;
  timestamp : string ;
  signature : string ;
} [@@deriving sexp]

let auth_encoding =
  let open Json_encoding in
  conv
    (fun { key ; passphrase ; timestamp ; signature } ->
       (key, passphrase, timestamp, signature))
    (fun (key, passphrase, timestamp, signature) ->
       { key ; passphrase ; timestamp ; signature })
    (obj4
       (req "key" string)
       (req "passphrase" string)
       (req "timestamp" string)
       (req "signature" string))

let auth ~timestamp ~key ~secret ~passphrase =
  let meth = "GET" in
  let path = "/users/self/verify" in
  let prehash = timestamp ^ meth ^ path in
  let signature = Base64.encode_exn
      Digestif.SHA256.(hmac_string ~key:secret prehash |> to_raw_string) in
  { key ; passphrase ; timestamp ; signature }

let subscription_encoding =
  let open Json_encoding in
  conv
    (fun chans -> ([], chans))
    (fun (_, a) -> a)
    (obj2
       (dft "product_ids" (list string) [])
       (req "channels" (list channel_full_encoding)))

let authed_subscription_encoding =
  let open Json_encoding in
  union [
    case subscription_encoding
      (function (None, a) -> Some a | _ -> None)
      (fun a -> (None, a)) ;
    case (merge_objs auth_encoding subscription_encoding)
      (function (Some auth, a) -> Some (auth, a) | _ -> None)
      (fun (auth, s) -> Some auth, s) ;
  ]

type order = {
  ts : Ptime.t ;
  product_id : string ;
  sequence : int64 ;
  order_id : Uuidm.t ;
  client_oid : Uuidm.t option ;
  size : float option ;
  remaining_size : float option ;
  price : float option ;
  side : [`buy|`sell] ;
  ord_type : [`limit|`market] option ;
  ord_status : [`filled|`canceled] option ;
  funds : float option ;
} [@@deriving sexp]

let side_encoding =
  let open Json_encoding in
  string_enum [
    "buy", `buy ;
    "sell", `sell ;
  ]

let ord_type_encoding =
  let open Json_encoding in
  string_enum [
    "limit", `limit ;
    "market", `market ;
  ]

let ord_status_encoding =
  let open Json_encoding in
  string_enum [
    "filled", `filled ;
    "canceled", `canceled ;
  ]

let or_empty_string encoding =
  let open Json_encoding in
  union [
    case (constant "") (fun _ -> None) (fun _ -> None) ;
    case encoding (fun a -> a) (fun a -> Some a) ;
  ]

let order_encoding =
  let open Json_encoding in
  conv
    (fun { ts ; product_id ; sequence ; order_id ; client_oid ;
           size ; remaining_size ; price ; side ; ord_type ; ord_status ; funds} ->
      ((ts, product_id, sequence, order_id, size,
        remaining_size, price, side, ord_type, ord_status), (client_oid, funds)))
    (fun ((ts, product_id, sequence, order_id, size,
           remaining_size, price, side, ord_type, ord_status), (client_oid, funds)) ->
      { ts ; product_id ; sequence ; order_id ; client_oid ;
        size ; remaining_size ; price ; side ; ord_type ; ord_status ; funds })
    (merge_objs
       (obj10
          (req "time" Ptime.encoding)
          (req "product_id" string)
          (req "sequence" int53)
          (req "order_id" Uuidm.encoding)
          (opt "size" strfloat)
          (opt "remaining_size" strfloat)
          (opt "price" strfloat)
          (req "side" side_encoding)
          (opt "order_type" ord_type_encoding)
          (opt "reason" ord_status_encoding))
       (obj2
          (dft "client_oid" (or_empty_string Uuidm.encoding) None)
          (opt "funds" strfloat)))

type ord_match = {
  ts : Ptime.t ;
  product_id : string ;
  sequence : int64 ;
  trade_id : int64 ;
  maker_order_id : Uuidm.t ;
  taker_order_id : Uuidm.t ;
  side : [`buy|`sell] ;
  size : float ;
  price : float ;
} [@@deriving sexp]

let ord_match_encoding =
  let open Json_encoding in
  conv
    (fun { ts ; product_id ; sequence ; trade_id ;
           maker_order_id ; taker_order_id ; side ; size ; price } ->
      (ts, product_id, sequence, trade_id, maker_order_id,
       taker_order_id, side, size, price))
    (fun (ts, product_id, sequence, trade_id, maker_order_id,
          taker_order_id, side, size, price) ->
      { ts ; product_id ; sequence ; trade_id ;
        maker_order_id ; taker_order_id ; side ; size ; price })
    (obj9
       (req "time" Ptime.encoding)
       (req "product_id" string)
       (req "sequence" int53)
       (req "trade_id" int53)
       (req "maker_order_id" Uuidm.encoding)
       (req "taker_order_id" Uuidm.encoding)
       (req "side" side_encoding)
       (req "size" strfloat)
       (req "price" strfloat))

type change = {
  ts : Ptime.t ;
  sequence : int64 ;
  order_id : Uuidm.t ;
  product_id : string ;
  new_size : float option ;
  old_size : float option ;
  new_funds : float option ;
  old_funds : float option ;
  price : float option ;
  side : [`buy|`sell] ;
} [@@deriving sexp]

let change_encoding =
  let open Json_encoding in
  conv
    (fun { ts ; sequence ; order_id ; product_id ; new_size ;
           old_size ; new_funds ; old_funds ; price ; side } ->
      (ts, sequence, order_id, product_id, new_size, old_size,
       new_funds, old_funds, price, side))
    (fun (ts, sequence, order_id, product_id, new_size, old_size,
          new_funds, old_funds, price, side) ->
      { ts ; sequence ; order_id ; product_id ; new_size ;
        old_size ; new_funds ; old_funds ; price ; side })
    (obj10
       (req "time" Ptime.encoding)
       (req "sequence" int53)
       (req "order_id" Uuidm.encoding)
       (req "product_id" string)
       (opt "new_size" strfloat)
       (opt "old_size" strfloat)
       (opt "new_funds" strfloat)
       (opt "old_funds" strfloat)
       (opt "price" strfloat)
       (req "side" side_encoding))

type l2snapshot = {
  product_id : string ;
  bids : (float * float) list ;
  asks : (float * float) list ;
} [@@deriving sexp]

let l2snapshot_encoding =
  let open Json_encoding in
  conv
    (fun { product_id ; bids ; asks } -> (product_id, bids, asks))
    (fun (product_id, bids, asks) -> { product_id ; bids ; asks })
    (obj3
       (req "product_id" string)
       (req "bids" (list (tup2 strfloat strfloat)))
       (req "asks" (list (tup2 strfloat strfloat))))

type l2update = {
  ts : Ptime.t ;
  product_id : string ;
  changes : ([`buy|`sell] * float * float) list ;
} [@@deriving sexp]

let l2update_encoding =
  let open Json_encoding in
  conv
    (fun { ts ; product_id ; changes } -> (ts, product_id, changes))
    (fun (ts, product_id, changes) -> { ts ; product_id ; changes })
    (obj3
       (req "time" Ptime.encoding)
       (req "product_id" string)
       (req "changes" (list (tup3 side_encoding strfloat strfloat))))

type error = {
  msg : string ;
  reason : string option ;
} [@@deriving sexp]

let error_encoding =
  let open Json_encoding in
  conv
    (fun { msg ; reason } -> (msg, reason))
    (fun (msg, reason) -> { msg ; reason })
    (obj2
       (req "message" string)
       (opt "reason" string))

type t =
  | Subscribe of auth option * channel_full list
  | Unsubscribe of channel_full list
  | Subscriptions of channel_full list
  | Received of order
  | Done of order
  | Open of order
  | Match of ord_match
  | Change of change
  | L2Snapshot of l2snapshot
  | L2Update of l2update
  | Error of error
[@@deriving sexp]

let subscribe_full ?auth product_ids =
  Subscribe (auth, [full product_ids])

let subscribe_level2 ?auth product_ids =
  Subscribe (auth, [level2 product_ids])

let is_ctrl_msg = function
  | Subscribe _
  | Unsubscribe _
  | Subscriptions _ -> true
  | _ -> false

let has_seq_gt seq = function
  | Error _
  | Subscribe _
  | Unsubscribe _
  | Subscriptions _
  | L2Snapshot _
  | L2Update _ -> false
  | Received { sequence ; _ }
  | Done { sequence ; _}
  | Open { sequence ; _ }
  | Match { sequence ; _ }
  | Change { sequence ; _ } ->
    sequence > seq

let pp ppf t =
  Format.fprintf ppf "%a" Sexplib.Sexp.pp (sexp_of_t t)

let encoding =
  let open Json_encoding in
  let sub_e =
    merge_objs (obj1 (req "type" (constant "subscribe")))
      authed_subscription_encoding in
  let unsub_e =
    merge_objs (obj1 (req "type" (constant "unsubscribe")))
      subscription_encoding in
  let subs_e =
    merge_objs (obj1 (req "type" (constant "subscriptions")))
      subscription_encoding in
  let received_e =
    merge_objs (obj1 (req "type" (constant "received")))
      order_encoding in
  let done_e =
    merge_objs (obj1 (req "type" (constant "done")))
      order_encoding in
  let open_e =
    merge_objs (obj1 (req "type" (constant "open")))
      order_encoding in
  let match_e =
    merge_objs (obj1 (req "type" (constant "match")))
      ord_match_encoding in
  let change_e =
    merge_objs (obj1 (req "type" (constant "change")))
      change_encoding in
  let snapshot_e =
    merge_objs (obj1 (req "type" (constant "snapshot")))
      l2snapshot_encoding in
  let l2update_e =
    merge_objs (obj1 (req "type" (constant "l2update")))
      l2update_encoding in
  let error_e =
    merge_objs (obj1 (req "type" (constant "error")))
      error_encoding in
  union [
    case sub_e (function Subscribe (a, t) -> Some ((), (a, t)) | _ -> None) (fun ((), (a, t)) -> Subscribe (a, t)) ;
    case unsub_e (function Unsubscribe t -> Some ((), t) | _ -> None) (fun ((), t) -> Unsubscribe t) ;
    case subs_e (function Subscriptions t -> Some ((), t) | _ -> None) (fun ((), t) -> Subscriptions t) ;
    case received_e (function Received t -> Some ((), t) | _ -> None) (fun ((), t) -> Received t) ;
    case done_e (function Done t -> Some ((), t) | _ -> None) (fun ((), t) -> Done t) ;
    case open_e (function Open t -> Some ((), t) | _ -> None) (fun ((), t) -> Open t) ;
    case match_e (function Match t -> Some ((), t) | _ -> None) (fun ((), t) -> Match t) ;
    case change_e (function Change t -> Some ((), t) | _ -> None) (fun ((), t) -> Change t) ;
    case snapshot_e (function L2Snapshot t -> Some ((), t) | _ -> None) (fun ((), t) -> L2Snapshot t) ;
    case l2update_e (function L2Update t -> Some ((), t) | _ -> None) (fun ((), t) -> L2Update t) ;
    case error_e (function Error t -> Some ((), t) | _ -> None) (fun ((), t) -> Error t) ;
  ]

