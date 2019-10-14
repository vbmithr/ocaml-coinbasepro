open Sexplib.Std

module Pair = struct
  type t = {
    base: string ;
    quote: string ;
  } [@@deriving sexp]

  let create ~base ~quote = { base ; quote }

  let compare { base ; quote } { base = base' ; quote = quote' } =
    match String.compare base base' with
    | 0 -> String.compare quote quote'
    | n -> n

  let equal a b = compare a b = 0

  let pp ppf { base ; quote } =
    Format.fprintf ppf "%s-%s" base quote

  let to_string { base ; quote } =
    base ^ "-" ^ quote

  let of_string s =
    match String.split_on_char '-' s with
    | [base ; quote] -> Some { base ; quote }
    | _ -> None

  let of_string_exn s =
    match String.split_on_char '-' s with
    | [base ; quote] -> { base ; quote }
    | _ -> invalid_arg "pair_of_string_exn"

  let encoding =
    Json_encoding.(conv to_string of_string_exn string)
end

module Ezjsonm_encoding = struct
  include Json_encoding.Make(Json_repr.Ezjsonm)

  let destruct_safe encoding value =
    try destruct encoding value with exn ->
      Format.eprintf "%a@."
        (Json_encoding.print_error ?print_unknown:None) exn ;
      raise exn
end

module Ptime = struct
  include Ptime

  let t_of_sexp sexp =
    let sexp_str = string_of_sexp sexp in
    match of_rfc3339 sexp_str with
    | Ok (t, _, _) -> t
    | _ -> invalid_arg "Ptime.t_of_sexp"

  let sexp_of_t t =
    sexp_of_string (to_rfc3339 t)

  let encoding =
    let open Json_encoding in
    conv
      (fun t -> Ptime.to_rfc3339 t)
      (fun ts -> match Ptime.of_rfc3339 ts with
         | Error _ -> invalid_arg "Ptime.encoding"
         | Ok (t, _, _) -> t)
      string
end

module Uuidm = struct
  include Uuidm

  let t_of_sexp sexp =
    let sexp_str = string_of_sexp sexp in
    match of_string sexp_str with
    | None -> invalid_arg "Uuidm.t_of_sexp"
    | Some u -> u

  let sexp_of_t t =
    sexp_of_string (to_string t)

  let encoding =
    let open Json_encoding in
    conv
      (fun u -> to_string u)
      (fun s -> match of_string s with
         | None -> invalid_arg "Uuidm.encoding"
         | Some u -> u)
      string
end

open Json_encoding
open Fixtypes

let strfloat =
  union [
    case float (fun s -> Some s) (fun s -> s) ;
    case string (fun s -> Some (string_of_float s)) float_of_string ;
  ]

let side_encoding =
  string_enum [
    "buy", Side.Buy ;
    "sell", Side.Sell ;
  ]

let ord_type_encoding =
  string_enum [
    "limit", OrdType.Limit ;
    "market", OrdType.Market ;
  ]

let time_in_force_encoding =
  string_enum [
    "GTC", TimeInForce.GoodTillCancel ;
    "GTT", GoodTillDate ;
    "IOC", ImmediateOrCancel ;
    "FOK", FillOrKill
  ]

let ord_status_encoding =
  string_enum [
    "open", OrdStatus.New
  ]

let liquidity_encoding =
  string_enum [
    "M", LastLiquidityInd.AddedLiquidity ;
    "T", LastLiquidityInd.RemovedLiquidity
  ]
