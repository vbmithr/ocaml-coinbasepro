open Sexplib.Std
open Fastrest
open Coinbasepro

let base_url =
  Uri.make ~scheme:"https" ~host:"api.pro.coinbase.com" ()

let sandbox_url =
  Uri.make ~scheme:"https" ~host:"api-public.sandbox.pro.coinbase.com" ()

type order = {
  price : float ;
  size : float ;
  order_id : Uuidm.t ;
} [@@deriving sexp]

let order_encoding =
  let open Json_encoding in
  conv
    (fun { price ; size ; order_id } -> price, size, order_id)
    (fun (price, size, order_id) -> { price ; size ; order_id })
    (tup3 strfloat strfloat Uuidm.encoding)

type book = {
  sequence : int64 ;
  bids : order list ;
  asks : order list ;
} [@@deriving sexp]

(* let int64str =
 *   Json_encoding.(conv Int64.to_string Int64.of_string string) *)

let book_encoding =
  let open Json_encoding in
  conv
    (fun { sequence ; bids ; asks } -> (sequence, bids, asks))
    (fun (sequence, bids, asks) -> { sequence ; bids ; asks })
    (obj3
       (req "sequence" int53)
       (req "bids" (list order_encoding))
       (req "asks" (list order_encoding)))

let result_encoding encoding =
  Json_encoding.conv
    (function Ok v -> v | _ -> invalid_arg "result_encoding")
    (fun v -> Ok v) encoding

let book ?(sandbox=false) symbol =
  let url = if sandbox then sandbox_url else base_url in
  get (result_encoding book_encoding)
    (Uri.with_query'
       (Uri.with_path url ("/products/" ^ symbol ^ "/book"))
       ["level", "3"])
