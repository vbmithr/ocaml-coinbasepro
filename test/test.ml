open Core
open Async

open Coinbasepro
open Coinbasepro_rest

module Cfg = struct
  type cfg = {
    key: string ;
    secret: string ;
    passphrase: string [@default ""];
    quote: (string * int) list [@default []];
  } [@@deriving sexp]

  type t = (string * cfg) list [@@deriving sexp]
end

let default_cfg = Filename.concat (Option.value_exn (Sys.getenv "HOME")) ".virtu"
let cfg =
  List.Assoc.find_exn ~equal:String.equal
    (Sexplib.Sexp.load_sexp_conv_exn default_cfg Cfg.t_of_sexp) "CBPRO"

let () =
  Logs.set_reporter (Logs_async_reporter.reporter ()) ;
  Logs.set_level (Some Debug)

let auth = {
  Fastrest.key = cfg.Cfg.key ;
  secret = Base64.decode_exn cfg.Cfg.secret ;
  meta = ["passphrase", cfg.Cfg.passphrase] ;
}

let wrap_request ?(speed=`Quick) n service =
  Alcotest_async.test_case n speed begin fun () ->
    (Fastrest.request ~auth service) |>
    Deferred.Or_error.ignore |>
    Deferred.Or_error.ok_exn
  end

let wrap_request_light ?(speed=`Quick) n f =
  Alcotest_async.test_case ~timeout:(Time.Span.of_int_sec 10) n speed begin fun () ->
    f () |>
    Deferred.Or_error.ignore |>
    Deferred.Or_error.ok_exn
  end

let accounts_full () =
  Fastrest.request ~auth (accounts ~sandbox:true ()) >>=?
  Deferred.Or_error.List.map ~f:begin fun (a:account) ->
    Fastrest.request ~auth (ledger ~sandbox:true a.id) >>=? fun _ ->
    Fastrest.request ~auth (hold ~sandbox:true a.id)
  end

let rest = [
  wrap_request "book" (book ~sandbox:true (Pair.create ~base:"BTC" ~quote:"USD")) ;
  wrap_request_light "accounts" (accounts_full)
]

let () =
  Alcotest.run "coinbasepro" [
    "rest", rest ;
  ]
