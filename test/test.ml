open Core
open Async

open Coinbasepro_rest

let auth =
  match String.split ~on:':' (Sys.getenv_exn "TOKEN_CBPRO") with
  | [key; secret; passphrase] -> {
      Fastrest.key ;
      secret = Base64.decode_exn secret ;
      meta = ["passphrase", passphrase] ;
    }
  | _ -> assert false

let wrap_request
    ?(timeout=Time.Span.of_int_sec 5)
    ?(speed=`Quick) n service =
  Alcotest_async.test_case ~timeout n speed begin fun () ->
    Deferred.ignore_m (Fastrest.request ~auth service)
  end

let wrap_request_light
    ?(timeout=Time.Span.of_int_sec 5)
    ?(speed=`Quick) n f =
  Alcotest_async.test_case ~timeout n speed begin fun () ->
    f () |>
    Deferred.Or_error.ignore |>
    Deferred.Or_error.ok_exn
  end

let test_later = ref []

let accounts_full () =
  Fastrest.request ~auth (accounts ~sandbox:true ()) >>= fun accts ->
  test_later := [
    "ledgers", (List.map accts ~f:begin fun a ->
        wrap_request ~timeout:(Time.Span.of_int_sec 10)
          (Format.asprintf "%a" Uuidm.pp a.id)
          (ledger ~sandbox:true a.id) end) ;
    "holds", (List.map accts ~f:begin fun a ->
        wrap_request ~timeout:(Time.Span.of_int_sec 10)
          (Format.asprintf "%a" Uuidm.pp a.id)
          (hold ~sandbox:true a.id) end) ;
  ] ;
  Deferred.Or_error.return accts

let rest = [
  wrap_request "book"
    (book ~sandbox:true
       (Coinbasepro.Pair.create ~base:"BTC" ~quote:"USD")) ;
  wrap_request_light "accounts" accounts_full ;
]

let () =
  Logs.set_reporter (Logs_async_reporter.reporter ()) ;
  Logs.set_level (Some Debug) ;
  Alcotest.run ~and_exit:false "coinbasepro" [
    "rest", rest ;
  ] ;
  Alcotest.run "cbpro_later" !test_later
