module Ezjsonm_encoding : sig
  include module type of Json_encoding.Make(Json_repr.Ezjsonm)
  val destruct_safe : 'a Json_encoding.encoding -> Ezjsonm.value -> 'a
end

module Pair : sig
  type t = {
    base: string ;
    quote: string ;
  }  [@@deriving sexp]

  val create : base:string -> quote:string -> t
  val compare : t -> t -> int
  val equal : t -> t -> bool

  val pp : Format.formatter -> t -> unit
  val to_string : t -> string
  val of_string : string -> t option
  val of_string_exn : string -> t
  val encoding : t Json_encoding.encoding
end

module Ptime : sig
  include module type of Ptime
    with type t = Ptime.t
     and type span = Ptime.span

  include Sexplib0.Sexpable.S with type t = Ptime.t
  val encoding : t Json_encoding.encoding
end

module Uuidm : sig
  include module type of Uuidm
    with type t = Uuidm.t

  include Sexplib0.Sexpable.S with type t = Uuidm.t
  val encoding : t Json_encoding.encoding
end

open Fixtypes

val strfloat : float Json_encoding.encoding
val strint53 : int64 Json_encoding.encoding
val side_encoding : Side.t Json_encoding.encoding
val ord_type_encoding : OrdType.t Json_encoding.encoding
val time_in_force_encoding : TimeInForce.t Json_encoding.encoding
val ord_status_encoding : OrdStatus.t Json_encoding.encoding
val liquidity_encoding : LastLiquidityInd.t Json_encoding.encoding
