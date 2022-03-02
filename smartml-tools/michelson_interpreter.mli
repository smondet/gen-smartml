(* Copyright 2019-2021 Smart Chain Arena LLC. *)

open SmartML
open Utils
open Michelson
open Primitives

type maddress = string [@@deriving show]

type 'v mvalue_f =
  | Unit
  | Bool      of bool
  | Int       of tezos_int
  | String    of string
  | Operation of 'v moperation
  | Address   of maddress
  | Pair      of 'v * 'v
  | None_
  | Some_     of 'v
  | Left      of 'v
  | Right     of 'v
  | List      of 'v list
  | Set       of 'v list
  | Map       of ('v * 'v) list
  | Closure   of tinstr * 'v list

and 'v moperation =
  | Transfer_tokens of mtype * 'v * Bigint.t * maddress
  | Set_delegate    of string option
  | Create_contract of
      { code : instr
      ; balance : Bigint.t
      ; baker : string option
      ; tparameter : mtype
      ; tstorage : mtype
      ; storage : 'v }
[@@deriving show, map]

type mvalue = {mv : mvalue mvalue_f} [@@deriving show]

type tmvalue =
  { t : mtype
  ; tv : tmvalue mvalue_f }
[@@deriving show {with_path = false}]

val mvalue_of_value : Type.t -> Basics.value -> mvalue

val value_of_mvalue : Config.t -> Type.t -> mvalue -> Basics.value

val value_of_tmvalue : Config.t -> tmvalue -> Basics.value

val typecheck_mvalue : Michelson.mtype -> mvalue -> tmvalue

type context =
  { config : Config.t
  ; amount : Bigint.t
  ; balance : Bigint.t }

type 'a result =
  | Ok     of 'a
  | Failed of tmvalue
  | Error  of string

module M (P : Primitives) : sig
  val interpret : context -> tinstr -> mvalue list -> mvalue list result
end
