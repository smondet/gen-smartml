(* Copyright 2019-2021 Smart Chain Arena LLC. *)

open Utils
open Control
open Expr

type checked_precontract = private {checked_precontract : texpr precontract}
[@@deriving eq]

val print_checked_precontract : Format.formatter -> checked_precontract -> unit

val show_checked_precontract : checked_precontract -> string

val typecheck_precontract : expr precontract -> checked_precontract Result.t

val typecheck :
     tparameter:Type.ty * string option
  -> (string * Type.ty) list
  -> Expr.expr
  -> Expr.texpr Result.t
