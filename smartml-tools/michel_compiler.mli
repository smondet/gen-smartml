(* Copyright 2019-2021 Smart Chain Arena LLC. *)

open Michelson

type stack =
  | Stack_ok     of string option list
  | Stack_failed
[@@deriving show]

val compile_expr : stack -> Michel.Expr.expr -> instr list * stack

val compile_contract :
     ?storage:literal
  -> ?lazy_entry_points:literal
  -> views:tinstr Michelson.view list
  -> Michel.Typing.checked_precontract
  -> Michelson.tcontract
