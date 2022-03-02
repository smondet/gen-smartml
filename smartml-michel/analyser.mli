(* Copyright 2019-2021 Smart Chain Arena LLC. *)

open Utils
open Expr

type 'a var_count_mode

val mode_occurrences : int var_count_mode

val mode_consumptions : [ `Zero | `One | `Unknown ] var_count_mode

val is_linear : expr -> bool

val loops_closed : expr -> bool

val bindings_used : expr -> bool

val bindings_unique : expr -> bool

val free_vars_f : String.Set.t Expr.expr_f -> String.Set.t

val count_free_vars_f :
  'a var_count_mode -> 'a String.Map.t Expr.expr_f -> 'a String.Map.t

val count_free_vars : 'a var_count_mode -> Expr.expr -> 'a String.Map.t

val may_fail : expr -> bool

val may_fail_typed_f : (bool * Type.tys) Expr.expr_f -> bool

val always_fails : expr -> bool
