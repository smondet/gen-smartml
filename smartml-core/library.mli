(* Copyright 2019-2021 Smart Chain Arena LLC. *)

open Basics

val unit : line_no:line_no -> Expr.t

val open_some : line_no:line_no -> Expr.t -> Expr.t

val add_operations : line_no:line_no -> Expr.t -> Command.t

val transfer : line_no:line_no -> Expr.t -> Expr.t -> Expr.t -> Command.t

val send : line_no:line_no -> Expr.t -> Expr.t -> Command.t

val set_delegate : line_no:line_no -> Expr.t -> Command.t

val create_contract :
  line_no:line_no -> Expr.t -> Expr.t -> Expr.t -> contract -> Command.t

val tez : Big_int.big_int -> Literal.t

val map_function_full_stack : line_no:line_no -> Expr.t -> Expr.t -> Expr.t
