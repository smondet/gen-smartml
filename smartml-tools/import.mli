(* Copyright 2019-2021 Smart Chain Arena LLC. *)

open SmartML
open Basics
open Untyped

type env

val init_env : unit -> env

val import_type : env -> Sexplib0.Sexp.t -> Type.t

val import_contract_id : Sexplib0.Sexp.t -> contract_id

val import_literal : Sexplib0.Sexp.t list -> Literal.t

val import_expr : env -> Sexplib0.Sexp.t -> expr

val import_contract : env -> Sexplib0.Sexp.t -> contract
(** Parse an s-expression into a contract definition. *)

val import_line_no : Sexplib0.Sexp.t -> line_no
