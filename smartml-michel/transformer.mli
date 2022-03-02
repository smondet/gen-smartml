(* Copyright 2019-2021 Smart Chain Arena LLC. *)

open Expr

type state = {var_counter : int ref}

val fresh : state -> string -> string

val freshen : state -> string -> string list -> string list

val simplify : state -> expr precontract -> expr precontract

val smartMLify : state -> expr precontract -> expr precontract

val michelsonify : state -> expr precontract -> expr precontract
