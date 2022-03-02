(* Copyright 2019-2021 Smart Chain Arena LLC. *)

(** Compiler from {!Basics.tcontract} to Michelson. *)

open SmartML
open Utils
open Michelson

type scenario_vars = Basics.value String.Map.t

(*val mtype_of_record : ('a -> mtype) -> (string * 'a) list -> mtype*)
(** Compile a record to Michelson, see {!Scenario.run}, it useful to
    reproduce the same binary tree structure as normal compilation. *)

val compile_value :
     config:Config.t
  -> scenario_vars:scenario_vars
  -> ?tstorage:Type.t
  -> Basics.value
  -> literal
(** Compile a value into a Michelson literal. *)

val pack_value :
     config:Config.t
  -> scenario_vars:scenario_vars
  -> ?tstorage:Type.t
  -> Basics.value
  -> string
(** Serialize a value into the same binary format as Michelson in the
    protocol.  *)

val unpack_value : config:Config.t -> Type.t -> string -> Basics.value
(** Deserialize michelson bytes. *)

val compile_instance :
  scenario_vars:scenario_vars -> Basics.instance -> Michelson.tcontract
(** Compile a smart-contract to the Michelson intermediate representation. *)

val compile_literal :
  ?for_error:Basics.Typed.texpr -> Literal.t -> Michelson.literal

val compile_record :
     layout:Layout.l Binary_tree.t
  -> (string * 'a) list
  -> (string * 'a) Binary_tree.t

val compile_variant :
     row:(string * 'a) list
  -> layout:Layout.l Utils.Binary_tree.t
  -> string
  -> 'a Utils.Binary_tree.context

val compile_tuple : ('a -> 'a -> 'a) -> 'a list -> 'a

val simplify_via_michel :
     config:Config.t
  -> tcontract
  -> Michel.Expr.expr Michel.Expr.precontract * tcontract
