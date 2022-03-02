(* Copyright 2019-2021 Smart Chain Arena LLC. *)

open SmartML
open Utils
open Basics
open Typed

(** The initial state of the execution environment, a.k.a. this
    provides a “pseudo-blockchain.” *)
type context

val context_sender : context -> string option

val context_time : context -> Bigint.t

val context_amount : context -> Bigint.t

val context_contract_id : context -> contract_id option

val context_line_no : context -> line_no

val context_debug : context -> bool

val build_context :
     ?sender:string
  -> ?source:string
  -> ?chain_id:string
  -> time:Bigint.t
  -> amount:Bigint.t
  -> level:Bigint.t
  -> voting_powers:(string * Bigint.t) list
  -> line_no:line_no
  -> debug:bool
  -> unit
  -> context
(** Build a {!context}. *)

val interpret_message :
     config:Config.t
  -> primitives:(module Primitives.Primitives)
  -> scenario_state:scenario_state
  -> context
  -> contract_id
  -> tmessage
  -> instance option
     * operation list
     * Execution.error option
     * Execution.step list
(** Evaluation of a contract call ({!tmessage}) within a {!context}. *)

val interpret_expr_external :
     config:Config.t
  -> primitives:(module Primitives.Primitives)
  -> no_env:Basics.smart_except list
  -> scenario_state:scenario_state
  -> texpr
  -> value
(** Evaluation of an expression ({!texpr}) within a {!context}. *)

val interpret_contract :
     config:Config.t
  -> primitives:(module Primitives.Primitives)
  -> scenario_state:scenario_state
  -> tcontract
  -> instance

val update_contract_address :
  ?address:string -> scenario_state -> contract_id -> unit

val get_contract_address : scenario_state -> contract_id -> string
