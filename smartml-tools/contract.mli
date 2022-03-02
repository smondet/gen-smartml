(* Copyright 2019-2021 Smart Chain Arena LLC. *)

open SmartML

(** {1 Contract manipulation and evaluation.} *)

type t = Basics.instance [@@deriving show]

(** {2 Website Implementation} *)

val execMessageInner :
     config:Config.t
  -> primitives:(module Primitives.Primitives)
  -> scenario_state:Basics.scenario_state
  -> title:string
  -> execMessageClass:string
  -> context:Interpreter.context
  -> channel:string
  -> params:Basics.value
  -> id:Ids.contract_id
  -> line_no:SmartML.Basics.line_no
  -> string Basics.Execution.exec_message

(** Evaluate a contract call with {!eval} and output an HTML result. *)
