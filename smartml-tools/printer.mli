(* Copyright 2019-2021 Smart Chain Arena LLC. *)

open SmartML
open Basics
open Typed
open Untyped

(** {1 Target Printer Options} *)

module type Printer = sig
  module Options : sig
    type t = private
      { html : bool
      ; stripStrings : bool
      ; types : bool }

    val string : t

    val html : t

    val htmlStripStrings : t

    val types : t
  end

  (** {1 Types} *)

  val type_to_string :
       ?multiline:unit
    -> ?toplevel:unit
    -> ?protect:unit
    -> ?options:Options.t
    -> Type.t
    -> string

  (** {1 Values} *)

  val html_of_data : Options.t -> value -> string

  val value_to_string :
    ?deep:bool -> ?noEmptyList:bool -> ?options:Options.t -> value -> string

  val literal_to_sp_string :
    html:bool -> ?protect:unit -> ?strip_strings:unit -> Literal.t -> string

  val literal_to_string :
    ?deep:bool -> ?options:Options.t -> Basics.Literal.t -> string

  val ppAmount : bool -> Utils.Bigint.t -> string

  (** {1 Expressions, Commands, Contracts} *)

  val layout_to_string : Layout.t Unknown.t -> string

  val mprim0_to_string :
    Michelson.mtype Michelson_base.Primitive.prim0 -> string

  val mprim1_to_string :
       language:Config.language
    -> Michelson.mtype Michelson_base.Primitive.prim1
    -> string

  val mprim2_to_string :
    Michelson.mtype Michelson_base.Primitive.prim2 -> string

  val mprim3_to_string : Michelson_base.Primitive.prim3 -> string

  val expr_to_string : ?options:Options.t -> ?protect:unit -> expr -> string

  val texpr_to_string : ?options:Options.t -> ?protect:unit -> texpr -> string

  val variable_to_string :
    ?options:Options.t -> ?protect:unit -> string -> vClass -> string

  val tvariable_to_string :
    ?options:Options.t -> ?protect:unit -> string * Type.t -> vClass -> string

  val command_to_string :
    ?indent:string -> ?options:Options.t -> command -> string

  val tcommand_to_string :
    ?indent:string -> ?options:Options.t -> tcommand -> string

  val tcontract_to_string :
    ?contract_id:contract_id -> ?options:Options.t -> instance -> string

  val pp_tcontract : ?options:Options.t -> Format.formatter -> instance -> unit

  val html_of_record_list :
    string list * 'a list list -> ('a -> string) -> string

  (** {1 Operations} *)

  val operation_to_string : ?options:Options.t -> operation -> string

  (** {1 Exceptions} *)

  val wrong_condition_string : texpr -> string

  val error_to_string : Execution.error -> string

  val exception_to_smart_except : exn -> Basics.smart_except list

  val exception_to_string : bool -> exn -> string

  val pp_smart_except : bool -> Basics.smart_except list -> string

  val string_of_contract_id : contract_id -> string
end

val get_by_language : Config.language -> (module Printer)

val get : Config.t -> (module Printer)
