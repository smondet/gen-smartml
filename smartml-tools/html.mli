(* Copyright 2019-2021 Smart Chain Arena LLC. *)

open SmartML
open Basics

type t =
  | Raw of string
  | Div of string * t list

val pp_render : Format.formatter -> t -> unit

val render : t -> string

(** {1 Contracts} *)

val contract_sizes_html :
     codeSize:int option
  -> simplifiedCodeSize:int option
  -> storageSize:int option
  -> nb_bigmaps:int
  -> t

val michelson_html :
     title:string
  -> lazy_tabs:bool
  -> id:string
  -> ?simplified_contract:Michelson.tcontract
  -> Michelson.tcontract
  -> t

val full_html :
     config:Config.t
  -> contract:instance
  -> compiled_contract:Michelson.tcontract
  -> def:string
  -> onlyDefault:bool
  -> id:string
  -> line_no:line_no
  -> accept_missings:bool
  -> contract_id:contract_id option
  -> t

(** {1 Helpers} *)

type tab

val call_tab : int -> int -> t

val tab : ?active:unit -> ?lazy_tab:t Lazy.t -> string -> t -> tab
(** tab ?active name inner_html *)

val tabs : ?global_id:int -> string -> tab list -> t

val showLine : line_no -> t

(** {1 Dynamic UI} *)

val nextInputGuiId : unit -> string

val nextOutputGuiId : unit -> string

val contextSimulationType : Type.t

val delayedInputGui : Type.t -> t

val simulatedContracts : (int, instance) Hashtbl.t

val simulation : instance -> int -> line_no:line_no -> t

val div : ?args:string -> t list -> t

val copy_div : id:string -> ?className:string -> string -> t -> t
