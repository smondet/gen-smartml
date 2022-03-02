(* Copyright 2019-2021 Smart Chain Arena LLC. *)

open SmartML
open Basics

val write_pretty :
     language:Config.language
  -> ?suffix:string
  -> string
  -> instance
  -> string * int

val write_pretty_html :
     language:Config.language
  -> install:string
  -> string
  -> instance
  -> string * int

val write_contract_michelson : string -> Michelson.tcontract -> string * int

val write_micheline : string -> Micheline.t -> string * int

val write_contract_michel :
  string -> Michel.Expr.expr Michel.Expr.precontract -> string * int

val write_tvalue : string -> Value.t -> string * int

val write_mliteral : string -> Michelson.literal -> string * int

val pp_contract_types : instance -> string

val write_contract_types : string -> instance -> string * int

val write_metadata : string -> Utils.Misc.json -> string * int

val write_html : string -> string -> string * int

val write_csv : string -> string list list -> string * int

val wrap_html_document : install:string -> string -> string
