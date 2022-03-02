(* Copyright 2019-2021 Smart Chain Arena LLC. *)

open SmartML

val mich_of_bytes : string -> Basics.micheline

val parse_hex_bytes :
     string
  -> ( Data_encoding.json * string
     , Tezos_error_monad.Error_monad.error list )
     result

val pack_node_expression : ('a, string) Tezos_micheline.Micheline.node -> string

val encode_michelson_string : string -> string

val micheline_of_ezjsonm :
  Data_encoding.json -> (int, string) Tezos_micheline.Micheline.node

val micheline_node_to_micheline_canonical :
     ('a, 'b) Tezos_micheline.Micheline.node
  -> 'b Tezos_micheline.Micheline.canonical

val micheline_canonical_to_string :
  string Tezos_micheline.Micheline.canonical -> string

val micheline_node_to_string :
  ('a, string) Tezos_micheline.Micheline.node -> string

val node_of_mich : Micheline.t -> (int, string) Tezos_micheline.Micheline.node

val mich_of_node : (_, string) Tezos_micheline.Micheline.node -> Micheline.t

val pack_prefix : string

(* Micheline encoding size in bytes *)
val micheline_size : Micheline.t -> int

val micheline_size_opt : Micheline.t -> int option

val parse_node : string -> Micheline.t
