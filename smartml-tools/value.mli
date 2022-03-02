(* Copyright 2019-2021 Smart Chain Arena LLC. *)

open SmartML
open Utils
open Basics

type t = value [@@deriving eq, show]

val literal : Literal.t -> t

val bounded : Literal.t Type.bounded_cases -> Literal.t -> t

val int : Bigint.t -> t

val nat : Bigint.t -> t

val intOrNat : bool Unknown.t -> Bigint.t -> t

val mutez : Bigint.t -> t

val int_of_value : t -> int

val bool_of_value : t -> bool

val compare : t -> t -> int

val lt : t -> t -> bool

val le : t -> t -> bool

val openV : t -> value value_f

val string : string -> t

val bytes : string -> t

val bls12_381_g1 : string -> t

val bls12_381_g2 : string -> t

val bls12_381_fr : string -> t

val chest_key : string -> t

val chest : string -> t

val chain_id : string -> t

val unString : pp:(unit -> smart_except list) -> t -> string

val unAddress : pp:(unit -> smart_except list) -> t -> string

val unKey_hash : pp:(unit -> smart_except list) -> t -> string

val un_baker : t -> string option

val build_list : t list -> Type.t -> t

val set : telement:Type.t -> t list -> t

val map : big:bool -> tkey:Type.t -> tvalue:Type.t -> (t * t) list -> t

val unit : t

val bool : bool -> t

val unBool : pp:(unit -> smart_except list) -> t -> bool

val unList : pp:(unit -> smart_except list) -> t -> t list

val unMap : pp:(unit -> smart_except list) -> t -> (t * t) list

val unSet : pp:(unit -> smart_except list) -> t -> t list

val unOption : t -> t option

val unInt : pp:(unit -> smart_except list) -> t -> Bigint.t

val unMutez : pp:(unit -> smart_except list) -> t -> Bigint.t

val unBls12_381 : t -> string

val unTimestamp : pp:(unit -> smart_except list) -> t -> Bigint.t

val unChain_id : pp:(unit -> smart_except list) -> t -> string

val plus_inner : primitives:(module Primitives.Primitives) -> t -> t -> t

val plus : primitives:(module Primitives.Primitives) -> t -> t -> t

val mul : primitives:(module Primitives.Primitives) -> t -> t -> t

val e_mod : t -> t -> t

val div_inner : t -> t -> t

val div : t -> t -> t

val minus : t -> t -> t

val key_hash : string -> t

val key : string -> t

val secret_key : string -> t

val signature : string -> t

val record : layout:Layout.t -> (string * t) list -> t

val record_comb : (string * t) list -> t

val tuple : t list -> t

val untuple : pp:(unit -> smart_except list) -> t -> t list

val un_record : t -> (string * t) list

val variant_with_type :
  ?no_check:unit -> line_no:line_no -> string -> t -> Type.t -> t

val none : Type.t -> t

val some : t -> t

val left : Type.t -> t -> t

val right : Type.t -> t -> t

val option : Type.t -> t option -> t

val ediv : t -> t -> t

val timestamp : Bigint.t -> t

val intXor : t -> t -> 'a

val address : ?entry_point:string -> string -> t

val contract : ?entry_point:string -> string -> Type.t -> t

val cons : t -> t -> t

val lens_list : (t, t list) Lens.t

val lens_list_nth : int -> (t, t option) Lens.t

val lens_map : (t, (t * t) list) Lens.t

val lens_map_at : key:t -> (t, t option) Lens.t

val lens_set : (t, t list) Lens.t

val lens_set_at : elem:t -> (t, bool) Lens.t

val lens_record : (t, (string * t) list) Lens.t

val lens_record_at : attr:string -> (t, t option) Lens.t

val zero_of_type : Type.t -> t

val nextId : string -> unit -> string

val shift_left : t -> t -> t

val shift_right : t -> t -> t

val xor : t -> t -> t

val closure_init : tlambda -> t

val closure_apply : t -> t -> t

val unclosure : pp:(unit -> smart_except list) -> t -> tlambda * t list

val project_literals : (Literal.t -> 'a option) -> t -> ('a * string list) list

val get_field_opt : string -> t -> t option

val operation : operation -> t

val unoperation : pp:(unit -> smart_except list) -> t -> operation

val unSaplingTransaction :
     pp:(unit -> smart_except list)
  -> t
  -> string option * string option * Bigint.t

val unSaplingState :
  pp:(unit -> smart_except list) -> t -> int * (string * Bigint.t) list

val ticket : string -> t -> Bigint.t -> t

val baker_value_of_protocol : Config.protocol -> string -> t

(*val error : string -> t -> value*)

val of_micheline :
     config:Config.t
  -> pp_mich:(Type.t -> Micheline.t -> string)
  -> Type.t
  -> Micheline.t
  -> t

val typecheck : Type.t -> value -> tvalue

val smartMLify : Type.t -> value -> value
