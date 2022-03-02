(* Copyright 2019-2021 Smart Chain Arena LLC. *)

type type0 =
  | T_unit
  | T_bool
  | T_nat
  | T_int
  | T_mutez
  | T_string
  | T_bytes
  | T_chain_id
  | T_timestamp
  | T_address
  | T_key
  | T_key_hash
  | T_signature
  | T_operation
  | T_sapling_state       of {memo : int}
  | T_sapling_transaction of {memo : int}
  | T_never
  | T_bls12_381_g1
  | T_bls12_381_g2
  | T_bls12_381_fr
  | T_chest_key
  | T_chest
[@@deriving eq, ord, show]

type type1 =
  | T_option
  | T_list
  | T_set
  | T_contract
  | T_ticket
[@@deriving eq, ord, show]

type type2 =
  | T_lambda
  | T_map
  | T_big_map
  | T_pair    of
      { annot_fst : string option
      ; annot_snd : string option }
  | T_or      of
      { annot_left : string option
      ; annot_right : string option }
[@@deriving eq, ord, show]

val string_of_type0 : type0 -> string * string option

val string_of_type1 : type1 -> string

val string_of_type2 : type2 -> string * string option * string option

type 'm mtype_f =
  | MT0    of type0
  | MT1    of type1 * 'm
  | MT2    of type2 * 'm * 'm
  | MT_var of string
[@@deriving eq, ord, show, map, fold]

type mtype =
  { mt : mtype mtype_f
  ; annot_type : string option (* :a *)
  ; annot_variable : string option (* @a *) }
[@@deriving eq, ord, show]

val cata_mtype :
     (?annot_type:string -> ?annot_variable:string -> 'a mtype_f -> 'a)
  -> mtype
  -> 'a

val mk_mtype :
  ?annot_type:string -> ?annot_variable:string -> mtype mtype_f -> mtype

val mt0 : type0 -> mtype

val mt1 : type1 -> mtype -> mtype

val mt2 : type2 -> mtype -> mtype -> mtype

val mt_unit : mtype

val mt_bool : mtype

val mt_nat : mtype

val mt_int : mtype

val mt_mutez : mtype

val mt_string : mtype

val mt_bytes : mtype

val mt_chain_id : mtype

val mt_timestamp : mtype

val mt_address : mtype

val mt_key : mtype

val mt_key_hash : mtype

val mt_signature : mtype

val mt_operation : mtype

val mt_sapling_state : int -> mtype

val mt_sapling_transaction : int -> mtype

val mt_never : mtype

val mt_bls12_381_g1 : mtype

val mt_bls12_381_g2 : mtype

val mt_bls12_381_fr : mtype

val mt_chest_key : mtype

val mt_chest : mtype

val mt_option : mtype -> mtype

val mt_list : mtype -> mtype

val mt_set : mtype -> mtype

val mt_contract : mtype -> mtype

val mt_ticket : mtype -> mtype

val mt_lambda : mtype -> mtype -> mtype

val mt_map : mtype -> mtype -> mtype

val mt_big_map : mtype -> mtype -> mtype

val mt_pair : ?annot_fst:string -> ?annot_snd:string -> mtype -> mtype -> mtype

val mt_or : ?annot_left:string -> ?annot_right:string -> mtype -> mtype -> mtype

val mt_var : string -> mtype
