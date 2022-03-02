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
[@@deriving eq, ord, show {with_path = false}]

type type1 =
  | T_option
  | T_list
  | T_set
  | T_contract
  | T_ticket
[@@deriving eq, ord, show {with_path = false}]

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
[@@deriving eq, ord, show {with_path = false}]

let string_of_type0 = function
  | T_unit -> ("unit", None)
  | T_bool -> ("bool", None)
  | T_nat -> ("nat", None)
  | T_int -> ("int", None)
  | T_mutez -> ("mutez", None)
  | T_string -> ("string", None)
  | T_bytes -> ("bytes", None)
  | T_chain_id -> ("chain_id", None)
  | T_timestamp -> ("timestamp", None)
  | T_address -> ("address", None)
  | T_key -> ("key", None)
  | T_key_hash -> ("key_hash", None)
  | T_signature -> ("signature", None)
  | T_operation -> ("operation", None)
  | T_sapling_state {memo} -> ("sapling_state", Some (string_of_int memo))
  | T_sapling_transaction {memo} ->
      ("sapling_transaction", Some (string_of_int memo))
  | T_never -> ("never", None)
  | T_bls12_381_g1 -> ("bls12_381_g1", None)
  | T_bls12_381_g2 -> ("bls12_381_g2", None)
  | T_bls12_381_fr -> ("bls12_381_fr", None)
  | T_chest_key -> ("chest_key", None)
  | T_chest -> ("chest", None)

let string_of_type1 = function
  | T_option -> "option"
  | T_list -> "list"
  | T_set -> "set"
  | T_contract -> "contract"
  | T_ticket -> "ticket"

let string_of_type2 = function
  | T_lambda -> ("lambda", None, None)
  | T_map -> ("map", None, None)
  | T_big_map -> ("big_map", None, None)
  | T_pair {annot_fst; annot_snd} -> ("pair", annot_fst, annot_snd)
  | T_or {annot_left; annot_right} -> ("or", annot_left, annot_right)

type 'm mtype_f =
  | MT0    of type0
  | MT1    of type1 * 'm
  | MT2    of type2 * 'm * 'm
  | MT_var of string
[@@deriving eq, ord, fold, map, show {with_path = false}]

type mtype =
  { mt : mtype mtype_f
  ; annot_type : string option (* :a *)
  ; annot_variable : string option (* @a *) }
[@@deriving eq, ord, show {with_path = false}]

let mk_mtype ?annot_type ?annot_variable mt = {mt; annot_type; annot_variable}

let rec cata_mtype f {mt; annot_type; annot_variable} =
  f ?annot_type ?annot_variable (map_mtype_f (cata_mtype f) mt)

let mt0 t = mk_mtype (MT0 t)

let mt1 t t1 = mk_mtype (MT1 (t, t1))

let mt2 t t1 t2 = mk_mtype (MT2 (t, t1, t2))

let mt_unit = mt0 T_unit

let mt_bool = mt0 T_bool

let mt_nat = mt0 T_nat

let mt_int = mt0 T_int

let mt_mutez = mt0 T_mutez

let mt_string = mt0 T_string

let mt_bytes = mt0 T_bytes

let mt_chain_id = mt0 T_chain_id

let mt_timestamp = mt0 T_timestamp

let mt_address = mt0 T_address

let mt_key = mt0 T_key

let mt_key_hash = mt0 T_key_hash

let mt_signature = mt0 T_signature

let mt_operation = mt0 T_operation

let mt_sapling_state memo = mt0 (T_sapling_state {memo})

let mt_sapling_transaction memo = mt0 (T_sapling_transaction {memo})

let mt_never = mt0 T_never

let mt_bls12_381_g1 = mt0 T_bls12_381_g1

let mt_bls12_381_g2 = mt0 T_bls12_381_g2

let mt_bls12_381_fr = mt0 T_bls12_381_fr

let mt_chest_key = mt0 T_chest_key

let mt_chest = mt0 T_chest

let mt_option = mt1 T_option

let mt_list = mt1 T_list

let mt_set = mt1 T_set

let mt_contract = mt1 T_contract

let mt_ticket = mt1 T_ticket

let mt_lambda t1 t2 = mk_mtype (MT2 (T_lambda, t1, t2))

let mt_map t1 t2 = mk_mtype (MT2 (T_map, t1, t2))

let mt_big_map t1 t2 = mk_mtype (MT2 (T_big_map, t1, t2))

let mt_pair ?annot_fst ?annot_snd = mt2 (T_pair {annot_fst; annot_snd})

let mt_or ?annot_left ?annot_right = mt2 (T_or {annot_left; annot_right})

let mt_var s = mk_mtype (MT_var s)
