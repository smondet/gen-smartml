(* Copyright 2019-2021 Smart Chain Arena LLC. *)

open Utils_pure

type sapling_test_state =
  { test : bool
  ; memo : int
  ; elements : (string * Bigint.t) list }
[@@deriving show, eq, ord]

type sapling_test_transaction =
  { source : string option
  ; target : string option
  ; amount : Bigint.t
  ; memo : int }
[@@deriving show, eq, ord]

type address_and_entry_point =
  { address : string
        [@compare
          fun a1 a2 ->
            match (String.is_prefix "KT1" a1, String.is_prefix "KT1" a2) with
            | b1, b2 when b1 = b2 ->
                (* addresses of the same type are compared lexicographically *)
                String.compare a1 a2
            | b1, b2 ->
                (* addresses of implicit accounts are strictly less than addresses of originated accounts *)
                Bool.compare b1 b2]
  ; entry_point : string option [@compare fun ep1 ep2 -> Stdlib.compare ep1 ep2]
  }
[@@deriving eq, ord, show {with_path = false}]

type t = private
  | Unit
  | Bool                     of bool
  | Int                      of
      { i : Bigint.t
      ; is_nat : bool Unknown.t }
  | String                   of string
  | Bytes                    of string
  | Chain_id                 of string
  | Timestamp                of Bigint.t
  | Mutez                    of Bigint.t
  | Address                  of address_and_entry_point
  | Key                      of string
  | Secret_key               of string
  | Key_hash                 of string
  | Signature                of string
  | Sapling_test_state       of sapling_test_state
  | Sapling_test_transaction of sapling_test_transaction
  | Bls12_381_g1             of string
  | Bls12_381_g2             of string
  | Bls12_381_fr             of string
  | Chest_key                of string
  | Chest                    of string
[@@deriving eq, ord, show]

val unit : t

val bool : bool -> t

val int : Bigint.t -> t

val nat : Bigint.t -> t

val intOrNat : bool Unknown.t -> Bigint.t -> t

val small_int : int -> t

val small_nat : int -> t

val string : string -> t

val bytes : string -> t

val chain_id : string -> t

val timestamp : Bigint.t -> t

val mutez : Bigint.t -> t

val address : ?entry_point:string -> string -> t

val key : string -> t

val secret_key : string -> t

val key_hash : string -> t

val signature : string -> t

val sapling_test_state : int -> (string * Bigint.t) list -> t

val sapling_state_real : int -> t

val sapling_test_transaction :
  int -> string option -> string option -> Bigint.t -> t

val unBool : t -> bool option

val unInt : t -> Bigint.t option

val unAddress : t -> address_and_entry_point option

val bls12_381_g1 : string -> t

val bls12_381_g2 : string -> t

val bls12_381_fr : string -> t

val chest_key : string -> t

val chest : string -> t
