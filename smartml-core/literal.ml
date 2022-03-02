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

type t =
  | Unit
  | Bool                     of bool
  | Int                      of
      { i : Bigint.t
      ; is_nat : bool Unknown.t [@equal fun _ _ -> true] [@compare fun _ _ -> 0]
      }
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
[@@deriving eq, ord, show {with_path = false}]

let unit = Unit

let bool x = Bool x

let int i = Int {i; is_nat = Unknown.value false}

let nat i = Int {i; is_nat = Unknown.value true}

let intOrNat is_nat i = Int {i; is_nat}

let small_int i = int (Bigint.of_int i)

let small_nat i = nat (Bigint.of_int i)

let string s = String s

let bytes s = Bytes s

let chain_id s = Chain_id s

let timestamp i = Timestamp i

let mutez i = Mutez i

let address ?entry_point address = Address {address; entry_point}

(* let contract ?entry_point address type_ = Contract {address; entry_point; type_} *)

let key s = Key s

let secret_key s = Secret_key s

let key_hash s = Key_hash s

let signature s = Signature s

let sapling_test_state memo elements =
  Sapling_test_state {test = true; elements; memo}

let sapling_state_real memo =
  Sapling_test_state {test = false; elements = []; memo}

let sapling_test_transaction memo source target amount =
  Sapling_test_transaction {source; target; amount; memo}

let unBool = function
  | Bool b -> Some b
  | _ -> None

let unInt = function
  | Int {i} -> Some i
  | _ -> None

let unAddress = function
  | Address a -> Some a
  | _ -> None

let bls12_381_g1 s = Bls12_381_g1 s

let bls12_381_g2 s = Bls12_381_g2 s

let bls12_381_fr s = Bls12_381_fr s

let chest_key b = Chest_key b

let chest b = Chest b
