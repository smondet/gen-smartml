(* Copyright 2019-2021 Smart Chain Arena LLC. *)

type account =
  { pkh : string
  ; pk : string
  ; sk : string }
[@@deriving show]

(** Parametrized implementations of low-level functions (cryptography,
    hashes, etc.), to be filled-in by the execution context (["js_of_ocaml"],
    native, etc.). *)
module type Primitives = sig
  module Timelock : sig
    type opening_result =
      | Correct       of string
      | Bogus_cipher
      | Bogus_opening

    val open_chest : string -> string -> int -> opening_result
  end

  module Bls12 : sig
    (* G1 Point *)
    val negateG1 : string -> string

    val addG1 : string -> string -> string

    val multiplyG1ByFr : string -> string -> string

    (* G2 Point *)
    val negateG2 : string -> string

    val addG2 : string -> string -> string

    val multiplyG2ByFr : string -> string -> string

    (* Fr *)
    val negateFr : string -> string

    val addFr : string -> string -> string

    val multiplyFrByInt : string -> string -> string

    val multiplyFrByFr : string -> string -> string

    val convertFrToInt : string -> string

    (* Pairing check *)
    val pairingCheck : (string * string) list -> bool
  end

  module Crypto : sig
    val blake2b : string -> string

    val sha256 : string -> string

    val sha512 : string -> string

    val keccak : string -> string

    val sha3 : string -> string

    val sign : secret_key:string -> string -> string

    val check_signature :
      public_key:string -> signature:string -> string -> bool

    val account_of_seed : string -> account

    val hash_key : string -> string
  end
end

val test_primitives : (module Primitives) -> (string, string) result list
(** Run a quick unit test on the primitives, to check that they are
      consistent w.r.t. assumptions made throughout the library. *)
