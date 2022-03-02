(* Copyright 2019-2021 Smart Chain Arena LLC. *)

type account =
  { pkh : string
  ; pk : string
  ; sk : string }
[@@deriving show {with_path = false}]

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

let test_primitives p =
  let module P = (val p : Primitives) in
  let open P.Crypto in
  let open Format in
  let results = ref [] in
  let assert_bool cond s =
    results :=
      ( try if cond () then Ok s else Error s with
      | e -> Error (sprintf "%s -> Exn: %s" s (Printexc.to_string e)) )
      :: !results
  in
  let assertf cond fmt = Format.kasprintf (assert_bool cond) fmt in
  let a1 = account_of_seed "a1" in
  let a2 = account_of_seed "a2" in
  assertf (fun () -> a1 <> a2) "seed works: neq";
  let a3 = account_of_seed "a1" in
  assertf (fun () -> a1 = a3) "seed works: eq";
  let b1 = "hello world!" in
  assertf
    (fun () ->
      let s1 = sign ~secret_key:a1.sk b1 in
      check_signature ~public_key:a1.pk ~signature:s1 b1)
    "sign makes sense: ok";
  assertf
    (fun () ->
      let s1 = sign ~secret_key:a1.sk b1 in
      not (check_signature ~public_key:a2.pk ~signature:s1 b1))
    "sign makes sense: not ok";
  assertf (fun () -> hash_key a1.pk = a1.pkh) "hash_key hashes the key";
  List.rev !results
