(* Copyright 2019-2021 Smart Chain Arena LLC. *)

open Utils_pure
open Control

let sort_row r = List.sort (fun (lbl1, _) (lbl2, _) -> compare lbl1 lbl2) r

(** Maps over a reference, creating a new one. *)
let map_ref f {contents} = {contents = f contents}

type 't row = (string * 't) list
[@@deriving eq, ord, map, fold, show {with_path = false}]

type 't bounded_cases =
  { final : bool
  ; cases : 't list }
[@@deriving eq, map, fold, show {with_path = false}, ord]

type with_storage =
  | Read_only
  | Read_write
[@@deriving eq, ord, show]

type effects =
  { with_storage : with_storage option Unknown.t
  ; with_operations : bool Unknown.t }
[@@deriving eq, ord, show {with_path = false}]

type 't f =
  | TVar                of string
  | T0                  of Michelson_base.Type.type0
  | T1                  of Michelson_base.Type.type1 * 't
  | T2                  of Michelson_base.Type.type2 * 't * 't
  | TInt                of {isNat : bool Unknown.t}
  | TLambda             of effects * 't * 't
  | TBounded            of
      { t : 't
      ; cases : Literal.t bounded_cases Unknown.t }
  | TRecord             of
      { layout : Layout.t Unknown.t
      ; row : 't row }
  | TVariant            of
      { layout : Layout.t Unknown.t
      ; row : 't row }
  | TUnknown            of 't unknownType_f ref
  | TTuple              of 't list
  | TSecretKey
  | TSaplingState       of {memo : int Unknown.t}
  | TSaplingTransaction of {memo : int Unknown.t}
[@@deriving eq, map, fold, show {with_path = false}, ord]

and 't unknownType_f =
  | UUnknown of string
  | URecord  of (string * 't) list
  | UTuple   of (int * 't) list
  | UVariant of (string * 't) list
  | UExact   of 't
[@@deriving eq, map, fold, show {with_path = false}, ord]

module F = struct
  type 'a t = 'a f [@@deriving eq, ord, show {with_path = false}, map]
end

include Fix (F)
include FixEQ (F)
include FixORD (F)
include FixSHOW (F)
include FixFUNCTOR (F)

type unknownType = t unknownType_f

type tvariable = string * t [@@deriving eq, show {with_path = false}]

let tree_layout l =
  let l = ref l in
  let rec layout n =
    if n = 1
    then (
      match !l with
      | [] -> assert false
      | a :: rest ->
          l := rest;
          Layout.leaf a a )
    else
      let n2 = n / 2 in
      let l1 = layout n2 in
      let l2 = layout (n - n2) in
      Binary_tree.Node (l1, l2)
  in
  let size = List.length !l in
  if size = 0 then failwith "default_layout of size 0";
  layout size

let rec comb_layout right_left = function
  | [] -> failwith "comb_layout"
  | [lbl] -> Layout.leaf lbl lbl
  | lbl :: xs ->
    ( match right_left with
    | `Right ->
        Binary_tree.node (Layout.leaf lbl lbl) (comb_layout right_left xs)
    | `Left ->
        Binary_tree.node (comb_layout right_left xs) (Layout.leaf lbl lbl) )

let comb_layout_of_row right_left r = comb_layout right_left r

let default_layout_of_row layout r =
  match layout with
  | Config.Tree -> tree_layout r
  | Comb -> comb_layout_of_row `Right r

let build t = F t

let unknown_raw x = build (TUnknown x)

let rec getRepr t =
  match unF t with
  | TUnknown {contents = UExact t} -> getRepr t
  | u -> u

let normalize : t -> t =
  let f = function
    | TUnknown {contents = UExact (F t)} -> t
    | TUnknown _ as t -> t
    | TInt {isNat} -> TInt {isNat = Unknown.normalize isNat}
    | TBounded {t; cases} -> TBounded {t; cases = Unknown.normalize cases}
    | TRecord {layout; row} -> TRecord {layout = Unknown.normalize layout; row}
    | TVariant {layout; row} -> TVariant {layout = Unknown.normalize layout; row}
    | TSaplingState {memo} -> TSaplingState {memo = Unknown.normalize memo}
    | TSaplingTransaction {memo} ->
        TSaplingTransaction {memo = Unknown.normalize memo}
    | (TVar _ | TLambda _ | T0 _ | T1 _ | T2 _ | TSecretKey | TTuple _) as t ->
        t
  in
  cata (fun x -> F (f x))

let mk0 t = build (T0 t)

let mk1 t t1 = build (T1 (t, t1))

let mk2 t t1 t2 = build (T2 (t, t1, t2))

let unit = build (T0 T_unit)

let address = build (T0 T_address)

let contract t = build (T1 (T_contract, t))

let bool = build (T0 T_bool)

let bytes = build (T0 T_bytes)

let variant layout row = build (TVariant {layout; row = sort_row row})

let variant_default_layout default_layout row =
  build
    (TVariant
       { layout =
           Unknown.value
             (default_layout_of_row default_layout (List.map fst row))
       ; row = sort_row row })

let key_hash = build (T0 T_key_hash)

(* baker_hash  TODO (Incoming proposal will change key_hash to baker_hash) *)
let baker_type_of_protocol = function
  | Config.Delphi | Edo | Florence | Granada | Hangzhou | Ithaca -> key_hash

(*| BakingAccounts -> baker_hash*)

let int_raw ~isNat = build (TInt {isNat})

let int () = int_raw ~isNat:(Unknown.value false)

let nat () = int_raw ~isNat:(Unknown.value true)

let intOrNat () = int_raw ~isNat:(Unknown.unknown ())

let key = build (T0 T_key)

let chain_id = build (T0 T_chain_id)

let secret_key = build TSecretKey

let operation = build (T0 T_operation)

let sapling_state memo =
  let memo = Option.cata (Unknown.unknown ()) Unknown.value memo in
  build (TSaplingState {memo})

let sapling_transaction memo =
  let memo = Option.cata (Unknown.unknown ()) Unknown.value memo in
  build (TSaplingTransaction {memo})

let never = build (T0 T_never)

let map ~big ~tkey ~tvalue =
  if big
  then build (T2 (T_big_map, tkey, tvalue))
  else build (T2 (T_map, tkey, tvalue))

let set ~telement = build (T1 (T_set, telement))

let record layout row = build (TRecord {layout; row = sort_row row})

let record_default_layout layout row =
  let layout =
    Unknown.value (default_layout_of_row layout (List.map fst row))
  in
  build (TRecord {layout; row = sort_row row})

let record_or_unit layout = function
  | [] -> unit
  | l -> record layout l

let signature = build (T0 T_signature)

let option t = build (T1 (T_option, t))

let key_value tkey tvalue =
  record_default_layout Config.Comb [("key", tkey); ("value", tvalue)]

let head_tail thead ttail =
  record_default_layout Config.Comb [("head", thead); ("tail", ttail)]

let tor t u = variant_default_layout Config.Comb [("Left", t); ("Right", u)]

let string = build (T0 T_string)

let bounded t final cases =
  let cases = List.sort Literal.compare cases in
  build (TBounded {t; cases = Unknown.value {final; cases}})

let timestamp = build (T0 T_timestamp)

let token = build (T0 T_mutez)

let uvariant name t = unknown_raw (ref (UVariant [(name, t)]))

let urecord fields =
  let cmp (x, _) (y, _) = Stdlib.compare x y in
  let fields = List.sort cmp fields in
  unknown_raw (ref (URecord fields))

let utuple i t = unknown_raw (ref (UTuple [(i, t)]))

let account =
  record_default_layout
    Config.Comb
    [ ("seed", string)
    ; ("address", address)
    ; ("public_key", key)
    ; ("public_key_hash", key_hash)
    ; ("secret_key", secret_key) ]

let pair t1 t2 = build (TTuple [t1; t2])

let tuple ts = build (TTuple ts)

let list t = build (T1 (T_list, t))

let ticket t = build (T1 (T_ticket, t))

let lambda effects t1 t2 = build (TLambda (effects, t1, t2))

let bls12_381_g1 = build (T0 T_bls12_381_g1)

let bls12_381_g2 = build (T0 T_bls12_381_g2)

let bls12_381_fr = build (T0 T_bls12_381_fr)

let chest_key = build (T0 T_chest_key)

let chest = build (T0 T_chest)

let has_unknowns =
  cata (function
      | TUnknown _ -> true
      | x -> fold_f ( || ) false x)

let is_hot =
  let open Ternary in
  cata (function
      | T1 (T_ticket, _) -> Yes
      | TLambda _ -> No
      | T2 (T_lambda, _, _) -> assert false
      | T1 (T_contract, _) -> No
      | TUnknown _ -> Maybe
      | t -> fold_f or_ No t)

let view_variant t =
  match getRepr t with
  | TVariant {layout; row} -> Some (layout, row)
  | T1 (T_option, t) ->
      let row = [("None", unit); ("Some", t)] in
      let layout = default_layout_of_row Comb (List.map fst row) in
      Some (Unknown.value layout, row)
  | _ -> None

let of_mtype =
  let f ?annot_type:_ ?annot_variable:_ : _ Michelson_base.Type.mtype_f -> _ =
    function
    | MT0 T_nat -> nat ()
    | MT0 T_int -> int ()
    | MT0 (T_sapling_state {memo}) ->
        F (TSaplingState {memo = Unknown.value memo})
    | MT0 (T_sapling_transaction {memo}) ->
        F (TSaplingTransaction {memo = Unknown.value memo})
    | MT0 c -> F (T0 c)
    | MT1 (c, t1) -> F (T1 (c, t1))
    | MT2 (T_map, tkey, tvalue) -> map ~big:false ~tkey ~tvalue
    | MT2 (T_big_map, tkey, tvalue) -> map ~big:true ~tkey ~tvalue
    | MT2 (T_pair _, t1, t2) -> pair t1 t2
    | MT2 (T_or {annot_left = Some left; annot_right = Some right}, t1, t2) ->
        variant_default_layout Config.Comb [(left, t1); (right, t2)]
    | MT2 (T_or _, t1, t2) ->
        variant_default_layout Config.Comb [("Left", t1); ("Right", t2)]
    | MT2 (c, t1, t2) -> F (T2 (c, t1, t2))
    | MT_var _ -> assert false
  in
  Michelson_base.Type.cata_mtype f

let type_of_literal = function
  | Literal.Unit -> unit
  | Bool _ -> bool
  | Int {is_nat} -> int_raw ~isNat:is_nat
  | String _ -> string
  | Bytes _ -> bytes
  | Chain_id _ -> chain_id
  | Timestamp _ -> timestamp
  | Mutez _ -> token
  | Address _ -> address
  | Key _ -> key
  | Secret_key _ -> secret_key
  | Key_hash _ -> key_hash
  | Signature _ -> signature
  | Sapling_test_state {memo} -> sapling_state (Some memo)
  | Sapling_test_transaction {memo} -> sapling_transaction (Some memo)
  | Bls12_381_g1 _ -> bls12_381_g1
  | Bls12_381_g2 _ -> bls12_381_g2
  | Bls12_381_fr _ -> bls12_381_fr
  | Chest_key _ -> chest_key
  | Chest _ -> chest

let no_effects =
  {with_storage = Unknown.value None; with_operations = Unknown.value false}

let unknown_effects () =
  {with_storage = Unknown.unknown (); with_operations = Unknown.unknown ()}

let rawify_lambda ~with_storage ~with_operations t1 t2 =
  let r t =
    match (with_storage, with_operations) with
    | None, false -> t
    | Some tstorage, false -> pair t tstorage
    | None, true -> pair t (list operation)
    | Some tstorage, true -> pair t (pair (list operation) tstorage)
  in
  (r t1, r t2)

let has_effects {with_storage; with_operations} =
  let msg = "rawify_lambda: unknown effect" in
  let with_storage = Option.of_some_exn ~msg (Unknown.get with_storage) in
  let with_operations = Option.of_some_exn ~msg (Unknown.get with_operations) in
  Option.is_some with_storage || with_operations
