(* Copyright 2019-2021 Smart Chain Arena LLC. *)

open SmartML
open Utils
open Basics
open Config

type t = value [@@deriving eq, show {with_path = false}]

let literal l = build_value (Literal l)

let bounded cases x = build_value (Bounded (cases, x))

let int x = literal (Literal.int x)

let nat x = literal (Literal.nat x)

let intOrNat t x = literal (Literal.intOrNat t x)

let mutez i = literal (Literal.mutez i)

let timestamp i = literal (Literal.timestamp i)

let int_of_value v =
  match v.v with
  | Literal (Int {i}) -> Big_int.int_of_big_int i
  | _ ->
      raise
        (SmartExcept [`Text "Cannot convert value"; `Value v; `Text "into int."])

let bool_of_value v =
  match v.v with
  | Literal (Bool x) -> x
  | _ ->
      raise
        (SmartExcept [`Text "Cannot convert value"; `Value v; `Text "into bool."])

(** Comparison (igoring [tvalue.t]). *)
let rec compare {v = v1} {v = v2} = compare_value_f compare v1 v2

let lt v1 v2 = compare v1 v2 < 0

let le v1 v2 = compare v1 v2 <= 0

let openV x = x.v

let string s = literal (Literal.string s)

let bytes s = literal (Literal.bytes s)

let bls12_381_g1 s = literal (Literal.bls12_381_g1 s)

let bls12_381_g2 s = literal (Literal.bls12_381_g2 s)

let bls12_381_fr s = literal (Literal.bls12_381_fr s)

let chest_key b = literal (Literal.chest_key b)

let chest b = literal (Literal.chest b)

let chain_id s = literal (Literal.chain_id s)

let unString ~pp = function
  | {v = Literal (String s)} -> s
  | x ->
      raise (SmartExcept [`Value x; `Text "is not a string"; `Br; `Rec (pp ())])

let operation op = build_value (Operation op)

let unSaplingTransaction ~pp = function
  | {v = Literal (Sapling_test_transaction {source; target; amount})} ->
      (source, target, amount)
  | x ->
      raise
        (SmartExcept
           [`Value x; `Text "is not a sapling transaction"; `Br; `Rec (pp ())])

let unSaplingState ~pp = function
  | {v = Literal (Sapling_test_state {memo; elements})} -> (memo, elements)
  | x ->
      raise
        (SmartExcept
           [`Value x; `Text "is not a sapling state"; `Br; `Rec (pp ())])

let same_type t1 t2 = Type.(equal (normalize t1) (normalize t2))

let build_list (l : t list) (t : Type.t) =
  if not (List.for_all (fun v -> same_type t (type_of_value v)) l)
  then
    raise
      (SmartExcept
         ( [`Text "Bad type"; `Type t]
         @ List.concat_map
             (fun x -> [`Text "AAAA"; `Value x; `Type (type_of_value x)])
             l ));
  build_value (List (t, l))

let ticket ticketer content amount =
  build_value (Ticket (ticketer, content, amount))

module V = struct
  type t = value

  let equal = equal_value

  let compare = compare_value
end

module VSet = Set.Make (V)
module VMap = Map.Make (V)

let set ~telement xs =
  List.iter (fun v -> assert (same_type telement (type_of_value v))) xs;
  let xs = VSet.(elements (of_list xs)) in
  build_value (Set (telement, xs))

let map ~big ~tkey ~tvalue xs =
  List.iter
    (fun (k, v) ->
      if not (same_type tkey (type_of_value k))
      then
        raise
          (SmartExcept
             [ `Text "Invalid key type in map"
             ; `Br
             ; `Type (type_of_value k)
             ; `Br
             ; `Text "expected a key of type"
             ; `Br
             ; `Type tkey
             ; `Br
             ; `Text "Key: "
             ; `Value k ]);
      if not (same_type tvalue (type_of_value v))
      then
        raise
          (SmartExcept
             [ `Text "Invalid value type in map"
             ; `Br
             ; `Type tvalue
             ; `Br
             ; `Text "expected a value of type"
             ; `Br
             ; `Type (type_of_value v)
             ; `Br
             ; `Text "Value: "
             ; `Value v ]))
    xs;
  let xs = VMap.(to_list (of_list xs)) in
  build_value (Map (tkey, tvalue, big, xs))

let unit = literal Literal.unit

let bool x = literal (Literal.bool x)

let unBool ~pp = function
  | {v = Literal (Bool b)} -> b
  | x ->
      raise (SmartExcept [`Value x; `Text "is not a bool"; `Br; `Rec (pp ())])

let unList ~pp = function
  | {v = List (_, l)} -> l
  | x ->
      raise (SmartExcept [`Value x; `Text "is not a list"; `Br; `Rec (pp ())])

let unMap ~pp = function
  | {v = Map (_, _, _, l)} -> l
  | x -> raise (SmartExcept [`Value x; `Text "is not a map"; `Br; `Rec (pp ())])

let unSet ~pp = function
  | {v = Set (_, l)} -> l
  | x -> raise (SmartExcept [`Value x; `Text "is not a set"; `Br; `Rec (pp ())])

let unOption v =
  match v.v with
  | Variant (_layout, _row, "Some", arg) -> Some arg
  | Variant (_layout, _row, "None", _) -> None
  | _ -> assert false

let unInt ~pp = function
  | {v = Literal (Int {i})} -> i
  | x ->
      raise
        (SmartExcept [`Value x; `Text "is not an integer"; `Br; `Rec (pp ())])

let unMutez ~pp = function
  | {v = Literal (Mutez b)} -> b
  | x ->
      raise (SmartExcept [`Value x; `Text "is not a mutez"; `Br; `Rec (pp ())])

let unBls12_381 = function
  | {v = Literal (Bls12_381_g1 g1)} -> g1
  | {v = Literal (Bls12_381_g2 g2)} -> g2
  | {v = Literal (Bls12_381_fr fr)} -> fr
  | _ -> failwith "Not a BLS12 type"

let unTimestamp ~pp = function
  | {v = Literal (Timestamp t)} -> t
  | x ->
      raise
        (SmartExcept [`Value x; `Text "is not a timestamp"; `Br; `Rec (pp ())])

let unChain_id ~pp = function
  | {v = Literal (Chain_id b)} -> b
  | x ->
      raise
        (SmartExcept [`Value x; `Text "is not a chain_id"; `Br; `Rec (pp ())])

let unAddress ~pp = function
  | {v = Literal (Address {address})} -> address
  | x ->
      raise
        (SmartExcept [`Value x; `Text "is not an address"; `Br; `Rec (pp ())])

let unKey_hash ~pp = function
  | {v = Literal (Key_hash b)} -> b
  | x ->
      raise
        (SmartExcept [`Value x; `Text "is not an key_hash"; `Br; `Rec (pp ())])

let un_baker = function
  | {v = Variant (_, _, "Some", {v = Literal (Key_hash b)})} -> Some b
  | {v = Variant (_, _, "None", _)} -> None
  | _ -> assert false

let plus_inner ~primitives x y =
  let module P = (val primitives : Primitives.Primitives) in
  match (x.v, y.v) with
  | Literal (Int {i}), Literal (Int {i = j}) ->
      let isNat =
        match Type.getRepr (type_of_value x) with
        | TInt {isNat} -> isNat
        | _ -> assert false
      in
      intOrNat isNat (Big_int.add_big_int i j)
      (* TODO type *)
  | Literal (Mutez x), Literal (Mutez y) -> mutez (Big_int.add_big_int x y)
  | Literal (String x), Literal (String y) -> string (x ^ y)
  | Literal (Bytes x), Literal (Bytes y) -> bytes (x ^ y)
  | Literal (Bls12_381_g1 x), Literal (Bls12_381_g1 y) ->
      bls12_381_g1
        (Misc.Hex.unhex
           ( try P.Bls12.addG1 (Misc.Hex.hexcape x) (Misc.Hex.hexcape y) with
           | e ->
               Printf.ksprintf failwith "add(G1, G1): %s" (Base.Exn.to_string e)
           ))
  | Literal (Bls12_381_g2 x), Literal (Bls12_381_g2 y) ->
      bls12_381_g2
        (Misc.Hex.unhex
           ( try P.Bls12.addG2 (Misc.Hex.hexcape x) (Misc.Hex.hexcape y) with
           | e ->
               Printf.ksprintf failwith "add(G2, G2): %s" (Base.Exn.to_string e)
           ))
  | Literal (Bls12_381_fr x), Literal (Bls12_381_fr y) ->
      bls12_381_fr
        (Misc.Hex.unhex
           ( try P.Bls12.addFr (Misc.Hex.hexcape x) (Misc.Hex.hexcape y) with
           | e ->
               Printf.ksprintf failwith "add(Fr, Fr): %s" (Base.Exn.to_string e)
           ))
  | _ ->
      raise
        (SmartExcept
           [ `Text "Invalid + operation with different types "
           ; `Value x
           ; `Value y ])

let plus ~primitives x y = plus_inner ~primitives x y

let mul ~primitives x y =
  let module P = (val primitives : Primitives.Primitives) in
  match (x.v, y.v) with
  | Literal (Int {i}), Literal (Int {i = j}) ->
      let isNat =
        match Type.getRepr (type_of_value x) with
        | TInt {isNat} -> isNat
        | _ -> assert false
      in
      intOrNat isNat (Big_int.mult_big_int i j)
      (* TODO type *)
  | Literal (Int {i}), Literal (Bls12_381_fr fr)
   |Literal (Bls12_381_fr fr), Literal (Int {i}) ->
      bls12_381_fr
        (Misc.Hex.unhex
           ( try
               P.Bls12.multiplyFrByInt
                 (Misc.Hex.hexcape fr)
                 (Big_int.string_of_big_int i)
             with
           | e ->
               Printf.ksprintf
                 failwith
                 "multiplyFrByInt: %s"
                 (Base.Exn.to_string e) ))
  | Literal (Bls12_381_g1 x), Literal (Bls12_381_fr fr) ->
      bls12_381_g1
        (Misc.Hex.unhex
           ( try
               P.Bls12.multiplyG1ByFr (Misc.Hex.hexcape x) (Misc.Hex.hexcape fr)
             with
           | e ->
               Printf.ksprintf
                 failwith
                 "multiplyG1ByFr: %s"
                 (Base.Exn.to_string e) ))
  | Literal (Bls12_381_g2 x), Literal (Bls12_381_fr fr) ->
      bls12_381_g2
        (Misc.Hex.unhex
           ( try
               P.Bls12.multiplyG2ByFr (Misc.Hex.hexcape x) (Misc.Hex.hexcape fr)
             with
           | e ->
               Printf.ksprintf
                 failwith
                 "multiplyG2ByFr: %s"
                 (Base.Exn.to_string e) ))
  | Literal (Bls12_381_fr x), Literal (Bls12_381_fr fr) ->
      bls12_381_fr
        (Misc.Hex.unhex
           ( try
               P.Bls12.multiplyFrByFr (Misc.Hex.hexcape x) (Misc.Hex.hexcape fr)
             with
           | e ->
               Printf.ksprintf
                 failwith
                 "multiplyFrByFr: %s"
                 (Base.Exn.to_string e) ))
  | Literal (Mutez x), Literal (Int {i}) | Literal (Int {i}), Literal (Mutez x)
    ->
      mutez (Big_int.mult_big_int x i)
  | _ -> failwith "Invalid * operation with different types"

let shift_left x y =
  match (x.v, y.v) with
  | Literal (Int {i}), Literal (Int {i = j}) ->
      if Bigint.compare j (Bigint.of_int 1000000) > 1
      then
        raise
          (SmartExcept
             [ `Text "shift_left with too big shift value"
             ; `Value x
             ; `Text "<<"
             ; `Value y ]);
      nat (Big_int.shift_left_big_int i (Big_int.int_of_big_int j))
  | _ -> failwith "Invalid << operation with different types"

let shift_right x y =
  match (x.v, y.v) with
  | Literal (Int {i}), Literal (Int {i = j}) ->
      if Bigint.compare j (Bigint.of_int 1000000) > 1
      then
        raise
          (SmartExcept
             [ `Text "shift_left with too big shift value"
             ; `Value x
             ; `Text ">>"
             ; `Value y ]);
      nat (Big_int.shift_right_big_int i (Big_int.int_of_big_int j))
  | _ -> failwith "Invalid >> operation with different types"

let xor x y =
  match (x.v, y.v) with
  | Literal (Int {i}), Literal (Int {i = j}) -> nat (Big_int.xor_big_int i j)
  | Literal (Bool i), Literal (Bool j) -> bool (i <> j)
  | _ -> failwith "Invalid xor operation with different types"

let e_mod x y =
  match (x.v, y.v) with
  | Literal (Int {i}), Literal (Int {i = j}) ->
      let isNat =
        match Type.getRepr (type_of_value x) with
        | TInt {isNat} -> isNat
        | _ -> assert false
      in
      intOrNat isNat (Big_int.mod_big_int i j)
      (* TODO type *)
  | _ -> failwith "Invalid * operation with different types"

let div_inner x y =
  match (x.v, y.v) with
  | Literal (Int {i = x}), Literal (Int {i = y}) ->
      nat (Big_int.div_big_int x y) (* TODO type *)
  | _ -> failwith "Invalid / operation with different types"

let div x y = div_inner x y

let minus x y =
  match (openV x, openV y) with
  | Literal (Int {i = x}), Literal (Int {i = y}) ->
      int (Big_int.sub_big_int x y) (* TODO type *)
  | Literal (Mutez x_), Literal (Mutez y_) ->
      let res = Big_int.sub_big_int x_ y_ in
      if Bigint.compare res Bigint.zero_big_int < 0
      then
        raise
          (SmartExcept
             [ `Text "Invalid - operation on mutez (negative result) "
             ; `Value x
             ; `Value y ]);
      mutez res
  | Literal (Timestamp x), Literal (Timestamp y) ->
      int (Big_int.sub_big_int x y)
  | _ -> failwith "Invalid - operation"

let key_hash s = literal (Literal.key_hash s)

let key s = literal (Literal.key s)

let secret_key s = literal (Literal.secret_key s)

let signature s = literal (Literal.signature s)

let record ~layout = function
  | [] -> unit
  | l -> build_value (Record (layout, l))

let record_comb = function
  | [] -> unit
  | l ->
      let layout = Type.comb_layout `Right (List.map fst l) in
      record ~layout l

let tuple vs = build_value (Tuple vs)

let untuple ~pp = function
  | {v = Tuple vs} -> vs
  | x ->
      raise (SmartExcept [`Value x; `Text "is not a tuple"; `Br; `Rec (pp ())])

let un_record = function
  | {v = Record (_, bs)} -> bs
  | _ -> failwith "unRecord"

let variant_with_type ?no_check ~line_no name x t =
  match Type.view_variant t with
  | None -> assert false
  | Some (layout, row) ->
      let layout = Option.get ~msg:"Value.variant" (Unknown.get layout) in
      let vt = type_of_value x in
      let err message t' =
        raise
          (SmartExcept
             ( [ `Text message
               ; `Br
               ; `Text name
               ; `Text "="
               ; `Value x
               ; `Text "of type"
               ; `Type vt
               ; `Br ]
             @ ( match t' with
               | None -> []
               | Some t' ->
                   [`Type vt; `Text "is not compatible with"; `Br; `Type t'; `Br]
               )
             @ [`Text "in type"; `Type t; `Br; `Line line_no] ))
      in
      ( if no_check = None
      then
        match List.assoc_opt name row with
        | Some t' ->
            if not (same_type vt t')
            then err "Variant type error in field" (Some t')
        | None -> err "Variant type missing field" None );
      build_value (Variant (layout, row, name, x))

let none t =
  let row = [("None", Type.unit); ("Some", t)] in
  let layout = Type.default_layout_of_row Config.Comb (List.map fst row) in
  build_value (Variant (layout, row, "None", unit))

let some x =
  let tx = type_of_value x in
  let row = [("None", Type.unit); ("Some", tx)] in
  let layout = Type.default_layout_of_row Config.Comb (List.map fst row) in
  build_value (Variant (layout, row, "Some", x))

let left ty x =
  let tx = type_of_value x in
  let row = [("Left", tx); ("Right", ty)] in
  let layout = Type.default_layout_of_row Config.Comb (List.map fst row) in
  build_value (Variant (layout, row, "Left", x))

let right tx y =
  let ty = type_of_value y in
  let row = [("Left", tx); ("Right", ty)] in
  let layout = Type.default_layout_of_row Config.Comb (List.map fst row) in
  build_value (Variant (layout, row, "Right", y))

let option t = Option.cata (none t) some

let ediv x y =
  let ediv x y ~a_t ~b_t ~a_f ~b_f =
    if Big_int.eq_big_int Big_int.zero_big_int y
    then none (Type.pair a_t b_t)
    else
      some
        (tuple [a_f (Big_int.div_big_int x y); b_f (Big_int.mod_big_int x y)])
  in
  match (x.v, y.v) with
  | Literal (Int {i = x_}), Literal (Int {i = y_}) ->
      let both_nat =
        match
          (Type.getRepr (type_of_value x), Type.getRepr (type_of_value y))
        with
        | TInt {isNat = isNat1}, TInt {isNat = isNat2} ->
          begin
            match (Unknown.get isNat1, Unknown.get isNat2) with
            | Some true, Some true -> true
            | (Some _ | None), (Some _ | None) -> false
          end
        | _, _ -> assert false
      in
      if both_nat
      then ediv x_ y_ ~a_t:(Type.nat ()) ~b_t:(Type.nat ()) ~a_f:nat ~b_f:nat
      else ediv x_ y_ ~a_t:(Type.int ()) ~b_t:(Type.nat ()) ~a_f:int ~b_f:nat
  | Literal (Mutez x_), Literal (Mutez y_) ->
      (* tez -> tez -> (nat * tez) *)
      ediv x_ y_ ~a_t:(Type.nat ()) ~b_t:Type.token ~a_f:nat ~b_f:mutez
  | Literal (Mutez x_), Literal (Int {i = y_}) ->
      (* tez -> nat -> (tez * tez) *)
      ediv x_ y_ ~a_t:Type.token ~b_t:Type.token ~a_f:mutez ~b_f:mutez
  | _ -> raise (SmartExcept [`Text "Invalid / operation with different types"])

let intXor a b =
  match (openV a, openV b) with
  | Literal (Int _a), Literal (Int _b) -> assert false (*int (a lxor b)*)
  | _ -> failwith "Invalid intXor operation"

let address ?entry_point s = literal (Literal.address ?entry_point s)

let contract ?entry_point address type_ =
  build_value (Contract {address; entry_point; type_})

let cons x l =
  ( match Type.normalize (type_of_value l) with
  | F (T1 (T_list, t)) -> assert (same_type t (type_of_value x))
  | _ -> assert false );
  match l with
  | {v = List (t, l)} ->
      assert (same_type t (type_of_value x));
      build_value (List (t, x :: l))
  | _ -> failwith "Type error list"

(** Access the elements of a list. *)
let lens_list =
  Lens.make (function
      | {v = List (t, focus)} -> {focus; zip = (fun focus -> build_list focus t)}
      | _ -> failwith "lens_list")

let lens_list_nth n = Lens.(lens_list @. Lens.nth n)

(** Access the entries of a map. *)
let lens_map =
  Lens.make (function
      | {v = Map (tkey, tvalue, big, focus)} ->
          {focus; zip = (fun focus -> map ~big ~tkey ~tvalue focus)}
      | _ -> failwith "lens_map")

let lens_map_at ~key = Lens.(lens_map @. Lens.assoc ~equal:V.equal ~key)

(** Access the elements of a set. *)
let lens_set =
  Lens.make (function
      | {v = Set (telement, focus)} ->
          {focus; zip = (fun focus -> set focus ~telement)}
      | _ -> failwith "lens_set")

let lens_set_at ~elem = Lens.(lens_set @. sorted_list ~equal:V.equal ~elem)

(** Access the entries of a record. *)
let lens_record =
  Lens.make (function
      | {v = Record (layout, focus)} -> {focus; zip = record ~layout}
      | _ -> failwith "lens_map")

let lens_record_at ~attr =
  Lens.(lens_record @. Lens.assoc ~equal:( = ) ~key:attr)

let rec zero_of_type t =
  match Type.getRepr t with
  | T0 T_unit -> unit
  | T0 T_bool -> bool false
  | TInt {isNat} ->
    ( match Typing.intType isNat with
    | `Nat -> nat Big_int.zero_big_int
    | `Int -> int Big_int.zero_big_int
    | `Unknown -> int Big_int.zero_big_int )
  | TBounded {cases} ->
    ( match Unknown.get cases with
    | None -> assert false
    | Some {cases = []} -> assert false
    | Some {cases = case :: _} -> literal case )
  | T0 T_timestamp -> timestamp Big_int.zero_big_int
  | T0 T_string -> string ""
  | T0 T_bytes -> bytes ""
  | TRecord {layout; row} ->
    ( match Unknown.get layout with
    | None -> assert false
    | Some layout ->
        record ~layout ((List.map (fun (lbl, t) -> (lbl, zero_of_type t))) row)
    )
  | TVariant {row = []} -> failwith "zero_of_type: empty variant type"
  | TVariant {row = (cons, t0) :: _} ->
      variant_with_type cons (zero_of_type t0) t ~line_no:[]
  | T1 (T_set, telement) -> set ~telement []
  | T2 (((T_map | T_big_map) as tm), tkey, tvalue) ->
      map ~big:(tm = T_big_map) ~tkey ~tvalue []
  | T0 T_address -> address ""
  | T0 T_key_hash -> key_hash ""
  | T0 T_key -> key ""
  | T0 T_signature -> signature ""
  | T0 T_mutez -> mutez Big_int.zero_big_int
  | TVar _ | TUnknown _ -> failwith "zero_of_type: unknown"
  | TTuple ts -> tuple (List.map zero_of_type ts)
  | T1 (T_list, t) -> build_list [] t
  | T0 T_chain_id -> chain_id ""
  | TSecretKey -> secret_key ""
  | TLambda _ | T1 (T_contract, _) | T2 (T_lambda, _, _) ->
      raise
        (SmartExcept [`Text "zero_of_type not implemented on type"; `Type t])
  | T0 T_operation -> failwith "zero_of_type: operation"
  | TSaplingState _ -> failwith "zero_of_type: sapling_state"
  | TSaplingTransaction _ -> failwith "zero_of_type: sapling_transaction"
  | T0 T_never -> failwith "zero_of_type: never"
  | T1 (T_ticket, t) -> ticket "" (zero_of_type t) (Bigint.of_int 0)
  | T0 T_bls12_381_g1 -> bls12_381_g1 "0x00"
  | T0 T_bls12_381_g2 -> bls12_381_g2 "0x00"
  | T0 T_bls12_381_fr -> bls12_381_fr "0x00"
  | T0 T_chest_key -> chest_key "0x00"
  | T0 T_chest -> chest "0x00"
  | T0 (T_nat | T_int | T_sapling_state _ | T_sapling_transaction _)
   |T1 (T_option, _)
   |T2 ((T_pair _ | T_or _), _, _) ->
      assert false

let nextId prefix =
  let ids = ref 0 in
  fun () ->
    incr ids;
    Printf.sprintf "%s%i" prefix !ids

let closure_init l = build_value (Closure (l, []))

let closure_apply v x =
  match v with
  | {v = Closure (l, args)} -> build_value (Closure (l, x :: args))
  | _ -> failwith "closure_apply: not a closure"

let unclosure ~pp = function
  | {v = Closure (l, args)} -> (l, args)
  | x ->
      raise
        (SmartExcept [`Value x; `Text "is not a closure"; `Br; `Rec (pp ())])

let project_literals proj v =
  let module Printer = ( val Printer.get_by_language Config.SmartPy
                           : Printer.Printer )
  in
  let rec aux acc path v =
    match v.v with
    | Literal l | Bounded (_, l) ->
      ( match proj l with
      | Some a -> (a, path) :: acc
      | None -> acc )
    | Record (_, l) ->
        List.fold_left (fun acc (f, v) -> aux acc (f :: path) v) acc l
    | Variant (_layout, _row, f, v) -> aux acc (f :: path) v
    | List (_, l) | Set (_, l) ->
        List.fold_left (fun acc v -> aux acc path v) acc l
    | Map (_, _, _, l) ->
        List.fold_left
          (fun acc (f, v) ->
            aux
              (aux acc ("key" :: path) f)
              (Printer.value_to_string f :: path)
              v)
          acc
          l
    | Tuple [f; s] ->
        let acc = aux acc ("fst" :: path) f in
        aux acc ("snd" :: path) s
    | Tuple _ -> failwith "project_literals: TODO n-tuples"
    | Contract _ -> acc
    | Closure _ -> acc
    | Operation _ -> acc
    | Ticket (_, v, _) -> aux acc path v
  in
  List.rev (aux [] [] v)

let rec get_field_opt field v =
  match v.v with
  | Record (_, l) -> List.assoc_opt field l
  | Tuple vs ->
      let rec find = function
        | [] -> None
        | x :: xs ->
          ( match get_field_opt field x with
          | None -> find xs
          | Some x -> Some x )
      in
      find vs
  | _ -> None

let unoperation ~pp = function
  | {v = Operation op} -> op
  | x ->
      raise
        (SmartExcept [`Value x; `Text "is not an operation"; `Br; `Rec (pp ())])

(* baker_hash  TODO (Incoming proposal will change key_hash to baker_hash) *)
let baker_value_of_protocol = function
  | Delphi | Edo | Florence | Granada | Hangzhou | Ithaca -> key_hash

let unPAIRn = function
  (* "pair" is necessary (tzstats KT1GgUJwMQoFayRYNwamRAYCvHBLzgorLoGo) *)
  | Sequence (m1 :: rest)
   |Primitive {name = "Pair" | "pair"; arguments = m1 :: (_ :: _ :: _ as rest)}
    ->
      Primitive
        { name = "Pair"
        ; annotations = []
        ; arguments =
            [m1; Primitive {name = "Pair"; annotations = []; arguments = rest}]
        }
  | Primitive {name = "pair"; arguments} ->
      Primitive {name = "Pair"; annotations = []; arguments}
  | x -> x

let of_micheline ~config ~pp_mich =
  let module Printer = (val Printer.get config : Printer.Printer) in
  let rec parse_record (layout : Layout.t) row m =
    let rec aux layout m =
      match (layout : _ Binary_tree.t) with
      | Leaf Layout.{source = a} ->
          let t = List.assoc a row in
          [(a, to_value t m)]
      | Node (l1, l2) ->
        begin
          match unPAIRn m with
          | Primitive {name = "Pair"; arguments = [m1; m2]} ->
              let r1 = aux l1 m1 in
              let r2 = aux l2 m2 in
              r1 @ r2
          | _ ->
              Printf.ksprintf
                failwith
                "parse_record %s %s"
                (Layout.show layout)
                (Micheline.pretty "" m)
        end
    in
    aux layout m
  and parse_variant (layout : Layout.t) row m =
    let rec aux layout m =
      match (layout, m) with
      | Binary_tree.Leaf Layout.{source = a}, m ->
          let t = List.assoc a row in
          (a, to_value t m)
      | Binary_tree.Node (l1, _l2), Primitive {name = "Left"; arguments = [m]}
        ->
          aux l1 m
      | Binary_tree.Node (_l1, l2), Primitive {name = "Right"; arguments = [m]}
        ->
          aux l2 m
      | _ ->
          Printf.ksprintf
            failwith
            "parse_variant %s %s"
            (Layout.show layout)
            (Micheline.pretty "" m)
    in
    aux layout m
  and to_value t m =
    let err () =
      Printf.ksprintf
        failwith
        "to_value %s %s"
        (Printer.type_to_string t)
        (Micheline.pretty "" m)
    in
    match (Type.getRepr t, m) with
    | TTuple [t1; t2], _ ->
      begin
        match unPAIRn m with
        | Primitive {name = "Pair"; arguments = [x1; x2]} ->
            tuple [to_value t1 x1; to_value t2 x2]
        | _ ->
            Printf.ksprintf
              failwith
              "to_value tuple || %s || %s ||"
              (Printer.type_to_string t)
              (Micheline.pretty "" m)
      end
    | ( TVariant {row = [("Left", t1); ("Right", _t2)]}
      , Primitive {name = "Left"; arguments = [x]} ) ->
        variant_with_type ~line_no:[] "Left" (to_value t1 x) t
    | ( TVariant {row = [("Left", _t1); ("Right", t2)]}
      , Primitive {name = "Right"; arguments = [x]} ) ->
        variant_with_type ~line_no:[] "Right" (to_value t2 x) t
    | ( TVariant {row = [("None", F (T0 T_unit)); ("Some", t)]}
      , Primitive {name = "Some"; arguments = [x]} ) ->
        some (to_value t x)
    | ( TVariant {row = [("None", F (T0 T_unit)); ("Some", t)]}
      , Primitive {name = "None"; arguments = []} ) ->
        none t
    | TVariant {layout; row}, m ->
      ( match Unknown.get layout with
      | Some l ->
          let name, v = parse_variant l row m in
          variant_with_type ~line_no:[] name v t
      | None -> err () )
    | (T0 T_unit | TRecord {row = []}), Primitive {name = "Unit"; arguments = []}
      ->
        unit
    | TRecord {row = []}, _ -> unit
    | TRecord {layout; row}, m ->
      ( match Unknown.get layout with
      | Some layout ->
          let entries = parse_record layout row m in
          record ~layout entries
      | None -> err () )
    | T0 T_bool, Primitive {name = "False"} -> bool false
    | T0 T_bool, Primitive {name = "True"} -> bool true
    | T0 T_mutez, (Int i | String i) -> mutez (Big_int.big_int_of_string i)
    | T0 T_timestamp, (Int i | String i) ->
        let i =
          if Base.String.contains i 'Z' then SmartDom.parseDate i else i
        in
        timestamp (Big_int.big_int_of_string i)
        (* TODO *)
    | TInt _, (Int i | String i) ->
        let isNat =
          match Type.getRepr t with
          | TInt {isNat} -> isNat
          | _ -> assert false
        in
        intOrNat isNat (Big_int.big_int_of_string i)
    | T0 T_string, String i -> string i
    | T0 T_key, String i -> key i
    | T0 T_key, Bytes b -> key (Utils.Bs58.encode_key (Misc.Hex.hexcape b))
    | T0 T_bls12_381_g1, String i -> key i
    | T0 T_bls12_381_g2, String i -> key i
    | T0 T_bls12_381_fr, String i -> key i
    | T0 T_bls12_381_g1, Bytes i -> bls12_381_g1 i
    | T0 T_bls12_381_g2, Bytes i -> bls12_381_g2 i
    | T0 T_bls12_381_fr, Bytes i -> bls12_381_fr i
    | T0 T_signature, String i -> signature i
    | T0 T_signature, Bytes b ->
        signature (Utils.Bs58.encode_signature (Misc.Hex.hexcape b))
    | T0 T_bytes, String i -> bytes (Misc.Hex.unhex i)
    | T0 T_bytes, Bytes i -> bytes i
    | T0 T_chain_id, String i -> chain_id (Misc.Hex.unhex i)
    | T0 T_chain_id, Bytes i -> chain_id i
    | T0 T_key_hash, String i -> key_hash i
    | T0 T_key_hash, Bytes b ->
        key_hash (Utils.Bs58.encode_key_hash (Misc.Hex.hexcape b))
    | T1 (T_contract, _), String i -> address i
    | T1 (T_contract, _), Bytes b ->
        address (Utils.Bs58.encode_address (Misc.Hex.hexcape b))
    | T0 T_address, String i -> address i
    | T0 T_address, Bytes a ->
        address (Utils.Bs58.encode_address (Misc.Hex.hexcape a))
    | T1 (T_list, elt), Sequence l -> build_list (List.map (to_value elt) l) elt
    | T1 (T_option, t), Primitive {name = "Some"; arguments = [x]} ->
        some (to_value t x)
    | T1 (T_option, t), Primitive {name = "None"; arguments = []} -> none t
    | T2 (((T_map | T_big_map) as tm), tkey, tvalue), Sequence l ->
        let pairOfStorage = function
          | Primitive {name = "Elt"; arguments = [k; v]} ->
              (to_value tkey k, to_value tvalue v)
          | p ->
              failwith
                (Printf.sprintf "Bad map pair %s " (Micheline.pretty "" p))
        in
        map ~big:(tm = T_big_map) ~tkey ~tvalue (List.map pairOfStorage l)
    | T2 (((T_map | T_big_map) as tm), tkey, tvalue), Int _ ->
        (* show empty big maps *)
        map ~big:(tm = T_big_map) ~tkey ~tvalue []
    | T1 (T_set, telement), Sequence l ->
        set ~telement (List.map (to_value telement) l)
    | ( T1 (T_ticket, t)
      , Primitive
          {name = "Pair"; arguments = [String ticketer; content; Int amount]} )
      ->
        let ticketer = ticketer in
        let content = to_value t content in
        let amount = Big_int.big_int_of_string amount in
        ticket ticketer content amount
    | ( T1 (T_ticket, t)
      , Primitive
          { name = "Pair"
          ; arguments =
              [ Bytes ticketer
              ; Primitive {name = "Pair"; arguments = [content; Int amount]} ]
          } ) ->
        let ticketer = Utils.Bs58.encode_address (Misc.Hex.hexcape ticketer) in
        let content = to_value t content in
        let amount = Big_int.big_int_of_string amount in
        ticket ticketer content amount
    | T2 (T_lambda, _, _), _ -> assert false
    | TLambda (effects, t1, t2), x ->
        assert (not (Type.has_effects effects));
        let line_no = [] in
        let name = "x" in
        let params = Expr.lambdaParams ~line_no name in
        let mich =
          let name = pp_mich t1 x in
          {name; parsed = x; typesIn = [t1]; typesOut = [t2]}
        in
        let mich = Expr.inline_michelson ~line_no mich [params] in
        let body = Command.result ~line_no mich in
        let lambda =
          { name
          ; tParams = U
          ; body
          ; tResult = U
          ; clean_stack = false
          ; with_storage = None
          ; with_operations = false }
        in
        let config = Config.default in
        let lambda = Checker.check_lambda config [] lambda in
        let t1' = get_extra lambda.tParams in
        let t2' = get_extra lambda.tResult in
        if not (same_type t1 t1')
        then
          raise
            (SmartExcept
               [ `Text "Unpacked lambda has wrong input type. Expected: "
               ; `Type t1
               ; `Text "Got: "
               ; `Type t1' ]);
        if not (same_type t2 t2')
        then
          raise
            (SmartExcept
               [ `Text "Unpacked lambda has wrong output type. Expected: "
               ; `Type t2
               ; `Text "Got: "
               ; `Type t2' ]);
        closure_init lambda
    | TSaplingState {memo}, _ ->
      begin
        match Unknown.get memo with
        | Some memo -> literal (Literal.sapling_state_real memo)
        | None ->
            Printf.ksprintf
              failwith
              "to_value (sapling) %s %s"
              (Printer.type_to_string t)
              (Micheline.pretty "" m)
      end
    | _ -> err ()
  in
  to_value

(* NB The large number of 'assert (same_type ...)' below are
   indicative of redundancy in 'value'. *)
let typecheck_f (tv : (Type.t -> tvalue) value_f) =
  let msg = "Value.typecheck_f" in
  match tv with
  | Literal l as tv ->
      fun t ->
        assert (same_type t (Type.type_of_literal l));
        {t; tv}
  | Bounded (_, l) as tv ->
      (function
      | F (TBounded _) as t ->
          (* TODO check cases *)
          assert (same_type t (Type.type_of_literal l));
          {t; tv}
      | _ -> assert false)
  | Contract c as tv ->
      fun t ->
        assert (same_type t (Type.contract c.type_));
        {t; tv}
  | Record (layout, entries) ->
      (function
      | F (TRecord {layout = l'; row}) as t ->
          let l' = Option.get ~msg (Unknown.get l') in
          assert (Layout.equal layout l');
          let entries =
            let f (fld, v) =
              let t = Option.get ~msg (List.assoc_opt fld row) in
              (fld, v t)
            in
            List.map f entries
          in
          (* TODO *)
          assert (same_type t t);
          {t; tv = Record (layout, entries)}
      | _ -> assert false)
  | Variant (layout, row', cons, arg) ->
      fun t ->
        ( match Type.view_variant t with
        | None -> assert false
        | Some (l', row) ->
            let l' = Option.get ~msg (Unknown.get l') in
            assert (Layout.equal layout l');
            assert (Type.equal_row Type.equal row row');
            let t_arg = Option.get ~msg (List.assoc_opt cons row) in
            let arg = arg t_arg in
            {t; tv = Variant (layout, row', cons, arg)} )
  | List (tx, xs) ->
      (function
      | F (T1 (T_list, tx')) as t ->
          assert (same_type tx tx');
          let xs = List.map (fun x -> x tx) xs in
          {t; tv = List (t, xs)}
      | _ -> assert false)
  | Set (tx, xs) ->
      (function
      | F (T1 (T_set, tx')) as t ->
          assert (same_type tx tx');
          let xs = List.map (fun x -> x tx) xs in
          {t; tv = Set (t, xs)}
      | _ -> assert false)
  | Map (tk, tv, false, entries) ->
      (function
      | F (T2 (T_map, tk', tv')) as t ->
          assert (same_type tk tk');
          assert (same_type tv tv');
          let entries = List.map (fun (k, v) -> (k tk, v tv)) entries in
          {t; tv = Map (tk, tv, false, entries)}
      | _ -> assert false)
  | Map (tk, tv, true, entries) ->
      (function
      | F (T2 (T_big_map, tk', tv')) as t ->
          assert (same_type tk tk');
          assert (same_type tv tv');
          let entries = List.map (fun (k, v) -> (k tk, v tv)) entries in
          {t; tv = Map (tk, tv, true, entries)}
      | _ -> assert false)
  | Tuple xs ->
      (function
      | F (TTuple ts) as t ->
          let xs = List.map2 (fun x t -> x t) xs ts in
          {t; tv = Tuple xs}
      | _ -> assert false)
  | Closure _ -> (* TODO *) assert false
  | Operation _ as tv ->
      (function
      | F (T0 T_operation) -> {t = Type.operation; tv}
      | _ -> assert false)
  | Ticket _ -> (* TODO *) assert false

let typecheck t v = cata_value typecheck_f v t

(** Reifies records and variants according to the specified type. *)
let rec smartMLify t_to (v : value) =
  let msg = "Value.smartMLify_f" in
  match (Type.unF t_to, v.v) with
  | TRecord {layout; row = [(fld, t)]}, _ ->
      let layout = Option.get ~msg (Unknown.get layout) in
      {v = Record (layout, [(fld, smartMLify t v)])}
  | TRecord {layout; row}, Tuple vs ->
      let layout = Option.get ~msg (Unknown.get layout) in
      let ts =
        List.map
          (fun ({source} : Layout.l) ->
            (source, Option.get ~msg (List.assoc_opt source row)))
          (Binary_tree.to_list layout)
      in
      let entries = List.map2 (fun v (fld, t) -> (fld, smartMLify t v)) vs ts in
      {v = Record (layout, entries)}
  | T1 (T_option, _), _ -> v
  | T2 (T_or _, _, _), _ -> v
  | TVariant {layout; row}, _ ->
      let layout = Option.get ~msg (Unknown.get layout) in
      let cases =
        Binary_tree.map
          (fun ({source} : Layout.l) ->
            (source, Option.get ~msg (List.assoc_opt source row)))
          layout
      in
      let rec find_case cases v =
        let open Binary_tree in
        match (cases, v.v) with
        | Leaf (case, t), _ -> {v = Variant (layout, row, case, smartMLify t v)}
        | Node (l, _), Variant (_, _, "Left", x) -> find_case l x
        | Node (_, r), Variant (_, _, "Right", x) -> find_case r x
        | _ -> assert false
      in
      find_case cases v
  | _, ((Literal _ | Bounded _ | Contract _ | Operation _) as v) -> {v}
  | T1 (T_list, t), List (t', xs) ->
      {v = List (t', List.map (fun x -> smartMLify t x) xs)}
  | T1 (T_set, t), Set (t', xs) ->
      {v = Set (t', List.map (fun x -> smartMLify t x) xs)}
  | T2 (T_map, tk, tv), Map (tk', tv', big, xs) ->
      let xs = List.map (fun (k, v) -> (smartMLify tk k, smartMLify tv v)) xs in
      {v = Map (tk', tv', big, xs)}
  | TTuple ts, Tuple xs -> {v = Tuple (List.map2 smartMLify ts xs)}
  | _, Closure _ -> (* TODO *) assert false
  | _, Ticket _ -> (* TODO *) assert false
  | _ -> failwith ("FAIL: " ^ Type.show t_to ^ "\n" ^ show v)
