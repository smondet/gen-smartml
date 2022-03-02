(* Copyright 2019-2021 Smart Chain Arena LLC. *)

open SmartML
open Utils
open Michelson

let intType isNat =
  match Unknown.get isNat with
  | None -> `Unknown
  | Some true -> `Nat
  | Some false -> `Int

let tree_of_layout row =
  Binary_tree.map (fun Layout.{source; target} ->
      match List.assoc_opt source row with
      | Some t -> (source, target, t)
      | None ->
          Printf.ksprintf
            failwith
            "Missing layout field %S in [%s]"
            source
            (String.concat "; " (List.map fst row)))

let get_layout row layout =
  match Unknown.get layout with
  | None ->
      Printf.ksprintf
        failwith
        "Missing layout for record %s"
        (String.concat " " (List.map fst row))
  | Some layout -> layout

let rec compile_tuple f = function
  | [] | [_] -> failwith "compile_tuple"
  | [x; y] -> f x y
  | x :: xs -> f x (compile_tuple f xs)

let rec mtype_of_type ~with_annots t =
  snd (mtype_of_type_with_single ~with_annots t)

and mtype_of_type_with_single ~with_annots t =
  let mtype_of_type = mtype_of_type ~with_annots in
  let row_of_tree build tr =
    let l (_source, target, t) = (Some target, mtype_of_type t) in
    let n (annot1, t1) (annot2, t2) =
      if with_annots
      then (None, build annot1 annot2 t1 t2)
      else (None, build None None t1 t2)
    in
    Binary_tree.cata l n tr
  in
  let mtype_of_layout build row layout =
    match row with
    | [] -> (None, mt_unit)
    | row ->
        let layout = get_layout row layout in
        row_of_tree build (tree_of_layout row layout)
  in
  match Type.getRepr t with
  | T0 t -> (None, mt0 t)
  | T1 (t, t1) -> (None, mt1 t (mtype_of_type t1))
  | T2 ((T_pair _ | T_or _), _, _) -> assert false
  | T2 (t, t1, t2) -> (None, mt2 t (mtype_of_type t1) (mtype_of_type t2))
  | TLambda (_effects, t1, t2) ->
      (None, mt_lambda (mtype_of_type t1) (mtype_of_type t2))
  | TBounded {t} -> (None, mtype_of_type t)
  | TInt {isNat} ->
      ( None
      , ( match intType isNat with
        | `Unknown -> mt_int
        | `Nat -> mt_nat
        | `Int -> mt_int ) )
  | TRecord {row; layout} ->
      let mt_pair annot_fst annot_snd = mt_pair ?annot_fst ?annot_snd in
      mtype_of_layout mt_pair row layout
  | TVariant {row; layout} ->
      let mt_or annot_left annot_right = mt_or ?annot_left ?annot_right in
      mtype_of_layout mt_or row layout
  | TSecretKey -> (None, mt_var "Secret keys are forbidden in contracts")
  | TVar x -> (None, mt_var x)
  | TUnknown {contents = UUnknown x} -> (None, mt_var x)
  | TUnknown _ -> (None, mt_var "Unknown Type")
  | TTuple ts -> (None, compile_tuple mt_pair (List.map mtype_of_type ts))
  | TSaplingState {memo} ->
      ( None
      , Option.fold
          ~none:(mt_var "sapling state with no explicit memo")
          ~some:(fun memo -> mt_sapling_state memo)
          (Unknown.get memo) )
  | TSaplingTransaction {memo} ->
      ( None
      , Option.fold
          ~none:(mt_var "sapling transaction with no explicit memo")
          ~some:(fun memo -> mt_sapling_transaction memo)
          (Unknown.get memo) )
