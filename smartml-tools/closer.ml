(* Copyright 2019-2021 Smart Chain Arena LLC. *)

open SmartML
open Basics
open Typed
open Utils
open Control

let has_unknowns_talg =
  {(monoid_talg ( || ) false) with f_ttype = Type.has_unknowns}

let has_unknowns_expr = cata_texpr has_unknowns_talg

let has_unknowns_command = cata_tcommand has_unknowns_talg

let gather_unknowns ~tstorage ~tparameter ~private_variables ~entry_points =
  let xs =
    [ ("storage", Type.has_unknowns tstorage)
    ; ("tparameter", Type.has_unknowns tparameter) ]
    @ List.concat
        (List.map
           (fun {channel; body; tparameter_ep_derived} ->
             [ ( channel ^ "(type)"
               , Type.has_unknowns (get_extra tparameter_ep_derived) )
             ; (channel ^ "(command)", has_unknowns_command body) ])
           entry_points)
    @ List.map (fun (name, e) -> (name, has_unknowns_expr e)) private_variables
  in
  let xs = List.filter_map (fun (s, b) -> if b then Some s else None) xs in
  if xs = [] then None else Some (String.concat ", " xs)

let full_unknown =
  let id = ref 0 in
  fun () ->
    let v = string_of_int !id in
    incr id;
    Type.unknown_raw (ref (Type.UUnknown ("c" ^ v)))

let close_layout default row layout =
  Unknown.default layout (Type.default_layout_of_row default (List.map fst row));
  layout

let close_type ~config =
  let open Type in
  let f t0 =
    match t0 with
    | TBounded {t; cases} ->
        let cases =
          match Unknown.get cases with
          | None -> []
          | Some {cases} -> cases
        in
        Type.bounded t true cases
    | TRecord {layout; row} ->
        record_or_unit
          (close_layout config.Config.default_record_layout row layout)
          row
    | TVariant {layout; row} ->
        variant
          (close_layout config.Config.default_variant_layout row layout)
          row
    | TUnknown ({contents = UExact t} as r) ->
        r := UExact t;
        t
    | TUnknown ({contents = URecord l} as r) ->
        let t = record_default_layout config.default_record_layout l in
        r := UExact t;
        t
    | TUnknown ({contents = UTuple l} as r) ->
        assert (l = List.sort Stdlib.compare l);
        let rec mk i0 = function
          | [] -> []
          | (i, t) :: xs ->
              if i = i0
              then t :: mk (i0 + 1) xs
              else full_unknown () :: mk (i0 + 1) ((i, t) :: xs)
        in
        let t = Type.tuple (mk 0 l) in
        r := UExact t;
        t
    | TUnknown ({contents = UVariant l} as r) ->
        let t = variant_default_layout config.default_variant_layout l in
        r := UExact t;
        t
    | TVar _
     |T0
        ( T_unit | T_bool | T_nat | T_int | T_mutez | T_string | T_bytes
        | T_chain_id | T_timestamp | T_address | T_key | T_key_hash
        | T_signature | T_operation | T_sapling_state _
        | T_sapling_transaction _ | T_never | T_bls12_381_g1 | T_bls12_381_g2
        | T_bls12_381_fr | T_chest_key | T_chest )
     |T1 ((T_option | T_list | T_set | T_contract | T_ticket), _)
     |T2 ((T_map | T_big_map), _, _)
     |TLambda _ | TSecretKey | TInt _ | TTuple _ | TSaplingState _
     |TSaplingTransaction _
     |TUnknown {contents = UUnknown _} ->
        F t0
    | T2 ((T_lambda | T_pair _ | T_or _), _, _) -> assert false
  in
  Type.cata f

let close_talg ~config =
  let f_texpr line_no et e = build_texpr ~line_no e (close_type ~config et) in
  let f_tcommand line_no ct c =
    build_tcommand ~line_no c (close_type ~config ct)
  in
  {f_texpr; f_tcommand; f_ttype = close_type ~config}

let close_expr ~config = cata_texpr (close_talg ~config)

let close_command ~config = cata_tcommand (close_talg ~config)

let close_contract {tcontract = c} =
  let extra = get_extra c.derived in
  let tparameter = extra.tparameter in
  let tstorage = extra.tstorage in
  let config = extra.config in
  let ({private_variables; entry_points; derived} as c : _ contract_f) =
    map_contract_f
      (close_expr ~config)
      (close_command ~config)
      (close_type ~config)
      id
      c
  in
  let c =
    { c with
      unknown_parts =
        gather_unknowns ~tstorage ~tparameter ~entry_points ~private_variables
    ; derived }
  in
  {tcontract = c}

let close_scenario ~config =
  Basics.map_scenario_f
    (close_expr ~config)
    (close_command ~config)
    (close_type ~config)
    id
