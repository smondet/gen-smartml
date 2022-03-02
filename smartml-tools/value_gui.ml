(* Copyright 2019-2021 Smart Chain Arena LLC. *)

open SmartML
open Utils
open Value

type inputGuiResult =
  { gui : string
  ; get : bool -> t }

let textInputGui path ?default nextId initialType cont =
  let id = nextId () in
  let gui = Printf.sprintf "<input type='text' id='%s'>" id in
  let get _withZeros =
    let s = SmartDom.getText id in
    try
      match (s, default) with
      | "", Some x -> cont x
      | x, _ -> cont x
    with
    | exn ->
        let error =
          match exn with
          | Failure error -> Printf.sprintf " (%s)" error
          | _ -> ""
        in
        failwith
          (Printf.sprintf
             "Cannot convert '%s' into %s for path %s%s"
             s
             initialType
             (String.concat "." (List.rev path))
             error)
  in
  {gui; get}

let error s = string s

let parseIntOrNat isNat s =
  let i = Big_int.big_int_of_string s in
  match Typing.intType isNat with
  | `Nat ->
      if Big_int.compare_big_int i Big_int.zero_big_int < 0
      then failwith (Printf.sprintf "%s is not a nat" s);
      nat i
  | `Unknown -> intOrNat (Unknown.unknown ()) i
  | `Int -> int i
  | `Error -> assert false

let enum_list t =
  Type.variant_default_layout
    Config.Comb
    [ ("Zero", Type.unit)
    ; ("One", t)
    ; ("Two", Type.pair t t)
    ; ("Three", Type.tuple [t; t; t])
    ; ("Four", Type.tuple [t; t; t; t]) ]

let fields_of_layout layout row =
  let module Printer = ( val Printer.get_by_language Config.SmartPy
                           : Printer.Printer )
  in
  match Unknown.get layout with
  | None -> List.map (fun (x, _) -> (x, x)) row
  | Some layout ->
      let leaf Layout.{source; target} = [(source, target)] in
      Binary_tree.cata leaf ( @ ) layout

let rec inputGuiR path nextId initialType =
  let module Printer = ( val Printer.get_by_language Config.SmartPy
                           : Printer.Printer )
  in
  match Type.getRepr initialType with
  | T0 T_bool ->
      let id = nextId () in
      let gui = Printf.sprintf "<input type='checkbox' id='%s'>" id in
      let get _ = bool (SmartDom.isChecked id) in
      {gui; get}
  | TInt {isNat} -> textInputGui path nextId "int" (parseIntOrNat isNat)
  | TBounded {t = _} ->
      assert false (*textInputGui path nextId "string" string*)
  | T0 T_string -> textInputGui path nextId "string" string
  | T0 T_bytes ->
      textInputGui path nextId "bytes" (fun s -> bytes (Misc.Hex.unhex s))
  | T0 T_mutez ->
      let {gui; get} =
        inputGuiR
          path
          nextId
          (Type.variant_default_layout
             Config.Comb
             [("Tez", Type.nat ()); ("Mutez", Type.nat ())])
      in
      let get context =
        match (get context).v with
        | Variant (_layout, _row, "Tez", i) ->
            Value.mutez
              (Big_int.mult_big_int
                 (Bigint.of_int 1000000)
                 (Value.unInt ~pp:(fun () -> []) i))
        | Variant (_layout, _row, "Mutez", i) ->
            Value.mutez (Value.unInt ~pp:(fun () -> []) i)
        | _ -> assert false
      in
      {gui; get}
      (* textInputGui path nextId "token" (fun i ->
       *     mutez (Big_int.big_int_of_string i)) *)
  | T0 T_key_hash -> textInputGui path nextId "key_hash" Value.key_hash
  | T0 T_timestamp ->
      textInputGui path nextId "timestamp" (fun i ->
          timestamp (Big_int.big_int_of_string i))
  | T0 T_chain_id ->
      textInputGui path nextId "chainid" (fun s -> chain_id (Misc.Hex.unhex s))
  | TSecretKey -> textInputGui path nextId "secret_key" secret_key
  | T0 T_address ->
      textInputGui path nextId "address" (fun s ->
          if not
               ( Base.String.is_prefix ~prefix:"tz" s
               || Base.String.is_prefix ~prefix:"KT" s )
          then failwith (Printf.sprintf "Badly formed address '%s'" s);
          address s)
  | T1 (T_contract, t) ->
      textInputGui path nextId "contract" (fun s ->
          if not (Base.String.is_prefix ~prefix:"KT" s)
          then failwith (Printf.sprintf "Badly formed contract '%s'" s);
          contract s t)
  | T0 T_key -> textInputGui path nextId "key" key
  | T0 T_signature -> textInputGui path nextId "signature" signature
  | (T1 (T_option, _) | TVariant _) as t ->
      let is_option, layout, row =
        match t with
        | TVariant {layout; row} -> (None, layout, row)
        | T1 (T_option, t) ->
            let row = [("None", Type.unit); ("Some", t)] in
            let layout = Type.default_layout_of_row Comb (List.map fst row) in
            (Some t, Unknown.value layout, row)
        | _ -> assert false
      in
      let monoSelectionId = nextId () in
      let putRadio i (name, target) =
        let t = List.assoc name row in
        let input = inputGuiR (name :: path) nextId t in
        let id = nextId () in
        let gui =
          Printf.sprintf
            "<div id='%s' class='%s'>%s</div>"
            id
            (if i = 0 then "" else "hidden")
            input.gui
        in
        let get x =
          match is_option with
          | Some t ->
              if name = "None" then Value.none t else Value.some (input.get x)
          | None -> variant_with_type name ~line_no:[] (input.get x) initialType
        in
        (target, id, {gui; get})
      in
      let fields = fields_of_layout layout row in
      let radios = List.mapi putRadio fields in
      let gui =
        Printf.sprintf
          "<div class='subtype'><select class='selection' id='%s' \
           onchange=\"setSelectVisibility('%s',%s)\">%s</select>%s</div>"
          monoSelectionId
          monoSelectionId
          (String.concat
             ","
             (List.map
                (fun (name, id, _) -> Printf.sprintf "'%s','%s'" name id)
                radios))
          (String.concat
             ""
             (List.map
                (fun (name, _, _) ->
                  Printf.sprintf
                    "<option value='%s'>%s</option>"
                    name
                    (String.capitalize_ascii name))
                radios))
          (String.concat "" (List.map (fun (_, _, input) -> input.gui) radios))
      in
      let get context =
        let selected = SmartDom.getText monoSelectionId in
        match
          List.find_opt (fun (name, _id, _input) -> name = selected) radios
        with
        | Some (_, _, input) -> input.get context
        | None ->
            error
              ( "Missing constructor "
              ^ selected
              ^ " in "
              ^ Printer.type_to_string initialType )
      in
      {gui; get}
  | TRecord {layout; row} ->
      let subs =
        List.map
          (fun (name, t) -> (name, inputGuiR (name :: path) nextId t))
          row
      in
      let gui =
        let fields = fields_of_layout layout row in
        Printer.html_of_record_list
          ( List.map snd fields
          , [List.map (fun (source, _) -> (List.assoc source subs).gui) fields]
          )
          (fun x -> x)
      in
      let get context =
        let layout = Option.get ~msg:"inputGuiR: layout" (Unknown.get layout) in
        record
          ~layout
          (List.map (fun (name, input) -> (name, input.get context)) subs)
      in
      {gui; get}
  | T0 T_unit -> {gui = ""; get = (fun _ -> unit)}
  | TTuple ts ->
      let subs =
        List.mapi
          (fun i t ->
            let name = string_of_int i in
            (name, inputGuiR (name :: path) nextId t))
          ts
      in
      let gui =
        Printer.html_of_record_list
          ([], [List.map (fun (_, input) -> input.gui) subs])
          (fun x -> x)
      in
      let get context =
        Value.tuple (List.map (fun (_name, input) -> input.get context) subs)
      in
      {gui; get}
  | TVar _ | TUnknown _ ->
      let notImplemented =
        Printf.sprintf
          "InputGui of partial types (%s) not implemented"
          (Printer.type_to_string initialType)
      in
      {gui = notImplemented; get = (fun _ -> error notImplemented)}
  | TLambda _ -> textInputGui path nextId "lambda" string
  | T2 (T_lambda, _, _) -> assert false
  | T1 (T_list, t) -> input_list Value.build_list t path nextId
  | T1 (T_set, telement) ->
      input_list (fun l telement -> Value.set l ~telement) telement path nextId
  | T2 (((T_map | T_big_map) as tm), tkey, tvalue) ->
      input_list
        (fun l _ ->
          Value.map
            (List.map
               (function
                 | ({v = Tuple [x; y]} : t) -> (x, y)
                 | _ -> assert false)
               l)
            ~big:(tm = T_big_map)
            ~tkey
            ~tvalue)
        (Type.pair tkey tvalue)
        path
        nextId
  | T0 T_operation ->
      let gui = "Type operation not handled" in
      {gui; get = (fun _ -> error gui)}
  | TSaplingState _ ->
      let gui = "Type sapling_state not handled" in
      {gui; get = (fun _ -> error gui)}
  | TSaplingTransaction _ ->
      let gui = "Type sapling_transaction not handled" in
      {gui; get = (fun _ -> error gui)}
  | T0 T_never ->
      let gui = "Type never has no value" in
      {gui; get = (fun _ -> error gui)}
  | T1 (T_ticket, _) ->
      let gui = "Type ticket cannot be forged" in
      {gui; get = (fun _ -> error gui)}
  | T0 T_bls12_381_g1 ->
      textInputGui path nextId "bls12_381_g1" (fun s ->
          bls12_381_g1 (Misc.Hex.unhex s))
  | T0 T_bls12_381_g2 ->
      textInputGui path nextId "bls12_381_g2" (fun s ->
          bls12_381_g2 (Misc.Hex.unhex s))
  | T0 T_bls12_381_fr ->
      textInputGui path nextId "bls12_381_fr" (fun s ->
          bls12_381_fr (Misc.Hex.unhex s))
  | T0 T_chest_key ->
      textInputGui path nextId "chest_key" (fun s ->
          chest_key (Misc.Hex.unhex s))
  | T0 T_chest ->
      textInputGui path nextId "chest" (fun s -> chest (Misc.Hex.unhex s))
  | ( T0 (T_nat | T_int | T_sapling_state _ | T_sapling_transaction _)
    | T2 ((T_pair _ | T_or _), _, _) ) as t ->
      failwith (Type.show (F t))

and input_list f t path nextId =
  let input = inputGuiR ("list" :: path) nextId (enum_list t) in
  let get context =
    match (input.get context).v with
    | Variant (_layout, _row, "Zero", _) -> f [] t
    | Variant (_layout, _row, "One", x) -> f [x] t
    | Variant (_layout, _row, "Two", {v = Tuple [x; y]}) -> f [x; y] t
    | Variant (_layout, _row, "Three", {v = Tuple [x1; x2; x3]}) ->
        f [x1; x2; x3] t
    | Variant (_layout, _row, "Four", {v = Tuple [x1; x2; x3; x4]}) ->
        f [x1; x2; x3; x4] t
    | _ -> assert false
  in
  {input with get}

let inputGuiR ?(path = []) ~nextId initialType =
  inputGuiR path nextId initialType
