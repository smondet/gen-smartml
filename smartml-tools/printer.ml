(* Copyright 2019-2021 Smart Chain Arena LLC. *)

open SmartML
open Utils
open Control
open Basics
open Typed
open Untyped
open Printf

module type Printer = sig
  (** {1 Types} *)

  module Options : sig
    type t = private
      { html : bool
      ; stripStrings : bool
      ; types : bool }

    val string : t

    val html : t

    val htmlStripStrings : t

    val types : t
  end

  val type_to_string :
       ?multiline:unit
    -> ?toplevel:unit
    -> ?protect:unit
    -> ?options:Options.t
    -> Type.t
    -> string

  (** {1 Values} *)

  val html_of_data : Options.t -> value -> string

  val value_to_string :
    ?deep:bool -> ?noEmptyList:bool -> ?options:Options.t -> value -> string

  val literal_to_sp_string :
    html:bool -> ?protect:unit -> ?strip_strings:unit -> Literal.t -> string

  val literal_to_string :
    ?deep:bool -> ?options:Options.t -> Basics.Literal.t -> string

  val ppAmount : bool -> Utils.Bigint.t -> string

  (** {1 Expressions, Commands, Contracts} *)

  val layout_to_string : Layout.t Unknown.t -> string

  val mprim0_to_string :
    Michelson.mtype Michelson_base.Primitive.prim0 -> string

  val mprim1_to_string :
       language:Config.language
    -> Michelson.mtype Michelson_base.Primitive.prim1
    -> string

  val mprim2_to_string :
    Michelson.mtype Michelson_base.Primitive.prim2 -> string

  val mprim3_to_string : Michelson_base.Primitive.prim3 -> string

  val expr_to_string : ?options:Options.t -> ?protect:unit -> expr -> string

  val texpr_to_string : ?options:Options.t -> ?protect:unit -> texpr -> string

  val variable_to_string :
    ?options:Options.t -> ?protect:unit -> string -> vClass -> string

  val tvariable_to_string :
    ?options:Options.t -> ?protect:unit -> string * Type.t -> vClass -> string

  val command_to_string :
    ?indent:string -> ?options:Options.t -> command -> string

  val tcommand_to_string :
    ?indent:string -> ?options:Options.t -> tcommand -> string

  val tcontract_to_string :
    ?contract_id:contract_id -> ?options:Options.t -> instance -> string

  val pp_tcontract : ?options:Options.t -> Format.formatter -> instance -> unit

  val html_of_record_list :
    string list * 'a list list -> ('a -> string) -> string

  (** {1 Operations} *)

  val operation_to_string : ?options:Options.t -> operation -> string

  (** {1 Exceptions} *)

  val wrong_condition_string : texpr -> string

  val error_to_string : Execution.error -> string

  val exception_to_smart_except : exn -> Basics.smart_except list

  val exception_to_string : bool -> exn -> string

  val pp_smart_except : bool -> Basics.smart_except list -> string

  val string_of_contract_id : contract_id -> string
end

type language = Config.language

module type Language = sig
  val language : language
end

module Build_printer (Language : Language) = struct
  include Language

  let rec layout_to_string = function
    | Binary_tree.Leaf Layout.{source; target} ->
        if source = target
        then sprintf "%S" source
        else sprintf "\"%s as %s\"" source target
    | Binary_tree.Node (l1, l2) ->
        sprintf "(%s, %s)" (layout_to_string l1) (layout_to_string l2)

  let layout_to_string layout =
    match Unknown.get layout with
    | Some l when language = SmartPy ->
        sprintf ".layout(%s)" (layout_to_string l)
    | _ -> ""

  let unknown_memo memo =
    Option.value (Option.map string_of_int (Unknown.get memo)) ~default:""

  module Options = struct
    type t =
      { html : bool
      ; stripStrings : bool
      ; types : bool }

    let string = {html = false; stripStrings = false; types = false}

    let html = {string with html = true}

    let htmlStripStrings = {html with stripStrings = true}

    let types = {html with types = true}
  end

  let html_of_record_list (columns, data) f =
    let ppRow row =
      sprintf
        "<tr>%s</tr>"
        (String.concat
           ""
           (List.map (fun s -> sprintf "<td class='data'>%s</td>" (f s)) row))
    in
    sprintf
      "<table class='recordList'><tr>%s</tr>%s</table>"
      (String.concat
         "\n"
         (List.map
            (fun s ->
              sprintf
                "<td class='dataColumn'>%s</td>"
                (String.capitalize_ascii s))
            columns))
      (String.concat "\n" (List.map ppRow data))

  let ppAmount html amount =
    let oneMillion = Bigint.of_int 1000000 in
    let quotient, modulo = Big_int.quomod_big_int amount oneMillion in
    if html
    then
      let mutez = Big_int.string_of_big_int amount in
      let mutez =
        if String.length mutez < 7
        then String.sub "0000000" 0 (7 - String.length mutez) ^ mutez
        else mutez
      in
      sprintf
        "%s.%s<img height=20 width=20 src='static/img/tezos.svg' alt='tz'/>"
        (String.sub mutez 0 (String.length mutez - 6))
        (String.sub mutez (String.length mutez - 6) 6)
    else if Big_int.compare_big_int modulo Big_int.zero_big_int = 0
    then
      match language with
      | SmartPy -> sprintf "sp.tez(%s)" (Big_int.string_of_big_int quotient)
      | SmartML -> sprintf "tez %s" (Big_int.string_of_big_int quotient)
      | SmartTS -> assert false
    else
      match language with
      | SmartPy -> sprintf "sp.mutez(%s)" (Big_int.string_of_big_int amount)
      | SmartML -> sprintf "mutez %s" (Big_int.string_of_big_int amount)
      | SmartTS -> assert false

  let literal_to_sp_string ~html ?protect ?strip_strings =
    let to_string ?protect (l : Literal.t) =
      let open Format in
      let entry_point_opt ppf = function
        | None | Some "default" -> fprintf ppf ""
        | Some ep -> fprintf ppf "%%%s" ep
      in
      let apply name arguments =
        match language with
        | SmartPy -> sprintf "%s(%s)" name (String.concat ", " arguments)
        | SmartML ->
            let s = String.concat " " arguments in
            if s = "" then s else sprintf "%s %s" name s
        | SmartTS -> assert false
      in
      let quote =
        match language with
        | SmartPy -> "'"
        | SmartML -> "\""
        | SmartTS -> "'"
      in
      let prot s = if protect = Some () then sprintf "(%s)" s else s in
      let mono name s =
        match language with
        | SmartPy -> apply ("sp." ^ name) [s]
        | SmartML -> prot (apply name [s])
        | SmartTS -> assert false
      in
      let mono_string name s =
        match language with
        | SmartPy -> mono name (sprintf "'%s'" s)
        | SmartML -> mono name (sprintf "\"%s\"" s)
        | SmartTS -> assert false
      in
      let mono_hex name s = mono_string name ("0x" ^ Misc.Hex.hexcape s) in
      match l with
      | Unit ->
        begin
          match language with
          | SmartPy -> "sp.unit"
          | SmartML -> "()"
          | SmartTS -> assert false
        end
      | Bool x ->
          let b = string_of_bool x in
          begin
            match language with
            | SmartPy -> String.capitalize_ascii b
            | SmartML -> b
            | SmartTS -> assert false
          end
      | Int {i} ->
          let r = Big_int.string_of_big_int i in
          if protect = Some () && Big_int.sign_big_int i < 0
          then "(" ^ r ^ ")"
          else r
      | String s when strip_strings = Some () -> s
      | String s -> sprintf "%s%s%s" quote s quote
      | Bytes s -> mono_hex "bytes" s
      | Chain_id s -> mono_hex "chain_id" s
      | Mutez i ->
          let res = ppAmount html i in
          begin
            match language with
            | SmartPy -> res
            | SmartML -> prot res
            | SmartTS -> assert false
          end
      | Address {address; entry_point} ->
        ( match language with
        | SmartPy ->
            asprintf "sp.address('%s%a')" address entry_point_opt entry_point
        | SmartML ->
            prot
              (asprintf "address \"%s%a\"" address entry_point_opt entry_point)
        | SmartTS -> assert false )
      | Timestamp i -> mono "timestamp" (Big_int.string_of_big_int i)
      | Key s -> mono_string "key" s
      | Secret_key s -> mono_string "secret_key" s
      | Key_hash s -> mono_string "key_hash" s
      | Signature s -> mono_string "signature" s
      | Sapling_test_state _ -> "sapling test state"
      | Sapling_test_transaction _ -> "sapling test transaction"
      | Bls12_381_g1 s -> mono_hex "bls12_381_g1" s
      | Bls12_381_g2 s -> mono_hex "bls12_381_g2" s
      | Bls12_381_fr s -> mono_hex "bls12_381_fr" s
      | Chest_key s -> mono_hex "chest_key" s
      | Chest s -> mono_hex "chest" s
    in
    fun s -> to_string ?protect s

  let literal_to_string_py ?(deep = false) ?(options = Options.string) :
      Literal.t -> string = function
    | Bool x ->
        let res = string_of_bool x in
        begin
          match language with
          | SmartPy -> String.capitalize_ascii res
          | SmartML -> res
          | SmartTS -> assert false
        end
    | Int {i} ->
        let pp = Big_int.string_of_big_int i in
        if options.html && not deep
        then sprintf "<input type='text' value='%s' readonly></input>" pp
        else pp
    | String s -> if options.stripStrings then s else sprintf "'%s'" s
    | Bytes s ->
        if options.html
        then sprintf "<span class='bytes'>0x%s</span>" (Misc.Hex.hexcape s)
        else sprintf "sp.bytes('0x%s')" (Misc.Hex.hexcape s)
    | Bls12_381_g1 s ->
        if options.html
        then sprintf "<span class='bytes'>0x%s</span>" (Misc.Hex.hexcape s)
        else sprintf "sp.bls12_381_g1('0x%s')" (Misc.Hex.hexcape s)
    | Bls12_381_g2 s ->
        if options.html
        then sprintf "<span class='bytes'>0x%s</span>" (Misc.Hex.hexcape s)
        else sprintf "sp.bls12_381_g2('0x%s')" (Misc.Hex.hexcape s)
    | Bls12_381_fr s ->
        if options.html
        then sprintf "<span class='bytes'>0x%s</span>" (Misc.Hex.hexcape s)
        else sprintf "sp.bls12_381_fr('0x%s')" (Misc.Hex.hexcape s)
    | Chest_key s ->
        if options.html
        then sprintf "<span class='bytes'>0x%s</span>" (Misc.Hex.hexcape s)
        else sprintf "sp.chest_key('0x%s')" (Misc.Hex.hexcape s)
    | Chest s ->
        if options.html
        then sprintf "<span class='bytes'>0x%s</span>" (Misc.Hex.hexcape s)
        else sprintf "sp.chest('0x%s')" (Misc.Hex.hexcape s)
    | Chain_id s ->
        if options.html
        then sprintf "<span class='bytes'>0x%s</span>" (Misc.Hex.hexcape s)
        else sprintf "sp.chain_id('0x%s')" (Misc.Hex.hexcape s)
    | Unit -> if options.html then "" else "sp.unit"
    | Key_hash s ->
        if options.html
        then sprintf "<span class='key'>%s</span>" s
        else sprintf "sp.key_hash('%s')" s
    | Key s ->
        if options.html
        then sprintf "<span class='key'>%s</span>" s
        else sprintf "sp.key('%s')" s
    | Secret_key s ->
        if options.html
        then sprintf "<span class='key'>%s</span>" s
        else sprintf "sp.secret_key('%s')" s
    | Signature s ->
        if options.html
        then sprintf "<span class='signature'>%s</span>" s
        else sprintf "sp.signature('%s')" s
    | Address {address} as lit ->
        if options.html
        then sprintf "<span class='address'>%s</span>" address
        else literal_to_sp_string ~html:false lit
    | Timestamp i ->
        if options.html
        then
          sprintf
            "<span class='timestamp'>timestamp(%s)</span>"
            (Big_int.string_of_big_int i)
        else sprintf "sp.timestamp(%s)" (Big_int.string_of_big_int i)
    | Mutez i ->
        let amount = ppAmount options.html i in
        if options.html
        then sprintf "<span class='token'>%s</span>" amount
        else amount
    | Sapling_test_state {test = false} ->
        if options.html
        then html_of_record_list (["SaplingState"], []) (fun s -> s)
        else
          sprintf "[%s]" (String.concat "; " (List.map (String.concat ",") []))
    | Sapling_test_state {elements} ->
        let l =
          List.map
            (fun (key, amount) -> [key; Bigint.string_of_big_int amount])
            (List.sort compare elements)
        in
        if options.html
        then html_of_record_list (["key"; "amount"], l) (fun s -> s)
        else
          sprintf "[%s]" (String.concat "; " (List.map (String.concat ",") l))
    | Sapling_test_transaction {source; target; amount} ->
        sprintf
          "sp.sapling_test_transaction(%S, %S, %s)"
          (Utils.Option.default "" source)
          (Utils.Option.default "" target)
          (Bigint.string_of_big_int amount)

  let literal_to_string_ml ?(deep = false) ?(options = Options.string) :
      Literal.t -> string =
    let quote = sprintf "%S" in
    let app1 f x0 = sprintf "%s %s" f x0 in
    function
    | Bool x ->
        let res = string_of_bool x in
        begin
          match language with
          | SmartPy -> String.capitalize_ascii res
          | SmartML -> res
          | SmartTS -> assert false
        end
    | Int {i} ->
        let pp = Big_int.string_of_big_int i in
        let pp = if Big_int.sign_big_int i < 0 then "(" ^ pp ^ ")" else pp in
        if options.html && not deep
        then sprintf "<input type='text' value='%s' readonly></input>" pp
        else pp
    | String s -> if options.stripStrings then s else sprintf "%S" s
    | Bytes s ->
        if options.html
        then sprintf "<span class='bytes'>0x%s</span>" (Misc.Hex.hexcape s)
        else app1 "bytes" (quote (sprintf "0x%s" (Misc.Hex.hexcape s)))
    | Bls12_381_g1 s ->
        if options.html
        then sprintf "<span class='bytes'>0x%s</span>" (Misc.Hex.hexcape s)
        else app1 "bls12_381_g1" (quote (sprintf "0x%s" (Misc.Hex.hexcape s)))
    | Bls12_381_g2 s ->
        if options.html
        then sprintf "<span class='bytes'>0x%s</span>" (Misc.Hex.hexcape s)
        else app1 "bls12_381_g2" (quote (sprintf "0x%s" (Misc.Hex.hexcape s)))
    | Bls12_381_fr s ->
        if options.html
        then sprintf "<span class='bytes'>0x%s</span>" (Misc.Hex.hexcape s)
        else sprintf "bls12_381_fr('0x%s')" (Misc.Hex.hexcape s)
    | Chain_id s ->
        if options.html
        then sprintf "<span class='bytes'>0x%s</span>" (Misc.Hex.hexcape s)
        else sprintf "chain_id('0x%s')" (Misc.Hex.hexcape s)
    | Chest s ->
        if options.html
        then sprintf "<span class='bytes'>0x%s</span>" (Misc.Hex.hexcape s)
        else sprintf "chest('0x%s')" (Misc.Hex.hexcape s)
    | Chest_key s ->
        if options.html
        then sprintf "<span class='bytes'>0x%s</span>" (Misc.Hex.hexcape s)
        else sprintf "chest_key('0x%s')" (Misc.Hex.hexcape s)
    | Unit -> if options.html then "" else "()"
    | Key_hash s ->
        if options.html
        then sprintf "<span class='key'>%s</span>" s
        else app1 "key_hash" (quote s)
    | Key s ->
        if options.html
        then sprintf "<span class='key'>%s</span>" s
        else app1 "key" (quote s)
    | Secret_key s ->
        if options.html
        then sprintf "<span class='key'>%s</span>" s
        else app1 "secret_key" (quote s)
    | Signature s ->
        if options.html
        then sprintf "<span class='signature'>%s</span>" s
        else app1 "signature" (quote s)
    | Address {address} as lit ->
        if options.html
        then sprintf "<span class='address'>%s</span>" address
        else literal_to_sp_string ~html:false lit
    | Timestamp i ->
        if options.html
        then
          sprintf
            "<span class='timestamp'>timestamp(%s)</span>"
            (Big_int.string_of_big_int i)
        else app1 "timestamp" (Big_int.string_of_big_int i)
    | Mutez i ->
        let amount = ppAmount options.html i in
        if options.html
        then sprintf "<span class='token'>%s</span>" amount
        else amount
    | Sapling_test_state {test = false} ->
        if options.html
        then html_of_record_list (["SaplingState"], []) (fun s -> s)
        else
          sprintf "[%s]" (String.concat "; " (List.map (String.concat ",") []))
    | Sapling_test_state {elements} ->
        let l =
          List.map
            (fun (key, amount) -> [key; Bigint.string_of_big_int amount])
            (List.sort compare elements)
        in
        if options.html
        then html_of_record_list (["key"; "amount"], l) (fun s -> s)
        else
          sprintf "[%s]" (String.concat "; " (List.map (String.concat ",") l))
    | Sapling_test_transaction {source; target; amount} ->
        sprintf
          "sapling_test_transaction(%S, %S, %s)"
          (Utils.Option.default "" source)
          (Utils.Option.default "" target)
          (Bigint.string_of_big_int amount)

  let literal_to_string ?deep ?options =
    match language with
    | SmartPy -> literal_to_string_py ?deep ?options
    | SmartML -> literal_to_string_ml ?deep ?options
    | SmartTS -> assert false

  let rec type_to_string_py
      ~multiline ?toplevel ?(options = Options.string) ?(indent = "") t =
    let open Type in
    let pp ?indent = type_to_string_py ~multiline ?toplevel ~options ?indent in
    let shiftIdent = if options.html then "&nbsp;&nbsp;" else "  " in
    let cst name = "sp.T" ^ String.capitalize_ascii name in
    let mono name arg = sprintf "%s(%s)" (cst name) arg in
    let bin name t1 t2 = sprintf "%s(%s, %s)" (cst name) t1 t2 in
    let print_row ?(options = Options.string) ?(sep = ", ") ~multiline row =
      String.concat
        sep
        (List.map
           (fun (s, t) ->
             sprintf "%s = %s" s (type_to_string_py ~multiline ~options t))
           row)
    in
    match getRepr t with
    | T0 T_bool -> cst "bool"
    | T0 T_string -> cst "string"
    | T0 T_timestamp -> cst "timestamp"
    | T0 T_bytes -> cst "bytes"
    | TInt {isNat} ->
      ( match Typing.intType isNat with
      | `Unknown -> cst "intOrNat"
      | `Nat -> cst "nat"
      | `Int -> cst "int" )
    | TBounded {t; cases} ->
        let final, cases =
          match Unknown.get cases with
          | None -> (false, [])
          | Some {final; cases} -> (final, cases)
        in
        let final = if final then "" else ", final=False" in
        sprintf
          "sp.TBounded([%s], t=%s%s)"
          (String.concat ", " (List.map literal_to_string cases))
          (pp t)
          final
    | TRecord {row; layout} ->
        if options.html
        then
          sprintf
            "%s%s<span class='record'>{</span>%s<br>%s<span \
             class='record'>}%s</span>"
            (if toplevel = Some () then "" else "<br>")
            indent
            (String.concat
               ""
               (List.map
                  (fun (s, t) ->
                    sprintf
                      "<br>%s<span class='record'>%s</span>: %s;"
                      (indent ^ shiftIdent)
                      s
                      (pp ~indent:(indent ^ shiftIdent ^ shiftIdent) t))
                  row))
            indent
            (layout_to_string layout)
        else
          sprintf
            "sp.TRecord(%s%s)%s"
            (if multiline then "\n" ^ indent ^ shiftIdent else "")
            (print_row
               ~multiline
               ~options
               ~sep:(if multiline then ",\n" ^ indent ^ shiftIdent else ", ")
               row)
            (layout_to_string layout)
    | TVariant {row; layout} ->
        if options.html
        then
          let ppVariant (s, t) =
            let t =
              match getRepr t with
              | T0 T_unit -> ""
              | _ -> sprintf " %s" (pp ~indent:(indent ^ shiftIdent) t)
            in
            sprintf "%s<span class='variant'>| %s</span>%s" indent s t
          in
          (if toplevel = Some () then "" else "<br>")
          ^ String.concat "<br>" (List.map ppVariant row)
        else
          sprintf
            "sp.TVariant(%s)%s"
            (print_row ~multiline ~options row)
            (layout_to_string layout)
    | T1 (T_set, telement) ->
        sprintf "sp.TSet(%s)" (pp ~indent:(indent ^ shiftIdent) telement)
    | T2 (((T_map | T_big_map) as tm), tkey, tvalue) ->
        let name =
          match tm with
          | T_big_map -> "bigMap"
          | T_map -> "map"
          | _ -> assert false
        in
        bin
          name
          (pp ~indent:(indent ^ shiftIdent) tkey)
          (pp ~indent:(indent ^ shiftIdent) tvalue)
    | T0 T_mutez -> cst "mutez"
    | T0 T_unit -> cst "unit"
    | T0 T_address -> cst "address"
    | T0 T_key_hash -> cst "keyHash"
    | T0 T_key -> cst "key"
    | TSecretKey -> cst "secretkey"
    | T0 T_chain_id -> cst "chainId"
    | T0 T_signature -> cst "signature"
    | T1 (T_contract, t) -> mono "contract" (pp ~indent:(indent ^ shiftIdent) t)
    | TUnknown {contents = UExact t} -> pp t
    | TVar _ | TUnknown {contents = UUnknown _} ->
        if options.html
        then "<span class='partialType'>sp.TUnknown()</span>"
        else sprintf "sp.TUnknown()"
    | TUnknown {contents = URecord l} ->
        sprintf "TRecord++(%s)" (print_row ~options ~multiline l)
    | TUnknown {contents = UTuple l} ->
        sprintf
          "TTuple++(%s)"
          (String.concat
             ", "
             (List.map (fun (i, t) -> sprintf "#%d = %s" i (pp t)) l))
    | TUnknown {contents = UVariant l} ->
        sprintf
          "TVariant++(%s)"
          (String.concat
             " | "
             (List.map (fun (s, t) -> sprintf "%s %s" s (pp t)) l))
    | TTuple [t1; t2] ->
        let t1 = pp t1 in
        let t2 = pp t2 in
        sprintf "sp.TPair(%s, %s)" t1 t2
    | TTuple ts ->
        let ts = List.map pp ts in
        sprintf "sp.TTuple(%s)" (String.concat ", " ts)
    | T1 (T_list, t) -> mono "list" (pp ~indent:(indent ^ shiftIdent) t)
    | T1 (T_ticket, t) -> mono "ticket" (pp ~indent:(indent ^ shiftIdent) t)
    | TLambda ({with_storage; with_operations}, t1, t2) ->
        let f name = function
          | Some true -> ", " ^ name ^ "=True"
          | Some false -> ""
          | None -> ", " ^ name ^ "=?"
        in
        let with_storage =
          match Unknown.get with_storage with
          | None | Some None -> ""
          | Some (Some Read_only) -> ", with_storage=\"read-only\""
          | Some (Some Read_write) -> ", with_storage=\"read-write\""
        in
        let to_string = pp ~indent:(indent ^ shiftIdent) in
        sprintf
          "sp.TLambda(%s, %s%s%s)"
          (to_string t1)
          (to_string t2)
          with_storage
          (f "with_operations" (Unknown.get with_operations))
    | T2 (T_lambda, _, _) -> assert false
    | T0 T_operation -> cst "operation"
    | TSaplingState {memo} -> mono "saplingState" (unknown_memo memo)
    | TSaplingTransaction {memo} ->
        mono "saplingTransaction" (unknown_memo memo)
    | T0 T_never -> cst "never"
    | T0 T_bls12_381_g1 -> cst "bls12_381_g1"
    | T0 T_bls12_381_g2 -> cst "bls12_381_g2"
    | T0 T_bls12_381_fr -> cst "bls12_381_fr"
    | T0 T_chest_key -> cst "chest_key"
    | T0 T_chest -> cst "chest"
    | T0 (T_nat | T_int | T_sapling_state _ | T_sapling_transaction _) ->
        assert false
    | T1 (T_option, t) ->
        mono "option" (pp ~indent:(indent ^ shiftIdent ^ shiftIdent) t)
    | T2 ((T_pair _ | T_or _), _, _) -> assert false

  let rec type_to_string_ml
      ~multiline ?toplevel ?protect ?(options = Options.string) ?(indent = "") t
      =
    let open Type in
    let pp ?indent =
      type_to_string_ml ~multiline ?toplevel ~options ?indent ~protect:()
    in
    let prot s = if protect = Some () then sprintf "(%s)" s else s in
    let app2 f x0 x1 = prot (sprintf "%s %s %s" f x0 x1) in
    let shiftIdent = if options.html then "&nbsp;&nbsp;" else "  " in
    let mono name arg = prot (sprintf "%s %s" name arg) in
    let bin name t1 t2 = prot (sprintf "%s %s %s" name t1 t2) in
    let print_row ?(options = Options.string) ?(sep = ", ") ~multiline row =
      String.concat
        sep
        (List.map
           (fun (s, t) ->
             sprintf "%s = %s" s (type_to_string_ml ~multiline ~options t))
           row)
    in
    match getRepr t with
    | T0 T_bool -> "bool"
    | T0 T_string -> "string"
    | T0 T_timestamp -> "timestamp"
    | T0 T_bytes -> "bytes"
    | TInt {isNat} ->
      ( match Typing.intType isNat with
      | `Unknown -> "intOrNat"
      | `Nat -> "nat"
      | `Int -> "int" )
    | TBounded {t; cases} ->
        let final, cases =
          match Unknown.get cases with
          | None -> (false, [])
          | Some {final; cases} -> (final, cases)
        in
        let final = if final then "" else ", final=False" in
        sprintf
          "bounded([%s], t=%s%s)"
          (String.concat ", " (List.map literal_to_string cases))
          (pp t)
          final
    | TRecord {row; layout} ->
        if options.html
        then
          sprintf
            "%s%s<span class='record'>{</span>%s<br>%s<span \
             class='record'>}%s</span>"
            (if toplevel = Some () then "" else "<br>")
            indent
            (String.concat
               ""
               (List.map
                  (fun (s, t) ->
                    sprintf
                      "<br>%s<span class='record'>%s</span> = %s;"
                      (indent ^ shiftIdent)
                      s
                      (pp ~indent:(indent ^ shiftIdent ^ shiftIdent) t))
                  row))
            indent
            (layout_to_string layout)
        else
          sprintf
            "{%s%s}%s"
            (if multiline then "\n" ^ indent ^ shiftIdent else "")
            (print_row
               ~multiline
               ~options
               ~sep:(if multiline then ";\n" ^ indent ^ shiftIdent else "; ")
               row)
            (layout_to_string layout)
    | TVariant {row; layout = _} ->
        if options.html
        then
          let ppVariant (s, t) =
            let t =
              match getRepr t with
              | T0 T_unit -> ""
              | _ -> sprintf " %s" (pp ~indent:(indent ^ shiftIdent) t)
            in
            sprintf "%s<span class='variant'>| %s</span>%s" indent s t
          in
          (if toplevel = Some () then "" else "<br>")
          ^ String.concat "<br>" (List.map ppVariant row)
        else
          prot
            (String.concat
               " + "
               (List.map (fun (s, t) -> sprintf "`%s %s" s (pp t)) row))
    | T1 (T_set, telement) ->
        mono "set" (pp ~indent:(indent ^ shiftIdent) telement)
    | T2 (((T_map | T_big_map) as tm), tkey, tvalue) ->
        let name =
          match tm with
          | T_big_map -> "big_map"
          | T_map -> "map"
          | _ -> assert false
        in
        bin
          name
          (pp ~indent:(indent ^ shiftIdent) tkey)
          (pp ~indent:(indent ^ shiftIdent) tvalue)
    | T0 T_mutez -> "mutez"
    | T0 T_unit -> "unit"
    | T0 T_address -> "address"
    | T0 T_key_hash -> "key_hash"
    | T0 T_key -> "key"
    | TSecretKey -> "secretkey"
    | T0 T_chain_id -> "chain_id"
    | T0 T_signature -> "signature"
    | T1 (T_contract, t) -> mono "contract" (pp ~indent:(indent ^ shiftIdent) t)
    | TUnknown {contents = UExact t} -> pp t
    | TVar id | TUnknown {contents = UUnknown id} ->
        if options.html
        then "<span class='partialType'>unknown</span>"
        else prot (sprintf "unknown %S" id)
    | TUnknown {contents = URecord l} ->
        sprintf "TRecord++(%s)" (print_row ~options ~multiline l)
    | TUnknown {contents = UTuple l} ->
        sprintf
          "TTuple++(%s)"
          (String.concat
             ", "
             (List.map (fun (i, t) -> sprintf "#%d = %s" i (pp t)) l))
    | TUnknown {contents = UVariant l} ->
        sprintf
          "TVariant++(%s)"
          (String.concat
             " | "
             (List.map (fun (s, t) -> sprintf "%s %s" s (pp t)) l))
    | TTuple [t1; t2] -> app2 "pair" (pp t1) (pp t2)
    | TTuple ts ->
        let ts = List.map pp ts in
        sprintf "[%s]" (String.concat "; " ts)
    | T1 (T_list, t) -> mono "list" (pp ~indent:(indent ^ shiftIdent) t)
    | T1 (T_ticket, t) -> mono "ticket" (pp ~indent:(indent ^ shiftIdent) t)
    | TLambda ({with_storage; with_operations}, t1, t2) ->
        let f name = function
          | Some true -> " ~" ^ name ^ ":true"
          | Some false -> ""
          | None -> failwith "cannot print unknown effect"
        in
        let with_storage =
          match Unknown.get with_storage with
          | None | Some None -> ""
          | Some (Some Read_only) -> " ~with_storage:\"read-only\""
          | Some (Some Read_write) -> " ~with_storage:\"read-write\""
        in
        let to_string = pp ~indent:(indent ^ shiftIdent) in
        sprintf
          "lambda %s %s%s%s"
          (to_string t1)
          (to_string t2)
          with_storage
          (f "with_operations" (Unknown.get with_operations))
    | T2 (T_lambda, _, _) -> assert false
    | T0 T_operation -> "operation"
    | TSaplingState {memo} -> mono "sapling_state" (unknown_memo memo)
    | TSaplingTransaction {memo} ->
        mono "sapling_transaction" (unknown_memo memo)
    | T0 T_never -> "never"
    | T0 T_bls12_381_g1 -> "bls12_381_g1"
    | T0 T_bls12_381_g2 -> "bls12_381_g2"
    | T0 T_bls12_381_fr -> "bls12_381_fr"
    | T0 T_chest -> "chest"
    | T0 T_chest_key -> "chest_key"
    | T0 (T_nat | T_int | T_sapling_state _ | T_sapling_transaction _) ->
        assert false
    | T1 (T_option, t) ->
        mono "option" (pp ~indent:(indent ^ shiftIdent ^ shiftIdent) t)
    | T2 ((T_pair _ | T_or _), _, _) -> assert false

  let type_to_string ~multiline ?toplevel ?protect ?options ?indent t =
    match language with
    | SmartPy -> type_to_string_py ~multiline ?toplevel ?options ?indent t
    | SmartML ->
        type_to_string_ml ~multiline ?toplevel ?protect ?options ?indent t
    | SmartTS -> assert false

  let type_to_string ?multiline ?toplevel ?protect ?(options = Options.string) t
      =
    let multiline = multiline = Some () in
    if options.html
    then
      sprintf
        "<span class='type'>%s</span>"
        (type_to_string ~multiline ?toplevel ?protect ~options t)
    else type_to_string ~multiline ?toplevel ?protect ~options t

  let string_of_contract_id = function
    | C_static {static_id} -> sprintf "%i" static_id
    | C_dynamic {dynamic_id} -> sprintf "Dyn_%i" dynamic_id

  let unrec = function
    | {v = Record (_, l)} -> List.map snd l
    | x -> [x]

  let is_range_keys l =
    let rec aux n = function
      | [] -> true
      | ({v = Literal (Int {i})}, _) :: l ->
          Big_int.eq_big_int (Bigint.of_int n) i && aux (n + 1) l
      | (_, _) :: _ -> false
    in
    aux 0 l

  let is_vector v =
    match v.v with
    | Map (_, _, _, l) ->
        if is_range_keys l then Some (List.map snd l) else None
    | _ -> None

  let is_matrix l =
    match is_vector l with
    | None -> None
    | Some vect ->
        let new_line lines a =
          match (lines, is_vector a) with
          | None, _ | _, None -> None
          | Some lines, Some v -> Some (v :: lines)
        in
        ( match List.fold_left new_line (Some []) vect with
        | Some lines
          when 2 <= List.length lines
               && List.exists (fun line -> 2 <= List.length line) lines ->
            Some ([], List.rev lines)
        | _ -> None )

  let is_record_list = function
    | {v = Record _} :: _ -> true
    | _ -> false

  let vclass_to_string = function
    | Simple -> "variable"
    | Local -> "local"
    | Scenario -> "scenario"

  let with_vclass ~html vclass x =
    if html
    then sprintf "<span class='%s'>%s</span>" (vclass_to_string vclass) x
    else x

  let tvariable_to_string ?(options = Options.string) ?protect (s, t) vclass =
    let prot s = if protect = Some () then sprintf "(%s)" s else s in
    match options with
    | {html = false; types = false} -> s
    | {html; types = true} ->
        prot
          (sprintf
             "%s : <span class='type'>%s</span>"
             (with_vclass ~html vclass s)
             (type_to_string t))
    | {types = false} ->
        sprintf "<span class='%s'>%s</span>" (vclass_to_string vclass) s

  let variable_to_string ?(options = Options.string) ?protect s vclass =
    let prot s = if protect = Some () then sprintf "(%s)" s else s in
    match options with
    | {html = false; types = false} -> s
    | {html; types = true} -> prot (with_vclass ~html vclass s)
    | {types = false} ->
        sprintf "<span class='%s'>%s</span>" (vclass_to_string vclass) s

  let string_of_binOp op =
    match (op, language) with
    | BNeq, (SmartPy | SmartTS) -> "!="
    | BNeq, SmartML -> "<>"
    | BEq, (SmartPy | SmartTS) -> "=="
    | BEq, SmartML -> "="
    | BAnd, SmartPy -> "&"
    | BAnd, _ -> "&&"
    | BOr, SmartPy -> "|"
    | BOr, _ -> "||"
    | BAdd, _ -> "+"
    | BSub, _ -> "-"
    | BDiv, SmartPy -> "//"
    | BDiv, SmartML -> "/"
    | BDiv, _ -> assert false
    | BEDiv, _ -> "ediv"
    | BMul {overloaded = false}, _ -> "*"
    | BMul {overloaded = true}, _ -> "mul"
    | BMod, _ -> "%"
    | BLt, _ -> "<"
    | BLe, _ -> "<="
    | BGt, _ -> ">"
    | BGe, _ -> ">="
    | BXor, _ -> "^"
    | BMax, _ -> "max"
    | BMin, _ -> "min"

  let collect_lambdas =
    let p_expr _ = function
      | ELambda {body = {c = CResult _}, _} -> []
      | ELambda {name; body} -> [("f" ^ name, name, fst body)]
      | e ->
          let e = map_expr_f snd (fun _ -> []) (fun _ -> []) id e in
          fold_expr_f ( @ ) ( @ ) ( @ ) (fun _ -> elim_untyped) [] e
    in
    let p_command _ _ = [] in
    para_expr (para_alg ~p_expr ~p_command ~p_type:id)

  let collect_lambdas {c} =
    let c = map_command_f collect_lambdas (fun _ -> []) (fun _ -> []) c in
    fold_command_f ( @ ) ( @ ) ( @ ) [] c

  let pps, ppS =
    let comma buf = bprintf buf ", " in
    ( List.buffer_sep comma (fun buf -> bprintf buf "%s")
    , List.buffer_sep comma (fun buf -> bprintf buf "%S") )

  let prefix ~language name =
    match (language : Config.language) with
    | SmartPy -> "sp." ^ name
    | SmartML -> name
    | SmartTS -> assert false

  let mprim0_to_string p =
    let Michelson.{name} = Michelson.spec_of_instr ~strict_dup:true (MI0 p) in
    String.lowercase_ascii name

  let mprim1_to_string ~(language : language) :
      _ Michelson_base.Primitive.prim1 -> _ = function
    | IsNat -> "sp.is_nat"
    | Not ->
      ( match language with
      | SmartPy -> "~"
      | SmartML -> "not"
      | SmartTS -> assert false )
    | p ->
        let Michelson.{name} =
          Michelson.spec_of_instr ~strict_dup:true (MI1 p)
        in
        prefix ~language (String.lowercase_ascii name)

  let mprim2_to_string p =
    let Michelson.{name} = Michelson.spec_of_instr ~strict_dup:true (MI2 p) in
    String.lowercase_ascii name

  let mprim3_to_string p =
    let Michelson.{name} = Michelson.spec_of_instr ~strict_dup:true (MI3 p) in
    String.lowercase_ascii name

  let rec expr_to_string ?options ?protect e =
    match language with
    | SmartPy -> expr_to_string_py ?options ?protect e
    | SmartML -> expr_to_string_ml ?options ?protect e
    | SmartTS -> expr_to_string_ts ?options ?protect e

  and expr_to_string_py ?(options = Options.string) ?protect e =
    let prot s = if protect = Some () then sprintf "(%s)" s else s in
    let htmlClass name s =
      if options.html then sprintf "<span class='%s'>%s</span>" name s else s
    in
    let putSelf s = sprintf "%s.%s" (htmlClass "self" "self") s in
    let to_string = expr_to_string_py ~options in
    let apply name arguments =
      sprintf "%s(%s)" name (String.concat ", " arguments)
    in
    let std name arguments = apply ("sp." ^ name) arguments in
    let std_no_prefix name arguments = apply name arguments in
    let pp_exprs arguments = List.map (fun s -> to_string s) arguments in
    let mono name s = std name (pp_exprs [s]) in
    let bin name x y = std name (pp_exprs [x; y]) in
    let tern name x y z = std name (pp_exprs [x; y; z]) in
    let mprim0_to_string p =
      match (p : _ Michelson_base.Primitive.prim0) with
      | Self None -> prefix ~language "self"
      | Self (Some name) -> sprintf "sp.self_entry_point('%s')" name
      | Sapling_empty_state {memo} -> sprintf "sp.sapling_empty_state(%i)" memo
      | p -> prefix ~language (mprim0_to_string p)
    in
    let mprim1_to_string p x =
      match (p : _ Michelson_base.Primitive.prim1) with
      | Abs -> prot (apply "abs" (pp_exprs [x]))
      | Not -> prot (sprintf "~ %s" (to_string ~protect:() x))
      | p -> std_no_prefix (mprim1_to_string ~language p) (pp_exprs [x])
    in
    let mprim2_to_string p x1 x2 =
      match (p : _ Michelson_base.Primitive.prim2) with
      | Lsl -> prot (infix ~options "<<" x1 x2)
      | Lsr -> prot (infix ~options ">>" x1 x2)
      | p -> bin (mprim2_to_string p) x1 x2
    in
    let mprim3_to_string p x1 x2 x3 =
      match (p : Michelson_base.Primitive.prim3) with
      | Check_signature ->
          sprintf
            "sp.check_signature(%s, %s, %s)"
            (to_string x1)
            (to_string x2)
            (to_string x3)
      | p ->
          let Michelson.{name} =
            Michelson.spec_of_instr ~strict_dup:true (MI3 p)
          in
          tern (String.lowercase_ascii name) x1 x2 x3
    in
    match e.e with
    | EVar (x, Simple) -> with_vclass Simple ~html:options.html x
    | EVar ("__parameter__", _) -> sprintf "params"
    | EVar ("__operations__", _) -> prefix ~language "operations()"
    | EVar ("__storage__", _) -> putSelf "data"
    | EVar (name, Local) -> sprintf "%s.value" name
    | EVar (id, Scenario) -> sprintf "sp.scenario_var(%s)" id
    | EPrivate name -> sprintf "self.%s" name
    | EMPrim0 p -> mprim0_to_string p
    | EMPrim1 (p, x) -> mprim1_to_string p x
    | EMPrim1_fail _ -> assert false
    | EMPrim2 (p, x1, x2) -> mprim2_to_string p x1 x2
    | EMPrim3 (p, x1, x2, x3) -> mprim3_to_string p x1 x2 x3
    | EPrim0 prim ->
      begin
        match prim with
        | ECst v | EBounded v ->
            htmlClass
              "constant"
              (literal_to_sp_string
                 ~html:false
                 ?protect
                 ?strip_strings:(if options.stripStrings then Some () else None)
                 v)
        | ECstContract {entry_point; address} ->
            htmlClass
              "constant"
              (literal_to_sp_string
                 ~html:false
                 ?strip_strings:(if options.stripStrings then Some () else None)
                 (Literal.address ?entry_point address))
        | EMatchCons x -> with_vclass Simple ~html:options.html x
        | EMetaLocal name -> sprintf "%s" name
        | EContract_address (id, entry_point) ->
            sprintf
              "sp.address('%s')"
              (Aux.address_of_contract_id ~html:false id entry_point)
        | EContract_balance id ->
            sprintf "sp.contract_balance(%s)" (string_of_contract_id id)
        | EContract_baker id ->
            sprintf "sp.contract_baker(%s)" (string_of_contract_id id)
        | EContract_data id ->
            sprintf "sp.contract_data(%s)" (string_of_contract_id id)
        | EContract_typed (id, entry_point) ->
            sprintf
              "sp.contract_typed(%s)"
              (Aux.address_of_contract_id ~html:false id entry_point)
        | EAccount_of_seed {seed} -> sprintf "sp.test_account(%S)" seed
        | EConstant (e, t) ->
            prefix
              ~language
              (sprintf "constant(%S, t = %s)" e (type_to_string t))
        | EConstantVar e -> prefix ~language (sprintf "constant_var(%S)" e)
      end
    | EPrim1 (prim, x) ->
        let mod_fun name x = sprintf "%s.%s()" (to_string ~protect:() x) name in
        ( match prim with
        | EResolve -> mono "resolve" x
        | EListRev -> mod_fun "rev" x
        | EListItems false -> mod_fun "items" x
        | EListKeys false -> mod_fun "keys" x
        | EListValues false -> mod_fun "values" x
        | EListElements false -> mod_fun "elements" x
        | EListItems true -> mod_fun "rev_items" x
        | EListKeys true -> mod_fun "rev_keys" x
        | EListValues true -> mod_fun "rev_values" x
        | EListElements true -> mod_fun "rev_elements" x
        | EPack -> mono "pack" x
        | EToInt -> mono "to_int" x
        | ENeg -> prot (sprintf "- %s" (to_string ~protect:() x))
        | ESign -> mono "sign" x
        | ESum -> mono "sum" x
        | EUnpack t -> std "unpack" (pp_exprs [x] @ [type_to_string t])
        | EAddress ->
          begin
            match x.e with
            | EMPrim0 (Self None) -> prefix ~language "self_address"
            | EMPrim0 (Self (Some name)) ->
                sprintf "sp.self_entry_point_address('%s')" name
            | _ -> mono "to_address" x
          end
        | EImplicit_account -> mono "implicit_account" x
        | EProject 0 -> mono "fst" x
        | EProject 1 -> mono "snd" x
        | EProject i -> sprintf "%s[%d]" (to_string x) i
        | EConcat_list -> mono "concat" x
        | ESize -> mono "len" x
        | ESetDelegate -> mono "set_delegate_operation" x
        | EType_annotation t ->
            std "set_type_expr" (pp_exprs [x] @ [type_to_string ~protect:() t])
        | EAttr name -> sprintf "%s.%s" (to_string ~protect:() x) name
        | EIsVariant "Some" -> sprintf "%s.is_some()" (to_string ~protect:() x)
        | EIsVariant name -> sprintf "%s.is_variant('%s')" (to_string x) name
        | EVariant name ->
          begin
            match (name, x) with
            | "None", {e = EPrim0 (ECst Unit)} -> prefix ~language "none"
            | "Some", x -> mono "some" x
            | name, x -> sprintf "variant('%s', %s)" name (to_string x)
          end
        | EReadTicket -> mono "read_ticket_raw" x
        | EJoinTickets -> mono "join_tickets_raw" x
        | EPairingCheck -> mono "pairing_check" x
        | EVotingPower -> mono "voting_power" x
        | EUnbounded -> mono "unbound" x
        | EConvert -> mono "convert" x
        | EStaticView (static_id, name) ->
            sprintf
              "sp.contract_view(%s, %S, %s)"
              (string_of_contract_id (C_static static_id))
              name
              (to_string x) )
    | EPrim2 (prim2, x, y) ->
      begin
        match prim2 with
        | ECallLambda ->
            let parameter, lambda = (x, y) in
            sprintf
              "%s(%s)" (* "%s.__call__(%s)" *)
              (to_string ~protect:() lambda)
              (to_string parameter)
        | EApplyLambda ->
            let parameter, lambda = (x, y) in
            sprintf
              "%s.apply(%s)"
              (to_string ~protect:() lambda)
              (to_string parameter)
        | EBinOp ((BEDiv | BMax | BMin | BMul {overloaded = true}) as op) ->
            bin (string_of_binOp op) x y
        | EBinOp op -> prot (infix ~options (string_of_binOp op) x y)
        | EGetOpt ->
            let k, m = (x, y) in
            sprintf "%s.get_opt(%s)" (to_string ~protect:() m) (to_string k)
        | ECons ->
            let e1, e2 = (x, y) in
            sprintf "sp.cons(%s, %s)" (to_string e1) (to_string e2)
        | EAdd_seconds ->
            let e1, e2 = (x, y) in
            bin "add_seconds" e1 e2
        | EContains ->
            let member, items = (x, y) in
            prot
              (sprintf
                 "%s.contains(%s)"
                 (to_string ~protect:() items)
                 (to_string member))
        | ETicket ->
            let content, amount = (x, y) in
            bin "ticket" content amount
        | ESplitTicket ->
            let ticket, decomposition = (x, y) in
            bin "split_ticket_raw" ticket decomposition
        | EView (name, return_type) ->
            let address, param = (x, y) in
            sprintf
              "sp.view(%S, %s, %s, %s)"
              name
              (to_string address)
              (to_string param)
              (type_to_string return_type)
      end
    | EPrim3 (prim3, x, y, z) ->
      begin
        match prim3 with
        | ERange ->
          ( match (x, y, z) with
          | a, b, {e = EPrim0 (ECst (Int {i = step}))}
            when Big_int.eq_big_int step (Bigint.of_int 1) ->
              sprintf "sp.range(%s, %s)" (to_string a) (to_string b)
          | e1, e2, e3 ->
              sprintf
                "sp.range(%s, %s, %s)"
                (to_string e1)
                (to_string e2)
                (to_string e3) )
        | EUpdate_map ->
            sprintf
              "sp.update_map(%s, %s, %s)"
              (to_string z)
              (to_string x)
              (to_string y)
        | EGet_and_update ->
            sprintf
              "sp.get_and_update(%s, %s, %s)"
              (to_string z)
              (to_string x)
              (to_string y)
        | ESplit_tokens ->
          begin
            match (x, y, z) with
            | ( {e = EPrim0 (ECst (Mutez tok))}
              , quantity
              , {e = EPrim0 (ECst (Int {i}))} )
              when Big_int.eq_big_int tok (Bigint.of_int 1000000)
                   && Big_int.eq_big_int i (Bigint.of_int 1) ->
                sprintf "sp.tez(%s)" (to_string quantity)
            | ( {e = EPrim0 (ECst (Mutez tok))}
              , quantity
              , {e = EPrim0 (ECst (Int {i}))} )
              when Big_int.eq_big_int tok (Bigint.of_int 1)
                   && Big_int.eq_big_int i (Bigint.of_int 1) ->
                sprintf "sp.mutez(%s)" (to_string quantity)
            | e1, e2, e3 ->
                sprintf
                  "sp.split_tokens(%s, %s, %s)"
                  (to_string e1)
                  (to_string e2)
                  (to_string e3)
          end
        | ETest_ticket ->
            sprintf
              "sp.test_ticket(%s, %s, %s)"
              (to_string x)
              (to_string y)
              (to_string z)
      end
    | EOpenVariant (name, x, missing_message) ->
        let missing_message =
          match missing_message with
          | None -> None
          | Some x -> Some (sprintf "message = %s" (to_string x))
        in
        ( match (name, x, missing_message) with
        | "Some", {e = EMPrim1 (IsNat, x)}, None -> mono "as_nat" x
        | "Some", {e = EMPrim1 (IsNat, x)}, Some message ->
            sprintf "sp.as_nat(%s, %s)" (to_string x) message
        | "Some", x, None -> sprintf "%s.open_some()" (to_string x)
        | "Some", x, Some message ->
            sprintf "%s.open_some(%s)" (to_string x) message
        | name, x, None -> sprintf "%s.open_variant('%s')" (to_string x) name
        | name, x, Some message ->
            sprintf "%s.open_variant('%s', %s)" (to_string x) name message )
    | ELambda
        { name
        ; body =
            {c = CResult {e = EMichelson (michelson, [{e = EVar (name', _)}])}}
        }
      when name = name' ->
        sprintf "sp.lambda_michelson(%S)" michelson.name
    | ELambda {name; body = {c = CResult r}} ->
        sprintf "sp.build_lambda(lambda %s: %s)" name (to_string r)
    | ELambda {name} -> sprintf "sp.build_lambda(f%s)" name
    | EMapFunction {f; l} ->
        sprintf "%s.map(%s)" (to_string ~protect:() l) (to_string f)
    | ECreate_contract _ -> sprintf "create contract ..."
    | EItem {items; key; default_value = None; missing_message = None} ->
        sprintf "%s[%s]" (to_string ~protect:() items) (to_string key)
    | EItem {items; key; default_value = Some d} ->
        sprintf
          "%s.get(%s, default_value = %s)"
          (to_string ~protect:() items)
          (to_string key)
          (to_string d)
    | EItem {items; key; default_value = None; missing_message = Some message}
      ->
        sprintf
          "%s.get(%s, message = %s)"
          (to_string ~protect:() items)
          (to_string key)
          (to_string message)
    | ERecord entries ->
        sprintf
          "sp.record(%s)"
          (String.concat
             ", "
             (List.map
                (fun (n, e) -> sprintf "%s = %s" n (to_string e))
                entries))
    | EList l ->
        sprintf "sp.list([%s])" (String.concat ", " (List.map to_string l))
    | EMap (_, l) ->
        sprintf
          "{%s}"
          (String.concat
             ", "
             (List.map
                (fun (k, e) -> sprintf "%s : %s" (to_string k) (to_string e))
                l))
    | ESet l ->
        sprintf "sp.set([%s])" (String.concat ", " (List.map to_string l))
    | EContract {arg_type; entry_point; address} ->
        sprintf
          "sp.contract(%s, %s%s)"
          (type_to_string arg_type)
          (to_string address)
          (Base.Option.value_map entry_point ~default:"" ~f:(fun ep ->
               sprintf ", entry_point='%s'" ep))
    | ETuple es ->
        let es = List.map to_string es in
        sprintf "(%s)" (String.concat ", " es)
    | ESlice {offset; length; buffer} ->
        prot
          (sprintf
             "sp.slice(%s, %s, %s)"
             (to_string buffer)
             (to_string offset)
             (to_string length))
    | EMake_signature {secret_key; message; message_format} ->
        sprintf
          "sp.make_signature(secret_key = %s, message = %s, message_format = \
           %s)"
          (to_string secret_key)
          (to_string message)
          ( match message_format with
          | `Hex -> "'Hex'"
          | `Raw -> "'Raw'" )
    | EMichelson (michelson, exprs) ->
        sprintf
          "sp.michelson(%S)(%s)"
          michelson.name
          (String.concat ", " (List.map to_string exprs))
    | ETransfer {arg; amount; destination} ->
        sprintf
          "sp.transfer_operation(%s, %s, %s)"
          (to_string arg)
          (to_string amount)
          (to_string destination)
    | EMatch (scrutinee, clauses) ->
        sprintf
          "sp.ematch(%s, %s)"
          (to_string scrutinee)
          (String.concat
             ", "
             (List.map
                (fun (cons, rhs) -> sprintf "(\"%s\", %s)" cons (to_string rhs))
                clauses))
    | ESaplingVerifyUpdate {transaction; state} ->
        sprintf
          "sp.sapling_verify_update(%s, %s)"
          (to_string state)
          (to_string transaction)
    | EHasEntryPoint ep -> sprintf "sp.has_entry_point(%S)" ep
    | EIf (cond, a, b) ->
        sprintf
          "sp.eif(%s, %s, %s)"
          (to_string cond)
          (to_string a)
          (to_string b)
    | EIsFailing x -> mono "is_failing" x
    | ECatchException (t, x) ->
        std "catch_exception" (pp_exprs [x] @ [type_to_string ~protect:() t])

  and expr_to_string_ml ?(options = Options.string) ?protect e =
    let pp = expr_to_string_ml ~options ~protect:() in
    let prot s = if protect = Some () then sprintf "(%s)" s else s in
    let app1 f x0 = prot (sprintf "%s %s" f x0) in
    let app2 f x0 x1 = prot (sprintf "%s %s %s" f x0 x1) in
    let app3 f x0 x1 x2 = prot (sprintf "%s %s %s %s" f x0 x1 x2) in
    let to_string = expr_to_string_ml ~options in
    let type_to_string = type_to_string ~protect:() in
    let htmlClass name s =
      if options.html then sprintf "<span class='%s'>%s</span>" name s else s
    in
    let mprim0_to_string p =
      match (p : _ Michelson_base.Primitive.prim0) with
      | Self None -> "self"
      | Self (Some name) -> app1 "self_entry_point" (sprintf "%S" name)
      | Sapling_empty_state {memo} ->
          app1 "sapling_empty_state" (string_of_int memo)
      | p -> mprim0_to_string p
    in
    let mprim1_to_string p x =
      match (p : _ Michelson_base.Primitive.prim1) with
      | IsNat -> app1 "is_nat" (pp x)
      | p ->
          let Michelson.{name} =
            Michelson.spec_of_instr ~strict_dup:true (MI1 p)
          in
          app1 (String.lowercase_ascii name) (pp x)
    in
    let mprim2_to_string p x1 x2 =
      match (p : _ Michelson_base.Primitive.prim2) with
      | Lsl -> prot (infix ~options "<<" x1 x2)
      | Lsr -> prot (infix ~options ">>" x1 x2)
      | p -> app2 (mprim2_to_string p) (pp x1) (pp x2)
    in
    let mprim3_to_string p x1 x2 x3 =
      match (p : Michelson_base.Primitive.prim3) with
      | Check_signature -> app3 "check_signature" (pp x1) (pp x2) (pp x3)
      | p ->
          let Michelson.{name} =
            Michelson.spec_of_instr ~strict_dup:true (MI3 p)
          in
          app3 (String.lowercase_ascii name) (pp x1) (pp x2) (pp x3)
    in
    match e.e with
    | EVar (x, Simple) -> with_vclass Simple ~html:options.html x
    | EVar ("__parameter__", _) -> sprintf "params"
    | EVar ("__operations__", _) -> "operations"
    | EVar ("__storage__", _) -> "data"
    | EVar (name, Local) -> name
    | EVar (id, Scenario) -> app1 "scenario_var" id
    | EPrivate name -> sprintf "self.%s" name
    | EMPrim0 p -> mprim0_to_string p
    | EMPrim1 (p, x) -> mprim1_to_string p x
    | EMPrim1_fail _ -> assert false
    | EMPrim2 (p, x1, x2) -> mprim2_to_string p x1 x2
    | EMPrim3 (p, x1, x2, x3) -> mprim3_to_string p x1 x2 x3
    | EPrim0 prim ->
      begin
        match prim with
        | ECst v | EBounded v ->
            htmlClass
              "constant"
              (literal_to_sp_string
                 ~html:false
                 ?protect
                 ?strip_strings:(if options.stripStrings then Some () else None)
                 v)
        | ECstContract {entry_point; address} ->
            htmlClass
              "constant"
              (literal_to_sp_string
                 ~html:false
                 ?strip_strings:(if options.stripStrings then Some () else None)
                 (Literal.address ?entry_point address))
        | EMatchCons x -> with_vclass Simple ~html:options.html x
        | EMetaLocal name -> name
        | EContract_address (id, entry_point) ->
            app1
              "address"
              (Aux.address_of_contract_id ~html:false id entry_point)
        | EContract_balance id ->
            app1 "contract_balance" (string_of_contract_id id)
        | EContract_baker id -> app1 "contract_baker" (string_of_contract_id id)
        | EContract_data id -> app1 "contract_data" (string_of_contract_id id)
        | EContract_typed (id, entry_point) ->
            app1
              "contract_typed"
              (Aux.address_of_contract_id ~html:false id entry_point)
        | EAccount_of_seed {seed} -> app1 "test_account" (sprintf "%S" seed)
        | EConstant (e, t) ->
            prefix
              ~language
              (sprintf "constant(%S, t = %s)" e (type_to_string t))
        | EConstantVar e -> app1 "constant_var" (sprintf "%S" e)
      end
    | EPrim1 (prim, x) ->
      ( match prim with
      | EResolve -> app1 "resolve" (pp x)
      | EListRev -> app1 "List.rev" (pp x)
      | EListItems false -> app1 "Map.items" (pp x)
      | EListKeys false -> app1 "Map.keys" (pp x)
      | EListValues false -> app1 "Map.values" (pp x)
      | EListElements false -> app1 "Set.elements" (pp x)
      | EListItems true -> app1 "Map.rev_items" (pp x)
      | EListKeys true -> app1 "Map.rev_keys" (pp x)
      | EListValues true -> app1 "Map.rev_values" (pp x)
      | EListElements true -> app1 "Set.rev_elements" (pp x)
      | EPack -> app1 "pack" (pp x)
      | EToInt -> app1 "to_int" (pp x)
      | ENeg -> prot (sprintf "- %s" (pp x))
      | ESign -> app1 "sign" (pp x)
      | ESum -> app1 "sum" (pp x)
      | EUnpack t -> app2 "unpack" (pp x) (type_to_string t)
      | EAddress ->
        begin
          match x.e with
          | EMPrim0 (Self None) -> "self_address"
          | EMPrim0 (Self (Some name)) -> app1 "self_entry_point_address" name
          | _ -> app1 "to_address" (pp x)
        end
      | EImplicit_account -> app1 "implicit_account" (pp x)
      | EProject 0 -> app1 "fst" (pp x)
      | EProject 1 -> app1 "snd" (pp x)
      | EProject i -> sprintf "%s[%d]" (pp x) i
      | EConcat_list -> app1 "concat" (pp x)
      | ESize -> app1 "len" (pp x)
      | ESetDelegate -> app1 "set_delegate_operation" (pp x)
      | EType_annotation t -> app2 "set_type_expr" (pp x) (type_to_string t)
      | EAttr name -> sprintf "%s.%s" (pp x) name
      | EIsVariant "Some" -> app1 "is_some" (pp x)
      | EIsVariant name -> app2 "is_variant" (sprintf "%S" name) (pp x)
      | EVariant name ->
        begin
          match (name, x) with
          | "None", {e = EPrim0 (ECst Unit)} -> "None"
          | "Some", x -> app1 "some" (pp x)
          | name, x -> app2 "variant" name (pp x)
        end
      | EReadTicket -> app1 "read_ticket_raw" (pp x)
      | EJoinTickets -> app1 "join_tickets_raw" (pp x)
      | EPairingCheck -> app1 "pairing_check" (pp x)
      | EVotingPower -> app1 "voting_power" (pp x)
      | EUnbounded -> app1 "unbound" (pp x)
      | EConvert -> app1 "convert" (pp x)
      | EStaticView (static_id, name) ->
          app3
            "contract_view"
            (string_of_contract_id (C_static static_id))
            name
            (to_string x) )
    | EPrim2 (prim2, x, y) ->
      begin
        match prim2 with
        | ECallLambda ->
            let lambda, parameter = (x, y) in
            prot (sprintf "%s %s" (pp lambda) (pp parameter))
        | EApplyLambda ->
            let lambda, parameter = (x, y) in
            app2 "apply_lambda" (pp lambda) (pp parameter)
        | EBinOp ((BEDiv | BMax | BMin | BMul {overloaded = true}) as op) ->
            app2 (string_of_binOp op) (pp x) (pp y)
        | EBinOp op -> prot (infix ~options (string_of_binOp op) x y)
        | EGetOpt ->
            let k, m = (x, y) in
            app2 "get_opt" (pp m) (pp k)
        | ECons ->
            let e1, e2 = (x, y) in
            prot (sprintf "%s :: %s" (pp e1) (pp e2))
        | EAdd_seconds ->
            let e1, e2 = (x, y) in
            app2 "add_seconds" (pp e1) (pp e2)
        | EContains ->
            let member, items = (x, y) in
            app2 "contains" (pp member) (pp items)
        | ETicket ->
            let content, amount = (x, y) in
            app2 "ticket" (pp content) (pp amount)
        | ESplitTicket ->
            let ticket, decomposition = (x, y) in
            app2 "split_ticket_raw" (pp ticket) (pp decomposition)
        | EView (name, return_type) ->
            let address, param = (x, y) in
            sprintf
              "view(%S, %s, %s, %s)"
              name
              (pp address)
              (pp param)
              (type_to_string return_type)
      end
    | EPrim3 (prim3, x, y, z) ->
      begin
        match prim3 with
        | ERange -> app3 "range" (pp x) (pp y) (pp z)
        | EUpdate_map -> app3 "Map.update" (pp x) (pp y) (pp z)
        | EGet_and_update -> app3 "get_and_update" (pp x) (pp y) (pp z)
        | ESplit_tokens ->
          begin
            match (x, y, z) with
            | ( {e = EPrim0 (ECst (Mutez tok))}
              , quantity
              , {e = EPrim0 (ECst (Int {i}))} )
              when Big_int.eq_big_int tok (Bigint.of_int 1000000)
                   && Big_int.eq_big_int i (Bigint.of_int 1) ->
                app1 "tez" (to_string quantity)
            | ( {e = EPrim0 (ECst (Mutez tok))}
              , quantity
              , {e = EPrim0 (ECst (Int {i}))} )
              when Big_int.eq_big_int tok (Bigint.of_int 1)
                   && Big_int.eq_big_int i (Bigint.of_int 1) ->
                app1 "mutez" (to_string quantity)
            | e1, e2, e3 -> app3 "split_tokens" (pp e1) (pp e2) (pp e3)
          end
        | ETest_ticket -> app3 "test_ticket" (pp x) (pp y) (pp z)
      end
    | EOpenVariant (name, x, missing_message) ->
        let missing_message =
          match missing_message with
          | None -> None
          | Some x -> Some (sprintf "~message:%s" (pp x))
        in
        ( match (name, x, missing_message) with
        (* | "Some", {e = EMPrim1 (IsNat, x)}, None ->
               app1 "as_nat" (pp x)
           | "Some", {e = EMPrim1 (IsNat, x)}, Some message ->
               app2 "as_nat" (pp x) message
        *)
        | "Some", x, None -> app1 "open_some" (pp x)
        | "Some", x, Some message -> app2 "open_some" (pp x) message
        | name, x, None -> app2 "open_variant" (pp x) name
        | name, x, Some message -> app3 "open_variant" (pp x) name message )
    | ELambda
        { name
        ; body =
            {c = CResult {e = EMichelson (michelson, [{e = EVar (name', _)}])}}
        }
      when name = name' ->
        sprintf "lambda_michelson(%S)" michelson.name
    | ELambda {name; body} ->
        prot (sprintf "fun %s -> %s" name (command_to_string ~options body))
    | EMapFunction {f; l} -> app2 "map" (pp f) (pp l)
    | ECreate_contract _ -> sprintf "create contract ..."
    | EItem {items; key; default_value = None; missing_message = None} ->
        app2 "Map.get" (pp items) (pp key)
    | EItem {items; key; default_value = Some d} ->
        app3 "get ~default_value:%s" (pp items) (pp key) (pp d)
    | EItem {items; key; default_value = None; missing_message = Some message}
      ->
        app3 "get ~message:%s" (pp items) (pp key) (pp message)
    | ERecord entries ->
        sprintf
          "{%s}"
          (String.concat
             "; "
             (List.map (fun (n, e) -> sprintf "%s = %s" n (pp e)) entries))
    | EList l -> sprintf "[%s]" (String.concat "; " (List.map to_string l))
    | EMap (_, l) ->
        let f (k, e) = sprintf "(%s, %s)" (to_string k) (to_string e) in
        app1 "Map.make" (sprintf "[%s]" (String.concat "; " (List.map f l)))
    | ESet l ->
        app1
          "Set.make"
          (sprintf "[%s]" (String.concat "; " (List.map to_string l)))
    | EContract {arg_type; entry_point; address} ->
        app3
          "contract"
          (type_to_string arg_type)
          (pp address)
          (Base.Option.value_map entry_point ~default:"" ~f:(fun ep ->
               sprintf ", entry_point='%s'" ep))
    | ETuple es ->
        let es = List.map to_string es in
        sprintf "(%s)" (String.concat ", " es)
    | ESlice {offset; length; buffer} ->
        app3 "slice" (pp buffer) (pp offset) (pp length)
    | EMake_signature {secret_key; message; message_format} ->
        sprintf
          "make_signature ~secret_key:%s ~message:%s ~message_format:%s"
          (pp secret_key)
          (pp message)
          ( match message_format with
          | `Hex -> "'Hex'"
          | `Raw -> "'Raw'" )
    | EMichelson (michelson, exprs) ->
        sprintf
          "michelson(%S)(%s)"
          michelson.name
          (String.concat ", " (List.map to_string exprs))
    | ETransfer {arg; amount; destination} ->
        app3 "transfer_operation" (pp arg) (pp amount) (pp destination)
    | EMatch (scrutinee, clauses) ->
        sprintf
          "ematch(%s, %s)"
          (pp scrutinee)
          (String.concat
             ", "
             (List.map
                (fun (cons, rhs) -> sprintf "(\"%s\", %s)" cons (pp rhs))
                clauses))
    | ESaplingVerifyUpdate {transaction; state} ->
        app2 "sapling_verify_update" (pp state) (pp transaction)
    | EHasEntryPoint ep -> app1 "has_entry_point" (sprintf "%S" ep)
    | EIf (cond, a, b) -> app3 "eif" (pp cond) (pp a) (pp b)
    | EIsFailing x -> app1 "is_failing" (pp x)
    | ECatchException (t, x) -> app2 "catch_exception" (pp x) (type_to_string t)

  and expr_to_string_ts ?(_options = Options.string) ?_protect _e = assert false

  and infix ~options op x y =
    let x = expr_to_string ~options ~protect:() x in
    let y = expr_to_string ~options ~protect:() y in
    sprintf "%s %s %s" x op y

  and pp_command ?indent ?options ?protect =
    match language with
    | SmartPy -> pp_command_py ?indent ?options
    | SmartML -> pp_command_ml ?indent ?options ?protect
    | SmartTS -> assert false

  and pp_command_py ?(indent = "") ?(options = Options.string) =
    let pp_command = pp_command_py ~options in
    let shiftIdent = if options.html then "&nbsp;&nbsp;" else "  " in
    let newline = if options.html then "\n<br>" else "\n" in
    let print_lambda buf (l, v, body) =
      let ppc x = pp_command ~indent:(indent ^ shiftIdent) x in
      bprintf buf "%sdef %s(%s):%s%a%s" indent l v newline ppc body newline
    in
    let expr_to_string = expr_to_string ~options in
    let rec pp ~indent buf c =
      let pp_exprs arguments = List.map expr_to_string arguments in
      let std name arguments =
        bprintf buf "%ssp.%s(%s)" indent name (String.concat ", " arguments)
      in
      let mono name s = std name (pp_exprs [s]) in
      let bin name x y = std name (pp_exprs [x; y]) in
      List.iter (print_lambda buf) (collect_lambdas c);
      match c.c with
      | CNever message -> mono "never" message
      | CFailwith message -> mono "failwith" message
      | CIf (c, t, e) ->
          let (then_format, else_format) : _ format * _ format =
            ("%ssp.if %s:%s%a", "%s%ssp.else:%s%a")
          in
          bprintf
            buf
            then_format
            indent
            (expr_to_string c)
            newline
            (pp ~indent:(indent ^ shiftIdent))
            t;
          ( match e.c with
          | CResult {e = EPrim0 (ECst Literal.Unit)} -> ()
          | _ ->
              bprintf
                buf
                else_format
                newline
                indent
                newline
                (pp ~indent:(indent ^ shiftIdent))
                e )
      | CMatch (scrutinee, [(constructor, arg_name, c)]) ->
          bprintf
            buf
            "%swith %s.match('%s') as %s:%s%a"
            indent
            (expr_to_string ~protect:() scrutinee)
            constructor
            arg_name
            newline
            (pp ~indent:(indent ^ shiftIdent))
            c
      | CMatch (scrutinee, cases) ->
          bprintf
            buf
            "%swith %s.match_cases() as arg:%s%a"
            indent
            (expr_to_string ~protect:() scrutinee)
            newline
            (fun buf ->
              List.iter (fun (constructor, arg_name, c) ->
                  bprintf
                    buf
                    "%swith arg.match('%s') as %s:%s%a%s"
                    (indent ^ shiftIdent)
                    constructor
                    arg_name
                    newline
                    (pp ~indent:(indent ^ shiftIdent ^ shiftIdent))
                    c
                    newline))
            cases
      | CMatchProduct (_, Pattern_single _, _) -> assert false
      | CMatchProduct (s, Pattern_tuple ns, c) ->
          let f = bprintf buf "%s%a = sp.match_tuple(%s, %a)%s%a" in
          f indent pps ns (expr_to_string s) ppS ns newline (pp ~indent) c
      | CMatchProduct (s, Pattern_record (_name, bs), c) ->
          let vs = List.map (fun {var} -> var) bs in
          let fs = List.map (fun {field} -> field) bs in
          let f = bprintf buf "%s%a = sp.match_record(%s, %a)%s%a" in
          f indent pps vs (expr_to_string s) ppS fs newline (pp ~indent) c
      | CModifyProduct (s, Pattern_single x, c) ->
          let indent' = indent ^ shiftIdent in
          let f = bprintf buf "%swith sp.modify(%s, %S) as %S:%s%a" in
          f indent (expr_to_string s) x x newline (pp ~indent:indent') c
      | CModifyProduct (s, Pattern_tuple ls, c) ->
          let indent' = indent ^ shiftIdent in
          let f = bprintf buf "%swith sp.match_tuple(%s, %a) as %a:%s%a" in
          f
            indent
            (expr_to_string s)
            ppS
            ls
            pps
            ls
            newline
            (pp ~indent:indent')
            c
      | CModifyProduct (s, Pattern_record (name, _bs), c) ->
          let indent' = indent ^ shiftIdent in
          let f = bprintf buf "%swith sp.match_record(%s, %S) as %s:%s%a" in
          f indent (expr_to_string s) name name newline (pp ~indent:indent') c
      | CMatchCons matcher ->
          bprintf
            buf
            "%swith sp.match_cons(%s) as %s:%s%a%s%selse:%s%a"
            indent
            (expr_to_string ~protect:() matcher.expr)
            matcher.id
            newline
            (pp ~indent:(indent ^ shiftIdent))
            matcher.ok_match
            newline
            indent
            newline
            (pp ~indent:(indent ^ shiftIdent))
            matcher.ko_match
      | CBind (None, c1, c2) ->
          pp ~indent buf c1;
          bprintf buf "%s" newline;
          pp ~indent buf c2
      | CBind (Some x, {c = CResult e}, c2) ->
          bprintf
            buf
            "%s%s = sp.local(%S, %s)%s"
            indent
            x
            x
            (expr_to_string e)
            newline;
          pp ~indent buf c2
      | CBind (Some x, c1, c2) ->
          bprintf buf "%s%s = sp.bind_block(%S):%s" indent x x newline;
          bprintf buf "%swith %s:%s" indent x newline;
          pp ~indent:(indent ^ shiftIdent) buf c1;
          bprintf buf "%s" newline;
          pp ~indent buf c2
      | CResult {e = EPrim0 (ECst Literal.Unit)} -> bprintf buf "%spass" indent
      | CResult r -> bprintf buf "%ssp.result(%s)" indent (expr_to_string r)
      | CFor (var, e, c) ->
          bprintf
            buf
            "%ssp.for %s in %s:%s%a"
            indent
            (variable_to_string ~options var Simple)
            (expr_to_string e)
            newline
            (pp ~indent:(indent ^ shiftIdent))
            c
      | CWhile (e, c) ->
          bprintf
            buf
            "%ssp.while %s:%s%a"
            indent
            (expr_to_string e)
            newline
            (pp ~indent:(indent ^ shiftIdent))
            c
      | CDefineLocal
          {var; rhs = {e = EPrim1 (EType_annotation t, e)}; is_mutable = _} ->
          bprintf
            buf
            "%s%s = sp.local(%S, %s, %s)"
            indent
            (tvariable_to_string ~options (var, t) Local)
            (tvariable_to_string ~options (var, t) Local)
            (expr_to_string e)
            (type_to_string t)
      | CDefineLocal {var; rhs} ->
          bprintf
            buf
            "%s%s = sp.local(%S, %s)"
            indent
            (variable_to_string ~options var Local)
            (variable_to_string ~options var Local)
            (expr_to_string rhs)
      | CSetVar (s, t) ->
        ( match t.e with
        | EPrim2 (ECons, u, s') when s = s' ->
          ( match (s.e, u.e) with
          | ( EVar ("__operations__", _)
            , ETransfer
                { destination =
                    { e =
                        EOpenVariant
                          ( "Some"
                          , {e = EContract {arg_type = F (T0 T_unit); address}}
                          , None ) }
                ; amount } ) ->
              bprintf
                buf
                "%ssp.send(%s, %s)"
                indent
                (expr_to_string address)
                (expr_to_string amount)
          | EVar ("__operations__", _), ETransfer {arg; amount; destination} ->
              bprintf
                buf
                "%ssp.transfer(%s, %s, %s)"
                indent
                (expr_to_string arg)
                (expr_to_string amount)
                (expr_to_string destination)
          | EVar ("__operations__", _), EPrim1 (ESetDelegate, e) ->
              bprintf buf "%ssp.set_delegate(%s)" indent (expr_to_string e)
          | _ ->
              bprintf
                buf
                "%s%s.push(%s)"
                indent
                (expr_to_string s)
                (expr_to_string u) )
        | EPrim2
            ( EBinOp ((BAdd | BSub | BMul {overloaded = false} | BDiv) as op)
            , s'
            , u )
          when equal_expr_modulo_line_nos s s' ->
            bprintf
              buf
              "%s%s %s= %s"
              indent
              (expr_to_string s)
              (string_of_binOp op)
              (expr_to_string u)
        | _ ->
            bprintf buf "%s%s = %s" indent (expr_to_string s) (expr_to_string t)
        )
      | CDelItem (expr, item) ->
          bprintf
            buf
            "%s%s %s[%s]"
            indent
            (if options.html then "<span class='keyword'>del</span>" else "del")
            (expr_to_string expr)
            (expr_to_string item)
      | CUpdateSet (expr, element, add) ->
          bprintf
            buf
            "%s%s.%s(%s)"
            indent
            (expr_to_string expr)
            (if add then "add" else "remove")
            (expr_to_string element)
      | CVerify (e, None) -> mono "verify" e
      | CVerify (e, Some msg) -> bin "verify" e msg
      | CComment s -> bprintf buf "%s# %s" indent s
      | CSetType (e, t) ->
          std "set_type" (pp_exprs [e] @ [type_to_string ~protect:() t])
      | CSetResultType (c, t) ->
          bprintf
            buf
            "%swith sp.set_result_type(%s):%s"
            indent
            (type_to_string t)
            newline;
          pp ~indent:(indent ^ shiftIdent) buf c
      | CTrace e -> mono "trace" e
      | CSetEntryPoint (s, e) ->
          bprintf buf "%ssp.set_entry_point(%S, %s)" indent s (expr_to_string e)
    in
    pp ~indent

  and pp_command_ml ?(indent = "") ?(options = Options.string) ?protect =
    let shiftIdent = if options.html then "&nbsp;&nbsp;" else "  " in
    let newline = if options.html then "\n<br>" else "\n" in
    let expr_to_string = expr_to_string ~options in
    let rec pp ?protect ~indent buf c =
      let app1 f x0 = bprintf buf "%s%s %s" indent f x0 in
      let app2 f x0 x1 = bprintf buf "%s%s %s %s" indent f x0 x1 in
      let app3 f x0 x1 x2 = bprintf buf "%s%s %s %s %s" indent f x0 x1 x2 in
      let pp_expr = expr_to_string ~protect:() in
      match c.c with
      | CNever message -> app1 "never" (pp_expr message)
      | CFailwith message -> app1 "failwith" (pp_expr message)
      | CIf (c, t, e) ->
          bprintf
            buf
            "%sif %s then%s%a"
            indent
            (expr_to_string c)
            newline
            (pp ~indent:(indent ^ shiftIdent) ~protect:())
            t;
          ( match e.c with
          | CResult {e = EPrim0 (ECst Literal.Unit)} -> ()
          | _ ->
              bprintf
                buf
                "%s%selse%s%a"
                newline
                indent
                newline
                (pp ~indent:(indent ^ shiftIdent) ~protect:())
                e )
      | CMatch (scrutinee, [(constructor, arg_name, c)]) ->
          if options.html
          then
            bprintf
              buf
              "%swith %s.match('%s') as %s:<br>%a"
              indent
              (expr_to_string ~protect:() scrutinee)
              constructor
              arg_name
              (fun x -> pp ~indent:(indent ^ shiftIdent) x)
              c
          else
            bprintf
              buf
              "%swith %s.match('%s') as %s:\n%a"
              indent
              (expr_to_string ~protect:() scrutinee)
              constructor
              arg_name
              (fun x -> pp ~indent:(indent ^ shiftIdent) x)
              c
      | CMatch (scrutinee, cases) ->
          if options.html
          then
            bprintf
              buf
              "%swith %s.match_cases() as arg:<br>%a"
              indent
              (expr_to_string ~protect:() scrutinee)
              (fun buf ->
                List.iter (fun (constructor, arg_name, c) ->
                    bprintf
                      buf
                      "%swith arg.match('%s') as %s:<br>%a<br>"
                      (indent ^ shiftIdent)
                      constructor
                      arg_name
                      (fun x -> pp ~indent:(indent ^ shiftIdent ^ shiftIdent) x)
                      c))
              cases
          else
            bprintf
              buf
              "%smatch %s with\n%a"
              indent
              (expr_to_string scrutinee)
              (fun buf ->
                List.iter (fun (constructor, arg_name, c) ->
                    bprintf
                      buf
                      "%s| `%s %s ->\n%a\n"
                      (indent ^ shiftIdent)
                      constructor
                      arg_name
                      (fun x -> pp ~indent:(indent ^ shiftIdent ^ shiftIdent) x)
                      c))
              cases
      | CMatchProduct (_, Pattern_single _, _) -> assert false
      | CMatchProduct (s, Pattern_tuple ns, c) ->
          let f = bprintf buf "%s%a = match_tuple(%s, %a)%s%a" in
          f indent pps ns (expr_to_string s) ppS ns newline (pp ~indent) c
      | CMatchProduct (s, Pattern_record (_name, bs), c) ->
          let vs = List.map (fun {var} -> var) bs in
          let fs = List.map (fun {field} -> field) bs in
          let f = bprintf buf "%s%a = match_record(%s, %a)%s%a" in
          f indent pps vs (expr_to_string s) ppS fs newline (pp ~indent) c
      | CModifyProduct (s, Pattern_single x, c) ->
          let indent' = indent ^ shiftIdent in
          let f = bprintf buf "%swith modify(%s, %S) as %S:%s%a" in
          f indent (expr_to_string s) x x newline (pp ~indent:indent') c
      | CModifyProduct (s, Pattern_tuple ls, c) ->
          let indent' = indent ^ shiftIdent in
          let f = bprintf buf "%swith match_tuple(%s, %a) as %a:%s%a" in
          f
            indent
            (expr_to_string s)
            ppS
            ls
            pps
            ls
            newline
            (pp ~indent:indent')
            c
      | CModifyProduct (s, Pattern_record (name, _bs), c) ->
          let indent' = indent ^ shiftIdent in
          let f = bprintf buf "%swith match_record(%s, %S) as %s:%s%a" in
          f indent (expr_to_string s) name name newline (pp ~indent:indent') c
      | CMatchCons matcher ->
          bprintf
            buf
            "%swith match_cons(%s) as %s:%s%a%s%selse:%s%a"
            indent
            (expr_to_string ~protect:() matcher.expr)
            matcher.id
            newline
            (fun x -> pp ~indent:(indent ^ shiftIdent) x)
            matcher.ok_match
            newline
            indent
            newline
            (fun x -> pp ~indent:(indent ^ shiftIdent) x)
            matcher.ko_match
      | CBind (None, _, _) ->
          let rec unroll r = function
            | {c = CBind (None, c1, c2)} -> unroll (c1 :: r) c2
            | c -> List.rev (c :: r)
          in
          ( match unroll [] c with
          | [] -> assert false
          | c :: cs ->
              if protect = Some () then bprintf buf "%s(%s" indent newline;
              (let indent =
                 if protect = Some () then indent ^ shiftIdent else indent
               in
               pp ~indent buf c;
               List.iter
                 (fun c ->
                   bprintf buf ";%s" newline;
                   pp ~indent buf c)
                 cs);
              if protect = Some () then bprintf buf "%s%s)" newline indent )
      | CBind (Some x, {c = CResult e}, c2) ->
          bprintf
            buf
            "%slet%%var %s = %s in%s"
            indent
            x
            (expr_to_string e)
            newline;
          pp ~indent buf c2
      | CBind (Some x, c1, c2) ->
          bprintf buf "%slet %s =%s" indent x newline;
          pp ~indent:(indent ^ shiftIdent) buf c1;
          bprintf buf "%s%sin%s" newline indent newline;
          pp ~indent buf c2
      | CResult {e = EPrim0 (ECst Literal.Unit)} -> bprintf buf "%s()" indent
      | CResult r -> app1 "result" (pp_expr r)
      | CFor (var, e, c) ->
          bprintf
            buf
            "%sList.iter (fun %s ->\n%a\n%s) %s"
            indent
            (variable_to_string ~options var Simple)
            (fun x -> pp ~indent:(indent ^ shiftIdent) x)
            c
            indent
            (expr_to_string ~protect:() e)
      | CWhile (e, c) ->
          bprintf
            buf
            "%swhile %s do\n%a\n%sdone"
            indent
            (expr_to_string e)
            (fun x -> pp ~indent:(indent ^ shiftIdent) x)
            c
            indent
      | CDefineLocal {var; rhs; is_mutable} ->
          bprintf
            buf
            "%slet%s %s = %s in ()"
            indent
            (if is_mutable then "%mutable" else "")
            (variable_to_string ~options var Local)
            (expr_to_string ~protect:() rhs)
      | CSetVar (lhs, rhs) ->
          let default () =
            bprintf
              buf
              "%s%s <- %s"
              indent
              (expr_to_string lhs)
              (expr_to_string rhs)
          in
          ( match (lhs.e, rhs.e) with
          | EItem {items; key; default_value = None; missing_message = None}, _
            ->
              bprintf
                buf
                "%sMap.set %s %s %s"
                indent
                (expr_to_string ~protect:() items)
                (expr_to_string ~protect:() key)
                (expr_to_string ~protect:() rhs)
          | ( EVar ("__operations__", _)
            , EPrim2 (ECons, op, {e = EVar ("__operations__", _)}) ) ->
            ( match op.e with
            | ETransfer
                { destination =
                    { e =
                        EOpenVariant
                          ( "Some"
                          , {e = EContract {arg_type = F (T0 T_unit); address}}
                          , None ) }
                ; amount } ->
                app2
                  "send"
                  (expr_to_string ~protect:() address)
                  (expr_to_string ~protect:() amount)
            | ETransfer {arg; amount; destination} when false ->
                bprintf
                  buf
                  "%stransfer%s%s %s"
                  indent
                  ( match arg with
                  | {e = EPrim0 (ECst Unit)} -> ""
                  | _ ->
                      Printf.sprintf " ~arg:%s" (expr_to_string ~protect:() arg)
                  )
                  ( match amount with
                  | {e = EPrim0 (ECst (Mutez amount))}
                    when Big_int.eq_big_int amount Big_int.zero_big_int ->
                      ""
                  | _ ->
                      Printf.sprintf
                        " ~amount:%s"
                        (expr_to_string ~protect:() amount) )
                  (expr_to_string ~protect:() destination)
            | ETransfer {arg; amount; destination} ->
                app3
                  "transfer"
                  (pp_expr arg)
                  (pp_expr amount)
                  (pp_expr destination)
            | EPrim1 (ESetDelegate, e) -> app1 "set_delegate" (pp_expr e)
            | _ -> default () )
          | _ -> default () )
      | CDelItem (expr, item) -> app2 "Map.delete" (pp_expr expr) (pp_expr item)
      | CUpdateSet (expr, element, true) ->
          app2 "Set.add" (pp_expr expr) (pp_expr element)
      | CUpdateSet (expr, element, false) ->
          app2 "Set.remove" (pp_expr expr) (pp_expr element)
      | CVerify (e, None) -> app1 "verify" (pp_expr e)
      | CVerify (e, Some msg) ->
          bprintf buf "%sverify %s ~msg:%s" indent (pp_expr e) (pp_expr msg)
      | CComment s -> bprintf buf "%s# %s" indent s
      | CSetType (e, t) ->
          bprintf
            buf
            "%sset_type %s %s"
            indent
            (pp_expr e)
            (type_to_string ~protect:() t)
      | CSetResultType (c, t) ->
          bprintf buf "%sset_result_type(%s)" indent (type_to_string t);
          pp ~indent:(indent ^ shiftIdent) buf c
      | CTrace e -> app1 "trace" (pp_expr e)
      | CSetEntryPoint (s, e) ->
          bprintf buf "%sset_entry_point(%S, %s)" indent s (expr_to_string e)
    in
    pp ~indent ?protect

  and command_to_string ?indent ?options c =
    let buf = Buffer.create 64 in
    pp_command ?indent ?options buf c;
    Buffer.contents buf

  let texpr_to_string ?options ?protect x =
    expr_to_string ?options ?protect (erase_types_expr (layout_records_expr x))

  let tcommand_to_string ?indent ?options x =
    command_to_string
      ?indent
      ?options
      (erase_types_command (layout_records_command x))

  let rec value_to_string_py
      ?(deep = false) ?(noEmptyList = false) ?(options = Options.string) v =
    let value_to_string = value_to_string_py in
    let operation_to_string = operation_to_string_py in
    match v.v with
    | Literal x -> literal_to_string ~deep ~options x
    | Bounded (_, x) -> literal_to_string ~deep ~options x
    | Contract {address; entry_point; type_} ->
        Printf.sprintf
          "sp.contract(%s, %s).open_some()"
          (type_to_string type_)
          (literal_to_string
             ~deep
             ~options
             (Literal.address ?entry_point address))
    | Record (layout, l) ->
        let fields =
          let f Layout.{source; target} = (source, target) in
          List.map f (Binary_tree.to_list layout)
        in
        if options.html
        then
          html_of_record_list
            ( List.map snd fields
            , [List.map (fun (source, _) -> List.assoc source l) fields] )
            (fun s -> value_to_string ~noEmptyList:true ~options s)
        else
          sprintf
            "sp.record(%s)"
            (String.concat
               ", "
               (List.map
                  (fun (n, _) ->
                    let x = List.assoc n l in
                    sprintf "%s = %s" n (value_to_string ~options x))
                  fields))
    | Variant (_layout, _row, name, v) when options.html ->
        sprintf
          "<div class='subtype'><select class='selection'><option \
           value='%s'>%s</option></select>%s</div>"
          name
          (String.capitalize_ascii name)
          (value_to_string v ~deep ~noEmptyList ~options)
    | Variant (_layout, _row, "None", {v = Literal Unit}) ->
        if options.html then "None" else "sp.none"
    | Variant (_layout, _row, name, {v = Literal Unit}) -> name
    | Variant (_layout, _row, "Some", v) ->
        if options.html
        then value_to_string ~options v (* TODO ? *)
        else sprintf "sp.some(%s)" (value_to_string ~options v)
    | Variant (_layout, _row, x, v) ->
        sprintf "%s(%s)" x (value_to_string ~options v)
    | List (_, []) when noEmptyList -> ""
    | List (_, l) when deep && not (is_record_list l) ->
        if options.html
        then
          sprintf
            "[%s]"
            (String.concat
               ", "
               (List.map (value_to_string ~options ~deep:true) l))
        else
          sprintf
            "[%s]"
            (String.concat ", " (List.map (value_to_string ~options) l))
    | List (_, l) when options.html ->
        let l =
          match l with
          | {v = Record (_, r)} :: _ as l ->
              ( List.map fst r
              , List.map
                  (function
                    | {v = Record (_, r)} -> List.map snd r
                    | _ -> assert false)
                  l )
          | _ -> ([""], List.map (fun x -> [x]) l)
        in
        html_of_record_list l (fun s ->
            value_to_string ~noEmptyList:true ~deep:true ~options s)
    | List (_, l) ->
        sprintf
          "[%s]"
          (String.concat ", " (List.map (value_to_string ~options) l))
    | Ticket (ticketer, content, amount) ->
        if options.html
        then
          html_of_record_list
            ( ["Ticketer"; "Content"; "Amount"]
            , [ [ sprintf "<span class='address'>%s</span>" ticketer
                ; value_to_string ~noEmptyList:true ~deep:true ~options content
                ; Bigint.string_of_big_int amount ] ] )
            (fun s -> s)
        else
          Format.asprintf
            "sp.ticket(sp.address('%s'), %s, %a)"
            ticketer
            (value_to_string ~options content)
            Bigint.pp
            amount
    | Set (_, []) when noEmptyList -> ""
    | Set (_, set) ->
        if options.html
        then
          html_of_record_list
            ([""], List.map (fun x -> [x]) set)
            (fun s -> value_to_string ~noEmptyList:true ~deep:true ~options s)
        else
          sprintf
            "sp.set([%s])"
            (String.concat ", " (List.map (value_to_string ~options) set))
    | Map (_, tvalue, _, map) ->
        if options.html
        then
          match is_matrix v with
          | Some (columns, l) ->
              html_of_record_list (columns, l) (fun s ->
                  value_to_string ~noEmptyList:true ~options s)
          | None ->
              let result =
                match Type.getRepr tvalue with
                | TRecord {row} ->
                    Some
                      (html_of_record_list
                         ( "Key" :: List.map fst row
                         , List.map (fun (x, y) -> x :: unrec y) map )
                         (fun s -> value_to_string ~noEmptyList:true ~options s))
                | _ -> None
              in
              ( match result with
              | None ->
                  html_of_record_list
                    (["Key"; "Value"], List.map (fun (x, y) -> [x; y]) map)
                    (fun s -> value_to_string ~noEmptyList:true ~options s)
              | Some t -> t )
        else
          sprintf
            "{%s}"
            (String.concat
               ", "
               (List.map
                  (fun (k, v) ->
                    sprintf
                      "%s : %s"
                      (value_to_string ~options k)
                      (value_to_string ~options v))
                  map))
    | Tuple vs ->
        if options.html
        then
          html_of_record_list
            ([], [vs])
            (fun s -> value_to_string ~noEmptyList:true ~options s)
        else
          let vs = List.map (value_to_string ~noEmptyList:true ~options) vs in
          sprintf "(%s)" (String.concat ", " vs)
    | Closure ({name; body = {c = CResult r}}, _) ->
        sprintf
          "sp.build_lambda(lambda %s: %s)"
          name
          (texpr_to_string ~options r)
    | Closure _ -> sprintf "lambda(%s)" (type_to_string (type_of_value v))
    | Operation operation -> operation_to_string ~options operation

  and operation_to_string_py ?(options = Options.string) operation =
    let value_to_string = value_to_string_py in
    if options.html
    then
      match operation with
      | Transfer {params; destination = {address; entry_point; type_}; amount}
        ->
          sprintf
            "<div class='operation'>Transfer %s to %s<br>%s</div>"
            (literal_to_string ~options (Literal.mutez amount))
            (value_to_string
               ~options
               (build_value (Basics.Contract {entry_point; address; type_})))
            (value_to_string ~options params)
      | SetDelegate None -> "<div class='operation'>Remove Delegate</div>"
      | SetDelegate (Some d) ->
          sprintf
            "<div class='operation'>Set Delegate(%s)</div>"
            (literal_to_string ~options (Literal.key_hash d))
      | CreateContract {id; instance = {state = {baker; storage}}} ->
          let baker =
            match baker with
            | None -> "sp.none"
            | Some baker ->
                sprintf
                  "sp.some(%s)"
                  (literal_to_string (Literal.key_hash baker))
          in
          sprintf
            "<div class='operation'>Create Contract(address: %s, baker: \
             %s)%s</div>"
            (Aux.address_of_contract_id ~html:options.html id None)
            baker
            (Base.Option.value_map
               ~f:(value_to_string ~options)
               ~default:""
               storage)
    else
      match operation with
      | Transfer {params; destination = {address; entry_point; type_}; amount}
        ->
          sprintf
            "Transfer\n     params: %s\n     amount: %s\n     to:     %s"
            (value_to_string ~options params)
            (literal_to_string ~options (Literal.mutez amount))
            (value_to_string
               ~options
               (build_value (Basics.Contract {entry_point; address; type_})))
      | SetDelegate None -> "Remove Delegate"
      | SetDelegate (Some d) ->
          sprintf
            "Set Delegate(%s)"
            (literal_to_string ~options (Literal.key_hash d))
      | CreateContract {id; instance = {state = {baker; storage}}} ->
          let baker =
            match baker with
            | None -> "sp.none"
            | Some baker ->
                sprintf
                  "sp.some(%s)"
                  (literal_to_string (Literal.key_hash baker))
          in
          sprintf
            "Create Contract(address: %s, baker: %s)%s"
            (Aux.address_of_contract_id ~html:options.html id None)
            baker
            (Base.Option.value_map
               ~f:(value_to_string ~options)
               ~default:""
               storage)

  let rec value_to_string_ml
      ?(deep = false) ?(noEmptyList = false) ?(options = Options.string) v =
    let value_to_string = value_to_string_ml in
    let operation_to_string = operation_to_string_ml in
    match v.v with
    | Literal x -> literal_to_string ~deep ~options x
    | Bounded (_, x) -> literal_to_string ~deep ~options x
    | Contract {address; entry_point; type_} ->
        Printf.sprintf
          "sp.contract(%s, %s).open_some()"
          (type_to_string type_)
          (literal_to_string
             ~deep
             ~options
             (Literal.address ?entry_point address))
    | Record (layout, l) ->
        let fields =
          let f Layout.{source; target} = (source, target) in
          List.map f (Binary_tree.to_list layout)
        in
        if options.html
        then
          html_of_record_list
            ( List.map snd fields
            , [List.map (fun (source, _) -> List.assoc source l) fields] )
            (fun s -> value_to_string ~noEmptyList:true ~options s)
        else
          sprintf
            "{%s}"
            (String.concat
               "; "
               (List.map
                  (fun (n, _) ->
                    let x = List.assoc n l in
                    sprintf "%s = %s" n (value_to_string ~options x))
                  fields))
    | Variant (_layout, _row, name, v) when options.html ->
        sprintf
          "<div class='subtype'><select class='selection'><option \
           value='%s'>%s</option></select>%s</div>"
          name
          (String.capitalize_ascii name)
          (value_to_string v ~deep ~noEmptyList ~options)
    | Variant (_layout, _row, "None", {v = Literal Unit}) -> "None"
    | Variant (_layout, _row, name, {v = Literal Unit}) -> name
    | Variant (_layout, _row, "Some", v) ->
        if options.html
        then value_to_string ~options v (* TODO ? *)
        else sprintf "Some(%s)" (value_to_string ~options v)
    | Variant (_layout, _row, x, v) ->
        sprintf "%s(%s)" x (value_to_string ~options v)
    | List (_, []) when noEmptyList -> ""
    | List (_, l) when deep && not (is_record_list l) ->
        if options.html
        then
          sprintf
            "[%s]"
            (String.concat
               "; "
               (List.map (value_to_string ~options ~deep:true) l))
        else
          sprintf
            "[%s]"
            (String.concat "; " (List.map (value_to_string ~options) l))
    | List (_, l) when options.html ->
        let l =
          match l with
          | {v = Record (_, r)} :: _ as l ->
              ( List.map fst r
              , List.map
                  (function
                    | {v = Record (_, r)} -> List.map snd r
                    | _ -> assert false)
                  l )
          | _ -> ([""], List.map (fun x -> [x]) l)
        in
        html_of_record_list l (fun s ->
            value_to_string ~noEmptyList:true ~deep:true ~options s)
    | List (_, l) ->
        sprintf
          "[%s]"
          (String.concat "; " (List.map (value_to_string ~options) l))
    | Ticket (ticketer, content, amount) ->
        if options.html
        then
          html_of_record_list
            ( ["Ticketer"; "Content"; "Amount"]
            , [ [ sprintf "<span class='address'>%s</span>" ticketer
                ; value_to_string ~noEmptyList:true ~deep:true ~options content
                ; Bigint.string_of_big_int amount ] ] )
            (fun s -> s)
        else
          Format.asprintf
            "sp.ticket(sp.address('%s'), %s, %a)"
            ticketer
            (value_to_string ~options content)
            Bigint.pp
            amount
    | Set (_, []) when noEmptyList -> ""
    | Set (_, set) ->
        if options.html
        then
          html_of_record_list
            ([""], List.map (fun x -> [x]) set)
            (fun s -> value_to_string ~noEmptyList:true ~deep:true ~options s)
        else
          sprintf
            "Set.make([%s])"
            (String.concat "; " (List.map (value_to_string ~options) set))
    | Map (_, tvalue, _, map) ->
        if options.html
        then
          match is_matrix v with
          | Some (columns, l) ->
              html_of_record_list (columns, l) (fun s ->
                  value_to_string ~noEmptyList:true ~options s)
          | None ->
              let result =
                match Type.getRepr tvalue with
                | TRecord {row} ->
                    Some
                      (html_of_record_list
                         ( "Key" :: List.map fst row
                         , List.map (fun (x, y) -> x :: unrec y) map )
                         (fun s -> value_to_string ~noEmptyList:true ~options s))
                | _ -> None
              in
              ( match result with
              | None ->
                  html_of_record_list
                    (["Key"; "Value"], List.map (fun (x, y) -> [x; y]) map)
                    (fun s -> value_to_string ~noEmptyList:true ~options s)
              | Some t -> t )
        else
          sprintf
            "Map.make [%s]"
            (String.concat
               "; "
               (List.map
                  (fun (k, v) ->
                    sprintf
                      "(%s, %s)"
                      (value_to_string ~options k)
                      (value_to_string ~options v))
                  map))
    | Tuple vs ->
        if options.html
        then
          html_of_record_list
            ([], [vs])
            (fun s -> value_to_string ~noEmptyList:true ~options s)
        else
          let vs = List.map (value_to_string ~noEmptyList:true ~options) vs in
          sprintf "(%s)" (String.concat ", " vs)
    | Closure _ -> sprintf "lambda(%s)" (type_to_string (type_of_value v))
    | Operation operation -> operation_to_string ~options operation

  and operation_to_string_ml ?(options = Options.string) operation =
    let value_to_string = value_to_string_ml in
    if options.html
    then
      match operation with
      | Transfer {params; destination = {address; entry_point; type_}; amount}
        ->
          sprintf
            "<div class='operation'>Transfer %s to %s<br>%s</div>"
            (literal_to_string ~options (Literal.mutez amount))
            (value_to_string
               ~options
               (build_value (Basics.Contract {entry_point; address; type_})))
            (value_to_string ~options params)
      | SetDelegate None -> "<div class='operation'>Remove Delegate</div>"
      | SetDelegate (Some d) ->
          sprintf
            "<div class='operation'>Set Delegate(%s)</div>"
            (literal_to_string ~options (Literal.key_hash d))
      | CreateContract {id; instance = {state = {baker; storage}}} ->
          let baker =
            match baker with
            | None -> "sp.none"
            | Some baker ->
                sprintf "Some(%s)" (literal_to_string (Literal.key_hash baker))
          in
          sprintf
            "<div class='operation'>Create Contract(address: %s, baker: \
             %s)%s</div>"
            (Aux.address_of_contract_id ~html:options.html id None)
            baker
            (Base.Option.value_map
               ~f:(value_to_string ~options)
               ~default:""
               storage)
    else
      match operation with
      | Transfer {params; destination = {address; entry_point; type_}; amount}
        ->
          sprintf
            "Transfer\n     params: %s\n     amount: %s\n     to:     %s"
            (value_to_string ~options params)
            (literal_to_string ~options (Literal.mutez amount))
            (value_to_string
               ~options
               (build_value (Basics.Contract {entry_point; address; type_})))
      | SetDelegate None -> "Remove Delegate"
      | SetDelegate (Some d) ->
          sprintf
            "Set Delegate(%s)"
            (literal_to_string ~options (Literal.key_hash d))
      | CreateContract {id; instance = {state = {baker; storage}}} ->
          let baker =
            match baker with
            | None -> "sp.none"
            | Some baker ->
                sprintf "Some(%s)" (literal_to_string (Literal.key_hash baker))
          in
          sprintf
            "Create Contract(address: %s, baker: %s)%s"
            (Aux.address_of_contract_id ~html:options.html id None)
            baker
            (Base.Option.value_map
               ~f:(value_to_string ~options)
               ~default:""
               storage)

  let value_to_string ?deep ?noEmptyList ?options v =
    match language with
    | SmartPy -> value_to_string_py ?deep ?noEmptyList ?options v
    | SmartML -> value_to_string_ml ?deep ?noEmptyList ?options v
    | SmartTS -> assert false

  let operation_to_string ?options op =
    match language with
    | SmartPy -> operation_to_string_py ?options op
    | SmartML -> operation_to_string_ml ?options op
    | SmartTS -> assert false

  let html_of_data options data =
    Printf.sprintf
      "<div id='storageDivInternal'>%s</div>"
      (value_to_string ~options data)
    ^
    match options with
    | {html = true; types = true} ->
        type_to_string ~options (type_of_value data) ^ "<br><br><br>"
    | _ -> ""

  let init_html_of_data options (x, v) =
    sprintf
      "%s = %s"
      (tvariable_to_string ~options x Simple)
      (value_to_string v)

  let init_html_of_data options : value -> string list = function
    | {v = Record (_, l)} ->
        List.map
          (fun (n, v) -> init_html_of_data options ((n, type_of_value v), v))
          l
    | {v = Literal Unit} -> []
    | v -> [value_to_string v]

  let pp_entry_point
      (options : Options.t) ppf pos {channel; originate; body; tparameter_ep} =
    let newline = if options.html then "\n<br>" else "\n" in
    let indent = if options.html then "&nbsp;&nbsp;" else "    " in
    match language with
    | SmartPy ->
        Format.fprintf ppf "%s" newline;
        let iter_f : _ format =
          if options.html
          then
            "<div class='on'>%s%s<span class='keyword'>def</span> %s(<span \
             class='self'>self</span>%s)<span class='keyword'>:</span><br><div \
             class='indent5'>%a</div></div>"
          else "%s%s  def %s(self%s):\n%a"
        in
        Format.fprintf
          ppf
          iter_f
          ( if originate
          then "\n  @sp.entry_point"
          else "\n  @sp.entry_point(private = True)" )
          newline
          channel
          ( match tparameter_ep with
          | `Annotated _ | `Present -> ", params"
          | `Absent -> "" )
          (fun ppf c ->
            let c =
              command_to_string ~options ~indent (erase_types_command c)
            in
            Format.fprintf ppf "%s" c)
          body
    | SmartML ->
        if pos <> 0 then Format.fprintf ppf "%s" newline;
        let iter_f : _ format =
          if options.html
          then
            "<div class='on'>let<span class='keyword'>%%entry_point</span> %s \
             %s<span class='keyword'> =</span><br><div \
             class='indent5'>%a</div></div>"
          else "\n  let%%entry_point %s %s =\n%a"
        in
        Format.fprintf
          ppf
          iter_f
          channel
          ( match tparameter_ep with
          | `Annotated _ | `Present -> "params"
          | `Absent -> "()" )
          (fun ppf c ->
            let c =
              command_to_string ~options ~indent (erase_types_command c)
            in
            Format.fprintf ppf "%s" c)
          body
    | SmartTS -> assert false

  let pp_private_lambda (options : Options.t) ppf (n, {name; body}) =
    let newline = if options.html then "\n<br>" else "\n" in
    let indent = if options.html then "&nbsp;&nbsp;" else "  " in
    Format.fprintf
      ppf
      "%s@sp.private_lambda()%sdef %s(%s):%s%s"
      (newline ^ newline ^ indent)
      (newline ^ indent)
      n
      name
      newline
      (tcommand_to_string ~indent:(indent ^ indent) ~options body)

  let pp_contract
      ?contract_id
      ?(options = Options.string)
      ppf
      { state = {balance; storage}
      ; template = {tcontract = {entry_points; private_variables; derived}} } =
    let tstorage = (get_extra derived).tstorage in
    let newline = if options.html then "\n<br>" else "\n" in
    let init =
      let storage =
        match storage with
        | None -> ""
        | Some storage ->
          begin
            match language with
            | SmartPy ->
                let (init_f, sep) : _ format * _ =
                  if options.html
                  then
                    ( "<div class='indent5'>&nbsp;&nbsp;self.init(%s)</div>"
                    , ",<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"
                    )
                  else ("\n    self.init(%s)", ",\n              ")
                in
                sprintf
                  init_f
                  (String.concat sep (init_html_of_data options storage))
            | SmartML ->
                let (init_f : _ format), sep =
                  if options.html
                  then
                    ( "<br>&nbsp;&nbsp;&nbsp;&nbsp;~storage:%s"
                    , "<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"
                    )
                  else ("\n      ~storage:%s", "\n                 ")
                in
                let storage =
                  match storage with
                  | {v = Record (_, _)} ->
                      sprintf
                        "[%%expr%s{%s}]"
                        sep
                        (String.concat
                           (";" ^ sep ^ " ")
                           (init_html_of_data options storage))
                  | _ -> sprintf "[%%expr %s]" (value_to_string storage)
                in
                sprintf init_f storage
            | SmartTS -> assert false
          end
      in
      match language with
      | SmartPy ->
          let init_f : _ format =
            if options.html
            then
              "<div class='on'><span class='keyword'>def</span> \
               __init__(self)<span class='keyword'>:</span><div \
               class='indent5'>&nbsp;&nbsp;self.init_type(%s)</div>\n\
               %s</div><br>"
            else "  def __init__(self):\n    self.init_type(%s)%s"
          in
          sprintf init_f (type_to_string tstorage) storage
      | SmartML ->
          let init_f : _ format =
            if options.html
            then
              "<div class='on'><span class='keyword'>let</span> init () <span \
               class='keyword'>=</span><div \
               class='indent5'>&nbsp;&nbsp;Basics.build_contract<br>&nbsp;&nbsp;&nbsp;&nbsp;~tstorage_explicit:%s%s\n\
               <br>&nbsp;&nbsp;&nbsp;&nbsp;[%s]</div></div>"
            else
              "  let init () =\n\
              \    Basics.build_contract\n\
              \      ~tstorage_explicit:[%%typ %s]%s\n\
              \      [%s]"
          in
          sprintf
            init_f
            (type_to_string tstorage)
            storage
            (String.concat
               "; "
               (List.map (fun (n : _ entry_point) -> n.channel) entry_points))
      | SmartTS -> assert false
    in
    let contract_id =
      match contract_id with
      | None -> ""
      | Some contract_id ->
          Printf.sprintf
            "<h3>New contract: %s</h3>"
            (Aux.address_of_contract_id ~html:options.html contract_id None)
    in
    if options.html
    then
      Format.fprintf
        ppf
        "<div class='contract'>%s<h3>Balance: %s</h3>\n\
         <h3>Storage:</h3>\n\
         %s\n\
         <h3>Code:</h3>\n"
        contract_id
        (ppAmount true balance)
        (Base.Option.value_map ~default:"" ~f:(html_of_data options) storage);
    begin
      match language with
      | SmartPy ->
          Format.fprintf
            ppf
            "import smartpy as sp%s%sclass Contract(sp.Contract):%s%s"
            newline
            newline
            newline
            init
      | SmartML ->
          Format.fprintf
            ppf
            "open SmartML%s%smodule Contract = struct"
            newline
            newline
      | SmartTS -> assert false
    end;
    List.iteri (pp_entry_point options ppf) entry_points;
    begin
      match language with
      | SmartPy ->
          let private_lambdas =
            List.filter_map
              (function
                | name, Typed.{e = ELambda f} -> Some (name, f)
                | _ -> None)
              private_variables
          in
          List.iter (pp_private_lambda options ppf) private_lambdas
      | SmartML -> ()
      | SmartTS -> assert false
    end;
    begin
      match language with
      | SmartPy ->
          Format.fprintf
            ppf
            "%ssp.add_compilation_target(\"test\", Contract())"
            (newline ^ newline)
      | SmartML ->
          Format.fprintf
            ppf
            "%s\n\
             %s%send%s%slet () =%s  Target.register_compilation%s    \
             ~name:\"contract\"%s    ~is_default:true%s    (Contract.init ())"
            newline
            init
            newline
            newline
            newline
            newline
            newline
            newline
            newline
      | SmartTS -> assert false
    end;
    if options.html then Format.fprintf ppf "</div>"

  let contract_to_string ?contract_id ?options c =
    Format.asprintf "%a" (pp_contract ?contract_id ?options) c

  let pp_tcontract ?options fmt c =
    pp_contract ?options fmt (layout_records_instance c)

  let tcontract_to_string ?contract_id ?options c =
    contract_to_string ?contract_id ?options (layout_records_instance c)

  let ppType ?multiline html t =
    if html
    then
      sprintf
        "<span class='type'>%s</span>"
        (type_to_string ~options:Options.html ?multiline t)
    else type_to_string ?multiline t

  let of_file (s, l) =
    match l with
    | -1 -> ""
    | _ ->
        let s =
          let l = sprintf "line %i" l in
          match s with
          | "" -> l
          | s -> s ^ ", " ^ l
        in
        sprintf " (%s)" s

  let ppExpr html (e : texpr) =
    if html
    then
      let pp =
        match e.line_no with
        | (_, l) :: _ ->
            sprintf
              "<button class='text-button' onClick='showLine(%i)'>%s</button>"
              l
        | [] -> id
      in
      pp (sprintf "(%s : %s)" (texpr_to_string e) (ppType html e.et))
    else
      sprintf
        "(%s : %s)%s"
        (texpr_to_string e)
        (ppType html e.et)
        (String.concat " " (List.map of_file e.line_no))

  let ppExpr_untyped html (e : expr) =
    if html
    then
      let pp =
        match e.line_no with
        | (_, l) :: _ ->
            sprintf
              "<button class='text-button' onClick='showLine(%i)'>%s</button>"
              l
        | [] -> id
      in
      pp (expr_to_string e)
    else
      sprintf
        "(%s)%s"
        (expr_to_string e)
        (String.concat " " (List.map of_file e.line_no))

  let rec simplify acc (x : smart_except list) =
    match x with
    | [] -> acc
    | ( ( `Literal _ | `Value _ | `Expr _ | `Exprs _ | `Expr_untyped _ | `Text _
        | `Type _ ) as x )
      :: rest ->
        simplify (x :: acc) rest
    | `Br :: rest ->
        let acc =
          match acc with
          | `Br :: _ -> acc
          | _ -> `Br :: acc
        in
        simplify acc rest
    | `Rec l :: rest -> simplify acc (l @ rest)
    | `Line i :: rest ->
      ( match i with
      | [] -> simplify acc rest
      | i :: (_ :: _ as j) -> simplify acc (`Line [i] :: `Line j :: rest)
      | [i] ->
        begin
          match acc with
          | `Line [j] :: `Br :: _ when i = j -> simplify acc rest
          | `Br :: _ -> simplify (`Line [i] :: acc) rest
          | _ -> simplify (`Line [i] :: `Br :: acc) rest
        end )

  let simplify x = List.rev (simplify [] x)

  let pp_smart_except html = function
    | `Literal literal -> literal_to_sp_string ~html literal
    | `Value value -> value_to_string value
    | `Expr expr -> ppExpr html expr
    | `Exprs exprs -> String.concat ", " (List.map (ppExpr html) exprs)
    | `Expr_untyped expr -> ppExpr_untyped html expr
    | `Line line_no ->
        if html
        then
          let line_no, i =
            match line_no with
            | [] -> (("", -1), -1)
            | (s, i) :: _ -> ((s, i), i)
          in
          sprintf
            "<button class='text-button' onClick='showLine(%i)'>%s</button>"
            i
            (of_file line_no)
        else String.concat " " (List.map of_file line_no)
    | `Text s -> s
    | `Type t -> ppType ~multiline:() html t
    | `Rec _ | `Br -> assert false

  let rec flatten acc = function
    | `Br :: rest -> List.rev acc :: flatten [] rest
    | `Rec l :: rest -> flatten acc (l @ rest)
    | x :: rest -> flatten (x :: acc) rest
    | [] -> if acc = [] then [] else [List.rev acc]

  let pp_smart_except html l =
    let l = flatten [] (simplify l) in
    let f xs = String.concat " " (List.map (pp_smart_except html) xs) in
    String.concat (if html then "<br>" else "\n") (List.map f l)

  let wrong_condition_string x =
    Printf.sprintf "WrongCondition: %s" (texpr_to_string x)

  let error_to_string operation =
    let open Execution in
    match operation with
    | Exec_failure (_, message) -> pp_smart_except false message

  let exception_to_smart_except = function
    | Failure s -> [`Text s]
    | SmartExcept l -> l
    | ExecFailure (_value, message) -> message
    | Yojson.Basic.Util.Type_error (msg, _t) ->
        [`Text ("Yojson exception - " ^ msg)]
    | e -> [`Text (Printexc.to_string e)]

  let exception_to_string html exc =
    pp_smart_except html (exception_to_smart_except exc)
end

module SmartPy : Printer = Build_printer ((
  struct
    let language = Config.SmartPy
  end :
    Language ))

module SmartML : Printer = Build_printer ((
  struct
    let language = Config.SmartML
  end :
    Language ))

module SmartTS : Printer = Build_printer ((
  struct
    let language = Config.SmartTS
  end :
    Language ))

let get_by_language = function
  | Config.SmartPy -> (module SmartPy : Printer)
  | SmartML -> (module SmartML : Printer)
  | SmartTS -> (module SmartTS : Printer)

let get Config.{languages} =
  match languages with
  | [] -> assert false
  | language :: _ -> get_by_language language
