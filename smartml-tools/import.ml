(* Copyright 2019-2021 Smart Chain Arena LLC. *)

open SmartML
open Basics
open Utils

type env =
  { unknowns : (string, Type.t) Hashtbl.t
  ; entry_point : string option }

let init_env () = {unknowns = Hashtbl.create 5; entry_point = None}

let attribute_source name =
  match Base.String.split (Base.String.strip name) ~on:' ' with
  | [name] -> name
  | [name; "as"; _] -> name
  | [name; "as"] -> name
  | _ -> Printf.ksprintf failwith "Bad attribute format %S" name

let attribute_target name =
  match Base.String.split (Base.String.strip name) ~on:' ' with
  | [name] -> name
  | [_; "as"; target] -> target
  | [_; "as"] -> ""
  | _ -> Printf.ksprintf failwith "Bad attribute format %S" name

let rec import_inner_layout = function
  | Base.Sexp.List [Atom leaf] ->
      ( [attribute_source leaf]
      , Layout.leaf (attribute_source leaf) (attribute_target leaf) )
  | Base.Sexp.List [n1; n2] ->
      let n1, l1 = import_inner_layout n1 in
      let n2, l2 = import_inner_layout n2 in
      (n1 @ n2, Binary_tree.node l1 l2)
  | l -> failwith ("Layout format error " ^ Base.Sexp.to_string l)

let import_layout ~line_no l layout =
  match l with
  | [] -> Unknown.unknown ()
  | _ ->
    ( match layout with
    | Base.Sexp.Atom "None" -> Unknown.unknown ()
    | List [Atom "Some"; Atom "Right"] ->
        Unknown.value (Type.comb_layout_of_row `Right l)
    | List [Atom "Some"; layout] ->
        let n, layout = import_inner_layout layout in
        let l1 = List.sort compare n in
        let l2 = List.sort compare l in
        if l1 <> l2
        then
          raise
            (SmartExcept
               [ `Text "Bad layout for type"
               ; `Br
               ; `Text (String.concat ", " l1)
               ; `Br
               ; `Text "!="
               ; `Br
               ; `Text (String.concat ", " l2)
               ; `Br
               ; `Line line_no ]);
        Unknown.value layout
    | l -> failwith ("Layout format error " ^ Base.Sexp.to_string l) )

let rec assocLists (ns : string list) = function
  | Base.Sexp.Atom (a : string) :: List l :: _ when List.mem a ns -> l
  | _ :: l -> assocLists ns l
  | _ -> failwith ("Cannot find " ^ List.show String.pp ns)

let rec assocList (n : string) = function
  | Base.Sexp.Atom (a : string) :: List l :: _ when Stdlib.(a = n) -> l
  | _ :: l -> assocList n l
  | _ -> failwith ("Cannot find " ^ n)

let default_line_no_f =
  let id x = x in
  let f_expr line_no e default_line_no =
    let line_no = if line_no = [] then default_line_no else line_no in
    let e = map_expr_f (fun e -> e line_no) id id elim_untyped e in
    {Untyped.line_no; e}
  in
  let f_command line_no c =
    let c = map_command_f (fun e -> e line_no) id id c in
    {Untyped.line_no; c}
  in
  let f_type = id in
  {f_expr; f_command; f_type}

let _default_line_no_expr e line_no = cata_expr default_line_no_f e line_no

let default_line_no_command e = cata_command default_line_no_f e

let int_option_of_string = function
  | "None" -> None
  | l -> Some (int_of_string l)

let import_line_no = function
  | Base.Sexp.Atom "None" -> []
  | Atom l -> [("", int_of_string l)]
  | List [Atom s; Atom l] -> [(s, int_of_string l)]
  | x -> failwith ("import_line_no: " ^ Base.Sexp.to_string x)

let string_of_line_no l =
  String.concat "_" (List.map (fun (_, i) -> string_of_int i) l)

let import_bool = function
  | "True" -> true
  | "False" -> false
  | x -> failwith ("import_bool: " ^ x)

let import_opt_bool = function
  | "None" -> None
  | "True" -> Some true
  | "False" -> Some false
  | x -> failwith ("import_opt_bool: " ^ x)

let import_unknown f = function
  | "None" -> Unknown.unknown ()
  | x -> Unknown.value (f x)

let unAtom = function
  | Base.Sexp.Atom n -> n
  | _ -> failwith "unAtom"

let rec import_flags =
  let open Base.Sexp in
  function
  | [] -> []
  | List x :: xs ->
      let x = List.map unAtom x in
      ( match Config.parse_flag x with
      | Some x -> x :: import_flags xs
      | _ -> failwith ("invalid flag usage: " ^ String.concat ", " x) )
  | xs -> failwith ("import_flags: " ^ to_string (List xs))

let import_binding = function
  | Base.Sexp.List [Atom "binding"; Atom var; Atom field] -> {var; field}
  | x -> failwith ("import_binding: " ^ Base.Sexp.to_string x)

let rec import_literal =
  let open Literal in
  function
  | [Base.Sexp.Atom "unit"] -> unit
  | [Atom "string"; Atom n] -> string n
  | [Atom "bytes"; Atom n] ->
      let n =
        Base.(String.chop_prefix n ~prefix:"0x" |> Option.value ~default:n)
      in
      bytes (Misc.Hex.unhex n)
  | [Atom "bls12_381_g1"; Atom n] ->
      let n =
        Base.(String.chop_prefix n ~prefix:"0x" |> Option.value ~default:n)
      in
      bls12_381_g1 (Misc.Hex.unhex n)
  | [Atom "bls12_381_g2"; Atom n] ->
      let n =
        Base.(String.chop_prefix n ~prefix:"0x" |> Option.value ~default:n)
      in
      bls12_381_g2 (Misc.Hex.unhex n)
  | [Atom "bls12_381_fr"; Atom n] ->
      let n =
        Base.(String.chop_prefix n ~prefix:"0x" |> Option.value ~default:n)
      in
      bls12_381_fr (Misc.Hex.unhex n)
  | [Atom "chest_key"; Atom n] ->
      let n =
        Base.(String.chop_prefix n ~prefix:"0x" |> Option.value ~default:n)
      in
      chest_key (Misc.Hex.unhex n)
  | [Atom "chest"; Atom n] ->
      let n =
        Base.(String.chop_prefix n ~prefix:"0x" |> Option.value ~default:n)
      in
      chest (Misc.Hex.unhex n)
  | [Atom "chain_id_cst"; Atom n] ->
      let n =
        Base.(String.chop_prefix n ~prefix:"0x" |> Option.value ~default:n)
      in
      chain_id (Misc.Hex.unhex n)
  | [Atom "int"; Atom n] -> int (Big_int.big_int_of_string n)
  | [Atom "intOrNat"; Atom n] ->
      intOrNat (Unknown.unknown ()) (Big_int.big_int_of_string n)
  | [Atom "nat"; Atom n] -> nat (Big_int.big_int_of_string n)
  | [Atom "timestamp"; Atom n] -> timestamp (Big_int.big_int_of_string n)
  | [Atom "bool"; Atom "True"] -> bool true
  | [Atom "bool"; Atom "False"] -> bool false
  | [Atom "key"; Atom n] -> key n
  | [Atom "secret_key"; Atom n] -> secret_key n
  | [Atom "signature"; Atom n] -> signature n
  | [Atom "address"; Atom n] -> address n
  | [Atom "key_hash"; Atom n] -> key_hash n
  | [Atom "mutez"; List l] ->
      mutez (Base.Option.value_exn (unInt (import_literal l)))
  | [Atom "mutez"; Atom n] -> mutez (Big_int.big_int_of_string n)
  | [ Atom "sapling_test_transaction"
    ; Atom memo
    ; Atom source
    ; Atom target
    ; Atom amount
    ; _ ] ->
      let source = if source = "" then None else Some source in
      let target = if target = "" then None else Some target in
      sapling_test_transaction
        (int_of_string memo)
        source
        target
        (Big_int.big_int_of_string amount)
  | [Atom "literal"; List x; _line_no] -> import_literal x
  | l ->
      Format.kasprintf failwith "Literal format error: %a" Base.Sexp.pp (List l)

let import_atom_type s =
  let open Type in
  match s with
  | "bool" -> bool
  | "int" -> int ()
  | "nat" -> nat ()
  | "intOrNat" -> intOrNat ()
  | "string" -> string
  | "bytes" -> bytes
  | "chain_id" -> chain_id
  | "mutez" -> token
  | "timestamp" -> timestamp
  | "address" -> address
  | "key_hash" -> key_hash
  | "key" -> key
  | "signature" -> signature
  | "operation" -> operation
  | "bls12_381_g1" -> bls12_381_g1
  | "bls12_381_g2" -> bls12_381_g2
  | "bls12_381_fr" -> bls12_381_fr
  | "chest_key" -> chest_key
  | "chest" -> chest
  | "never" -> never
  | s -> failwith ("Type format error atom " ^ s)

let import_type env =
  let open Type in
  let rec import_type = function
    | Base.Sexp.Atom "unit" -> unit
    | Atom s -> import_atom_type s
    | List (Atom "bounded" :: t :: Atom final :: cases) ->
        let import_case = function
          | Base.Sexp.List l -> import_literal l
          | Atom a -> failwith ("AAAAAAAAAAAA " ^ a)
        in
        bounded (import_type t) (import_bool final) (List.map import_case cases)
    | List [Atom "sapling_state"; Atom memo_size] ->
        sapling_state (int_option_of_string memo_size)
    | List [Atom "sapling_transaction"; Atom memo_size] ->
        sapling_transaction (int_option_of_string memo_size)
    | List [Atom "unknown"; Atom id] ->
        let i = "sp:" ^ id in
        if i = ""
        then failwith "empty unknown"
        else (
          match Hashtbl.find_opt env.unknowns i with
          | Some t -> t
          | None ->
              let r = unknown_raw (ref (UUnknown i)) in
              Hashtbl.replace env.unknowns i r;
              r )
    | List [Atom "record"; List l; layout; line_no] ->
        let line_no = import_line_no line_no in
        let l = List.map importField l in
        record_or_unit (import_layout ~line_no (List.map fst l) layout) l
    | List [Atom "variant"; List l; layout; line_no] ->
        let line_no = import_line_no line_no in
        let l = List.map importField l in
        variant (import_layout ~line_no (List.map fst l) layout) l
    | List [Atom "list"; t] -> list (import_type t)
    | List [Atom "ticket"; t] -> ticket (import_type t)
    | List [Atom "option"; t] -> option (import_type t)
    | List [Atom "contract"; t] -> contract (import_type t)
    | List (Atom "tuple" :: l) -> tuple (List.map import_type l)
    | List [Atom "set"; t] -> set ~telement:(import_type t)
    | List [Atom "map"; k; v] ->
        map ~big:false ~tkey:(import_type k) ~tvalue:(import_type v)
    | List [Atom "bigmap"; k; v] ->
        map ~big:true ~tkey:(import_type k) ~tvalue:(import_type v)
    | List [Atom "lambda"; t1; t2] ->
        lambda no_effects (import_type t1) (import_type t2)
    | List [Atom "lambda"; Atom with_storage; Atom with_operations; t1; t2] ->
        let with_storage =
          match with_storage with
          | "read-only" -> Some Read_only
          | "read-write" | "True" -> Some Read_write
          | "None" | "False" -> None
          | _ -> assert false
        in
        let with_storage = Unknown.value with_storage in
        let with_operations = import_unknown import_bool with_operations in
        let effects = {with_storage; with_operations} in
        lambda effects (import_type t1) (import_type t2)
    | List l as t ->
        failwith
          ( "Type format error list "
          ^ Base.Sexp.to_string t
          ^ "  "
          ^ string_of_int (List.length l) )
  and importField = function
    | List [Atom name; v] -> (name, import_type v)
    | l -> failwith ("Type field format error " ^ Base.Sexp.to_string l)
  in
  import_type

let import_contract_id = function
  | Base.Sexp.List [Atom "static_id"; Atom id; _] ->
      C_static {static_id = int_of_string id}
  | Base.Sexp.List [Atom "dynamic_id"; Atom id; _] ->
      C_dynamic {dynamic_id = int_of_string id}
  | l -> Format.kasprintf failwith "Contract_id format error: %a" Base.Sexp.pp l

let rec import_expr_inline_michelson env =
  let import_type t = import_type env t in
  let rec import_expr_inline_michelson = function
    | Base.Sexp.List [Atom "call_michelson"; instr; Atom _line_no] ->
        import_expr_inline_michelson instr
    | Base.Sexp.List (Atom "op" :: Atom name :: args) ->
        let parsed = Micheline_encoding.parse_node name in
        let rec extractTypes acc = function
          | [] -> assert false
          | Base.Sexp.Atom "out" :: out ->
              (List.rev acc, List.map import_type out)
          | t :: args -> extractTypes (import_type t :: acc) args
        in
        let typesIn, typesOut = extractTypes [] args in
        {name; parsed; typesIn; typesOut}
    | input ->
        failwith
          (Printf.sprintf
             "Cannot parse inline michelson %s"
             (Base.Sexp.to_string input))
  in
  import_expr_inline_michelson

and import_expr env =
  let import_type t = import_type env t in
  let import_expr_inline_michelson = import_expr_inline_michelson env in
  let open Expr in
  let rec import_expr = function
    | Base.Sexp.List (Atom f :: args) as input ->
      ( match (f, args) with
      | "sender", [] -> sender ~line_no:[]
      | "source", [] -> source ~line_no:[]
      | "amount", [] -> amount ~line_no:[]
      | "balance", [] -> balance ~line_no:[]
      | "now", [] -> now ~line_no:[]
      | "self", [] -> self ~line_no:[]
      | "self", [Atom name; line_no] ->
          let name =
            if name = ""
            then
              match env.entry_point with
              | None -> failwith "import: params type yet unknown"
              | Some name -> name
            else name
          in
          self_entry_point ~line_no:(import_line_no line_no) name
      | "self_address", [] -> self_address ~line_no:[]
      | "chain_id", [] -> chain_id ~line_no:[]
      | "total_voting_power", [] -> total_voting_power ~line_no:[]
      | "sapling_empty_state", [Atom memo; _line_no] ->
          sapling_empty_state (int_of_string memo)
      | "level", [] -> level ~line_no:[]
      | "eq", [e1; e2; line_no] ->
          bin_op
            ~op:BEq
            ~line_no:(import_line_no line_no)
            (import_expr e1)
            (import_expr e2)
      | "neq", [e1; e2; line_no] ->
          bin_op
            ~op:BNeq
            ~line_no:(import_line_no line_no)
            (import_expr e1)
            (import_expr e2)
      | "le", [e1; e2; line_no] ->
          bin_op
            ~op:BLe
            ~line_no:(import_line_no line_no)
            (import_expr e1)
            (import_expr e2)
      | "lt", [e1; e2; line_no] ->
          bin_op
            ~op:BLt
            ~line_no:(import_line_no line_no)
            (import_expr e1)
            (import_expr e2)
      | "ge", [e1; e2; line_no] ->
          bin_op
            ~op:BGe
            ~line_no:(import_line_no line_no)
            (import_expr e1)
            (import_expr e2)
      | "gt", [e1; e2; line_no] ->
          bin_op
            ~op:BGt
            ~line_no:(import_line_no line_no)
            (import_expr e1)
            (import_expr e2)
      | "add", [e1; e2; line_no] ->
          bin_op
            ~op:BAdd
            ~line_no:(import_line_no line_no)
            (import_expr e1)
            (import_expr e2)
      | "sub", [e1; e2; line_no] ->
          bin_op
            ~op:BSub
            ~line_no:(import_line_no line_no)
            (import_expr e1)
            (import_expr e2)
      | "mul", [e1; e2; line_no] ->
          bin_op
            ~op:(BMul {overloaded = false})
            ~line_no:(import_line_no line_no)
            (import_expr e1)
            (import_expr e2)
      | "mul_overloaded", [e1; e2; line_no] ->
          bin_op
            ~op:(BMul {overloaded = true})
            ~line_no:(import_line_no line_no)
            (import_expr e1)
            (import_expr e2)
      | "ediv", [e1; e2; line_no] ->
          bin_op
            ~op:BEDiv
            ~line_no:(import_line_no line_no)
            (import_expr e1)
            (import_expr e2)
      | "truediv", [e1; e2; line_no] ->
          bin_op
            ~op:BDiv
            ~line_no:(import_line_no line_no)
            (import_expr e1)
            (import_expr e2)
      | "floordiv", [e1; e2; line_no] ->
          bin_op
            ~op:BDiv
            ~line_no:(import_line_no line_no)
            (import_expr e1)
            (import_expr e2)
      | "mod", [e1; e2; line_no] ->
          bin_op
            ~op:BMod
            ~line_no:(import_line_no line_no)
            (import_expr e1)
            (import_expr e2)
      | "or", [e1; e2; line_no] ->
          bin_op
            ~op:BOr
            ~line_no:(import_line_no line_no)
            (import_expr e1)
            (import_expr e2)
      | "and", [e1; e2; line_no] ->
          bin_op
            ~op:BAnd
            ~line_no:(import_line_no line_no)
            (import_expr e1)
            (import_expr e2)
      | "max", [e1; e2; line_no] ->
          bin_op
            ~op:BMax
            ~line_no:(import_line_no line_no)
            (import_expr e1)
            (import_expr e2)
      | "min", [e1; e2; line_no] ->
          bin_op
            ~op:BMin
            ~line_no:(import_line_no line_no)
            (import_expr e1)
            (import_expr e2)
      | "sum", [a; line_no] ->
          sum ~line_no:(import_line_no line_no) (import_expr a)
      | "to_address", [e; line_no] ->
          to_address ~line_no:(import_line_no line_no) (import_expr e)
      | "implicit_account", [e; line_no] ->
          implicit_account ~line_no:(import_line_no line_no) (import_expr e)
      | "cons", [e1; e2; line_no] ->
          cons
            ~line_no:(import_line_no line_no)
            (import_expr e1)
            (import_expr e2)
      | "range", [e1; e2; e3; line_no] ->
          range
            ~line_no:(import_line_no line_no)
            (import_expr e1)
            (import_expr e2)
            (import_expr e3)
      | "literal", [List l; line_no] ->
          cst ~line_no:(import_line_no line_no) (import_literal l)
      | "bounded", [x; line_no] ->
          let line_no = import_line_no line_no in
          let l =
            match import_expr x with
            | {e = EPrim0 (ECst l)} -> l
            | _ ->
                raise
                  (SmartExcept
                     [ `Text
                         "sp.bounded can only be applied to simple literals \
                          (integers, strings, addresses, etc.), not containers \
                          (records, variants, lists, etc.) nor general \
                          expressions."
                     ; `Line line_no ])
          in
          bounded ~line_no l
      | "unbounded", [x; line_no] ->
          let line_no = import_line_no line_no in
          unbounded ~line_no (import_expr x)
      | "is_failing", [x; line_no] ->
          let line_no = import_line_no line_no in
          is_failing ~line_no (import_expr x)
      | "catch_exception", [x; t; line_no] ->
          let line_no = import_line_no line_no in
          catch_exception ~line_no (import_expr x) ~t:(import_type t)
      | "convert", [x; line_no] ->
          let line_no = import_line_no line_no in
          convert ~line_no (import_expr x)
      | "list", line_no :: l ->
          build_list
            ~line_no:(import_line_no line_no)
            ~elems:(List.map import_expr l)
      | "test_ticket", [ticketer; content; amount; line_no] ->
          test_ticket
            ~line_no:(import_line_no line_no)
            (import_expr ticketer)
            (import_expr content)
            (import_expr amount)
      | "ticket", [content; amount; line_no] ->
          build_ticket
            ~line_no:(import_line_no line_no)
            (import_expr content)
            (import_expr amount)
      | "read_ticket", [ticket; line_no] ->
          read_ticket ~line_no:(import_line_no line_no) (import_expr ticket)
      | "split_ticket", [ticket; decomposition; line_no] ->
          split_ticket
            ~line_no:(import_line_no line_no)
            (import_expr ticket)
            (import_expr decomposition)
      | "join_tickets", [tickets; line_no] ->
          join_tickets ~line_no:(import_line_no line_no) (import_expr tickets)
      | "pairing_check", [pairs; line_no] ->
          pairing_check ~line_no:(import_line_no line_no) (import_expr pairs)
      | "voting_power", [e; line_no] ->
          voting_power ~line_no:(import_line_no line_no) (import_expr e)
      | "first", [e; line_no] ->
          first ~line_no:(import_line_no line_no) (import_expr e)
      | "second", [e; line_no] ->
          second ~line_no:(import_line_no line_no) (import_expr e)
      | "tuple", line_no :: es ->
          tuple ~line_no:(import_line_no line_no) (List.map import_expr es)
      | "tuple", _ ->
          failwith
            (Printf.sprintf
               "Only supported tuples are pairs: %s"
               (Base.Sexp.to_string input))
      | "neg", [e1; line_no] ->
          negE ~line_no:(import_line_no line_no) (import_expr e1)
      | "abs", [e1; line_no] ->
          absE ~line_no:(import_line_no line_no) (import_expr e1)
      | "toInt", [e1; line_no] ->
          to_int ~line_no:(import_line_no line_no) (import_expr e1)
      | "isNat", [e1; line_no] ->
          is_nat ~line_no:(import_line_no line_no) (import_expr e1)
      | "sign", [e1; line_no] ->
          signE ~line_no:(import_line_no line_no) (import_expr e1)
      | "not", [e1; line_no] ->
          notE ~line_no:(import_line_no line_no) (import_expr e1)
      | "contract_address", [id; Atom entry_point; line_no] ->
          let line_no = import_line_no line_no in
          let entry_point =
            match entry_point with
            | "" -> None
            | x -> Some x
          in
          contract_address ~line_no entry_point (import_contract_id id)
      | "contract_typed", [id; Atom entry_point; line_no] ->
          let line_no = import_line_no line_no in
          let entry_point =
            match entry_point with
            | "" -> None
            | x -> Some x
          in
          contract_typed ~line_no entry_point (import_contract_id id)
      | "contract_balance", [id; line_no] ->
          let line_no = import_line_no line_no in
          contract_balance ~line_no (import_contract_id id)
      | "contract_baker", [id; line_no] ->
          let line_no = import_line_no line_no in
          contract_baker ~line_no (import_contract_id id)
      | "contract_data", [id; line_no] ->
          let line_no = import_line_no line_no in
          contract_data ~line_no (import_contract_id id)
      | "contract", [Atom entry_point; t; addr; line_no] ->
          let entry_point =
            match entry_point with
            | "" -> None
            | _ -> Some entry_point
          in
          let address = import_expr addr in
          contract
            ~line_no:(import_line_no line_no)
            entry_point
            (import_type t)
            address
      | "view", [Atom name; addr; param; t; line_no] ->
          let address = import_expr addr in
          let param = import_expr param in
          let ty = import_type t in
          view ~line_no:(import_line_no line_no) name address param ty
      | "static_view", [Atom name; contract_id; param; line_no] ->
          let static_id =
            match import_contract_id contract_id with
            | C_static static_id -> static_id
            | C_dynamic _ -> assert false
          in
          let param = import_expr param in
          static_view ~line_no:(import_line_no line_no) static_id name param
      | "data", [] -> local ~line_no:[] "__storage__"
      | "operations", [line_no] -> operations ~line_no:(import_line_no line_no)
      | "attr", [x; Atom name; line_no] ->
          attr ~line_no:(import_line_no line_no) (import_expr x) ~name
      | "match_cons", [_e; line_no] ->
          let line_no = import_line_no line_no in
          let name =
            Printf.sprintf "match_cons_%s" (string_of_line_no line_no)
          in
          match_cons ~line_no name
      | "isVariant", [x; Atom name; line_no] ->
          isVariant ~line_no:(import_line_no line_no) ~name (import_expr x)
      | "variant_arg", [Atom arg_name; line_no] ->
          variable ~line_no:(import_line_no line_no) arg_name
      | "openVariant", [x; Atom name; missing_message; line_no] ->
          let missing_message =
            match missing_message with
            | Atom "None" -> None
            | _ -> Some (import_expr missing_message)
          in
          openVariant
            ~line_no:(import_line_no line_no)
            name
            (import_expr x)
            missing_message
      | "variant", [Atom name; x; line_no] ->
          variant ~line_no:(import_line_no line_no) ~name (import_expr x)
      | "blake2b", [e; line_no] ->
          blake2b ~line_no:(import_line_no line_no) (import_expr e)
      | "sha256", [e; line_no] ->
          sha256 ~line_no:(import_line_no line_no) (import_expr e)
      | "sha512", [e; line_no] ->
          sha512 ~line_no:(import_line_no line_no) (import_expr e)
      | "keccak", [e; line_no] ->
          keccak ~line_no:(import_line_no line_no) (import_expr e)
      | "sha3", [e; line_no] ->
          sha3 ~line_no:(import_line_no line_no) (import_expr e)
      | "hash_key", [e; line_no] ->
          hash_key ~line_no:(import_line_no line_no) (import_expr e)
      | "pack", [e; line_no] ->
          let e = import_expr e in
          pack ~line_no:(import_line_no line_no) e
      | "unpack", [e; t; line_no] ->
          unpack
            ~line_no:(import_line_no line_no)
            (import_expr e)
            (import_type t)
      | "getLocal", [Atom name; line_no] ->
          local ~line_no:(import_line_no line_no) name
      | "getMetaLocal", [Atom name; line_no] ->
          meta_local ~line_no:(import_line_no line_no) name
      | "params", [line_no] ->
          let line_no = import_line_no line_no in
          params ~line_no
      | "update_map", [map; key; v; line_no] ->
          updateMap
            ~line_no:(import_line_no line_no)
            (import_expr key)
            (import_expr v)
            (import_expr map)
      | "get_and_update", [map; key; v; line_no] ->
          get_and_update
            ~line_no:(import_line_no line_no)
            (import_expr key)
            (import_expr v)
            (import_expr map)
      | "open_chest", [chest_key; chest; time; line_no] ->
          open_chest
            ~line_no:(import_line_no line_no)
            (import_expr chest_key)
            (import_expr chest)
            (import_expr time)
      | "getItem", [a; pos; line_no] ->
          item
            ~line_no:(import_line_no line_no)
            (import_expr a)
            (import_expr pos)
            None
            None
      | "getItemDefault", [a; pos; def; line_no] ->
          item
            ~line_no:(import_line_no line_no)
            (import_expr a)
            (import_expr pos)
            (Some (import_expr def))
            None
      | "getItemMessage", [a; pos; message; line_no] ->
          item
            ~line_no:(import_line_no line_no)
            (import_expr a)
            (import_expr pos)
            None
            (Some (import_expr message))
      | "getOpt", [m; line_no; k] ->
          getOpt
            ~line_no:(import_line_no line_no)
            (import_expr k)
            (import_expr m)
      | "add_seconds", [t; s; line_no] ->
          add_seconds
            ~line_no:(import_line_no line_no)
            (import_expr t)
            (import_expr s)
      | "iter", [Atom name; line_no] ->
          variable ~line_no:(import_line_no line_no) name
      | "rev", [e; line_no] ->
          listRev ~line_no:(import_line_no line_no) (import_expr e)
      | "items", [e; line_no] ->
          listItems ~line_no:(import_line_no line_no) (import_expr e) false
      | "keys", [e; line_no] ->
          listKeys ~line_no:(import_line_no line_no) (import_expr e) false
      | "values", [e; line_no] ->
          listValues ~line_no:(import_line_no line_no) (import_expr e) false
      | "elements", [e; line_no] ->
          listElements ~line_no:(import_line_no line_no) (import_expr e) false
      | "rev_items", [e; line_no] ->
          listItems ~line_no:(import_line_no line_no) (import_expr e) true
      | "rev_keys", [e; line_no] ->
          listKeys ~line_no:(import_line_no line_no) (import_expr e) true
      | "rev_values", [e; line_no] ->
          listValues ~line_no:(import_line_no line_no) (import_expr e) true
      | "rev_elements", [e; line_no] ->
          listElements ~line_no:(import_line_no line_no) (import_expr e) true
      | "contains", [items; x; line_no] ->
          contains
            ~line_no:(import_line_no line_no)
            (import_expr x)
            (import_expr items)
      | "check_signature", [pk; s; msg; line_no] ->
          check_signature
            ~line_no:(import_line_no line_no)
            (import_expr pk)
            (import_expr s)
            (import_expr msg)
      | "resolve", [e; line_no] ->
          resolve ~line_no:(import_line_no line_no) (import_expr e)
      | "constant", [Atom e; t; line_no] ->
          let line_no = import_line_no line_no in
          let tExpr = import_type t in
          constant ~line_no e tExpr
      | "constant_scenario_var", [Atom e; line_no] ->
          let line_no = import_line_no line_no in
          constant_scenario_var ~line_no e
      | "scenario_var", [Atom id; line_no] ->
          scenario_var ~line_no:(import_line_no line_no) id
      | "make_signature", [sk; msg; Atom fmt; line_no] ->
          let line_no = import_line_no line_no in
          let secret_key = import_expr sk in
          let message = import_expr msg in
          let message_format =
            match String.lowercase_ascii fmt with
            | "raw" -> `Raw
            | "hex" -> `Hex
            | other ->
                Format.kasprintf
                  failwith
                  "make_signature: Wrong message format : %S (l. %s)"
                  other
                  (string_of_line_no line_no)
          in
          make_signature ~secret_key ~message ~message_format ~line_no
      | "account_of_seed", [Atom seed; line_no] ->
          account_of_seed ~seed ~line_no:(import_line_no line_no)
      | "split_tokens", [mutez; quantity; total; line_no] ->
          split_tokens
            ~line_no:(import_line_no line_no)
            (import_expr mutez)
            (import_expr quantity)
            (import_expr total)
      | "slice", [ofs; len; buf; line_no] ->
          let offset = import_expr ofs in
          let length = import_expr len in
          let buffer = import_expr buf in
          slice ~offset ~length ~buffer ~line_no:(import_line_no line_no)
      | "concat", [l; line_no] ->
          concat_list (import_expr l) ~line_no:(import_line_no line_no)
      | "size", [s; line_no] ->
          size (import_expr s) ~line_no:(import_line_no line_no)
      | "type_annotation", [e; t; line_no] ->
          let e = import_expr e in
          let t = import_type t in
          let line_no = import_line_no line_no in
          type_annotation ~line_no e ~t
      | "has_entry_point", [Atom ep_name; line_no] ->
          let line_no = import_line_no line_no in
          has_entry_point ~line_no ep_name
      | "map", line_no :: entries ->
          build_map
            ~line_no:(import_line_no line_no)
            ~big:false
            ~entries:(List.map import_map_entry entries)
      | "set", line_no :: entries ->
          build_set
            ~line_no:(import_line_no line_no)
            ~entries:(List.map import_expr entries)
      | "big_map", line_no :: entries ->
          build_map
            ~line_no:(import_line_no line_no)
            ~big:true
            ~entries:(List.map import_map_entry entries)
      | "record", line_no :: l ->
          let import_exprField = function
            | Base.Sexp.List [Atom name; e] -> (name, import_expr e)
            | l ->
                failwith
                  ("Expression field format error " ^ Base.Sexp.to_string l)
          in
          let cmp (fld1, _) (fld2, _) = compare fld1 fld2 in
          record
            ~line_no:(import_line_no line_no)
            (List.sort cmp (List.map import_exprField l))
      | "ematch", line_no :: scrutinee :: l ->
          let import_clause = function
            | Base.Sexp.List [Atom name; v] ->
                let e = import_expr v in
                (name, e)
            | l -> failwith ("Clause format error: " ^ Base.Sexp.to_string l)
          in
          let scrutinee = import_expr scrutinee in
          ematch
            ~line_no:(import_line_no line_no)
            scrutinee
            (List.map import_clause l)
      | "eif", [cond; a; b; line_no] ->
          let cond = import_expr cond in
          let a = import_expr a in
          let b = import_expr b in
          eif ~line_no:(import_line_no line_no) cond a b
      | "call_michelson", instr :: line_no :: args ->
          inline_michelson
            ~line_no:(import_line_no line_no)
            (import_expr_inline_michelson instr)
            (List.map import_expr args)
      | ("private" | "global"), [Atom name; line_no] ->
          private_ ~line_no:(import_line_no line_no) name
      | "map_function", [l; f; line_no] ->
          map_function
            ~line_no:(import_line_no line_no)
            (import_expr l)
            (allow_lambda_full_stack (import_expr f))
      | "lambda", [Atom id; Atom name; line_no; List commands] ->
          let name = if name = "" then "_x" ^ id else name in
          import_lambda
            name
            line_no
            commands
            ~clean_stack:true
            ~with_storage:None
            ~with_operations:false
      | ( "lambda"
        , [ Atom id
          ; Atom with_storage
          ; Atom with_operations
          ; Atom name
          ; line_no
          ; List commands ] ) ->
          let name = if name = "" then "_x" ^ id else name in
          let with_storage =
            match with_storage with
            | "read-only" -> Some Type.Read_only
            | "read-write" | "True" -> Some Read_write
            | "None" | "False" -> None
            | _ -> assert false
          in
          import_lambda
            name
            line_no
            commands
            ~clean_stack:true
            ~with_storage
            ~with_operations:(import_bool with_operations)
      | "lambdaParams", [Atom id; Atom name; line_no; _tParams] ->
          let line_no = import_line_no line_no in
          let name = if name = "" then "_x" ^ id else name in
          lambdaParams ~line_no name
      | "call_lambda", [lambda; parameter; line_no] ->
          call_lambda
            ~line_no:(import_line_no line_no)
            (import_expr parameter)
            (import_expr lambda)
      | "apply_lambda", [lambda; parameter; line_no] ->
          apply_lambda
            ~line_no:(import_line_no line_no)
            (import_expr parameter)
            (import_expr lambda)
      | "lsl", [expression; shift; line_no] ->
          lsl_
            ~line_no:(import_line_no line_no)
            (import_expr expression)
            (import_expr shift)
      | "lsr", [expression; shift; line_no] ->
          lsr_
            ~line_no:(import_line_no line_no)
            (import_expr expression)
            (import_expr shift)
      | "xor", [e1; e2; line_no] ->
          Expr.bin_op
            ~op:BXor
            ~line_no:(import_line_no line_no)
            (import_expr e1)
            (import_expr e2)
      | "set_delegate", [x; line_no] ->
          set_delegate ~line_no:(import_line_no line_no) (import_expr x)
      | "sapling_verify_update", [state; transaction; line_no] ->
          sapling_verify_update
            ~line_no:(import_line_no line_no)
            (import_expr state)
            (import_expr transaction)
      | "transfer", [e1; e2; e3; line_no] ->
          transfer
            ~line_no:(import_line_no line_no)
            ~arg:(import_expr e1)
            ~amount:(import_expr e2)
            ~destination:(import_expr e3)
      | ( "create_contract"
        , [ List [Atom "contract"; contract]
          ; List [Atom "storage"; storage]
          ; List [Atom "baker"; baker]
          ; List [Atom "amount"; amount]
          ; line_no ] ) ->
          let c = import_contract {env with entry_point = None} contract in
          let storage = import_expr storage in
          let balance = import_expr amount in
          let baker =
            match baker with
            | Atom "None" -> none ~line_no:[]
            | e -> import_expr e
          in
          create_contract
            ~line_no:(import_line_no line_no)
            ~baker
            ~balance
            ~storage
            c
      | s, l ->
        ( try cst ~line_no:[] (import_literal (Atom s :: l)) with
        | _ ->
            failwith
              (Printf.sprintf
                 "Expression format error (a %i) %s"
                 (List.length l)
                 (Base.Sexp.to_string_hum input)) ) )
    | x -> failwith ("Expression format error (b) " ^ Base.Sexp.to_string_hum x)
  and import_map_entry = function
    | Base.Sexp.List [k; v] -> (import_expr k, import_expr v)
    | e ->
        failwith
          (Printf.sprintf "import_map_entry: '%s'" (Base.Sexp.to_string e))
  and import_lambda
      name line_no body ~clean_stack ~with_storage ~with_operations =
    let line_no = import_line_no line_no in
    lambda
      ~line_no
      name
      (Command.seq ~line_no (import_commands env body))
      ~clean_stack
      ~with_storage
      ~with_operations
  in
  import_expr

and import_meta_expr env =
  let import_expr = import_expr env in
  let rec import_meta_expr = function
    | Base.Sexp.List (Atom f :: args) as input ->
      ( match (f, args) with
      | "meta_list", _loc :: l -> Meta.List (List.map import_meta_expr l)
      | "meta_map", _loc :: l ->
          let import_elem = function
            | Base.Sexp.List [Atom "elem"; k; v] ->
                (import_expr k, import_meta_expr v)
            | _ -> assert false
          in
          Meta.Map (List.map import_elem l)
      | "meta_expr", [e] -> Meta.Other (import_expr e)
      | "meta_view", [Atom name; _line_no] -> Meta.View name
      | _ ->
          failwith
            ("Meta expression format error " ^ Base.Sexp.to_string_hum input) )
    | input ->
        failwith
          ("Meta expression format error " ^ Base.Sexp.to_string_hum input)
  in
  import_meta_expr

and import_command env =
  let import_type t = import_type t in
  let import_expr e = import_expr env e in
  let import_commands cs = import_commands env cs in
  let open Command in
  function
  | Base.Sexp.List (List _ :: _ as cs) -> seq ~line_no:[] (import_commands cs)
  | Base.Sexp.List (Atom f :: args) as input ->
    ( match (f, args) with
    | "result", [x; line_no] ->
        result ~line_no:(import_line_no line_no) (import_expr x)
    | "failwith", [x; line_no] ->
        sp_failwith ~line_no:(import_line_no line_no) (import_expr x)
    | "never", [x; line_no] ->
        never ~line_no:(import_line_no line_no) (import_expr x)
    | "verify", [x; line_no] ->
        verify ~line_no:(import_line_no line_no) (import_expr x) None
    | "verify", [x; message; line_no] ->
        verify
          ~line_no:(import_line_no line_no)
          (import_expr x)
          (Some (import_expr message))
    | "defineLocal", [Atom name; expr; Atom mutable_; line_no] ->
        define_local
          ~line_no:(import_line_no line_no)
          name
          (import_expr expr)
          (import_bool mutable_)
    | "defineLocal", [Atom name; expr; line_no] ->
        define_local
          ~line_no:(import_line_no line_no)
          name
          (import_expr expr)
          true
    | "whileBlock", [e; List l; line_no] ->
        let line_no = import_line_no line_no in
        while_loop ~line_no (import_expr e) (seq ~line_no (import_commands l))
    | "forGroup", [Atom name; e; List l; line_no] ->
        let e = import_expr e in
        let line_no = import_line_no line_no in
        for_loop ~line_no name e (seq ~line_no (import_commands l))
    | "match", [scrutinee; Atom constructor; Atom arg_name; List body; line_no]
      ->
        let line_no = import_line_no line_no in
        mk_match
          ~line_no
          (import_expr scrutinee)
          [(constructor, arg_name, seq ~line_no (import_commands body))]
    | "match_cases", [scrutinee; _arg; List cases; line_no] ->
        let line_no = import_line_no line_no in
        let parse_case = function
          | Base.Sexp.List
              [ Atom "match"
              ; _
              ; Atom constructor
              ; Atom arg_name
              ; List body
              ; line_no ] ->
              let line_no = import_line_no line_no in
              (constructor, arg_name, seq ~line_no (import_commands body))
          | input -> failwith ("Bad case parsing: " ^ Base.Sexp.to_string input)
        in
        let cases = List.map parse_case cases in
        mk_match ~line_no (import_expr scrutinee) cases
    | "set_type", [e; t; line_no] ->
        set_type
          ~line_no:(import_line_no line_no)
          (import_expr e)
          (import_type env t)
    | "set_entry_point", [Atom ep_name; new_ep; line_no] ->
        let line_no = import_line_no line_no in
        set_entry_point ~line_no ep_name (import_expr new_ep)
    | "set_result_type", [List cs; t; line_no] ->
        let line_no = import_line_no line_no in
        set_result_type
          ~line_no
          (seq ~line_no (import_commands cs))
          (import_type env t)
    | "set", [v; e; line_no] ->
        set ~line_no:(import_line_no line_no) (import_expr v) (import_expr e)
    | "delItem", [e; k; line_no] ->
        delItem
          ~line_no:(import_line_no line_no)
          (import_expr e)
          (import_expr k)
    | "updateSet", [e; k; Atom b; line_no] ->
        updateSet
          ~line_no:(import_line_no line_no)
          (import_expr e)
          (import_expr k)
          (import_bool b)
    | "trace", [e; line_no] ->
        trace ~line_no:(import_line_no line_no) (import_expr e)
    | "bind", _ ->
        raise
          (SmartExcept
             [ `Text
                 "Command format error. sp.bind_block() can be used as follows:"
             ; `Br
             ; `Text "x = sp.bind_block('x'):"
             ; `Br
             ; `Text "with x:"
             ; `Br
             ; `Text "    ..."
             ; `Br
             ; `Text "... x ..."
             ; `Br
             ; `Text (Base.Sexp.to_string input) ])
    | _ -> failwith ("Command format error (a) " ^ Base.Sexp.to_string input) )
  | input -> failwith ("Command format error (b) " ^ Base.Sexp.to_string input)

and import_match_cons env expr ok_match ko_match line_no =
  let line_no = import_line_no line_no in
  Command.mk_match_cons
    ~line_no
    (import_expr env expr)
    (Printf.sprintf "match_cons_%s" (string_of_line_no line_no))
    (Command.seq ~line_no (import_commands env ok_match))
    (Command.seq ~line_no (import_commands env ko_match))

and import_commands env =
  let import_expr = import_expr env in
  let import_command = import_command env in
  let open Command in
  let rec import_commands r = function
    | Base.Sexp.List [Atom "bind"; Atom name; c1] :: c2 ->
        List.rev_append
          r
          [ bind
              ~line_no:[]
              (Some name)
              (import_command c1)
              (seq ~line_no:[] (import_commands [] c2)) ]
    | Base.Sexp.List [Atom "seq"; Atom seq_name; List seq_block; seq_line_no]
      :: List [Atom "bind"; Atom bind_name; List bind_block; bind_line_no]
         :: rest
      when seq_name = bind_name ->
        let c =
          bind
            ~line_no:(import_line_no seq_line_no)
            (Some seq_name)
            (seq
               ~line_no:(import_line_no seq_line_no)
               (import_commands [] seq_block))
            (seq
               ~line_no:(import_line_no bind_line_no)
               (import_commands [] bind_block))
        in
        import_commands (c :: r) rest
    | Base.Sexp.List [Atom "ifBlock"; e; List tBlock; line_no]
      :: List [Atom "elseBlock"; List eBlock] :: rest ->
        let line_no = import_line_no line_no in
        let c =
          ifte
            ~line_no
            (import_expr e)
            (seq ~line_no (import_commands [] tBlock))
            (seq ~line_no (import_commands [] eBlock))
        in
        import_commands (c :: r) rest
    | List [Atom "ifBlock"; e; List tBlock; line_no] :: rest ->
        let c =
          let line_no = import_line_no line_no in
          ifte
            ~line_no
            (import_expr e)
            (seq ~line_no (import_commands [] tBlock))
            (seq ~line_no [])
        in
        import_commands (c :: r) rest
    | List [Atom "ifSomeBlock"; e; Atom _; List tBlock; line_no]
      :: List [Atom "elseBlock"; List eBlock] :: rest ->
        let line_no = import_line_no line_no in
        let c =
          ifteSome
            ~line_no
            (import_expr e)
            (seq ~line_no (import_commands [] tBlock))
            (seq ~line_no (import_commands [] eBlock))
        in
        import_commands (c :: r) rest
    | List [Atom "ifSomeBlock"; e; Atom _; List tBlock; line_no] :: rest ->
        let line_no = import_line_no line_no in
        let c =
          ifteSome
            ~line_no
            (import_expr e)
            (seq ~line_no (import_commands [] tBlock))
            (seq ~line_no [])
        in
        import_commands (c :: r) rest
    | List (Atom "match_tuple" :: seq_line_no :: x :: ns) :: rest ->
        let line_no = import_line_no seq_line_no in
        let rest = seq ~line_no (import_commands [] rest) in
        let p = Pattern_tuple (List.map unAtom ns) in
        List.rev_append r [mk_match_product ~line_no (import_expr x) p rest]
    | List (Atom "match_record" :: seq_line_no :: x :: bs) :: rest ->
        let line_no = import_line_no seq_line_no in
        let rest = seq ~line_no (import_commands [] rest) in
        let p = Pattern_record ("", List.map import_binding bs) in
        List.rev_append r [mk_match_product ~line_no (import_expr x) p rest]
    | List [Atom "modify"; seq_line_no; x; List body; Atom v] :: rest ->
        let line_no = import_line_no seq_line_no in
        let body = seq ~line_no (import_commands [] body) in
        let p = Pattern_single v in
        let c = mk_modify_product ~line_no (import_expr x) p body in
        import_commands (c :: r) rest
    | List (Atom "modify_tuple" :: seq_line_no :: x :: List body :: ns) :: rest
      ->
        let line_no = import_line_no seq_line_no in
        let body = seq ~line_no (import_commands [] body) in
        let p = Pattern_tuple (List.map unAtom ns) in
        let c = mk_modify_product ~line_no (import_expr x) p body in
        import_commands (c :: r) rest
    | List
        (Atom "modify_record"
        :: seq_line_no :: x :: Atom var :: List body :: _bs)
      :: rest ->
        let line_no = import_line_no seq_line_no in
        let body = seq ~line_no (import_commands [] body) in
        let bs = Pattern_record (var, []) in
        let c = mk_modify_product ~line_no (import_expr x) bs body in
        import_commands (c :: r) rest
    | List [Atom "match_cons"; expr; List ok_match; line_no_]
      :: List [Atom "elseBlock"; List ko_match] :: rest ->
        let m = import_match_cons env expr ok_match ko_match line_no_ in
        import_commands (m :: r) rest
    | List [Atom "match_cons"; expr; List ok_match; line_no_] :: rest ->
        let m = import_match_cons env expr ok_match [] line_no_ in
        import_commands (m :: r) rest
    | x :: l ->
        let x = import_command x in
        (* This sequencing is important for type inferrence. *)
        import_commands (x :: r) l
    | [] -> List.rev r
  in
  import_commands []

and build_entry_point env = function
  | Base.Sexp.List
      [ Atom name
      ; Atom originate
      ; Atom lazify
      ; Atom lazy_no_code
      ; Atom has_param
      ; line_no
      ; List command ] ->
      let originate = import_bool originate in
      let lazify = import_opt_bool lazify in
      let lazy_no_code = import_opt_bool lazy_no_code in
      let has_param = import_bool has_param in
      let line_no = import_line_no line_no in
      let body =
        default_line_no_command
          (Command.seq ~line_no:[] (import_commands env command))
      in
      { channel = name
      ; tparameter_ep = (if has_param then `Present else `Absent)
      ; originate
      ; lazify
      ; lazy_no_code
      ; line_no
      ; body
      ; tparameter_ep_derived = U }
  | x -> failwith ("Message format error " ^ Base.Sexp.to_string x)

and import_contract env = function
  | Base.Sexp.Atom _ -> failwith "Parse error contract"
  | List l ->
      let template_id =
        match import_contract_id (List (assocList "template_id" l)) with
        | C_static cid -> cid
        | C_dynamic _ -> assert false
      in
      let storage =
        match assocList "storage" l with
        | [] -> None
        | storage -> Some (import_expr env (Base.Sexp.List storage))
      in
      let tstorage_explicit =
        match assocList "storage_type" l with
        | [List []] -> None
        | [t] -> Some (import_type env t)
        | _ -> assert false
      in
      let entry_points_layout = assocList "entry_points_layout" l in
      let messages = assocList "messages" l in
      let flags = assocList "flags" l in
      let private_variables = assocLists ["privates"; "globals"] l in
      let metadata = assocList "initial_metadata" l in
      let views = assocList "views" l in
      let entry_points = List.map (build_entry_point env) messages in
      let private_variables =
        List.map
          (function
            | Base.Sexp.List [Atom name; variable] -> (name, variable)
            | x ->
                failwith
                  ("Private variable format error " ^ Base.Sexp.to_string x))
          private_variables
      in
      let metadata =
        List.map
          (function
            | Base.Sexp.List [Atom name; variable] -> (name, variable)
            | x -> failwith ("Metadata format error " ^ Base.Sexp.to_string x))
          metadata
      in
      let views =
        List.map
          (function
            | Base.Sexp.List
                [ Atom kind
                ; Atom name
                ; Atom has_param
                ; line_no
                ; Atom pure
                ; Atom doc
                ; List commands ] ->
                let kind =
                  match kind with
                  | "offchain" -> Offchain
                  | "onchain" -> Onchain
                  | _ -> assert false
                in
                ( kind
                , name
                , import_bool has_param
                , import_line_no line_no
                , import_bool pure
                , commands
                , doc )
            | x -> failwith ("View format error " ^ Base.Sexp.to_string x))
          views
      in
      let balance =
        match Base.Sexp.List (assocList "balance" l) with
        | List [] -> None
        | l -> Some (import_expr env l)
      in
      let build_private_variable (name, variable) =
        let expression = import_expr env variable in
        (name, expression)
      in
      let private_variables =
        List.map build_private_variable private_variables
      in
      let build_metadata (name, variable) =
        let expression = import_meta_expr env variable in
        (name, expression)
      in
      let metadata = List.map build_metadata metadata in
      let build_view (kind, name, has_param, line_no, pure, commands, doc) =
        let body = Command.seq ~line_no (import_commands env commands) in
        {kind; name; has_param; pure; body; doc; tparameter_derived = U}
      in
      let views = List.map build_view views in
      let entry_points_layout =
        match entry_points_layout with
        | [] -> None
        | _ ->
            let fields, layout =
              import_inner_layout (Base.Sexp.List entry_points_layout)
            in
            let l1 = List.sort compare fields in
            let l2 =
              List.sort
                compare
                (List.map (fun (ep : _ entry_point) -> ep.channel) entry_points)
            in
            if l1 <> l2
            then
              raise
                (SmartExcept
                   [ `Text "Bad entry point layout for contract"
                   ; `Text (string_of_int template_id.static_id)
                   ; `Br
                   ; `Text (String.concat ", " l1)
                   ; `Br
                   ; `Text "!="
                   ; `Br
                   ; `Text (String.concat ", " l2) ]);
            Some layout
      in
      let flags = import_flags flags in
      { contract =
          { template_id = Some template_id
          ; balance
          ; storage
          ; baker = None
          ; tstorage_explicit
          ; entry_points
          ; entry_points_layout
          ; unknown_parts = None
          ; flags
          ; private_variables
          ; metadata
          ; views
          ; derived = U } }
