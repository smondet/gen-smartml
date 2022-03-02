(* Copyright 2019-2021 Smart Chain Arena LLC. *)

open Utils_pure

type language =
  | SmartPy
  | SmartML
  | SmartTS
[@@deriving eq, ord, show {with_path = false}]

type protocol =
  | Delphi
  | Edo
  | Florence
  | Granada
  | Hangzhou
  | Ithaca
[@@deriving eq, ord, show {with_path = false}]

type exceptions =
  | FullDebug
  | Message
  | VerifyOrLine
  | DefaultLine
  | Line
  | DefaultUnit
  | Unit
[@@deriving eq, ord, show {with_path = false}]

type default_layout =
  | Tree
  | Comb
[@@deriving eq, ord, show {with_path = false}]

type t =
  { simplify : bool
  ; simplify_via_michel : bool
  ; decompile : bool
  ; erase_comments : bool
  ; disable_dup_check : bool
  ; contract_check_exception : bool
  ; view_check_exception : bool
  ; protocol : protocol
  ; lazy_entry_points : bool
  ; exceptions : exceptions
  ; default_variant_layout : default_layout
  ; default_record_layout : default_layout
  ; initial_cast : bool
  ; erase_var_annots : bool
  ; pairn : bool
  ; dump_michel : bool
  ; single_entry_point_annotation : bool
  ; warn_unused : bool
  ; languages : language list }
[@@deriving eq, ord, show {with_path = false}]

type bool_flag =
  | Contract_check_exception
  | Decompile
  | Disable_dup_check
  | Dump_michel
  | Erase_comments
  | Erase_var_annots
  | Initial_cast
  | Pairn
  | Simplify
  | Simplify_via_michel
  | Single_entry_point_annotation
  | Warn_unused
  | Lazy_entry_points
  | View_check_exception
[@@deriving eq, ord, show]

type flag =
  | Bool_Flag              of bool_flag * bool
  | Default_record_layout  of default_layout
  | Default_variant_layout of default_layout
  | Exceptions             of exceptions
  | Protocol               of protocol
  | Languages              of language list
[@@deriving eq, ord, show]

let parse_bool_flag = function
  | "simplify" -> Some Simplify
  | "simplify-via-michel" -> Some Simplify_via_michel
  | "decompile" -> Some Decompile
  | "erase-comments" -> Some Erase_comments
  | "disable-dup-check" -> Some Disable_dup_check
  | "contract-check-exception" -> Some Contract_check_exception
  | "initial-cast" -> Some Initial_cast
  | "erase-var-annots" -> Some Erase_var_annots
  | "pairn" -> Some Pairn
  | "dump-michel" -> Some Dump_michel
  | "single-entry-point-annotation" -> Some Single_entry_point_annotation
  | "warn-unused" -> Some Warn_unused
  | "lazy-entry-points" -> Some Lazy_entry_points
  | "view-check-exception" -> Some View_check_exception
  | _ -> None

let string_of_bool_flag = function
  | Simplify -> "simplify"
  | Simplify_via_michel -> "simplify-via-michel"
  | Contract_check_exception -> "contract-check-exception"
  | Decompile -> "decompile"
  | Erase_comments -> "erase-comments"
  | Disable_dup_check -> "disable-dup-check"
  | Initial_cast -> "initial-cast"
  | Erase_var_annots -> "erase-var-annots"
  | Pairn -> "pairn"
  | Dump_michel -> "dump-michel"
  | Single_entry_point_annotation -> "single-entry-point-annotation"
  | Warn_unused -> "warn-unused"
  | Lazy_entry_points -> "lazy-entry-points"
  | View_check_exception -> "view-check-exception"

let parse_bool_flag flag =
  let b, flag =
    if String.sub flag 0 3 = "no-"
    then
      let flag = String.sub flag 3 (String.length flag - 3) in
      (false, parse_bool_flag flag)
    else (true, parse_bool_flag flag)
  in
  Option.map (fun bf -> Bool_Flag (bf, b)) flag

let protocol_of_string protocol =
  match String.lowercase_ascii protocol with
  | "delphi" -> Delphi
  | "edo" -> Edo
  | "florence" -> Florence
  | "granada" -> Granada
  | "hangzhou" -> Hangzhou
  | "ithaca" -> Ithaca
  (* | "baking_accounts" -> BakingAccounts *)
  | _ -> Printf.ksprintf failwith "Unknown protocol: %S" protocol

let string_of_protocol = function
  | Delphi -> "delphi"
  | Edo -> "edo"
  | Florence -> "florence"
  | Granada -> "granada"
  | Hangzhou -> "hangzhou"
  | Ithaca -> "ithaca"

let layout_of_string layout =
  match String.lowercase_ascii layout with
  | "tree" -> Tree
  | "comb" -> Comb
  | _ -> Printf.ksprintf failwith "Unknown layout: %S" layout

let string_of_layout = function
  | Tree -> "tree"
  | Comb -> "comb"

let exceptions_of_string exceptions =
  match String.lowercase_ascii exceptions with
  | "full-debug" | "fulldebug" -> FullDebug
  | "debug-message" | "debugmessage" -> Message
  | "verify-or-line" | "verifyorline" -> VerifyOrLine
  | "default-line" | "defaultline" -> DefaultLine
  | "line" -> Line
  | "default-unit" | "defaultunit" -> DefaultUnit
  | "unit" -> Unit
  | _ -> Printf.ksprintf failwith "Unknown exception flag: %S" exceptions

let string_of_exceptions = function
  | FullDebug -> "full-debug"
  | Message -> "debug-message"
  | VerifyOrLine -> "verify-or-line"
  | DefaultLine -> "default-line"
  | Line -> "line"
  | DefaultUnit -> "default-unit"
  | Unit -> "unit"

let languages_of_string languages =
  let read = function
    | "smartpy" -> SmartPy
    | "smartml" -> SmartML
    | "smartts" -> SmartTS
    | language -> Printf.ksprintf failwith "Unknown language flag: %S" language
  in
  let languages =
    languages
    |> String.lowercase_ascii
    |> String.split_on_char ' '
    |> String.concat ""
    |> String.split_on_char ','
    |> List.filter (fun x -> x <> "")
    |> List.map read
  in
  match languages with
  | [] -> failwith "Empty 'languages' flag is forbidden"
  | languages -> languages

let string_of_language = function
  | SmartPy -> "smartpy"
  | SmartML -> "smartml"
  | SmartTS -> "smartts"

let string_of_languages xs = List.map string_of_language xs |> String.concat ","

let parse_flag = function
  | ["protocol"; protocol] -> Some (Protocol (protocol_of_string protocol))
  | ["exceptions"; exceptions] ->
      Some (Exceptions (exceptions_of_string exceptions))
  | ["default_variant_layout"; layout] ->
      Some (Default_variant_layout (layout_of_string layout))
  | ["default_record_layout"; layout] ->
      Some (Default_record_layout (layout_of_string layout))
  | ["languages"; languages] -> Some (Languages (languages_of_string languages))
  | [bf] -> parse_bool_flag bf
  | _ -> None

let string_of_flag = function
  | Protocol protocol -> ["protocol"; string_of_protocol protocol]
  | Exceptions exceptions -> ["exceptions"; string_of_exceptions exceptions]
  | Default_variant_layout layout ->
      ["default_variant_layout"; string_of_layout layout]
  | Default_record_layout layout ->
      ["default_record_layout"; string_of_layout layout]
  | Languages languages -> ["languages"; string_of_languages languages]
  | Bool_Flag (f, x) -> [(if x then "" else "no-") ^ string_of_bool_flag f]

let default =
  { contract_check_exception = true
  ; view_check_exception = true
  ; decompile = false
  ; disable_dup_check = false
  ; dump_michel = false
  ; erase_comments = false
  ; erase_var_annots = false
  ; initial_cast = false
  ; pairn = true
  ; simplify_via_michel = false
  ; simplify = true
  ; single_entry_point_annotation = true
  ; lazy_entry_points = false
  ; exceptions = VerifyOrLine
  ; protocol = Hangzhou
  ; default_variant_layout = Tree
  ; default_record_layout = Tree
  ; warn_unused = true
  ; languages = [SmartPy] }

let is_initial_flag = function
  | Bool_Flag
      ( ( Decompile | Disable_dup_check | Dump_michel | Erase_comments
        | Erase_var_annots | Initial_cast | Pairn | Simplify
        | Simplify_via_michel | Single_entry_point_annotation | Warn_unused
        | Lazy_entry_points )
      , _ )
   |Exceptions _ | Languages _ ->
      false
  | Protocol _ | Default_variant_layout _ | Default_record_layout _
   |Bool_Flag ((Contract_check_exception | View_check_exception), _) ->
      true

let apply_flag config = function
  | Bool_Flag (bf, b) ->
    begin
      match (bf, b) with
      | Contract_check_exception, contract_check_exception ->
          {config with contract_check_exception}
      | View_check_exception, view_check_exception ->
          {config with view_check_exception}
      | Simplify, simplify -> {config with simplify}
      | Simplify_via_michel, simplify_via_michel ->
          {config with simplify_via_michel}
      | Decompile, decompile -> {config with decompile}
      | Erase_comments, erase_comments -> {config with erase_comments}
      | Disable_dup_check, disable_dup_check -> {config with disable_dup_check}
      | Initial_cast, initial_cast -> {config with initial_cast}
      | Erase_var_annots, erase_var_annots -> {config with erase_var_annots}
      | Pairn, pairn -> {config with pairn}
      | Dump_michel, dump_michel -> {config with dump_michel}
      | Single_entry_point_annotation, single_entry_point_annotation ->
          {config with single_entry_point_annotation}
      | Warn_unused, warn_unused -> {config with warn_unused}
      | Lazy_entry_points, lazy_entry_points -> {config with lazy_entry_points}
    end
  | Protocol protocol -> {config with protocol}
  | Exceptions exceptions -> {config with exceptions}
  | Default_variant_layout default_variant_layout ->
      {config with default_variant_layout}
  | Default_record_layout default_record_layout ->
      {config with default_record_layout}
  | Languages languages -> {config with languages}
