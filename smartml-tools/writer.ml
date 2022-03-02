(* Copyright 2019-2021 Smart Chain Arena LLC. *)

open SmartML
open Printf
open Basics
open Utils

let write fn ext s =
  let fn = fn ^ "." ^ ext in
  Io.write_file fn s;
  (fn, List.length (String.split_on_char '\n' s))

let ext_of_language = function
  | Config.SmartPy -> "py"
  | SmartML -> "ml"
  | SmartTS -> "ts"

let write_pretty ~language ?(suffix = "") fn c =
  let module Printer = (val Printer.get_by_language language : Printer.Printer)
  in
  write fn (ext_of_language language) (Printer.tcontract_to_string c ^ suffix)

let wrap_html_document ~install html =
  let html =
    Base.String.substr_replace_all
      html
      ~pattern:"src='static/img"
      ~with_:"src='https://SmartPy.io/static/img"
  in
  sprintf
    "<html><head><style>%s%sbutton{font-size:inherit;}</style><script \
     src='https://ajax.googleapis.com/ajax/libs/jquery/3.5.1/jquery.min.js'></script><script>%s%s</script></head><body><div \
     id='outputPanel'>%s</div></div></html>"
    (Io.read_file (Printf.sprintf "%s/smart.css" install))
    (Io.read_file (Printf.sprintf "%s/typography.css" install))
    (Io.read_file (Printf.sprintf "%s/smart.js" install))
    (Io.read_file (Printf.sprintf "%s/theme.js" install))
    html

let write_pretty_html ~language ~install fn c =
  let module Printer = (val Printer.get_by_language language : Printer.Printer)
  in
  let html = Printer.tcontract_to_string ~options:Printer.Options.html c in
  write
    fn
    (ext_of_language language ^ ".html")
    (wrap_html_document ~install html)

let write_contract_michelson fn c =
  write fn "tz" (Michelson.display_tcontract c)

let write_micheline fn c =
  let c = Format.asprintf "%a" (Micheline.pp_as_json ()) c in
  write fn "json" c

let write_contract_michel fn c =
  let pp = Michel.Expr.(print_precontract print_expr) in
  let m ppf = Format.pp_set_margin ppf 160 in
  write fn "michel" (Format.asprintf "%t%a" m pp c)

let write_tvalue fn s =
  let module Printer = ( val Printer.get_by_language Config.SmartPy
                           : Printer.Printer )
  in
  write fn "py" (Printer.value_to_string s)

let write_mliteral fn s = write fn "tz" (Michelson.string_of_literal s)

let write_html fn s = write fn "html" s

let write_csv fn lines =
  write fn "csv" (String.concat "\n" (List.map (String.concat ",") lines))

let pp_contract_types
    {state = _; template = {tcontract = {private_variables; views; derived}}} =
  let module Printer = ( val Printer.get_by_language Config.SmartPy
                           : Printer.Printer )
  in
  let open Printer in
  let tparameter = (get_extra derived).tparameter in
  let tstorage = (get_extra derived).tstorage in
  let buf = Buffer.create 64 in
  let pp_private i (n, g) =
    let pref = if i = 0 then " " else ", " in
    bprintf buf "%s%S: %s" pref n (type_to_string g.Typed.et)
  in
  let pp_view i {name; tparameter_derived; body} =
    let pref = if i = 0 then " " else ", " in
    let t1 = Option.cata "()" type_to_string (get_extra tparameter_derived) in
    let t2 = type_to_string body.Typed.ct in
    bprintf buf "%s%S: (%s, %s)" pref name t1 t2
  in
  bprintf buf "import smartpy as sp\n\n";
  bprintf buf "tstorage = %s\n" (type_to_string tstorage);
  bprintf buf "tparameter = %s\n" (type_to_string tparameter);
  bprintf buf "tprivates = {";
  List.iteri pp_private private_variables;
  bprintf buf " }\n";
  bprintf buf "tviews = {";
  List.iteri pp_view views;
  bprintf buf " }\n";
  Buffer.contents buf

let write_contract_types fn c = write fn "py" (pp_contract_types c)

let write_metadata fn m =
  write fn "json" (Format.asprintf "%a" (Misc.pp_json_as_json ()) m)
