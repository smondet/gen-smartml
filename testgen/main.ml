open! Base
open! Stdio

let line_no = []
let config = SmartML.Config.default
let type_check = Tools.Checker.check_contract config

let compile tc =
  let instance =
    SmartML.Basics.
      { template = tc
      ; state =
          { balance = Utils_pure.Bigint.of_int 100
          ; storage = None
          ; baker = None
          ; lazy_entry_points = []
          ; metadata = []
          }
      }
  in
  Tools.Compiler.compile_instance ~scenario_vars:Utils.String.Map.empty instance
;;

let try_contract c =
  let pptmp n pp v =
    let tmp = Caml.Filename.concat "_build" (Fmt.str "testgen-%s" n) in
    let data = Fmt.str "%a" pp v in
    Out_channel.write_all tmp ~data;
    Fmt.epr "Output %S -> %s\n%!" n tmp
  in
  let checked = type_check c in
  pptmp "Type-checked" SmartML.Basics.pp_tcontract checked;
  let compiled = compile checked in
  pptmp "Compiled" Tools.Michelson.pp_tcontract compiled;
  let concrete = Tools.Michelson.display_tcontract compiled in
  pptmp "Concrete" Fmt.string concrete;
  ()
;;

let c0 =
  let open SmartML in
  let open Basics in
  let entry_points_layout =
    let open Utils_pure.Binary_tree in
    node
      (Layout.leaf "default" "default")
      (Layout.leaf "easy-delegation" "easy-delegation")
  in
  build_contract
    ~entry_points_layout
    [ build_entry_point
        ~name:"default"
        ~tparameter:Type.unit
        Command.(
          seq
            ~line_no
            [ comment ~line_no "This is the default entry point, it does nothing" ])
    ; build_entry_point
        ~name:"easy-delegation"
        ~tparameter:Type.address
        Command.(
          seq
            ~line_no
            [ comment ~line_no "This just allows anyone to delegate"
            ; Library.set_delegate ~line_no (Expr.none ~line_no)
            ])
    ]
;;

let () =
  try_contract c0;
  Fmt.epr "Done\n%!"
;;
