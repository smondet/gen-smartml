open! Base

let line_no = []

let c0 =
  let open SmartML in
  let open Basics in
  build_contract
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

let () = Fmt.epr "Done\n%!"
