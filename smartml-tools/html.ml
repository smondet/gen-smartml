(* Copyright 2019-2021 Smart Chain Arena LLC. *)

open SmartML
open Utils
open Basics

type t =
  | Raw of string
  | Div of string * t list

let rec pp_render ppf = function
  | Raw x -> Format.pp_print_string ppf x
  | Div (args, xs) ->
      Format.fprintf ppf "<div %s>" args;
      List.iter (pp_render ppf) xs;
      Format.pp_print_string ppf "</div>"

let render x = Format.asprintf "%a" pp_render x

let div ?(args = "") xs = Div (args, xs)

type tab_ =
  { name : string
  ; tablink : string
  ; content : t
  ; lazy_tab : t Lazy.t option }

type tab = int -> int -> tab_

let tab_functions = Hashtbl.create 100

let call_tab id global_id =
  match Hashtbl.find_opt tab_functions (id, global_id) with
  | Some f -> Lazy.force f
  | None -> Raw ""

let tab ?(active : unit option) ?lazy_tab name inner global_id (id : int) =
  ( match lazy_tab with
  | None -> Hashtbl.remove tab_functions (id, global_id)
  | Some lazy_tab -> Hashtbl.replace tab_functions (id, global_id) lazy_tab );
  let call_f =
    if 0 <= global_id
    then Printf.sprintf "openTabLazy(event, %i, %i);" id global_id
    else Printf.sprintf "openTab(event, %i);" id
  in
  let tablink =
    Printf.sprintf
      "<button name='button_generatedMichelson' class='tablinks%s' \
       onclick='%s'>%s</button>"
      (if active <> None then " active" else "")
      call_f
      name
  in
  let content =
    div
      ~args:
        (Printf.sprintf
           "class='tabcontent' style='display: %s;'"
           (if active = None then "none" else "block"))
      [inner]
  in
  {name; tablink; lazy_tab; content}

let tabs ?(global_id = -1) title tabs =
  let tabs = List.mapi (fun i x -> x global_id i) tabs in
  Raw
    (Printf.sprintf
       "<div class='tabs'><div class='tab'><span \
        class='title'>%s</span>%s</div>%s</div>"
       title
       (String.concat "" (List.map (fun x -> x.tablink) tabs))
       (String.concat "" (List.map (fun x -> render x.content) tabs)))

let copy_div ~id ?(className = "white-space-pre") name inner =
  let r =
    Format.asprintf
      "<div class='menu'> <button \
       onClick='copyMichelson(this);'>Copy</button></div><div id='%s%s' \
       class='michelson %s'>%a</div>"
      name
      id
      className
      pp_render
      inner
  in
  Raw (Base.String.substr_replace_all ~pattern:"\n" ~with_:"<br>" r)

let contract_sizes_html ~codeSize ~simplifiedCodeSize ~storageSize ~nb_bigmaps =
  let pp name size =
    let percentage = Printf.sprintf "%d" (size * 100 / 16384) in
    Printf.sprintf
      "<tr><td class='pad10'><span class='item'>%s: </span></td><td \
       class='pad10'><div class='progress' style='height: 20px; width: \
       150px;'><div class='progress-bar' style='width:%s;' role='progressbar' \
       style='aria-valuenow='10'></div></div></td><td class='pad10, \
       code_right'>%d Bytes</td></tr>"
      name
      percentage
      size
  in
  let bigmapSize = nb_bigmaps * 32 in
  let sep, simplifiedCode, simplifiedCombined =
    match (simplifiedCodeSize, storageSize) with
    | Some s1, None ->
        ("<tr><td><hr></td><td></td></tr>", pp "Simplified Code" s1, "")
    | Some s1, Some s2 ->
        ( "<tr><td><hr></td><td></td><td></td></tr>"
        , pp "Simplified Code" s1
        , pp "Simplified Combined" (s1 + s2 + bigmapSize) )
    | _ -> ("", "", "")
  in

  let code =
    match codeSize with
    | None -> ""
    | Some size -> pp "Code" size
  in
  let storage =
    match storageSize with
    | None -> ""
    | Some size -> pp "Storage" size
  in
  let combined =
    match (codeSize, storageSize) with
    | Some s1, Some s2 -> pp "Combined" (s1 + s2 + bigmapSize)
    | _ -> ""
  in
  let html =
    if 0 < nb_bigmaps
    then
      Printf.sprintf
        "<table>%s%s%s%s%s%s%s%s</table>"
        storage
        (pp (Printf.sprintf "Big map originations (%i)" nb_bigmaps) bigmapSize)
        sep
        code
        combined
        sep
        simplifiedCode
        simplifiedCombined
    else Printf.sprintf "<table>%s%s%s</table>" storage code simplifiedCode
  in
  Raw html

let michelson_html
    ~title ~lazy_tabs ~id ?simplified_contract (contract : Michelson.tcontract)
    =
  let open Michelson in
  let storageJson =
    Base.Option.map contract.storage ~f:(fun storage ->
        Format.asprintf
          "%a"
          (Micheline.pp_as_json ())
          (To_micheline.literal (erase_types_literal storage)))
  in
  let codeMicheline = Michelson.to_micheline_tcontract contract in
  let codeSize = Micheline_encoding.micheline_size_opt codeMicheline in
  let storageSize =
    let storageMicheline =
      Base.Option.map contract.storage ~f:(fun storage ->
          To_micheline.literal (erase_types_literal storage))
    in
    match storageMicheline with
    | None -> None
    | Some m -> Micheline_encoding.micheline_size_opt m
  in
  let codeJson = Format.asprintf "%a" (Micheline.pp_as_json ()) codeMicheline in
  let initialStorageHtml =
    copy_div
      ~id
      ~className:"white-space-pre-wrap"
      "storageCode"
      (Raw
         (Base.Option.value_map
            contract.storage
            ~default:"missing storage"
            ~f:Michelson.string_of_tliteral))
  in
  let codeHtml =
    lazy
      (div
         [ Raw "<h2>Michelson Code</h2>"
         ; div
             [ copy_div
                 ~id
                 "contractCode"
                 (Raw (Michelson.render_tcontract contract)) ] ])
  in
  let initialStorageHtmlJson =
    copy_div
      ~id
      "storageCodeJson"
      (Raw
         (Format.asprintf
            "<div class='white-space-pre'>%s</div>"
            (Option.value ~default:"missing storage" storageJson)))
  in
  let codeHtmlJson =
    copy_div
      ~id
      "contractCodeJson"
      (Raw
         ( try
             Format.asprintf "<div class='white-space-pre'>%s</div>" codeJson
           with
         | e ->
             Format.asprintf
               "<div class='white-space-pre'>Error: %s\n%s</div>"
               (Printexc.to_string e)
               (Printexc.get_backtrace ()) ))
  in
  let simplified =
    match simplified_contract with
    | Some simplified
      when not (Michelson.equal_tinstr contract.code simplified.code) ->
        Some simplified
    | _ -> None
  in
  let simplifiedCodeSize =
    match simplified with
    | None -> None
    | Some m ->
        Micheline_encoding.micheline_size_opt
          (Michelson.to_micheline_tcontract m)
  in
  let simplifiedCodeHtml =
    match simplified with
    | None -> None
    | Some simplified ->
        Some
          ( lazy
            (div
               [ Raw "<h2>Michelson Code</h2>"
               ; div
                   [ copy_div
                       ~id
                       "simplifiedContractCode"
                       (Raw (Michelson.render_tcontract simplified)) ] ]) )
  in
  let simplifiedCodeHtmlJson =
    match simplified with
    | None -> None
    | Some simplified ->
      ( try
          let simplified =
            Format.asprintf
              "%a"
              (Micheline.pp_as_json ())
              (Michelson.to_micheline_tcontract simplified)
          in
          Some
            (copy_div
               ~id
               "simplifiedContractCodeJson"
               (Raw
                  (Printf.sprintf
                     "<div class='white-space-pre'>%s</div>"
                     simplified)))
        with
      | e ->
          Some
            (Raw
               (Format.asprintf
                  "<div class='white-space-pre'>Error: %s\n%s</div>"
                  (Printexc.to_string e)
                  (Printexc.get_backtrace ()))) )
  in
  let nb_bigmaps =
    match contract.storage with
    | None -> 0
    | Some storage -> Michelson.count_bigmaps storage
  in
  let sizes =
    lazy
      (contract_sizes_html
         ~codeSize
         ~storageSize
         ~simplifiedCodeSize
         ~nb_bigmaps)
  in
  let tabs =
    tabs
      ~global_id:0
      title
      ( [ tab
            ~active:()
            "Sizes"
            (div
               [ Raw "<h2>Sizes</h2>"
               ; copy_div ~id "contractSizes" (Lazy.force sizes) ])
        ; tab
            "Storage"
            (div [Raw "<h2>Initial Storage</h2>"; div [initialStorageHtml]])
        ; ( if lazy_tabs
          then tab "Code" ~lazy_tab:codeHtml (Raw "")
          else tab "Code" (Lazy.force codeHtml) ) ]
      @ ( match simplifiedCodeHtml with
        | Some simplifiedCodeHtml ->
            [ ( if lazy_tabs
              then tab "Simplified Code" ~lazy_tab:simplifiedCodeHtml (Raw "")
              else tab "Simplified Code" (Lazy.force simplifiedCodeHtml) ) ]
        | None -> [] )
      @ [ tab
            "Storage JSON"
            (div [Raw "<h2>Initial Storage</h2>"; div [initialStorageHtmlJson]])
        ; tab
            "Code JSON"
            (div [Raw "<h2>JSON Representation</h2>"; div [codeHtmlJson]]) ]
      @
      match simplifiedCodeHtmlJson with
      | Some simplifiedCodeHtmlJson ->
          [ tab
              "Simplified Code JSON"
              (div
                 [ Raw "<h2>JSON Representation</h2>"
                 ; div [simplifiedCodeHtmlJson] ]) ]
      | None -> [] )
  in
  tabs

let michelson_html_comp ~accept_missings contract id =
  let tabs =
    michelson_html ~title:"Generated Michelson:" ~lazy_tabs:false ~id contract
  in
  let michelson =
    Printf.sprintf
      "<button class='centertextbutton extramarginbottom' \
       onClick='smartpyContext.gotoOrigination(contractSizes%s.innerHTML, \
       storageCode%s.innerText, contractCode%s.innerText, \
       storageCodeJson%s.innerText, \
       contractCodeJson%s.innerText,storageCode%s.innerHTML, \
       contractCode%s.innerHTML, storageCodeJson%s.innerHTML, \
       contractCodeJson%s.innerHTML)'>Deploy Michelson Contract</button>%s"
      id
      id
      id
      id
      id
      id
      id
      id
      id
      (render tabs)
  in
  (michelson, Michelson.has_error_tcontract ~accept_missings contract)

let showLine line_no =
  match line_no with
  | [] -> Raw "(no location info)"
  | (_, line_no) :: _ ->
      Raw
        (Printf.sprintf
           "<button class=\"text-button\" onClick='showLine(%i)'>(line \
            %i)</button>"
           line_no
           line_no)

let full_html
    ~config
    ~contract
    ~compiled_contract
    ~def
    ~onlyDefault
    ~id
    ~line_no
    ~accept_missings
    ~contract_id =
  let myName = "Contract" in
  let michelson, has_error =
    michelson_html_comp ~accept_missings compiled_contract id
  in
  let metadata =
    match Metadata.for_contract ~config contract compiled_contract with
    | [] -> []
    | metadata ->
        let metatab i (name, metadata) =
          let json = Format.asprintf "%a" (Misc.pp_json_as_json ()) metadata in
          let pp =
            copy_div
              ~id
              ("meta_" ^ name)
              (Raw
                 (Format.asprintf "<div class='white-space-pre'>%s</div>" json))
          in
          tab ?active:(if i = 0 then Some () else None) name pp
        in
        let tabs =
          Div ("metadata", [tabs "Metadata" (List.mapi metatab metadata)])
        in
        [tab "Metadata" tabs]
  in
  let myTabs =
    let tab title content =
      tab ?active:(if def = title then Some () else None) title content
    in
    let language_tab language =
      let module Printer = ( val Printer.get_by_language language
                               : Printer.Printer )
      in
      let title = Config.show_language language in
      [ tab
          title
          (Raw
             ( ( match contract.template.tcontract.unknown_parts with
               | Some msg ->
                   Printf.sprintf
                     "<span class='partialType'>Warning: unknown types or type \
                      errors: %s.</span><br>"
                     msg
               | None -> "" )
             ^ ( match has_error with
               | [] -> ""
               | _ ->
                   "<span class='partialType'>Warning: errors in the Michelson \
                    generated code.</span><br>" )
             ^ Printer.tcontract_to_string
                 ?contract_id
                 ~options:Printer.Options.html
                 contract
             ^ render (showLine line_no) )) ]
    in
    let languages = List.concat (List.map language_tab config.languages) in
    let module SmartPy_Printer = ( val Printer.get_by_language Config.SmartPy
                                     : Printer.Printer )
    in
    languages
    @ [ tab
          "Types"
          (Raw
             (Printf.sprintf
                "<div class='contract'><h3>Storage:</h3>%s<p><h3>Entry \
                 points:</h3>%s</div>"
                (SmartPy_Printer.type_to_string
                   ~toplevel:()
                   ~options:SmartPy_Printer.Options.types
                   (get_extra contract.template.tcontract.derived).tstorage)
                (SmartPy_Printer.type_to_string
                   ~toplevel:()
                   ~options:SmartPy_Printer.Options.types
                   (get_extra contract.template.tcontract.derived).tparameter)))
      ; tab "Deploy Michelson Contract" (Raw michelson) ]
    @ metadata
    @ [tab "&times;" (Raw "")]
  in
  if onlyDefault
  then
    div
      ~args:"class='tabs'"
      (List.mapi
         (fun i tab ->
           let tab = tab (-1) i in
           if tab.name = def
           then div ~args:"class='tabcontentshow'" [tab.content]
           else Raw "")
         myTabs)
  else tabs myName myTabs

let nextInputGuiId = Value.nextId "inputGui_"

let nextOutputGuiId = Value.nextId "outputGui_"

let nextLazyOutputGuiId = Value.nextId "lazyOutputGui_"

let contextSimulationType =
  let t1 =
    Type.record_default_layout
      Config.Comb
      [ ("sender", Type.string)
      ; ("source", Type.string)
      ; ("timestamp", Type.string)
      ; ( "amount"
        , Type.variant_default_layout
            Config.Comb
            [("Tez", Type.string); ("Mutez", Type.string)] )
      ; ("level", Type.string)
      ; ( "voting_powers"
        , Type.map ~big:false ~tkey:Type.key_hash ~tvalue:(Type.int ()) ) ]
  in
  let t2 =
    Type.record_default_layout
      Config.Comb
      [("debug", Type.bool); ("full_output", Type.bool)]
  in
  Type.record_default_layout Config.Comb [("context", t1); ("simulation", t2)]

let inputGui sim_id t tstorage buttonText ~line_no =
  let output = nextOutputGuiId () in
  let id = nextInputGuiId () ^ "_" in
  let nextId = Value.nextId id in
  let input = Value_gui.inputGuiR ~nextId t in
  let tstorage = Type.option tstorage in
  let input_storage = Value_gui.inputGuiR ~nextId tstorage in
  let contextInput = Value_gui.inputGuiR ~nextId contextSimulationType in
  div
    ~args:"class='simulationBuilder'"
    [ Raw (Printf.sprintf "<form><h3>Simulation Builder</h3>")
    ; Raw "<h4>Context</h4>"
    ; Raw contextInput.gui
    ; Raw "<h4>Edit Storage</h4>"
    ; Raw input_storage.gui
    ; Raw "<h4>Transaction</h4>"
    ; Raw input.gui
    ; Raw
        (Printf.sprintf
           "<button type='button' class='explorer_button' onClick=\"var t = \
            smartmlCtx.call_exn_handler('importType', '%s'); var tstorage = \
            smartmlCtx.call_exn_handler('importType', '%s');if (t) \
            smartmlCtx.call_exn_handler('callGui', '%s', %i, '%s', t, \
            tstorage,%i)\">%s</button></form>\n\
            <div id='%s'></div>"
           (SmartML.Export.export_type t)
           (SmartML.Export.export_type tstorage)
           id
           sim_id
           output
           (head_line_no line_no)
           buttonText
           output) ]

let delayedInputGui t =
  let output = nextLazyOutputGuiId () in
  let id = nextInputGuiId () ^ "." in
  Raw
    (Printf.sprintf
       "<button type='button' \
        onClick=\"smartmlCtx.call_exn_handler('addInputGui', '%s', '%s', \
        smartmlCtx.call_exn_handler('importType', '%s'))\">%s</button>\n\
        <div id='%s'></div>"
       id
       output
       (SmartML.Export.export_type t)
       "Add Another Step"
       output)

let simulatedContracts = Hashtbl.create 100

let simulation c id ~line_no =
  Hashtbl.replace simulatedContracts id c;
  let c = c.template.tcontract in
  inputGui
    id
    (get_extra c.derived).tparameter
    (get_extra c.derived).tstorage
    "Simulation"
    ~line_no
