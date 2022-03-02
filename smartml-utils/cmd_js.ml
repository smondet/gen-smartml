(* Copyright 2019-2021 Smart Chain Arena LLC. *)
open Control
open Js_of_ocaml
open Js
open Unsafe

let _console_log (x : any) : unit = meth_call (variable "console") "log" [|x|]

let require (x : string) : any =
  fun_call (variable "require") [|inject (string x)|]

let process_env () : any = get (variable "process") "env"

let object_assign xs : any = meth_call (variable "Object") "assign" xs

let spawn_sync
    (cmd : string) (args : string list) (env : (string * string) list) =
  let child_process = require "child_process" in
  let cmd = inject (string cmd) in
  let args = inject (array (Array.of_list (List.map string args))) in
  let options =
    let inh = string "inherit" in
    let env0 = process_env () in
    let env =
      obj (Array.of_list (List.map (map_snd (fun x -> inject (string x))) env))
    in
    let env = object_assign [|obj [||]; env0; env|] in
    obj [|("stdio", inject (array [|inh; inh; inh|])); ("env", env)|]
  in
  meth_call child_process "spawnSync" [|cmd; args; options|]

let run ?(env = []) cmd args =
  let result = spawn_sync cmd args env in
  if get result "error" = null
  then parseInt (get result "status")
  else failwith ("run: " ^ to_string (get (get result "error") "errno"))
