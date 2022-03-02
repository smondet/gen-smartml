(* Copyright 2019-2021 Smart Chain Arena LLC. *)

open Unix

let show_process_status =
  let open Unix in
  function
  | WEXITED n -> Printf.sprintf "WEXITED %d" n
  | WSIGNALED n -> Printf.sprintf "WSIGNALED %d" n
  | WSTOPPED n -> Printf.sprintf "WSTOPPED %d" n

let run ?env cmd args =
  let args = Array.of_list (cmd :: args) in
  let env =
    let f (k, v) = k ^ "=" ^ v in
    Array.of_list (Option.cata [] (List.map f) env)
  in
  let env = Array.append env (environment ()) in
  let pid = create_process_env cmd args env stdin stdout stderr in
  let pid', status = waitpid [] pid in
  assert (pid == pid');
  match status with
  | WEXITED n -> n
  | s ->
      let s = show_process_status s in
      failwith (Printf.sprintf "%s exited abnormally: %s" cmd s)
