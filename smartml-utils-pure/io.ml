(* Copyright 2019-2021 Smart Chain Arena LLC. *)

let read_file path =
  let i = open_in path in
  let buffer = Buffer.create (in_channel_length i) in
  let size = 1024 in
  let b = Bytes.create size in
  let rec step () =
    let r = input i b 0 size in
    if r <> 0
    then (
      Buffer.add_subbytes buffer b 0 r;
      step () )
  in
  step ();
  close_in i;
  Buffer.contents buffer

let write_file path s =
  let o = open_out path in
  output_string o s;
  close_out o
