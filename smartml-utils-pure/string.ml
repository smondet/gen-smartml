(* Copyright 2019-2021 Smart Chain Arena LLC. *)

include Stdlib.String

let take n x = sub x 0 n

let drop n x = sub x n (length x - n)

let is_prefix p x =
  let l = length p in
  l <= length x && take l x = p

let pp ppf = Format.fprintf ppf "%s"

let repeat s n = Array.fold_left ( ^ ) "" (Array.make n s)

include Data.Make (struct
  type t = string

  let compare = compare
end)
