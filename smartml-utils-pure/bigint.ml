(* Copyright 2019-2021 Smart Chain Arena LLC. *)
include Big_int

type t = big_int

let equal = Big_int.eq_big_int

let compare = Big_int.compare_big_int

let show = string_of_big_int

let pp ppf p = Format.fprintf ppf "%s" (show p)

let of_int = big_int_of_int
