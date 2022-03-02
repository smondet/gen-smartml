(* Copyright 2019-2021 Smart Chain Arena LLC. *)

module type S = sig
  include Stdlib.Map.S

  val lookup : default:'a -> key -> 'a t -> 'a

  val to_list : 'a t -> (key * 'a) list

  val keys : 'a t -> key list

  val values : 'a t -> 'a list

  val of_list : (key * 'a) list -> 'a t

  val of_hashtbl : (key, 'a) Hashtbl.t -> 'a t

  val intersect : 'a t -> 'b t -> ('a * 'b) t
end

module Make (Ord : Stdlib.Map.OrderedType) = struct
  include Stdlib.Map.Make (Ord)

  let lookup ~default k m = Option.default default (find_opt k m)

  let to_list = bindings

  let keys m = List.map fst (to_list m)

  let values m = List.map snd (to_list m)

  let of_list xs = of_seq (List.to_seq xs)

  let of_hashtbl xs = of_seq (Hashtbl.to_seq xs)

  let intersect =
    let f _ x y =
      match (x, y) with
      | Some x, Some y -> Some (x, y)
      | _ -> None
    in
    fun m -> merge f m
end
