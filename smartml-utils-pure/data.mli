(* Copyright 2019-2021 Smart Chain Arena LLC. *)

module type S = sig
  include Stdlib.Set.OrderedType

  module Set : Set.S with type elt = t

  module Map : Map.S with type key = t

  val map_of_set : (Map.key -> 'a) -> Set.t -> 'a Map.t
end

module Make (Ord : Stdlib.Set.OrderedType) : S with type t = Ord.t
