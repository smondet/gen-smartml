(* Copyright 2019-2021 Smart Chain Arena LLC. *)

module type S = sig
  include Stdlib.Set.S
end

module Make (Ord : Stdlib.Set.OrderedType) : S with type elt = Ord.t
