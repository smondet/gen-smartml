type t =
  | Yes
  | No
  | Maybe

val and_ : t -> t -> t

val or_ : t -> t -> t
