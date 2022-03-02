type dynamic_id = {dynamic_id : int} [@@deriving eq, ord, show]

type static_id = {static_id : int} [@@deriving eq, ord, show]

type contract_id =
  | C_static  of static_id
  | C_dynamic of dynamic_id
[@@deriving eq, ord, show]
