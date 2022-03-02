(* Copyright 2019-2021 Smart Chain Arena LLC. *)

module Scenario_bak = Scenario
open SmartML
module Scenario = Scenario_bak

module Error : sig
  type t = {message : Base.string}

  val make : string -> t

  val pp_quick : Format.formatter -> t -> unit
end

module Decorated_result : sig
  type content =
    [ `Code of string list
    | `Error of Error.t
    | `Int of int
    | `O of (string * content) list
    | `Path of string
    | `Text of string
    ]

  type 'a t =
    { result : ('a, Error.t) result
    ; attach : content list }

  val return : 'a -> 'a t

  val fail : Error.t -> 'a t

  val bind : 'a t -> f:('a -> 'b t) -> 'b t

  val attach : a:content list -> 'a t -> 'a t
end

module type Tezos_client = sig
  module Io : sig
    type 'a t

    val return : 'a -> 'a t

    val bind : 'a t -> f:('a -> 'b t) -> 'b t
  end

  type state

  val originate :
       state
    -> id:int
    -> contract:string
    -> storage:string
    -> balance:string
    -> string Decorated_result.t Io.t

  val transfer :
       ?arg:string
    -> ?entry_point:string
    -> ?amount:int
    -> ?sender:string Basics.account_or_address
    -> ?source:string Basics.account_or_address
    -> state
    -> dst:string
    -> unit Decorated_result.t Io.t

  val get_contract_storage :
    state -> address:string -> string Decorated_result.t Io.t

  val run_script :
       state
    -> contract:string
    -> parameter:string
    -> storage:string
    -> string Decorated_result.t Io.t
end

module History_event : sig
  type t = private
    { actions : Basics.taction list
    ; status : [ `Success | `Failure | `None ]
    ; expecting_success : bool
    ; attachements : Decorated_result.content list }

  val make :
       actions:Basics.taction list
    -> ?expecting_success:bool
    -> ?attachements:Decorated_result.content list
    -> [ `Success | `Failure | `None ]
    -> t
end

module Make_interpreter (Client : Tezos_client) (Prims : Primitives.Primitives) : sig
  module State : sig
    type t = private
      { client : Client.state
      ; smartml : Basics.scenario_state
      ; compute_variables : (string, string) Hashtbl.t
      ; history : History_event.t Queue.t
      ; log_advancement : string -> unit }

    val of_smartml :
         ?log_advancement:(string -> unit)
      -> client:Client.state
      -> Basics.scenario_state
      -> t

    val fresh : client:Client.state -> t
  end

  val run :
    Scenario.loaded_scenario -> State.t -> (unit, Error.t) result Client.Io.t
end
