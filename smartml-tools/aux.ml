(* Copyright 2019-2021 Smart Chain Arena LLC. *)

open SmartML
open Basics

let address_of_contract_id ~html contract_id entry_point =
  let address =
    match contract_id with
    | C_static {static_id} ->
        Utils.Bs58.address_of_contract_id ~static:true static_id entry_point
    | C_dynamic {dynamic_id} ->
        Utils.Bs58.address_of_contract_id ~static:false dynamic_id entry_point
  in
  if html
  then Printf.sprintf "<span class='address'>%s</span>" address
  else address
