(* Copyright 2019-2021 Smart Chain Arena LLC. *)

type variable_id = int

type contract_id = int

type ('constant, 'lambda) expr =
  | Constant           of 'constant
  | Secret_key         of string
  | Variable           of variable_id
  | Contract_data      of contract_id
  | Contract_balance   of contract_id
  | Make_signature     of
      { secret_key : ('constant, 'lambda) expr
      ; message : ('constant, 'lambda) expr }
  | Test_account       of ('constant, 'lambda) expr
  | Account_secret_key of ('constant, 'lambda) expr
  | Account_public_key of ('constant, 'lambda) expr
  | Account_key_hash   of ('constant, 'lambda) expr
  | KT1                of ('constant, 'lambda) expr
  | Michelson          of 'lambda * ('constant, 'lambda) expr list
