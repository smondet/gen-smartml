(* Copyright 2019-2021 Smart Chain Arena LLC. *)

val of_mtype : Michelson.mtype -> Michel.Type.ty

val decompile_contract :
     Michel.Transformer.state
  -> Michelson.tcontract
  -> Michel.Expr.expr Michel.Expr.precontract
