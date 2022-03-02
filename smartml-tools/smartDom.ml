(* Copyright 2019-2021 Smart Chain Arena LLC. *)

let getTextRef = ref (fun _ -> failwith "getText not implemented")

let setTextRef = ref (fun _ _ -> failwith "setText not implemented")

let setValueRef = ref (fun _ _ -> failwith "setValue not implemented")

let setOutputRef = ref (fun _ -> failwith "setOutput not implemented")

let addOutputRef = ref (fun _ -> failwith "addOutput not implemented")

let setOutputToMethodRef =
  ref (fun _ _ -> failwith "setOutputToMethod not implemented")

let isCheckedRef = ref (fun _ -> failwith "getRadioValue not implemented")

let parseDateRef = ref (fun _ -> failwith "parseDate not implemented")

let getText id = !getTextRef id

let setText id value = !setTextRef id value

let setValue id value = !setValueRef id value

let setOutput value = !setOutputRef value

let addOutput value = !addOutputRef value

let setOutputToMethod methodName output =
  !setOutputToMethodRef methodName output

let isChecked id = !isCheckedRef id

let parseDate t = !parseDateRef t
