(* a simple repeater *)

let repeater =
  if inPort = 1 then fwd(2)
  else fwd(1) in
monitorTable(1, repeater)
