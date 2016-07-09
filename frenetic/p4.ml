open Core.Std
open Frenetic_NetKAT
open Frenetic_P4

module Parser = Frenetic_NetKAT_Parser

let pol_file = "examples/p4.kat"

let () =
  let pol = Parser.policy_of_file pol_file in
  let p4 = p4_of_policy pol in
  print_endline (string_of_p4 p4)
