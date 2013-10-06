open NetKAT_Types
module SDN = SDN_Types
open VInt

(* 
  string policy
  parse -> pretty -> parse should be an identity
*)

let test_pretty p_str =
  let p_ast1 = NetKAT_Parser.program NetKAT_Lexer.token (Lexing.from_string p_str) in
  let p_str' = string_of_policy p_ast1 in
  Printf.printf "Policy is %s\n%!" p_str';
  let p_ast2 = NetKAT_Parser.program NetKAT_Lexer.token (Lexing.from_string p_str') in
  p_ast1 = p_ast2


let p_str1 = "filter inPort = 1"
let p_str2 = "filter inPort = 1 ; inPort -> 2"

TEST "simple filter" = test_pretty p_str1 = true
TEST "simple SEQ"    = test_pretty p_str2 = true

   
