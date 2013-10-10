open NetKAT_Types
module SDN = SDN_Types
open VInt

(* 
  string policy
  parse -> pretty -> parse should be an identity
*)


let policy_parse (p : string) : NetKAT_Types.policy =
  NetKAT_Parser.program NetKAT_Lexer.token (Lexing.from_string p)

let test_pretty p_str =
  let p_ast1 = NetKAT_Parser.program NetKAT_Lexer.token (Lexing.from_string p_str) in
  let p_str' = string_of_policy p_ast1 in
  (* Printf.printf "Policy is %s\n%!" p_str'; *)
  let p_ast2 = NetKAT_Parser.program NetKAT_Lexer.token (Lexing.from_string p_str') in
  p_ast1 = p_ast2


let p_str1 = "filter inPort = 1"
let p_str2 = "filter inPort = 1 ; inPort -> 2"
let p_str3 = "filter switch = 1 ; filter inPort = 1 ; inPort -> 2 + 
              (filter switch = 1 ; filter inPort = 2 ; inPort -> 1 +
               filter switch = 2 ; filter inPort = 1 ; inPort -> 2)"


TEST "simple filter" = test_pretty p_str1 = true
TEST "simple SEQ"    = test_pretty p_str2 = true
TEST "assoc par"     = test_pretty p_str3 = true




let testable_pol_to_bool = 
  QuickCheck.testable_fun (QuickCheck_gen.resize 10 NetKAT_Arbitrary.arbitrary_pol) NetKAT_Types.string_of_policy QuickCheck.testable_bool


let prop_parse_pol_idempotent (p : NetKAT_Types.policy) : bool =
  let p_str = string_of_policy p in 
  let p' = policy_parse (p_str) in
  p' = p


let qc () =
  let cfg = { QuickCheck.verbose with QuickCheck.maxTest = 200 } in
  let _ = QuickCheck.check testable_pol_to_bool cfg prop_parse_pol_idempotent in
  ()

let _ = qc ()
