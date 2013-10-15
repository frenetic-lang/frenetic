open NetKAT_Types
module SDN = SDN_Types
open VInt

let policy_parse (p : string) : NetKAT_Types.policy =
  NetKAT_Parser.program NetKAT_Lexer.token (Lexing.from_string p)

let test_pretty p_str =
  let p_ast1 = NetKAT_Parser.program NetKAT_Lexer.token (Lexing.from_string p_str) in
  let p_str' = string_of_policy p_ast1 in
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
  let open QuickCheck in
  let open QuickCheck_gen in
  let open NetKAT_Arbitrary in
    testable_fun 
      (resize 10
         arbitrary_pol) string_of_policy testable_bool

let prop_parse_pol_idempotent (p : NetKAT_Types.policy) : bool =
  policy_parse (string_of_policy p) = p

let qc () =
  ()

TEST "testing parse-pretty roundtrip" =
  let cfg = { QuickCheck.quick with QuickCheck.maxTest = 1000 } in
  match QuickCheck.check testable_pol_to_bool cfg prop_parse_pol_idempotent with
    | QuickCheck.Success -> true
    | _ -> failwith "quickchecking failed"
