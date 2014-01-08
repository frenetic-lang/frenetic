open Types
open Pretty
open VInt
module SDN = SDN_Types

let policy_parse (p : string) : Types.policy =
  Parser.program Lexer.token (Lexing.from_string p)

let parse_pretty str = 
  string_of_policy
    (Parser.program Lexer.token (Lexing.from_string str))
  
let test_pretty p_str =
  let p_ast1 = Parser.program Lexer.token (Lexing.from_string p_str) in
  let p_str' = string_of_policy p_ast1 in
  let p_ast2 = Parser.program Lexer.token (Lexing.from_string p_str') in
  p_ast1 = p_ast2

let p_str1 = "filter port = 1"
let p_str2 = "filter port = 1 ; port := 2"
let p_str3 = "filter switch = 1 ; filter port = 1 ; port := 2 + 
              (filter switch = 1 ; filter port = 2 ; port := 1 +
               filter switch = 2 ; filter port = 1 ; port := 2)"

TEST "simple filter" = test_pretty p_str1 = true
TEST "simple sequence" = test_pretty p_str2 = true
TEST "assoc par" = test_pretty p_str3 = true

TEST "conditional" = test_pretty "if port = 1 then drop else id" = true

TEST "line wrap" =
  let str = "\
  filter port = 1; filter port = 1; filter port = 1; filter port = 1 +\n\
  filter port = 1; filter port = 1; filter port = 1; filter port = 1 +\n\
  filter port = 1; filter port = 1; filter port = 1; filter port = 1" in
  parse_pretty str = str

TEST "pretty printing should wrap long lines nicely" =
  let str = "\
  filter port = 1; filter port = 1; filter port = 1; filter port = 1 +\n\
  filter port = 1; filter port = 1; filter port = 1; filter port = 1 +\n\
  filter port = 1; filter port = 1; filter port = 1; filter port = 1" in
  parse_pretty str = str

let testable_pol_to_bool = 
  let open QuickCheck in
  let open QuickCheck_gen in
  let open NetKAT_Arbitrary in
  testable_fun 
    (resize 11 arbitrary_policy) 
    string_of_policy testable_bool

let prop_parse_pol_idempotent (p : Types.policy) : bool =
  try policy_parse (string_of_policy p) = p
  with _ -> 
    Printf.printf "POL: %s\n" (string_of_policy p);
    assert false

TEST "testing parse-pretty roundtrip" =
  let cfg = { QuickCheck.quick with QuickCheck.maxTest = 1000 } in
  match QuickCheck.check testable_pol_to_bool cfg prop_parse_pol_idempotent with
    | QuickCheck.Success -> true
    | _ -> failwith "quickchecking failed"
