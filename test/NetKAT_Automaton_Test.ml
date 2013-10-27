open NetKAT_Types
open QuickCheck

let policy_parse (p : string) : policy =
  NetKAT_Parser.program NetKAT_Lexer.token (Lexing.from_string p)

let prop_roundtrip (p : policy) : bool =
  let open NetKAT_Automaton in
  regex_to_policy (regex_of_policy p) = p

let netkat_quickCheck arbitrary show pred =
  let test = testable_fun arbitrary show testable_bool in
  match quickCheck test pred with
      | Success -> true
      | Failure _ -> failwith "No failure expected"
      | Exhausted _ -> failwith "No exhaustion expected"

let arbitrary_char = 
  let module Gen = NetKAT_Arbitrary in
  let open QuickCheck_gen in
  resize 4 (Gen.arbitrary_lf_pol >>= (fun lfp -> 
  Gen.arbitrary_link   >>= (fun l ->
    ret_gen (Seq(lfp, l)))))

module QuickChecking = struct
  TEST "sequence a link-free policy with a link" =
    netkat_quickCheck arbitrary_char string_of_policy prop_roundtrip
end

TEST "the simplest test" =
  let open NetKAT_Automaton in
  let pol = policy_parse "filter port = 1; 0@0 => 1@1" in
  let re  = regex_of_policy pol in
  match re with 
    | Char(_) -> true
    | _       -> false

TEST "the simplest test II" =
  let open NetKAT_Automaton in
  let pol = policy_parse "1@1 => 2@2; 0@0 => 1@1" in
  let re  = regex_of_policy pol in
  match re with 
    | Cat(Char(_), Char(_)) -> true
    | _                     -> false

TEST "distributes across links" =
  let open NetKAT_Automaton in
  let pol = policy_parse "filter port = 10; (0@1 => 1@1 | 2@1 => 1@1)" in
  let re  = regex_of_policy pol in
  match re with
    | Alt(Char(_), Char(_)) -> true
    | _                     -> false

TEST "does not distribute across link-free policies" =
  let open NetKAT_Automaton in
  let pol = policy_parse "(filter port = 9 | filter port = 10); (0@1 => 1@1 | 2@1 => 1@1)" in
  let re  = regex_of_policy pol in
  match re with
    | Alt(Char(_), Char(_)) -> true
    | _                     -> false

TEST "regression test #1" =
  let open NetKAT_Automaton in
  let pol = policy_parse "(drop | ethSrc := 121 ; ipSrc := 41) ; 20@119 => 107@56" in
  prop_roundtrip pol

TEST "regression test #2" =
  let open NetKAT_Automaton in
  let pol = policy_parse "(ethTyp := 121 | (ethSrc := 38 | filter ipSrc = 21 or vlanId = 20) | ethSrc := 100) ; 15@182 => 185@121" in
  prop_roundtrip pol

TEST "regression test #3" =
  let open NetKAT_Automaton in
  let pol = policy_parse "id ; id ; (filter tcpSrcPort = 144 or tcpSrcPort = 71 ; ethSrc := 68) ; 0@59 => 18@85" in
  prop_roundtrip pol
