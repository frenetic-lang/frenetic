open OUnitHack
open Types
open Pretty
open QuickCheck

let policy_parse (p : string) : policy =
  Parser.program Lexer.token (Lexing.from_string p)

let netkat_quickCheck arbitrary show pred =
  let test = testable_fun arbitrary show testable_bool in
  match quickCheck test pred with
      | Success -> true
      | Failure _ -> failwith "No failure expected"
      | Exhausted _ -> failwith "No exhaustion expected"

module QuickChecking = struct
  open NetKAT_Automaton

  let rec same_same (r : regex) (s : policy) : bool =
    begin match r, s with
      | Char(lf_p, switch), Seq(s1, Link(sw3, pt3, sw4, pt4)) ->
          (topology_to_policy switch) = (Link(sw3, pt3, sw4, pt4))
      | Alt(r1, r2), Par(s1, s2) -> (same_same r1 s1) && (same_same r2 s2)
      | Cat(r1, r2), Seq(s1, s2) -> (same_same r1 s1) && (same_same r2 s2)
      | Kleene(r1) , Star(s1)    -> same_same r1 s1
      | _          , _           -> false
    end

  let prop_same_same (lf_p : policy) (lf_q : policy) : bool =
    same_same (regex_of_policy (Seq(lf_p, lf_q))) lf_q

  let prop_roundtrip (p : policy) : bool =
    regex_to_policy (regex_of_policy p) = p

  let arbitrary_char =
    let module Gen = NetKAT_Arbitrary in
    let open QuickCheck_gen in
    Gen.arbitrary_lf_pol >>= (fun lfp ->
    Gen.arbitrary_link   >>= (fun l ->
      ret_gen (Seq(lfp, l))))

  let arbitrary_regex_policy_no_star =
    let module Gen = NetKAT_Arbitrary in
    let open QuickCheck_gen in
    oneof [
      arbitrary_char;
      (arbitrary_char >>= fun p ->
       arbitrary_char >>= fun q ->
        ret_gen (Seq(p, q)));
      (arbitrary_char >>= fun p ->
       arbitrary_char >>= fun q ->
        ret_gen (Par(p, q)))
    ]

  TEST "sequence a link-free policy with a link" =
    netkat_quickCheck arbitrary_char string_of_policy prop_roundtrip

  TEST "roundtrip property for regex policy" =
    netkat_quickCheck arbitrary_regex_policy_no_star string_of_policy
      prop_roundtrip

  TEST "sequence a link-free policy with a regex policy (no star)" =
    let open QuickCheck_gen in
    let module Gen = NetKAT_Arbitrary in

    let arbitrary =
      Gen.arbitrary_lf_pol           >>= fun lfp ->
      arbitrary_regex_policy_no_star >>= fun r ->
        ret_gen (lfp, r) in

    let show (x, y) = string_of_policy (Seq(x, y)) in
    let prop = fun (x, y) -> prop_same_same x y in

    netkat_quickCheck arbitrary show prop
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

TEST "does not distribute across links" =
  let open NetKAT_Automaton in
  let pol = policy_parse "filter port = 10; (0@1 => 1@1 | 2@1 => 1@1)" in
  let re  = regex_of_policy pol in
  match re with
    | Char(_) -> true
    | _       -> false

TEST "distributes across complex links" =
  let open NetKAT_Automaton in
  let pol = policy_parse "filter port = 10; ((filter ethDst = 2; 0@1 => 1@1) | 2@1 => 1@1)" in
  let re  = regex_of_policy pol in
  match re with
    | Alt(Char(_), Char(_)) -> true
    | _                     -> false

TEST "does not distribute across link-free policies" =
  let open NetKAT_Automaton in
  let pol = policy_parse "(filter port = 9 | filter port = 10); (0@1 => 1@1 | 2@1 => 1@1)" in
  let re  = regex_of_policy pol in
  match re with
    | Char(_) -> true
    | _       -> false

TEST "associativity of seq" =
  let open NetKAT_Automaton in
  let pol = policy_parse "(filter switch = 1; port := 1 ; (1@1 => 2@1; filter switch = 2; port := 4)); 9@2 => 2@3" in
  let re  = regex_of_policy pol in
  match re with
    | Cat(Char(_), Char(_)) -> true
    | _                     -> false

TEST "regression test #1" =
  let open NetKAT_Automaton in
  let pol = policy_parse "(drop | ethSrc := 121 ; ipSrc := 41) ; 20@119 => 107@56" in
  QuickChecking.prop_roundtrip pol

TEST "regression test #2" =
  let open NetKAT_Automaton in
  let pol = policy_parse "(ethTyp := 121 | (ethSrc := 38 | filter ipSrc = 21 or vlanId = 20) | ethSrc := 100) ; 15@182 => 185@121" in
  QuickChecking.prop_roundtrip pol

TEST "regression test #3" =
  let open NetKAT_Automaton in
  let pol = policy_parse "id ; id ; (filter tcpSrcPort = 144 or tcpSrcPort = 71 ; ethSrc := 68) ; 0@59 => 18@85" in
  QuickChecking.prop_roundtrip pol
