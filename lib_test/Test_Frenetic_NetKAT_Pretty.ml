open Frenetic_NetKAT
open Frenetic_NetKAT_Pretty
module SDN = Frenetic_OpenFlow

let parse_pretty str =
  string_of_policy (Frenetic_NetKAT_Parser.pol_of_string str)

let test_parse p_str =
  try
    let _ = Frenetic_NetKAT_Parser.pol_of_string p_str in
    true
  with _ ->
    Printf.printf "Can't parse {%s}\n%!" p_str;
    false

let test_pretty p_str =
  let p_ast1 = Frenetic_NetKAT_Parser.pol_of_string p_str in
  let p_str' = string_of_policy p_ast1 in
  let p_ast2 = Frenetic_NetKAT_Parser.pol_of_string p_str' in
  p_ast1 = p_ast2

let p_str1 = "filter port = 1"
let p_str2 = "filter port = 1 ; port := 2"
let p_str3 = "filter switch = 1 ; filter port = 1 ; port := 2 |
              (filter switch = 1 ; filter port = 2 ; port := 1 |
               filter switch = 2 ; filter port = 1 ; port := 2)"

let%test "simple filter" = test_pretty p_str1 = true
let%test "simple sequence" = test_pretty p_str2 = true
let%test "assoc par" = test_pretty p_str3 = true

let%test "conditional parses" = test_parse "if port = 1 then drop else id"
let%test "prettified conditional parses" =
  test_parse (string_of_policy (
    Frenetic_NetKAT_Parser.pol_of_string "filter not port = 1; drop"))
let%test "conditional" = test_pretty "if port = 1 then drop else id" = true

let%test "line wrap" =
  let str = "\
  filter port = 1; filter port = 1; filter port = 1; filter port = 1 |\n\
  filter port = 1; filter port = 1; filter port = 1; filter port = 1 |\n\
  filter port = 1; filter port = 1; filter port = 1; filter port = 1" in
  parse_pretty str = str

let%test "pretty printing should wrap long lines nicely" =
  let str = "\
  filter port = 1; filter port = 1; filter port = 1; filter port = 1 |\n\
  filter port = 1; filter port = 1; filter port = 1; filter port = 1 |\n\
  filter port = 1; filter port = 1; filter port = 1; filter port = 1" in
  parse_pretty str = str

let testable_pol_to_bool =
  let open QuickCheck in
  let open QuickCheck_gen in
  let open Arbitrary_Frenetic_NetKAT in
  testable_fun
    (resize 11 arbitrary_policy)
    string_of_policy testable_bool

let prop_parse_pol_idempotent (p : policy) : bool =
  try
    let s = string_of_policy p in
    let p' = Frenetic_NetKAT_Parser.pol_of_string s in
    (if p <> p' then Printf.printf "[|%s|]\n%!" s);
    p = p'
  with _ ->
    Printf.printf "{|%s|}\n%!" (string_of_policy p);
    false

(*
let%test "testing parse-pretty roundtrip" =
  let cfg = { QuickCheck.quick with QuickCheck.maxTest = 1000 } in
  match QuickCheck.check testable_pol_to_bool cfg prop_parse_pol_idempotent with
    | QuickCheck.Success -> true
    | _ -> false
*)
