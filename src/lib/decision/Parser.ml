let pol_of_string ?pos (s : string) =
  Lexer.parse_string ?pos s Raw_parser.pol_eof

let formula_of_string ?pos (s : string) =
  Lexer.parse_string ?pos s Raw_parser.formula_eof

let pol_of_file (file : string) =
  Lexer.parse_file ~file Raw_parser.pol_eof

let formula_of_file (file : string) =
  Lexer.parse_file ~file Raw_parser.formula_eof



open Core
open! Expect_test_helpers_kernel

let test_formula s =
  formula_of_string s
  |> [%sexp_of: string Ast.formula]
  |> print_s

let%expect_test _ =
  test_formula "x1 := 12 == drop";
  [%expect {| (Equiv (Modify x1 12) Drop) |}]

let%expect_test _ =
  test_formula "x != 12; test_ := 031201; drop + skip* !== dup";
  [%expect {|
    (Nequiv
      (Union
        (Seq
          (Seq
            (TestNeq x     12)
            (Modify  test_ 31201))
          Drop)
        (Star Skip))
      Dup) |}]
