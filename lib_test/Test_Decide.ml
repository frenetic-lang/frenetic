open Core.Std

module DecideAst = Frenetic_Decide_Ast
module DecideParser = Frenetic_Decide_Parser
module DecideUtil = Frenetic_Decide_Util

(* TODO(mwhittaker): The parsing was copied from async/Frenetic_Shell.ml. It
 * should be factored out. Though, we may be throwing away the parser soon
 * anyway! *)

(* TODO(mwhittaker): Formulas with <= in them are not working right now. We
 * call Formula.terms to get the terms out and then check that they are
 * equivalent, but this is wrong for formulas with <=. It should be an easy fix
 * of replacing `a <= b` with `a + b == b`. *)

(* The line, column, and token of a parsing error. *)
exception ParseError of int * int * string

(* Try to parse lexbuf, derived from filename, with parser_function and throw
 * an exception if parsing fails. If lexbuf wasn't derived from a file,
 * filename can be any descriptive string. *)
let parse_exn parser_function lexbuf =
  try
    parser_function Frenetic_Decide_Lexer.token lexbuf
  with
    | Parsing.Parse_error -> begin
      let curr = lexbuf.Lexing.lex_curr_p in
      let line = curr.Lexing.pos_lnum in
      let char = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
      let token = Lexing.lexeme lexbuf in
      raise (ParseError (line, char, token))
    end

(* Tests that the decision procedure can correctly parse and evaluate formulas. *)
let%test_unit "decision procedure end to end tests" =
  let formulas = [
    "(x := 3; x = 3) == (x := 3)";
    "(x = 3; y = 4) == (y = 4; x = 3)";
    "(x := 3; y := 4) == (y := 4; x := 3)";
    "(x := 1; y = 2) == (y = 2; x := 1)";
    "(dup; x = 3) == (x = 3; dup)";
    "(x = 3; x:= 3) == (x = 3)";
    "(x := 3; x:=4 ) == (x := 4)";
    "(x = 3; x = 4) == drop";
    "(x = 3 + x != 3) == pass";
    (* "(x != 3) <= x != 4"; *)
    "(x := 3; x :=4; x := 5) == x:=5";
    "(x := 3; x :=4; x := 2; x := 5) == x:=5";
    "(x := 3; x :=4; x := 2; x := 1; x := 5) == x:=5";
    "(x := 3; x :=4; x := 2; x := 1; x := 0; x := 5) == x:=5";
    "(x := 3; x :=4; x := 2; x := 1; x := 0; x := 6; x := 5) == x:=5";
    "(x := 3; x :=4; x := 2; x := 1; x := 0; x := 6; x := 7; x := 5) == x:=5";
    "(x := 3; y :=4; x := 2; y := 1; x := 0; y := 6; x := 7; x := 5) == x:=5; y := 6";
    "(y := 1; x := 0; y := 6; x := 5) == x:=5; y := 6";
    "x := 0; y := 6; x := 5 == x := 5; y := 6";
    (* "z := 4; y := 6; x := 5 <= z := 4; x := 5 y := 6"; *)
    "(x = 3 + z = 4) + drop == (x = 3 + z = 4)";
    "(x = 3 + z = 4) + (x = 3 + z = 4) == (x = 3 + z = 4)";
    "pass; (x = 3 + z = 4) == (x = 3 + z = 4)";
    "(x = 3 + z = 4); pass == (x = 3 + z = 4)";
    "(y = 2);(x = 3 + z = 4) == (y = 2);(x = 3) + (y = 2);(z = 4)";
    "(x = 3 + z = 4);(y = 2) == (x = 3);(y = 2) + (z = 4);(y = 2)";
    (* "x = 3 <= x = 4"; *)
    "drop; (x = 3 + z = 4) == drop";
    "(x = 3 + z = 4); drop == drop";
    "pass + (x = 3 + z = 4); (x = 3 + z = 4)* == (x = 3 + z = 4)*";
    (* "(z = 5) + pass + ((x = 3 + z = 4); (x = 3 + z = 4)* ) <= z = 5 + (x = 3 + z = 4) *"; *)
    "(x = 4) + ~(x = 4) == pass";
    "(x = 4); ~(x = 4) == drop";
    "dup; x = 5 == x = 5; dup";
    "x := 4; x = 4 == x:= 4";
    "x = 4; x := 4 == x = 4";
    "x = 3; x = 5 == drop";
    "(x := 4; x:= 3; x = 3)* == pass + x := 3";
    (* "(x := 4; x:= 3; x = 3) <= pass + x := 3"; *)
    "(x := 4; x:= 3; x = 3) ==  x := 3";
    "sw = 0; sw := 1; dup; sw = 1; sw := 2; dup  ==  sw = 0; sw := 1; dup; sw := 2; dup";
    "sw = 0; sw := 1; dup; sw = 1 == sw = 0; sw := 1; dup";
    "(y = 3 + z = 4;z := 4)*;(y = 4 + z = 5)*== (y = 3 + z = 4; z := 4)*;(y = 4 + z = 5)*";
    (* "(a=3);(b=4);(d=1);(e=4);(f=0) <= a=3"; *)
    (* "pass + (a=1;b:=2);(c=3;drop) + (c=3;drop) <= c=3;drop"; *)
    (* "drop <= pass"; *)
    (* "x = 1; dup <= dup"; *)
  ] in

  (* `eval_formula s` parses and evaluates the formula `s`. For example,
   * `eval_formula "pass == pass"` evaluates to true and `eval_formula "pass ==
   * drop"` evaluates to false. *)
  let eval_formula (formula: string) : bool =
    let lexbuf = Lexing.from_string formula in
    let formula = parse_exn DecideParser.formula_main lexbuf in
    let lhs, rhs = DecideAst.Formula.terms formula in
    ignore (DecideUtil.set_univ DecideAst.([Term.values lhs; Term.values rhs]));
    Frenetic_Decide_Bisimulation.check_equivalent lhs rhs
  in

  (* `assert_formula s` asserts that the formula `s` is true, crashing if it
   * isn't *)
  let assert_formula (formula: string) : unit =
    if not (eval_formula formula) then
      failwith (sprintf "Formula '%s' is not true but should be!" formula)
  in

  List.iter ~f:assert_formula formulas
