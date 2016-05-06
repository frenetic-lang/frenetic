%{
open Frenetic_Decide_Ast
open Frenetic_Decide_Ast.Term
open Frenetic_Decide_Ast.Formula
module Measurement = Frenetic_Decide_Measurement
%}

%token <string> VAR
%token <string> STRING
%token ZERO ONE DUP
%token AND OR
%token PLUS TIMES STAR
%token NOT
%token LPAREN RPAREN COMMA
%token EQ NEQ EQUIV LE ASSG
%token EOL

%nonassoc EQ LE /* lowest precedence */
%left PLUS
%left TIMES VAR ZERO ONE DUP LPAREN
%nonassoc NOT STAR /* highest precedence */

%start formula_main term_main query_main  /* entry points */
%type <Frenetic_Decide_Ast.Formula.t> formula_main
%type <Frenetic_Decide_Ast.Term.t> term_main
%type <Frenetic_Decide_Measurement.Query.t> query_main

%%

formula_main:
  | formula EOL { $1 }
  | EOL { raise Frenetic_Decide_Deriv.Empty }
;

term_main:
  | term EOL { $1 }
;

query_main:
  | query EOL { $1 }
;

term:
  | VAR ASSG STRING { assg (Frenetic_Decide_Util.Field.of_string $1) (Frenetic_Decide_Util.Value.of_string $3) }
  | VAR EQ STRING   { test (Frenetic_Decide_Util.Field.of_string $1) (Frenetic_Decide_Util.Value.of_string $3) }
  | VAR NEQ STRING  { not (test (Frenetic_Decide_Util.Field.of_string $1) (Frenetic_Decide_Util.Value.of_string $3)) }
  | ZERO            { zero }
  | ONE             { one }
  | DUP             { dup }
  | LPAREN term RPAREN { $2 }
  | term PLUS term  { plus (TermSet.of_list [$1; $3]) }
  | term TIMES term { times [$1; $3] }
  | term STAR       { star $1 }
  | NOT term        { not $2 }
  | term term %prec TIMES { times [$1; $2] }
;

formula:
  | term EQUIV term { make_eq $1 $3 }
  | term LE term    { make_le $1 $3 }
;

predicate:
  | ONE                     { Measurement.Predicate.One }
  | ZERO                    { Measurement.Predicate.Zero }
  | VAR EQ STRING           { Measurement.Predicate.Test
                              (Frenetic_Decide_Util.Field.of_string $1,
                               Frenetic_Decide_Util.Value.of_string $3) }
  | predicate OR  predicate { Measurement.Predicate.Or ($1, $3) }
  | predicate AND predicate { Measurement.Predicate.And ($1, $3) }
  | LPAREN predicate RPAREN { $2 }
;

query:
  | LPAREN predicate COMMA predicate RPAREN  { Measurement.Query.Pred  ($2, $4) }
  | query PLUS  query                        { Measurement.Query.Plus  ($1, $3) }
  | query TIMES query                        { Measurement.Query.Times ($1, $3) }
  | query STAR                               { Measurement.Query.Star   $1      }
  | LPAREN query RPAREN { $2 }
;
