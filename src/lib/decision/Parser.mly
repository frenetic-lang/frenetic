%{
open Core
open Ast
%}

(* predicates and policies *)

%token SKIP DROP DUP
%token SEMICOLON PLUS STAR 
%token EQUALS NEQUALS ASSIGN
%token TILDE (* negation - only in surface syntax *)

(* fields *)
%token <string> FIELD

(* values *)
%token <string> VALUE

(* primitives *)
%token LPAR RPAR EOF

(* formulas *)
%token EQUIV NEQUIV LEQ GEQ


(* precedence and associativity - from lowest to highest *)
%left PLUS
%left SEMICOLON
%nonassoc STAR (* p;q* == p;(q* ) *)
%nonassoc TILDE (* ~p* == (~p)* *)

%start <string Ast.pol> pol

%start <string Ast.formula> formula_eof

%start <string Ast.formula> formula

%%

formula_eof:
  | f=formula; EOF { f }


pol:
  | SKIP { Skip }
  | DROP { Drop }
  | DUP { Dup }
  | p=pol; SEMICOLON; q=pol { Seq (p, q) }
  | p=pol; PLUS; q=pol { Union (p, q) }
  | p=pol; STAR { Star p }
  | f=FIELD; EQUALS; n=int { TestEq (f, n) }
  | f=FIELD; NEQUALS; n=int { TestNeq (f, n) }
  | f=FIELD; ASSIGN; n=int { TestEq (f, n) }
  | TILDE; p=pol { Ast.negate p }
  | LPAR; p=pol; RPAR { p }

int:
  | i = VALUE { Int.of_string i }

formula:
  | p=pol; EQUIV; q=pol { Equiv (p, q) }
  | p=pol; NEQUIV; q=pol { Nequiv (p, q) }
  | p=pol; LEQ; q=pol { Leq (p, q) }
  | p=pol; GEQ; q=pol { Geq (p, q) }
  (* | p=pol { Pol p } *)

%%
