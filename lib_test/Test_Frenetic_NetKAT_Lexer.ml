open Core.Std
open Frenetic_NetKAT_Lexer

TEST "Token.to_string serializes token" =
  Token.to_string (KEYWORD "and") = "KEYWORD and"

TEST "Token.print pretty-prints token in context of sprintf" =
  let fmt = Format.str_formatter in
  Token.print fmt (KEYWORD "and");
  (Format.flush_str_formatter ()) = "KEYWORD and"

TEST "Token.match_keyword returns true if token is the specified keyword" =
  Token.match_keyword "and" (KEYWORD "and") 

TEST "Token.match_keyword returns false if the token is not the specified keyword" =
  not (Token.match_keyword "and" (KEYWORD "or") )

TEST "Token.match_keyword returns false if token is not a keyword" =
  not (Token.match_keyword "and" EOI)

TEST "Token.extract_string returns identifier for identifier-type tokens" =
  Token.extract_string (INT32 "12345") = "12345"

TEST "Token.extract_string returns error for bare tokens" =
  try Token.extract_string EOI = "12345"
  with Invalid_argument _ -> true | _ -> false

TEST "mk takes a stream and location and returns tokens in a stream" = 
  let s = mk () (Loc.mk "<string>") (Stream.of_string "ip4Src = 8080l") in
  Stream.junk s;
  Stream.junk s;
  (fst (Stream.next s)) = (INT32 "8080l")