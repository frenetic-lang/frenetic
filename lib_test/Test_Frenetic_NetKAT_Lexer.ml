open Core.Std
open Frenetic_NetKAT_Lexer

let%test "Token.to_string serializes token" =
  Token.to_string (KEYWORD "and") = "KEYWORD and"

let%test "Token.print pretty-prints token in context of sprintf" =
  let fmt = Format.str_formatter in
  Token.print fmt (KEYWORD "and");
  (Format.flush_str_formatter ()) = "KEYWORD and"

let%test "Token.match_keyword returns true if token is the specified keyword" =
  Token.match_keyword "and" (KEYWORD "and") 

let%test "Token.match_keyword returns false if the token is not the specified keyword" =
  not (Token.match_keyword "and" (KEYWORD "or") )

let%test "Token.match_keyword returns false if token is not a keyword" =
  not (Token.match_keyword "and" EOI)

let%test "Token.extract_string returns identifier for identifier-type tokens" =
  Token.extract_string (INT32 "12345") = "12345"

let%test "Token.extract_string returns error for bare tokens" =
  Exn.does_raise (fun () ->
    Token.extract_string EOI = "12345"
  )

let%test "mk takes a stream and location and returns tokens in a stream" = 
  let s = mk () (Loc.mk "<string>") (Stream.of_string "ip4Src = 8080l") in
  Stream.junk s;
  Stream.junk s;
  (fst (Stream.next s)) = (INT32 "8080l")