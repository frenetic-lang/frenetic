let pol_of_string ?pos (s : string) =
  Lexer.parse_string ?pos s Raw_parser.pol_eof

let formula_of_string ?pos (s : string) =
  Lexer.parse_string ?pos s Raw_parser.formula_eof

let pol_of_file (file : string) =
  Lexer.parse_file ~file Raw_parser.pol_eof

let formula_of_file (file : string) =
  Lexer.parse_file ~file Raw_parser.formula_eof
