
module MAKE(Lexer : module type of Lexer)
    (Parser : module type of Generated_Parser) = struct

  let pol_of_string ?pos (s : string) =
    Lexer.parse_string ?pos s Parser.pol_eof
end

include MAKE(Lexer)(Generated_Parser)