open NetKAT_Parser
open NetKAT_Types

let netkat_of_string (s: string) : NetKAT_Types.policy = 
  program NetKAT_Lexer.token (Lexing.from_string s)

let rec repl () : unit =
  print_string "> ";
  (
  try
    print_string (string_of_policy (netkat_of_string (read_line ())))
  with _ -> print_string "Oops, parse error"
  );
  print_newline ();
  repl ()
   
let _  =
  repl ()
