(* Simple Camlp4 parser for NetKAT 

   This parser has a common structure with Frenetic_Syntax_Extension_Parser, but outputs
   pure NetKAT data structures instead of OCaml AST's.  It's be nice if you could share the logic
   but that doesn't look possible because there's no nice way to convert between AST's and the
   expressions they repesent.  So IF YOU CHANGE THE GRAMMAR HERE, be sure to change it in 
   Frenetic_Syntax_Extension_Parser as well

   The parser actually does almost the same thing as Frenetic_NetKAT_Json in that it turns
   strings into NetKAT.  It's just the underlying NetKAT "dialect" that's different.   
*)

open Frenetic_NetKAT

(** Given a policy string, parse it into abstract NetKAT form *)
val policy_from_string : string -> policy

(** Given a predicate string, parse it into abstract NetKAT form.  Raise exception if the string is a policy, not a predicate *)
val pred_from_string : string -> pred
