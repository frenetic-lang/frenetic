open Core
open Frenetic_Util

let%test "make_string_of applies a formatter to a type and returns a string" = 
  let formatter_for_strings (fmt: Format.formatter) (s1, s2): unit = 
    Format.fprintf fmt "%s, " s1;
    Format.fprintf fmt "%s" s2 
  in
  (make_string_of formatter_for_strings ("Hi", "mom!")) = "Hi, mom!"

  
