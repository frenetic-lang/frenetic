open Core
open Frenetic_NetKAT

let%test "string_of_fastfail gives a list of fastfail nodes" =
  string_of_fastfail [1l; 2l; 3l] = "[1,2,3]"
