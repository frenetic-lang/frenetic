open OUnitHack
open Core.Std

open Frenetic_NetKAT

TEST "string_of_fastfail gives a list of fastfail nodes" =
  string_of_fastfail [1l; 2l; 3l] = "[1,2,3]"