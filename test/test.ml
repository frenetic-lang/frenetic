open Probnetkat

(* check basic axioms using qcheck?
   https://github.com/c-cube/qcheck
*)

let add () =
    Alcotest.(check int) "1+1=2" (1+1) 3

let test_set = [
  "\xF0\x9F\x90\xAB  Capitalize", `Quick, add;
  "\xF0\x9F\x90\xAB  Capitalize", `Quick, add;
]

let () =
  Alcotest.run "Probnetkat" [
    "", test_set;
  ]
