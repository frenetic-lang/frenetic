open Probnetkat
(* check basic axioms using qcheck?
   https://github.com/c-cube/qcheck
*)

let add () =
    Alcotest.(check int) "1+1=2" (1+1) 3

let failing =
  QCheck.Test.make ~count:10
    ~name:"fail_sort_id"
    QCheck.(list small_nat)
    (fun l -> l = List.sort compare l);;


let basic_tests = [
  "predefined cases", `Quick, add
]

let qcheck_tests = [
  "round-trip", `Quick, fun () -> QCheck.Test.check_exn failing
]

let () =
  Alcotest.run "Probnetkat" [
    "basic", basic_tests;
    "qcheck", qcheck_tests;
  ]
