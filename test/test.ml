open Core
open Probnetkat
open Symbolic
open Syntax

module Fdd = struct
  include Fdd
  let pp fmt fdd = Format.fprintf fmt "%s" (to_string fdd)
  let equal = equivalent
end

let fdd = (module Fdd : Alcotest.TESTABLE with type t = Fdd.t)
let mkfdd = Fdd.of_pol

(* let failing =
  QCheck.Test.make ~count:10
    ~name:"fail_sort_id"
    QCheck.(list small_nat)
    (fun l -> l = List.sort compare l);; *)


let fdd_test name p q = 
  (name, `Quick, fun () -> Alcotest.check fdd "" (mkfdd p) (mkfdd q))

let fdd_ntest name p q =
  (name, `Quick, fun () -> Alcotest.(check (neg fdd) "" (mkfdd p) (mkfdd q)))

let basic_positive = [

  (* coin flip example *)
  fdd_test "coin flip terminates"
    PNK.(
      whl (???("c",0)) @@ ?@[
        !!("c", 1) @ 1//2;
        skip       @ 1//2
      ])
    PNK.(
      ite (???("c", 0)) 
        (!!("c", 1)) 
        skip
      );

  (* distributivity *)
  fdd_test "distributivity ; ⊕"
    PNK.(
      ?@[
        !!("a", 0) @ 1//2;
        !!("a", 1) @ 1//2;
      ] >>
      ?@[
        !!("b", 0) @ 1//2;
        !!("b", 1) @ 1//2;
      ]
    )
    PNK.(
      ?@[
        !!("a", 0) >> !!("b", 0) , 1//4;
        !!("a", 0) >> !!("b", 1) , 1//4;
        !!("a", 1) >> !!("b", 0) , 1//4;
        !!("a", 1) >> !!("b", 1) , 1//4;
      ]
    )
]

let basic_negative = [
  fdd_ntest "skip ≠ drop" PNK.skip PNK.drop
]

(* let qcheck_tests = [
  "round-trip", `Quick, fun () -> QCheck.Test.check_exn failing
] *)

let () =
  Alcotest.run "Probnetkat" [
    "fdd positive", basic_positive;
    "fdd negative", basic_negative;
    (* "qcheck", qcheck_tests; *)
  ]
