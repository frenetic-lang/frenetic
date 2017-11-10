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


let fdd_test_eq name p q = 
  (name, `Quick, fun () -> Alcotest.check fdd "" (Fdd.of_pol p) (Fdd.of_pol q))

let fdd_test_neq name p q =
  (name, `Quick, fun () -> Alcotest.(check (neg fdd) "" (Fdd.of_pol p) (Fdd.of_pol q)))

let basic_positive = [

  (* coin flip example *)
  fdd_test_eq "coin flip terminates"
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
  fdd_test_eq "distributivity ; ⊕"
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
    );

  (* sparse multi-coin convergence *)
  let field i = sprintf "F%d" i in
  let multi_coin m n =
    let open PNK in
    mk_while (neg @@ conji m ~f:(fun i -> ???(field i, 0))) begin
      seqi m ~f:(fun i ->
        ite (???("X", i)) (
          uniform n ~f:(fun j ->
            !!(field i, j)
          ) >>
          !! ("X", (i+1) mod m)
        ) (
          skip
        )
      )
    end >> (!!("X", 0))
  in
  let m, n = 2,2 in
  fdd_test_eq "multi-coin convergence" (multi_coin m n) PNK.(
    !!("X", 0) >> seqi m ~f:(fun i -> !!(field i, 0))
  )
]

let basic_negative = [
  fdd_test_neq "skip ≠ drop" PNK.skip PNK.drop
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
