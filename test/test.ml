open Core
open Probnetkat
open Symbolic
open Syntax


module Fdd_eq = struct
  include Fdd
  let pp fmt fdd = Format.fprintf fmt "%s" (to_string fdd)
end
let fdd_eq = (module Fdd_eq : Alcotest.TESTABLE with type t = Fdd.t)

module Fdd_equiv = struct
  include Fdd_eq
  let equal = equivalent ~modulo:[]
end
let fdd_equiv = (module Fdd_equiv : Alcotest.TESTABLE with type t = Fdd.t)

module Fdd_equiv_mod (M : sig val modulo : string list end) = struct
  include Fdd_equiv
  let equal = equivalent ~modulo:M.modulo
end
let fdd_equiv_mod modulo =
  let module Fdd_equiv_mod = Fdd_equiv_mod (struct let modulo = modulo end) in
  (module Fdd_equiv_mod : Alcotest.TESTABLE with type t = Fdd.t)

module Fdd_ser_eq = struct
  include Fdd_eq
  let equal x y =
    String.equal (serialize x) (serialize y) &&
    equivalent x y
end
let fdd_ser_eq = (module Fdd_ser_eq : Alcotest.TESTABLE with type t = Fdd.t)

let test kind name p q =
  (name, `Quick, (fun () ->
      Alcotest.check kind "" (Fdd.of_pol p) (Fdd.of_pol q);
      Fdd.clear_cache ~preserve:Int.Set.empty
    )
  )

let test_not kind p q =
  test (Alcotest.neg kind) p q


(*===========================================================================*)
(* FDD SERIALIZATION TESTS                                                   *)
(*===========================================================================*)

let serialization_roundtrips p = begin
  "serialization roundtrips",
  `Quick,
  fun () -> Alcotest.check fdd_ser_eq ""
    (p |> Fdd.of_pol)
    (p |> Fdd.of_pol |> Fdd.serialize |> Fdd.deserialize)
end

let serialization_without_cache_roundtrips p = begin
  "serialization roundtrips despite cache clearance",
  `Quick,
  fun () -> Alcotest.check fdd_ser_eq ""
    (p |> Fdd.of_pol)
    (p |> Fdd.of_pol |> Fdd.serialize |> (fun s ->
      Fdd.clear_cache ~preserve:Int.Set.empty;
      Fdd.deserialize s
    ))
end

let p1 = PNK.(
  whl (???("a",0)) (
    ?@[
      !!("a", 0) @ 3//6;
      !!("a", 1) @ 2//6;
      drop       @ 1//6;
    ]
  )
)

let serialization_tests = [

  serialization_roundtrips PNK.( skip );
  serialization_without_cache_roundtrips PNK.( skip );

  serialization_roundtrips PNK.( p1 );
  serialization_without_cache_roundtrips PNK.( p1 );


]



(*===========================================================================*)
(* DETERMINISTIC TESTS (WITHOUT ⊕)                                           *)
(*===========================================================================*)

let basic_deterministic = [

  test_not fdd_eq "skip ≠ drop" PNK.skip PNK.drop;

 (* predicate *)
  test fdd_equiv "predicate sequentially composed on right"
    PNK.(
      ite (???("X",0)) (!!("X", 1)) skip
    )
    PNK.(
      ite (???("X",0)) (!!("X", 1)) skip
      >> filter( neg (???("X", 0)) )
    );

  (* deterministic loop *)
  (* test fdd_equiv "deterministic loop" *)

  (* degenerate non-terminating loop *)
  test fdd_equiv "degenerate non-terminating loop"
    PNK.( whl True skip )
    PNK.( drop );

  (* non-degenerate non-terminating loop *)
  test fdd_equiv "non-degenerate non-terminating loop"
    PNK.( whl True @@ ?@[
            !!("x", 0) @ 1//3;
            ??("x", 1) @ 2//3;
          ]
    )
    PNK.( drop );

   (* fdd equivalence *)
  test fdd_equiv "equivalent but not equal fdds: equivalent"
    PNK.( ite (???("x", 0)) skip         skip )
    PNK.( ite (???("x", 0)) (!!("x", 0)) skip );
  test_not fdd_eq "equivalent but not equal fdds: not equal"
    PNK.( ite (???("x", 0)) skip         skip )
    PNK.( ite (???("x", 0)) (!!("x", 0)) skip );
]




(*===========================================================================*)
(* PROBABILISTIC TESTS (WITH ⊕)                                              *)
(*===========================================================================*)


let field i = sprintf "Y%d" i
let multi_coin m n =
  let open PNK in
  (!!("X", 0)) >>
  whl (neg @@ conji m ~f:(fun i -> ???(field i, 0))) begin
    seqi m ~f:(fun i ->
      ite (???("X", i)) (
        uniformi n ~f:(fun j ->
          !!(field i, j)
        ) >>
        !! ("X", (i+1) mod m)
      ) (
        skip
      )
    )
  end >> (!!("X", 0))



let basic_probabilistic = [


  (* coin flip example *)
  test fdd_equiv "coin flip terminates"
    PNK.(
      whl (???("c",0)) (
        ?@[ !!("c", 1) @ 1//2; skip @ 1//2 ]
      )
    )
    PNK.(
      ite (???("c", 0))
        (!!("c", 1))
        skip
      );

  (* distributivity *)
  test fdd_equiv "distributivity ; ⊕"
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
  (* FIXME: increase m as a regression test *)
  begin let m,n = 6,2 in
  test fdd_equiv "multi-coin convergence"
    PNK.( !!("X", 0) >> seqi m ~f:(fun i -> !!(field i, 0)) )
    (multi_coin m n)
  end;

  (* sparse multi-coin convergence *)
  begin let m,n = 1,3 in
  test fdd_equiv "multi-coin with three fields convergence"
    PNK.( !!("X", 0) >> seqi m ~f:(fun i -> !!(field i, 0)) )
    (multi_coin m n)
  end;

  (* loop-free speed *)
  begin
    let l i = sprintf "l%d" i in
    let n = 10 in
    test fdd_equiv "speed test: large matrix, but small FDD"
      PNK.( seqi n ~f:(fun i -> ?@[ !!(l i, 0) @ 1//2; !!(l i, 1) @ 1//2]) )
      PNK.( uniformi (Int.pow 2 n) ~f:(fun code ->
        seqi n ~f:(fun i ->
          !!(l i, Int.shift_right code i mod 2)
        )
      ));
  end;

  (* reweighting *)
  test fdd_equiv "loop reweights mass"
    PNK.(
      whl (???("a",0)) (
        ?@[
          !!("a", 0) @ 3//6;
          !!("a", 1) @ 2//6;
          drop       @ 1//6;
        ]
      )
    )
    PNK.(
      ite (???("a",0)) (
        ?@[ !!("a", 1) @ 2//3; drop @ 1//3; ]
      ) (
        skip
      )
    );

  (* local fields *)
  test fdd_equiv "local field loop"
    PNK.(
      local ("x", 0) ~mut:true (
        whl (neg @@ ???("x", 1)) (
          ?@[ !!("x", 1) @ 1//2; skip @ 1//2 ]
        )
      )
    )
    PNK.( skip );

]

let basic_performance = [

  (* joint distribution of n independent binary coins *)
  begin
    let n = 20 in
    let field i = sprintf "x%d" i in
    let local_field i = sprintf "y%d" i in
    let p =PNK.(
      seqi n ~f:(fun i -> ?@[
        !!(field i, 0) , 1//2;
        !!(field i, 1) , 1//2;
      ]) >>
      seqi n ~f:(fun i -> ?@[
        !!(local_field i, 0) , 1//2;
        !!(local_field i, 1) , 1//2;
      ])
      |> locals (List.init n ~f:(fun i -> (local_field i, 0, true)))
    )
    in
    test fdd_eq "joint distribution of independent binary variables" p p
  end;

  (* simplified up-bit example *)
  begin
    let n = 20 in
    let up i = sprintf "up_%d" i in
    let p = PNK.(
      seqi n ~f:(fun i -> ?@[
        !!(up i, 0) , 1//2;
        !!(up i, 1) , 1//2;
      ])
      >> uniformi n ~f:(fun i -> !!("pt",i))
      >> ??("pt", 0)
      |> locals (List.init n ~f:(fun i -> (up i, 0, true)))
    )
    in
    test fdd_eq "simplified failure model with up bits should scale O(n), not O(2^n)" p p
  end;

(*   (* up-bit example *)
  begin
    let n = 16 in
    let up i = sprintf "up_%d" i in
    let p = PNK.(
      seqi n ~f:(fun i -> ?@[
        !!(up i, 0) , 1//2;
        !!(up i, 1) , 1//2;
      ])
      >> uniformi n ~f:(fun i -> !!("pt",i))
      >> ite_cascade (List.range 0 n) ~otherwise:drop ~f:(fun i ->
        let guard = ???(up i, 1) & ???("pt", i) in
        let body = !!("pt", i+1 mod n) in
        (guard, body)
      )
      |> locals (List.init n ~f:(fun i -> (up i, 0, true)))
    )
    in
    test fdd_eq "failure model with up bits should scale O(n), not O(2^n)" p p
  end; *)

]


let misc_tests = [
  (* make sure we can change the order of fdd fields *)
  "fdd order", `Quick, (fun () ->
    Alcotest.(check bool) "is true" true begin
      let open Symbolic in
      Fdd.set_order ["a"; "b"];
      let a = Fdd.abstract_field "a" in
      let b = Fdd.abstract_field "b" in
      Field.compare a b = -1 &&
      (Fdd.set_order ["b"; "a"]; Field.compare a b = 1)
    end
  );

  test (fdd_equiv_mod ["x"]) "fdd equivalence modulo"
    PNK.(
      !!("y", 1)
    )
    PNK.(?@[
      !!("x", 1) >> !!("y", 1) , 1//2;
      !!("x", 2) >> !!("y", 1) , 1//2;
    ]);
]


(* let qcheck_tests = [
  "round-trip", `Quick, fun () -> QCheck.Test.check_exn failing
] *)

let () =
  Alcotest.run "Probnetkat" [
    "fdd serialization", serialization_tests;
    "fdd misc",          misc_tests;
    "fdd deterministic", basic_deterministic;
    "fdd probabilistic", basic_probabilistic;
    "fdd performance",   basic_performance;
    (* "qcheck", qcheck_tests; *)
  ]
