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

(* remember all policies we're testing with; we can use them to check that
   different compilation strategies give the same result *)
let pols = ref []

let test ?(use_slow_obs=false) kind name p q =
  pols := p :: q :: !pols;
  (name, `Quick, (fun () ->
      Fdd.use_slow_observe := use_slow_obs;
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

  test_not fdd_equiv "skip ≢ drop" PNK.skip PNK.drop;

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

  (* regression test: bug in while loop implementation *)
  test fdd_equiv "observe f10 regression test"
    PNK.(
      !!("x", 1)
      |> then_observe (conj (???("x", 1)) (???("y", 0)))
    )
    PNK.(
      !!("x", 1)
      |> do_whl (neg (conj (???("x", 1)) (???("y", 0))))
    )


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


(*===========================================================================*)
(* PERFORMANCE TESTS                                                         *)
(*===========================================================================*)

let basic_performance = [

  (* joint distribution of n independent binary coins *)
  begin
    let n = 20 in
    let field i = sprintf "x%d" i in
    let local_field i = sprintf "y%d" i in
    let p = PNK.(
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

  (* simplified up-bit example; we could do this efficiently with a better
     FactorizedActionDist.convex_sum, but that slows other things down too much
     and is currently not worth it.
  *)
  begin
    (* works with n=20 with somewhat smart, hacky convex_sum enabled *)
    let n = 2 in
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

  (* slightly harder up-bit example; this one works with smart FactorizedActionDist.convex_sum,
     but we won't make that the default for now since it is expensive. *)
  begin
    (* works with n=20 with smart convex_sum enabled *)
    let n = 2 in
    let up i = sprintf "up_%d" i in
    let p = PNK.(
      seqi n ~f:(fun i -> ?@[
        !!(up i, 0) , 1//2;
        !!(up i, 1) , 1//2;
      ])
      >> ite_cascade (List.range 0 n) ~otherwise:drop ~disjoint:false ~f:(fun i ->
        ???(up i, 1),
        !!("pt", i) >>
        (* erasing the up fields here is crucial to making this scale, at the moment *)
        seqi n ~f:(fun i -> !!(up i, 1))
      )
      |> locals (List.init n ~f:(fun i -> (up i, 0, true)))
    )
    in
    test fdd_eq "failure model with up bits should scale O(n), not O(2^n)" p p
  end;

  (* resilient up-bit example; this one is even harder and requires that factorization is mainted
     during fdd -> matrix -> fdd conversion.
  *)
  begin
    (* one day this should scale easily to n=20 ... *)
    let n = 2 in
    let up i = sprintf "up_%d" i in
    let at_good_pt = PNK.(
      disji n ~f:(fun i -> ???(up i, 1) & ???("pt", i))
    )
    in
    let p = PNK.(
      seqi n ~f:(fun i -> ?@[
        !!(up i, 0) , 1//2;
        !!(up i, 1) , 1//2;
      ])
      >> do_whl (neg at_good_pt) (
        uniformi n ~f:(fun i -> !!("pt", i))
      )
(*       >> ite_cascade (List.range 0 n) ~otherwise:drop ~f:(fun i ->
        ???(up i, 1),
        !!("pt", i) >> seqi n ~f:(fun i -> !!(up i, 1))
      ) *)
      (* |> locals (List.init n ~f:(fun i -> (up i, 0, true))) *)
    )
    in
    test fdd_eq "resilient failure model with up bits should scale O(n), not O(2^n)" p p
  end;


]


(*===========================================================================*)
(* MISC TESTS                                                                *)
(*===========================================================================*)

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

  test (fdd_equiv_mod ["x"]) "fdd seq homomorphism"
    PNK.(
      !!("y", 1)
    )
    PNK.(?@[
      !!("x", 1) >> !!("y", 1) , 1//2;
      !!("x", 2) >> !!("y", 1) , 1//2;
    ]);

  "fdd seq", `Quick, (fun () ->
    let p = PNK.(?@[
        !!("a", 0) @ 3//6;
        !!("a", 1) @ 2//6;
        drop       @ 1//6;
      ])
    in
    let a = PNK.( filter (???("a",1)) )
    in
    Alcotest.check fdd_equiv ""
      Fdd.(seq (of_pol p) (of_pol a)) Fdd.(of_pol PNK.(seq p a))
  );
]


(*===========================================================================*)
(* OBSERVE TESTS                                                             *)
(*===========================================================================*)

let up i = sprintf "up_%d" i

let observe_tests = [
  (* observe true *)
  test fdd_equiv "observe true filters out drop"
    PNK.(
      ?@[
        !!("a", 0) @ 2//6;
        !!("a", 1) @ 4//6;
      ]
    )
    PNK.(
      ?@[
        !!("a", 0) @ 1//6;
        !!("a", 1) @ 2//6;
        drop       @ 3//6;
      ]
      |> then_observe True
    );

  (* observe false *)
  test fdd_equiv "observe false = drop"
    PNK.(drop)
    PNK.(
      ?@[
        !!("a", 0) @ 3//6;
        !!("a", 1) @ 2//6;
        drop       @ 1//6;
      ]
      |> then_observe False
    );

  (* reweighting *)
  test fdd_equiv "observe reweighting"
    PNK.(
      ?@[
        !!("a", 1) @ 3//3;
        (* drop       @ 1//3; *)
      ]
    )
    PNK.(
      ?@[
        !!("a", 0) @ 3//6;
        !!("a", 1) @ 2//6;
        drop       @ 1//6;
      ]
      |> then_observe ???("a",1)
    );

  (* reweighting *)
  test fdd_equiv "observe filter equiv"
    PNK.(
      (?@[
        !!("a", 0) @ 3//6;
        !!("a", 1) @ 2//6;
        drop       @ 1//6;
      ]
      |> then_observe ???("a", 1))
    )
    PNK.(
      (?@[
        !!("a", 0) @ 3//6;
        !!("a", 1) @ 2//6;
        drop       @ 1//6;
      ]
      >> filter (???("a", 1)))
      |> then_observe True
    );

  (* observe disjunction *)
  test fdd_equiv "observe disjunction"
    PNK.(
      ?@[
        !!("a", 1) @ 2//5;
        !!("a", 2) @ 3//5;
        (* drop       @ 4//9; *)
      ]
    )
    PNK.(
      ?@[
        !!("a", 0) @ 1//10;
        !!("a", 1) @ 2//10;
        !!("a", 2) @ 3//10;
        drop       @ 4//10;
      ]
      |> then_observe (disj (???("a",1)) (???("a", 2)))
    );

  (* observe up fields = big conditional *)
  test fdd_equiv "observe up fields = big conditional"
    PNK.(
      ?@[
        !!("pt", 0) @ 1//3;
        !!("pt", 1) @ 1//3;
        !!("pt", 2) @ 1//3;
      ]
      |> then_observe (disji 3 ~f:(fun i ->
        conj (???("pt", i)) (???(up i, 1))
      ))
    )
    PNK.(
      ite (???(up 0, 1)) begin
        ite (???(up 1, 1)) begin
          ite (???(up 2, 1)) begin
            uniform [ !!("pt", 0); !!("pt", 1); !!("pt", 2) ]
          end (* else *) begin
            uniform [ !!("pt", 0); !!("pt", 1) ]
          end
        end (* else *) begin
          ite (???(up 2, 1)) begin
            uniform [ !!("pt", 0); !!("pt", 2) ]
          end begin
            !!("pt", 0)
          end
        end
      end (* else *) begin
        ite (???(up 1, 1)) begin
          ite (???(up 2, 1)) begin
            uniform [ !!("pt", 1); !!("pt", 2) ]
          end (* else *) begin
            !!("pt", 1)
          end
        end (* else *) begin
          ite (???(up 2, 1)) begin
            !!("pt", 2)
          end begin
            drop
          end
        end
      end
    );

  (* observe up fields *)
  test fdd_equiv "observe up fields + erasure"
    PNK.(
      ?@[
        drop        @  1//64;
        !!("pt", 0) @ 21//64;
        !!("pt", 1) @ 21//64;
        !!("pt", 2) @ 21//64;
      ]
    )
    PNK.(
      seqi 3 ~f:(fun i -> ?@[
        !!(up i, 0) @ 1//4;
        !!(up i, 1) @ 3//4;
      ])
      >> begin
        ?@[
          !!("pt", 0) @ 1//3;
          !!("pt", 1) @ 1//3;
          !!("pt", 2) @ 1//3;
        ]
        |> then_observe (disji 3 ~f:(fun i ->
          conj (???("pt", i)) (???(up i, 1))
        ))
      end
      |> locals (List.init 3 ~f:(fun i ->
        (up i, 0, true)
      ))
      (* |> Util.tap ~f:(Format.printf "\n%a\n" Syntax.pp_policy) *)
    );


  test fdd_equiv "observe true"
    PNK.(
      skip
    )
    PNK.(
      ?@[
        skip @ 1//2;
        drop @ 1//2;
      ]
      |> then_observe True
    );

  test fdd_equiv "observe disguised false"
    PNK.(
      ite (???("a", 2)) skip drop
    )
    PNK.(
      ?@[
        !!("a", 0) @ 1//3;
        !!("a", 1) @ 1//3;
        skip       @ 1//3;
      ]
      |> then_observe (???("a", 2))
    );

  test fdd_equiv "observe f10 regression test"
    PNK.(
      ite (???("y", 0))
        (!!("x", 1))
        drop
    )
    PNK.(
      !!("x", 1)
      |> then_observe (conj (???("x", 1)) (???("y", 0)))
    );

  test_not fdd_equiv "observe and seq do not distribute"
    PNK.(
      seq (?@[ skip @ 1//2; drop @ 1//2 ]) skip
      |> then_observe True
    )
    PNK.(
      seq (?@[ skip @ 1//2; drop @ 1//2 ]) (then_observe True skip)
    )
]


let cps_tests =
  List.map (!pols) ~f:(fun p ->
    "", `Quick, (fun () ->
      try[@warning "-52"]
        Fdd.use_cps := false;
        let slow = Fdd.of_pol p in
        Fdd.use_cps := true;
        let fast = Fdd.of_pol p in
        Alcotest.check fdd_equiv "" slow fast;
        (* with bounds *)
        Fdd.use_cps := false;
        let slow = Fdd.of_pol ~bound:(Some 4) p in
        Fdd.use_cps := true;
        let fast = Fdd.of_pol ~bound:(Some 4) p in
        Alcotest.check fdd_equiv "" slow fast;
        (* with bounds *)
        Fdd.use_cps := false;
        let slow = Fdd.of_pol ~bound:(Some 8) p in
        Fdd.use_cps := true;
        let fast = Fdd.of_pol ~bound:(Some 8) p in
        Alcotest.check fdd_equiv "" slow fast;
      with Failure "too many fields! (may need to clear the cache?)" ->
        (* the CPS translation is less economical in the use of fields, but that
           should not be considered a bug *)
        ()
    )
  )


(* let qcheck_tests = [
  "round-trip", `Quick, fun () -> QCheck.Test.check_exn failing
]
*)

let () =
  Alcotest.run "Probnetkat" [
    "fdd serialization", serialization_tests;
    "fdd misc",          misc_tests;
    "fdd deterministic", basic_deterministic;
    "fdd probabilistic", basic_probabilistic;
    (* "fdd performance",   basic_performance; *)
    "fdd observe", observe_tests;
    "fdd cps compilation", cps_tests;
    (* "qcheck", qcheck_tests; *)
  ]
