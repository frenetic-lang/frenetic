open OUnitHack
module QCGen = QuickCheck_gen
open SDN_Types
open NetKAT_Types
open NetKAT_Pretty
open NetKAT_Verify

let gen_pol_1 =
  let open QuickCheck in
  let open QuickCheck_gen in
  let open NetKAT_Arbitrary in
  let open Packet in
  testable_fun
    (arbitrary_lf_pol >>= fun p ->
     ret_gen (p))
    (fun (p) -> string_of_policy p)
    testable_bool

let gen_pol_2 =
  let open QuickCheck in
  let open QuickCheck_gen in
  let open NetKAT_Arbitrary in
  let open Packet in
    testable_fun
      (arbitrary_lf_pol >>= fun p ->
       arbitrary_lf_pol >>= fun q ->
       ret_gen (p, q))
      (fun (p,q) -> (string_of_policy p) ^ " ## " ^ (string_of_policy q))
      testable_bool

let gen_pol_3 =
  let open QuickCheck in
  let open QuickCheck_gen in
  let open NetKAT_Arbitrary in
  let open Packet in
  testable_fun
    (arbitrary_lf_pol >>= fun p ->
     arbitrary_lf_pol >>= fun q ->
     arbitrary_lf_pol >>= fun r ->
     ret_gen (p, q, r))
    (fun (p,q,r) ->
      (string_of_policy p) ^ " ## " ^ (string_of_policy q) ^ " ## "
      ^ (string_of_policy r))
    testable_bool

let check_equivalent p q = Verify.check_equivalent (Dexterize.policy_to_term p) (Dexterize.policy_to_term q)

let check gen_fn compare_fn =
  let cfg = { QuickCheck.quick with QuickCheck.maxTest = 1000 } in
  match QuickCheck.check gen_fn cfg compare_fn with
        QuickCheck.Success -> true
    | _                  -> false

TEST "quickcheck-fail-6-13" = 
  let prop_compile_ok (p, q, r) =
    not (check_equivalent (Union(Union(q, Seq (p, r)), r)) r )
    ||  (check_equivalent (Union(Seq(Star p, q), r)) r ) in
  let filter_test = Test (NetKAT_Types.TCPSrcPort 10135) in 
  prop_compile_ok (Seq (Filter filter_test, Mod(EthSrc 123456L)) ,
		   Union (id,id),
		   Seq(Filter (Test (IPProto 165)), drop))



TEST "quickcheck ka-plus-assoc compiler" =
  let prop_compile_ok (p, q, r) =
    check_equivalent
      (Union(p,(Union(q,r))))
      (Union((Union(p,q)), r))
     in
  check gen_pol_3 prop_compile_ok

TEST "quickcheck ka-plus-comm compiler" =
  let prop_compile_ok (p, q) =
    check_equivalent (Union(p, q)) (Union(q, p))in
  check gen_pol_2 prop_compile_ok

TEST "quickcheck ka-plus-zero compiler" =
  let prop_compile_ok (pol) =
    check_equivalent pol (Union(pol, drop))in
  check gen_pol_1 prop_compile_ok

TEST "quickcheck ka-plus-idem compiler" =
  let prop_compile_ok (pol) =
    check_equivalent (Union(pol, pol)) pol in
  check gen_pol_1 prop_compile_ok

TEST "quickcheck ka-seq-assoc compiler" =
  let prop_compile_ok (p, q, r) =
    check_equivalent (Seq(p, (Seq (q, r)))) (Seq((Seq(p, q)), r))in
  check gen_pol_3 prop_compile_ok

TEST "quickcheck ka-one-seq compiler" =
  let prop_compile_ok (pol) =
    check_equivalent pol (Seq(id, pol))in
  check gen_pol_1 prop_compile_ok

TEST "quickcheck ka-seq-one compiler" =
  let prop_compile_ok (pol) =
    check_equivalent pol (Seq(pol, id))in
  check gen_pol_1 prop_compile_ok

TEST "quickcheck ka-seq-dist-l compiler" =
  let prop_compile_ok (p, q, r) =
    check_equivalent
      (Seq(p, (Union (q, r)))) (Union ((Seq(p, q)), (Seq(p, r))))in
  check gen_pol_3 prop_compile_ok

TEST "quickcheck ka-seq-dist-r compiler" =
  let prop_compile_ok (p, q, r) =
    check_equivalent
      (Seq (Union(p, q), r)) (Union (Seq(p, r), Seq(q, r)))in
  check gen_pol_3 prop_compile_ok

TEST "quickcheck ka-zero-seq compiler" =
  let prop_compile_ok (pol) =
    check_equivalent drop (Seq(drop, pol))in
  check gen_pol_1 prop_compile_ok

TEST "quickcheck ka-seq-zero compiler" =
  let prop_compile_ok (pol) =
    check_equivalent drop (Seq(pol, drop))in
  check gen_pol_1 prop_compile_ok

TEST "quickcheck ka-unroll-l compiler" =
  let prop_compile_ok (pol) =
    check_equivalent (Star pol) (Union(id, Seq(pol, Star pol)))in
  check gen_pol_1 prop_compile_ok

TEST "quickcheck ka-lfp-l compiler" =
  let prop_compile_ok (p, q, r) =
    not (check_equivalent (Union(Union(q, Seq (p, r)), r)) r )
    ||  (check_equivalent (Union(Seq(Star p, q), r)) r ) in
  check gen_pol_3 prop_compile_ok

TEST "quickcheck ka-unroll-r compiler" =
  let prop_compile_ok (pol) =
    check_equivalent (Star pol) (Union(id, Seq(Star pol, pol))) in
  check gen_pol_1 prop_compile_ok


TEST "quickcheck ka-lfp-r compiler" =
  let prop_compile_ok (p, q, r) =
    not (check_equivalent (Union(Union(p, Seq (q, r)), q)) q )
    ||  (check_equivalent (Union(Seq(p, Star r), q)) q ) in
  check gen_pol_3 prop_compile_ok

