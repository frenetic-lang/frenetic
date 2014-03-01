open OUnitHack
module QCGen = QuickCheck_gen
open SDN_Types
open NetKAT_Types
open NetKAT_Pretty

let test_compile lhs rhs =
  let rhs' =
    LocalCompiler.to_netkat
      (LocalCompiler.of_policy 0L lhs) in
  if rhs' = rhs then
    true
  else
    (Format.printf "compile @,%a@, produced %a@,,@,expected %a\n%!"
       format_policy lhs format_policy rhs' format_policy rhs;
     false)

let test_compile_table pol tbl = 
  let open LocalCompiler in 
  let tbl' = to_table (compile 0L pol) in
  if tbl = tbl' then 
    true
  else
    (Format.printf "compile @,%a@, produced %a@,,@,expected %a\n%!"
       format_policy pol format_flowTable tbl' format_flowTable tbl;
     false)

let ite (pred : pred) (then_pol : policy) (else_pol : policy) : policy =
  Union (Seq (Filter pred, then_pol), Seq (Filter (Neg pred), else_pol))

let testSrc n = Test (EthSrc (Int64.of_int n))
let testDst n = Test (EthDst (Int64.of_int n))
let modSrc n = Mod (EthSrc (Int64.of_int n))
let modDst n = Mod (EthDst (Int64.of_int n))

TEST "compile drop" =
  test_compile (Filter False) (Filter False)

TEST "compile test" =
  let pr = testSrc 0 in
  test_compile (Filter pr) (Filter pr)

TEST "compile negation" =
  let pr = testSrc 0 in
  test_compile (Filter (Neg pr)) (Filter (Neg pr))

TEST "compile negation of conjunction" =
  let pr1 = testSrc 0 in 
  let pr2 = testDst 0 in 
  let pr = And (pr1, pr2) in 
  test_compile
    (Filter (Neg pr))
    (Union (Filter(And(Neg pr2, pr1)), Filter (Neg pr1)))

TEST "commute test annihilator" =
  test_compile
    (Seq (modSrc 1 , Filter (testSrc 0)))
    (Filter False)

TEST "commute test different fields" =
  test_compile
    (Seq (modSrc 1, Filter (testDst 0)))
    (Seq (Filter (testDst 0), modSrc 1))

(* trivial optimization possible *)
TEST "commute same field" =
  test_compile
    (Seq (modSrc 1, Filter (testSrc 1)))
    (modSrc 1)

(* trivial optimization possible *)
TEST "same field, two values = drop" =
  let pr1 = testSrc 1 in
  let pr2 = testSrc 0 in
  test_compile
    (Filter (And (pr1, pr2)))
    (Filter False)

TEST "par1" =
  test_compile
    (Union(modSrc 1,
	 ite
	   (testSrc 1)
	   (modSrc 2)
	   (modSrc 3)))
    (ite
       (testSrc 1)
       (Union (modSrc 1,
	     modSrc 2))
       (Union (modSrc 1,
	     modSrc 3)))
       
TEST "star id" =
  test_compile
    (Star (Filter True))
    (Filter True)

TEST "star drop" =
  test_compile
    (Star (Filter False))
    (Filter True)

TEST "star modify1" =
  test_compile
    (Star (modSrc 1))
    (Union (Filter True, modSrc 1))

TEST "star modify2" =
  test_compile
    (Star (Union (modSrc 0,
	        ite (testSrc 0) (modSrc 1) (modSrc 2))))
    (ite
       (testSrc 0)
       (Union (Union (Union (Filter True, modSrc 0), modSrc 1), modSrc 2))
       (Union (Union (Union (Filter True, modSrc 0), modSrc 1), modSrc 2)))

(*
TEST "policy that caused stack overflow on 10/16/2013" =
  test_compile
    (Union (Seq (Filter (Or (Test (Dst, 1), And (Test (Dst, 1), Test (Src, 0)))),
            Union (Mod (Dst, 0), Filter (And (Or (Test (Src, 2), Test (Dst, 1)),
                                          Test (Dst, 0))))),
         Seq (drop, Mod (Src, 1))))
    id *)

(*  Src -> A ; (filter Src = C + Dst -> C) *)
TEST "quickcheck failure on 10/16/2013" =
  test_compile
    (Seq (modSrc 0, Union (Filter (testSrc 2), modDst 2)))
    (Seq (modDst 2, modSrc 0))
    
TEST "vlan" =
  let test_vlan_none = Test (Vlan 0xFFF) in
  let mod_vlan_none = Mod (Vlan 0xFFF) in
  let mod_port1 = Mod (Location (Physical 1l)) in
  let id = Filter True in
  let pol =
    Seq (ite 
	   test_vlan_none 
	   id
	   (Seq(id, mod_vlan_none)), 
	 mod_port1) in
  let pol' = 
    ite test_vlan_none
      mod_port1
      (Seq (mod_vlan_none, mod_port1)) in 
  test_compile pol pol'

TEST "quickcheck ka-plus-assoc" =
  let testable_pol_pkt_to_bool =
    let open QuickCheck in
    let open QuickCheck_gen in
    let open NetKAT_Arbitrary in
    let open Packet_Arbitrary in
    let open Packet in
    testable_fun
      (resize 11
        (arbitrary_pair
          (arbitrary_pair
            arbitrary_lf_pol
            (arbitrary_pair arbitrary_lf_pol arbitrary_lf_pol))
          NetKAT_Arbitrary.arbitrary_tcp))
      (fun ((p,(q,r)),_) ->
        (string_of_policy p) ^ " + (" ^
        (string_of_policy q) ^ " + " ^ (string_of_policy r) ^ ")")
      testable_bool in
  let prop_compile_ok ((p, (q, r)), pkt) =
    let p = Seq(Filter(Test(Location(Physical 0l))), p) in
    let q = Seq(Filter(Test(Location(Physical 0l))), q) in
    let r = Seq(Filter(Test(Location(Physical 0l))), r) in
    let open Semantics in
    PacketSet.compare
      (eval pkt (Union(p, (Union (q, r)))))
      (eval pkt (Union((Union(p, q)), r))) = 0 in
  let cfg = { QuickCheck.verbose with QuickCheck.maxTest = 1000 } in
  match QuickCheck.check testable_pol_pkt_to_bool cfg prop_compile_ok with
    QuickCheck.Success -> true
  | _ -> false

TEST "quickcheck ka-plus-comm" =
  let testable_pol_pkt_to_bool =
    let open QuickCheck in
    let open QuickCheck_gen in
    let open NetKAT_Arbitrary in
    let open Packet_Arbitrary in
    let open Packet in
    testable_fun
      (resize 11
        (arbitrary_pair
          (arbitrary_pair arbitrary_lf_pol arbitrary_lf_pol)
          NetKAT_Arbitrary.arbitrary_tcp))
      (fun ((p,q),_) -> string_of_policy p ^ " + " ^ string_of_policy q)
      testable_bool in
  let prop_compile_ok ((p, q), pkt) =
    let p = Seq(Filter(Test(Location(Physical 0l))), p) in
    let q = Seq(Filter(Test(Location(Physical 0l))), q) in
    let open Semantics in
    PacketSet.compare (eval pkt (Union(p, q))) (eval pkt (Union(q, p))) = 0 in
  let cfg = { QuickCheck.verbose with QuickCheck.maxTest = 1000 } in
  match QuickCheck.check testable_pol_pkt_to_bool cfg prop_compile_ok with
    QuickCheck.Success -> true
  | _ -> false

TEST "quickcheck ka-plus-zero" =
  let testable_pol_pkt_to_bool =
    let open QuickCheck in
    let open QuickCheck_gen in
    let open NetKAT_Arbitrary in
    let open Packet_Arbitrary in
    let open Packet in
    testable_fun
      (resize 11
        (arbitrary_pair arbitrary_lf_pol NetKAT_Arbitrary.arbitrary_tcp))
      (fun (p,_) -> string_of_policy p) testable_bool in
  let prop_compile_ok (pol, pkt) =
    let pol = Seq(Filter(Test(Location(Physical 0l))), pol) in
    let open Semantics in
    PacketSet.compare (eval pkt pol) (eval pkt (Union(pol, drop))) = 0 in
  let cfg = { QuickCheck.verbose with QuickCheck.maxTest = 1000 } in
  match QuickCheck.check testable_pol_pkt_to_bool cfg prop_compile_ok with
    QuickCheck.Success -> true
  | _ -> false

TEST "quickcheck ka-plus-idem" =
  let testable_pol_pkt_to_bool =
    let open QuickCheck in
    let open QuickCheck_gen in
    let open NetKAT_Arbitrary in
    let open Packet_Arbitrary in
    let open Packet in
    testable_fun
      (resize 11
        (arbitrary_pair arbitrary_lf_pol NetKAT_Arbitrary.arbitrary_tcp))
      (fun (p,_) -> string_of_policy p)
      testable_bool in
  let prop_compile_ok (pol, pkt) =
    let pol = Seq(Filter(Test(Location(Physical 0l))), pol) in
    let open Semantics in
    PacketSet.compare (eval pkt (Union(pol, pol))) (eval pkt pol) = 0 in
  let cfg = { QuickCheck.verbose with QuickCheck.maxTest = 1000 } in
  match QuickCheck.check testable_pol_pkt_to_bool cfg prop_compile_ok with
    QuickCheck.Success -> true
  | _ -> false

TEST "quickcheck ka-seq-assoc" =
  let testable_pol_pkt_to_bool =
    let open QuickCheck in
    let open QuickCheck_gen in
    let open NetKAT_Arbitrary in
    let open Packet_Arbitrary in
    let open Packet in
    testable_fun
      (resize 11
        (arbitrary_pair
          (arbitrary_pair
            arbitrary_lf_pol
            (arbitrary_pair arbitrary_lf_pol arbitrary_lf_pol))
          NetKAT_Arbitrary.arbitrary_tcp))
      (fun ((p,(q,r)),_) ->
        (string_of_policy p) ^ " * (" ^
        (string_of_policy q) ^ " * " ^ (string_of_policy r) ^ ")")
      testable_bool in
  let prop_compile_ok ((p, (q, r)), pkt) =
    let p = Seq(Filter(Test(Location(Physical 0l))), p) in
    let q = Seq(Filter(Test(Location(Physical 0l))), q) in
    let r = Seq(Filter(Test(Location(Physical 0l))), r) in
    let open Semantics in
    PacketSet.compare
      (eval pkt (Seq(p, (Seq (q, r)))))
      (eval pkt (Seq((Seq(p, q)), r))) = 0 in
  let cfg = { QuickCheck.verbose with QuickCheck.maxTest = 1000 } in
  match QuickCheck.check testable_pol_pkt_to_bool cfg prop_compile_ok with
    QuickCheck.Success -> true
  | _ -> false

TEST "quickcheck ka-one-seq" =
  let testable_pol_pkt_to_bool =
    let open QuickCheck in
    let open QuickCheck_gen in
    let open NetKAT_Arbitrary in
    let open Packet_Arbitrary in
    let open Packet in
    testable_fun
      (resize 11
        (arbitrary_pair arbitrary_lf_pol NetKAT_Arbitrary.arbitrary_tcp))
      (fun (p,_) -> string_of_policy p) testable_bool in
  let prop_compile_ok (pol, pkt) =
    let pol = Seq(Filter(Test(Location(Physical 0l))), pol) in
    let open Semantics in
    PacketSet.compare (eval pkt pol) (eval pkt (Seq(id, pol))) = 0 in
  let cfg = { QuickCheck.verbose with QuickCheck.maxTest = 1000 } in
  match QuickCheck.check testable_pol_pkt_to_bool cfg prop_compile_ok with
    QuickCheck.Success -> true
  | _ -> false

TEST "quickcheck ka-seq-one" =
  let testable_pol_pkt_to_bool =
    let open QuickCheck in
    let open QuickCheck_gen in
    let open NetKAT_Arbitrary in
    let open Packet_Arbitrary in
    let open Packet in
    testable_fun
      (resize 11
        (arbitrary_pair arbitrary_lf_pol NetKAT_Arbitrary.arbitrary_tcp))
      (fun (p,_) -> string_of_policy p) testable_bool in
  let prop_compile_ok (pol, pkt) =
    let pol = Seq(Filter(Test(Location(Physical 0l))), pol) in
    let open Semantics in
    PacketSet.compare (eval pkt pol) (eval pkt (Seq(pol, id))) = 0 in
  let cfg = { QuickCheck.verbose with QuickCheck.maxTest = 1000 } in
  match QuickCheck.check testable_pol_pkt_to_bool cfg prop_compile_ok with
    QuickCheck.Success -> true
  | _ -> false

TEST "quickcheck ka-seq-dist-l" =
  let testable_pol_pkt_to_bool =
    let open QuickCheck in
    let open QuickCheck_gen in
    let open NetKAT_Arbitrary in
    let open Packet_Arbitrary in
    let open Packet in
    testable_fun
      (resize 11
        (arbitrary_pair
          (arbitrary_pair
            arbitrary_lf_pol
            (arbitrary_pair arbitrary_lf_pol arbitrary_lf_pol))
          NetKAT_Arbitrary.arbitrary_tcp))
      (fun ((p,(q,r)),_) ->
        (string_of_policy p) ^ " * (" ^
        (string_of_policy q) ^ " + " ^ (string_of_policy r) ^ ")")
      testable_bool in
  let prop_compile_ok ((p, (q, r)), pkt) =
    let p = Seq(Filter(Test(Location(Physical 0l))), p) in
    let q = Seq(Filter(Test(Location(Physical 0l))), q) in
    let r = Seq(Filter(Test(Location(Physical 0l))), r) in
    let open Semantics in
    PacketSet.compare
      (eval pkt (Seq(p, (Union (q, r)))))
      (eval pkt (Union ((Seq(p, q)), (Seq(p, r))))) = 0 in
  let cfg = { QuickCheck.verbose with QuickCheck.maxTest = 1000 } in
  match QuickCheck.check testable_pol_pkt_to_bool cfg prop_compile_ok with
    QuickCheck.Success -> true
  | _ -> false

TEST "quickcheck ka-seq-dist-r" =
  let testable_pol_pkt_to_bool =
    let open QuickCheck in
    let open QuickCheck_gen in
    let open NetKAT_Arbitrary in
    let open Packet_Arbitrary in
    let open Packet in
    testable_fun
      (resize 11
        (arbitrary_pair
          (arbitrary_pair
            arbitrary_lf_pol
            (arbitrary_pair arbitrary_lf_pol arbitrary_lf_pol))
          NetKAT_Arbitrary.arbitrary_tcp))
      (fun ((p,(q,r)),_) ->
        "(" ^ (string_of_policy p) ^ " + " ^ (string_of_policy q) ^ ") * "
        ^ (string_of_policy r))
      testable_bool in
  let prop_compile_ok ((p, (q, r)), pkt) =
    let p = Seq(Filter(Test(Location(Physical 0l))), p) in
    let q = Seq(Filter(Test(Location(Physical 0l))), q) in
    let r = Seq(Filter(Test(Location(Physical 0l))), r) in
    let open Semantics in
    PacketSet.compare
      (eval pkt (Seq (Union(p, q), r)))
      (eval pkt (Union (Seq(p, r), Seq(q, r)))) = 0 in
  let cfg = { QuickCheck.verbose with QuickCheck.maxTest = 1000 } in
  match QuickCheck.check testable_pol_pkt_to_bool cfg prop_compile_ok with
    QuickCheck.Success -> true
  | _ -> false

TEST "quickcheck ka-zero-seq" =
  let testable_pol_pkt_to_bool =
    let open QuickCheck in
    let open QuickCheck_gen in
    let open NetKAT_Arbitrary in
    let open Packet_Arbitrary in
    let open Packet in
    testable_fun
      (resize 11
        (arbitrary_pair arbitrary_lf_pol NetKAT_Arbitrary.arbitrary_tcp))
      (fun (p,_) -> string_of_policy p) testable_bool in
  let prop_compile_ok (pol, pkt) =
    let pol = Seq(Filter(Test(Location(Physical 0l))), pol) in
    let open Semantics in
    PacketSet.compare (eval pkt drop) (eval pkt (Seq(drop, pol))) = 0 in
  let cfg = { QuickCheck.verbose with QuickCheck.maxTest = 1000 } in
  match QuickCheck.check testable_pol_pkt_to_bool cfg prop_compile_ok with
    QuickCheck.Success -> true
  | _ -> false

TEST "quickcheck ka-seq-zero" =
  let testable_pol_pkt_to_bool =
    let open QuickCheck in
    let open QuickCheck_gen in
    let open NetKAT_Arbitrary in
    let open Packet_Arbitrary in
    let open Packet in
    testable_fun
      (resize 11
        (arbitrary_pair arbitrary_lf_pol NetKAT_Arbitrary.arbitrary_tcp))
      (fun (p,_) -> string_of_policy p) testable_bool in
  let prop_compile_ok (pol, pkt) =
    let pol = Seq(Filter(Test(Location(Physical 0l))), pol) in
    let open Semantics in
    PacketSet.compare (eval pkt drop) (eval pkt (Seq(pol, drop))) = 0 in
  let cfg = { QuickCheck.verbose with QuickCheck.maxTest = 1000 } in
  match QuickCheck.check testable_pol_pkt_to_bool cfg prop_compile_ok with
    QuickCheck.Success -> true
  | _ -> false

TEST "quickcheck ka-unroll-l" =
  let testable_pol_pkt_to_bool =
    let open QuickCheck in
    let open QuickCheck_gen in
    let open NetKAT_Arbitrary in
    let open Packet_Arbitrary in
    let open Packet in
    testable_fun
      (resize 11
        (arbitrary_pair arbitrary_lf_pol NetKAT_Arbitrary.arbitrary_tcp))
      (fun (p,_) -> string_of_policy p) testable_bool in
  let prop_compile_ok (pol, pkt) =
    let pol = Seq(Filter(Test(Location(Physical 0l))), pol) in
    let open Semantics in
    PacketSet.compare
      (eval pkt (Star pol))
      (eval pkt (Union(id, Seq(pol, Star pol)))) = 0 in
  let cfg = { QuickCheck.verbose with QuickCheck.maxTest = 1000 } in
  match QuickCheck.check testable_pol_pkt_to_bool cfg prop_compile_ok with
    QuickCheck.Success -> true
  | _ -> false

TEST "quickcheck ka-lfp-l" =
  let testable_pol_pkt_to_bool =
    let open QuickCheck in
    let open QuickCheck_gen in
    let open NetKAT_Arbitrary in
    let open Packet_Arbitrary in
    let open Packet in
    testable_fun
      (resize 11
        (arbitrary_pair
          (arbitrary_pair
            arbitrary_lf_pol
            (arbitrary_pair arbitrary_lf_pol arbitrary_lf_pol))
          NetKAT_Arbitrary.arbitrary_tcp))
      (fun ((p,(q,r)),_) ->
        (string_of_policy p) ^ " , " ^ (string_of_policy q) ^ " , "
        ^ (string_of_policy r))
      testable_bool in
  let prop_compile_ok ((p, (q, r)), pkt) =
    let p = Seq(Filter(Test(Location(Physical 0l))), p) in
    let q = Seq(Filter(Test(Location(Physical 0l))), q) in
    let r = Seq(Filter(Test(Location(Physical 0l))), r) in
    let open Semantics in
    let lhs =
      PacketSet.compare
        (eval pkt (Union(Union(q, Seq (p, r)), r)))
        (eval pkt r) in
    let rhs =
      PacketSet.compare
        (eval pkt (Union(Seq(Star p, q), r)))
        (eval pkt r) in
    (lhs != 0) || (rhs = 0) in
  let cfg = { QuickCheck.verbose with QuickCheck.maxTest = 1000 } in
  match QuickCheck.check testable_pol_pkt_to_bool cfg prop_compile_ok with
    QuickCheck.Success -> true
  | _ -> false

TEST "quickcheck ka-unroll-r" =
  let testable_pol_pkt_to_bool =
    let open QuickCheck in
    let open QuickCheck_gen in
    let open NetKAT_Arbitrary in
    let open Packet_Arbitrary in
    let open Packet in
    testable_fun
      (resize 11
        (arbitrary_pair arbitrary_lf_pol NetKAT_Arbitrary.arbitrary_tcp))
      (fun (p,_) -> string_of_policy p) testable_bool in
  let prop_compile_ok (pol, pkt) =
    let pol = Seq(Filter(Test(Location(Physical 0l))), pol) in
    let open Semantics in
    PacketSet.compare
      (eval pkt (Star pol))
      (eval pkt (Union(id, Seq(Star pol, pol)))) = 0 in
  let cfg = { QuickCheck.verbose with QuickCheck.maxTest = 1000 } in
  match QuickCheck.check testable_pol_pkt_to_bool cfg prop_compile_ok with
    QuickCheck.Success -> true
  | _ -> false

TEST "quickcheck ka-lfp-r" =
  let testable_pol_pkt_to_bool =
    let open QuickCheck in
    let open QuickCheck_gen in
    let open NetKAT_Arbitrary in
    let open Packet_Arbitrary in
    let open Packet in
    testable_fun
      (resize 11
        (arbitrary_pair
          (arbitrary_pair
            arbitrary_lf_pol
            (arbitrary_pair arbitrary_lf_pol arbitrary_lf_pol))
          NetKAT_Arbitrary.arbitrary_tcp))
      (fun ((p,(q,r)),_) ->
        (string_of_policy p) ^ " , " ^ (string_of_policy q) ^ " , "
        ^ (string_of_policy r))
      testable_bool in
  let prop_compile_ok ((p, (q, r)), pkt) =
    let p = Seq(Filter(Test(Location(Physical 0l))), p) in
    let q = Seq(Filter(Test(Location(Physical 0l))), q) in
    let r = Seq(Filter(Test(Location(Physical 0l))), r) in
    let open Semantics in
    let lhs =
      PacketSet.compare
        (eval pkt (Union(Union(p, Seq (q, r)), q)))
        (eval pkt q) in
    let rhs =
      PacketSet.compare
        (eval pkt (Union(Seq(p, Star r), q)))
        (eval pkt q) in
    (lhs != 0) || (rhs = 0) in
  let cfg = { QuickCheck.verbose with QuickCheck.maxTest = 1000 } in
  match QuickCheck.check testable_pol_pkt_to_bool cfg prop_compile_ok with
    QuickCheck.Success -> true
  | _ -> false

(* TEST "quickcheck local compiler" = *)
(*   let testable_pol_pkt_to_bool = *)
(*     let open QuickCheck in *)
(*     let open QCGen in *)
(*     testable_fun *)
(*       (resize 3 *)
(*        (NetKATArb.arbitrary_policy >>= fun pol -> *)
(*           NetKATArb.arbitrary_packet >>= fun pkt -> *)
(*             Format.eprintf "Policy: %s\n%!" (NetKAT.string_of_policy pol); *)
(*             ret_gen (pol, pkt))) *)
(*       (fun (pol,pkt) -> NetKAT.string_of_policy pol) *)
(*       testable_bool in *)
(*   let prop_compile_ok (pol, pkt) = *)
(*     let open NetKAT in *)
(*     NetKAT.PacketSetSet.compare *)
(*       (NetKAT.eval pkt pol) *)
(*       (NetKAT.eval pkt (LocalCompiler.Local.to_netkat (LocalCompiler.Local.of_policy pol))) = 0 in *)
(*   let cfg = { QuickCheck.verbose with QuickCheck.maxTest = 1000 } in *)
(*   match QuickCheck.check testable_pol_pkt_to_bool cfg prop_compile_ok with *)
(*     | QuickCheck.Success -> true *)
(*     | _ -> failwith "quickchecking failed" *)
