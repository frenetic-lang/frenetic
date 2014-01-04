module QCGen = QuickCheck_gen

open SDN_Types
open Types
open Pretty

let test_compile lhs rhs =
  let rhs' =
    LocalCompiler.to_netkat
      (LocalCompiler.of_policy (VInt.Int64 0L) lhs) in
  if rhs' = rhs then
    true
  else
    (Format.printf "compile @,%a@, produced %a@,,@,expected %a\n%!"
       format_policy lhs format_policy rhs' format_policy rhs;
     false)

let test_compile_table pol tbl = 
  let open LocalCompiler in 
  let tbl' = to_table (compile (VInt.Int64 0L) pol) in
  if tbl = tbl' then 
    true
  else
    (Format.printf "compile @,%a@, produced %a@,,@,expected %a\n%!"
       format_policy pol format_flowTable tbl' format_flowTable tbl;
     false)

let ite (pred : pred) (then_pol : policy) (else_pol : policy) : policy =
  Par (Seq (Filter pred, then_pol), Seq (Filter (Neg pred), else_pol))

let testSrc n = Test (Header EthSrc, VInt.Int64 (Int64.of_int n))
let testDst n = Test (Header EthDst, VInt.Int64 (Int64.of_int n))
let modSrc n = Mod (Header EthSrc, VInt.Int64 (Int64.of_int n))
let modDst n = Mod (Header EthDst, VInt.Int64 (Int64.of_int n))

TEST "compile drop" =
  test_compile (Filter False) (Filter False)

TEST "compile test" =
  let pr = testSrc 0 in
  test_compile (Filter pr) (Filter pr)

TEST "compile negation" =
  let pr = testSrc 0 in
  test_compile (Filter (Neg pr)) (Filter (Neg pr))

TEST "compile negation of conjunction" =
  let pr = And (testSrc 0, testDst 0) in
  test_compile
    (Filter (Neg pr))
    (Filter (Neg pr))

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
    (Par(modSrc 1,
	 ite
	   (testSrc 1)
	   (modSrc 2)
	   (modSrc 3)))
    (ite
       (testSrc 1)
       (Par (modSrc 1,
	     modSrc 2))
       (Par (modSrc 1,
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
    (Par (Filter True, modSrc 1))

TEST "star modify2" =
  test_compile
    (Star (Par (modSrc 0,
	        ite (testSrc 0) (modSrc 1) (modSrc 2))))
    (ite
       (testSrc 0)
       (Par (Par (Par (Filter True, modSrc 0), modSrc 1), modSrc 2))
       (Par (Par (Par (Filter True, modSrc 0), modSrc 1), modSrc 2)))

(*
TEST "policy that caused stack overflow on 10/16/2013" =
  test_compile
    (Par (Seq (Filter (Or (Test (Dst, 1), And (Test (Dst, 1), Test (Src, 0)))),
            Par (Mod (Dst, 0), Filter (And (Or (Test (Src, 2), Test (Dst, 1)),
                                          Test (Dst, 0))))),
         Seq (drop, Mod (Src, 1))))
    id *)

(*  Src -> A ; (filter Src = C + Dst -> C) *)
TEST "quickcheck failure on 10/16/2013" =
  test_compile
    (Seq (modSrc 0, Par (Filter (testSrc 2), modDst 2)))
    (Seq (modDst 2, modSrc 0))

TEST "choice1" =
  test_compile
    (Choice (modSrc 0, modSrc 1))
    (Choice (modSrc 0, modSrc 1))

TEST "choice2" =
  test_compile
    (Seq (Filter (testSrc 2), Choice (modSrc 0, modSrc 1)))
    (Seq (Filter (testSrc 2), Choice (modSrc 0, modSrc 1)))

TEST "choice3" = 
   test_compile
     (Par (Seq (Filter (testSrc 3), Choice (modSrc 0, modSrc 1)),
           Par (Seq (Filter (testSrc 0), modSrc 2),
		Seq (Filter (testSrc 1), modSrc 2))))
     (Par(Seq(Filter (testSrc 0), modSrc 2),
	  Par(Seq(Filter (testSrc 1), modSrc 2),
	      Seq(Filter (testSrc 3), 
		  Choice (modSrc 0, modSrc 1)))))

TEST "regression test for sequencing choice with id" = 
  test_compile
    (Seq(Choice (modSrc 0, modSrc 1), Filter True))
    (Choice (modSrc 0, modSrc 1))
    
TEST "vlan" =
  let test_vlan_none = Test (Header Vlan, VInt.Int16 0xFFF) in
  let mod_vlan_none = Mod (Header Vlan, VInt.Int16 0xFFF) in
  let mod_port1 = Mod (Header InPort, VInt.Int16 1) in 
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
