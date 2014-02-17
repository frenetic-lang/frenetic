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

module FromPipe = struct
  open Core.Std

  module PipeSet = Set.Make(struct
    type t = string with sexp
    let compare = String.compare
  end)

  let test_from_pipes pol pkt pipes =
    let t = LocalCompiler.of_policy 0L pol in
    let p1 = PipeSet.of_list pipes in
    let p2 = PipeSet.of_list (List.map ~f:fst (LocalCompiler.from_pipes t pkt)) in
    PipeSet.equal p1 p2

  let default_packet headers= {
    switch = 0L;
    headers;
    payload = SDN_Types.NotBuffered (Cstruct.create 0)
  }

  TEST "all to controller" =
    let pol = Mod(Location(Pipe("all"))) in
    let pkt = default_packet Headers.empty in
    test_from_pipes pol pkt ["all"]

  TEST "all to controller, twice" =
    let pol = Union(
                Mod(Location(Pipe("all1"))),
                Mod(Location(Pipe("all2")))) in
    let pkt = default_packet Headers.empty in
    test_from_pipes pol pkt ["all1"; "all2"]

  TEST "ambiguous pipes" =
    let pol = Seq(Filter(Test(EthDst 2L)),
                  Union(Seq(Mod(EthDst 3L),
                            Mod(Location(Pipe("pipe1")))),
                        Seq(Mod(EthSrc 3L),
                            Mod(Location(Pipe("pipe2")))))) in
    let pkt = default_packet (Headers.mk_ethDst 2L) in
    test_from_pipes pol pkt ["pipe2"; "pipe1"]

  TEST "left side" =
    let pol = Union(
                Seq(Filter(Test(EthSrc 1L)),
                    Mod(Location(Pipe("left")))),
                Seq(Filter(Test(EthSrc 2L)),
                    Mod(Location(Pipe("right"))))) in
    let pkt = default_packet (Headers.mk_ethSrc 1L) in
    test_from_pipes pol pkt ["left"]

  TEST "right side" =
    let pol = Union(
                Seq(Filter(Test(EthSrc 1L)),
                    Mod(Location(Pipe("left")))),
                Seq(Filter(Test(EthSrc 2L)),
                    Mod(Location(Pipe("right"))))) in
    let pkt = default_packet (Headers.mk_ethSrc 2L) in
    test_from_pipes pol pkt ["right"]
end

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
