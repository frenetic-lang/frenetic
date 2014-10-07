open OUnitHack
module QCGen = QuickCheck_gen
open SDN_Types
open NetKAT_Types
open NetKAT_Pretty

let test_compile lhs rhs =
  let tbl = NetKAT_LocalCompiler.compile 0L lhs in
  let rhs' = NetKAT_LocalCompiler.to_policy tbl in
  if rhs' = rhs then
    true
  else
    (Format.printf "compile @,%a@, produced %a@,,@,expected %a\n%!%s\n%!"
       format_policy lhs format_policy rhs' format_policy rhs
       (NetKAT_LocalCompiler.to_string tbl);
     false)

let test_compile_table pol tbl =
  let open NetKAT_LocalCompiler in
  let tbl' = to_table (compile 0L pol) in
  if tbl = tbl' then
    true
  else
    (Format.printf "compile @,%a@, produced %a@,,@,expected %a\n%!%s\n"
       format_policy pol format_flowTable tbl' format_flowTable tbl
       (string_of_flowTable tbl);
     false)

let ite (pred : pred) (then_pol : policy) (else_pol : policy) : policy =
  Union (Seq (Filter (Neg pred), else_pol), Seq (Filter pred, then_pol))

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
    (Union (Filter (Neg pr1), (Filter(And(pr1, Neg pr2)))))

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

(* If a policy has a pipe location in a predicate, it should fail to compile. *)
(* JNF: unless that pipe is determined elsewhere (here the earlier filter port = 0) *)
(* TEST "quickcheck failure on 8/25/2014" = *)
(*   let b = "filter port = 0; (ethDst := fb:40:e5:6b:a8:f8; (ipSrc := 126.42.191.208 | ethTyp := 0x1464 | (filter ipDst = 155.173.129.111/22 | id); filter ipDst = 121.178.114.15/11 and port = __))" in *)
(*   try *)
(*     let _ = NetKAT_LocalCompiler.(to_table (compile 0L *)
(*       (NetKAT_Parser.program NetKAT_Lexer.token (Lexing.from_string b)))) in *)
(*     false *)
(*   with _ -> true *)

(* If a policy has a pipe location in a predicate, it should fail to compile. *)
(* JNF: unless that pipe is determined elsewhere (here the earlier filter port = 0) *)
TEST "indeterminate pipe" =
  let b = "filter port = __; ethDst := fb:40:e5:6b:a8:f8" in
  try
    let _ = NetKAT_LocalCompiler.(to_table (compile 0L
      (NetKAT_Parser.program NetKAT_Lexer.token (Lexing.from_string b)))) in
    false
  with _ -> true

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

(* Regression test for bug in expand_rules fixed on
   03/18/2014. The bug in the helper function that computes a
   cross product of the boolean tables produced by expanding each
   pattern -- the accumulator was being ignored, which is
   bogus. This test tickles the bug by simply compiling a
   predicate with two negated tests. *)
TEST "expand_rules" =
   let flow p a = { pattern = p; action = [a]; cookie = 0L; idle_timeout = Permanent; hard_timeout= Permanent } in
   let dropEthSrc v = flow { Pattern.match_all with Pattern.dlSrc = Some(v) } [] in
   let pol = Seq(Filter (And (Neg(Test(EthSrc 0L)), Neg(Test(EthSrc 1L)))),
                 Mod (Location (Physical 1l))) in
   (* Not testing the table itself because this is (a) tedious and (b) not stable. *)
   let a = [Output(Physical 1l)] in
   test_compile_table pol
     [ dropEthSrc 0L;
       dropEthSrc 1L;
       flow Pattern.match_all [a]]

module FromPipe = struct
  open Core.Std

  module PipeSet = Set.Make(struct
    type t = string with sexp
    let compare = String.compare
  end)

  let test_from_pipes pol pkt pipes =
    let ps, _, _ = NetKAT_Semantics.eval_pipes pkt pol in
    PipeSet.(equal (of_list pipes) (of_list (List.map ~f:fst ps)))

  let default_headers =
    let open NetKAT_Types.HeadersValues in
    { location = Physical 0l;
      ethSrc = 0L;
      ethDst = 0L;
      vlan = 0;
      vlanPcp = 0;
      ethType = 0;
      ipProto = 0;
      ipSrc = 0l;
      ipDst = 0l;
      tcpSrcPort = 0;
      tcpDstPort = 0; }

  let default_packet headers =
    { switch = 0L;
      headers;
      payload = SDN_Types.NotBuffered (Cstruct.create 0)
    }

TEST "all to controller" =
  let pol = Mod(Location(Pipe("all"))) in
  let pkt = default_packet default_headers in
  test_from_pipes pol pkt ["all"]

TEST "all to controller, twice" =
  let pol = Union(
    Mod(Location(Pipe("all1"))),
    Mod(Location(Pipe("all2")))) in
  let pkt = default_packet default_headers in
  test_from_pipes pol pkt ["all1"; "all2"]

TEST "ambiguous pipes" =
  let pol = Seq(Filter(Test(EthDst 2L)),
                Union(Seq(Mod(EthDst 3L),
                          Mod(Location(Pipe("pipe1")))),
                      Seq(Mod(EthSrc 3L),
                          Mod(Location(Pipe("pipe2")))))) in
  let open NetKAT_Types.HeadersValues in
   let pkt = default_packet { default_headers
                              with ethDst = 2L } in
   test_from_pipes pol pkt ["pipe2"; "pipe1"]

TEST "left side" =
  let pol = Union(
    Seq(Filter(Test(EthSrc 1L)),
        Mod(Location(Pipe("left")))),
    Seq(Filter(Test(EthSrc 2L)),
        Mod(Location(Pipe("right"))))) in
  let open NetKAT_Types.HeadersValues in
  let pkt = default_packet { default_headers
                             with ethSrc = 1L } in
  test_from_pipes pol pkt ["left"]

TEST "right side" =
  let pol = Union(
    Seq(Filter(Test(EthSrc 1L)),
        Mod(Location(Pipe("left")))),
    Seq(Filter(Test(EthSrc 2L)),
        Mod(Location(Pipe("right"))))) in
  let open NetKAT_Types.HeadersValues in
      let pkt = default_packet { default_headers
                                 with ethSrc = 2L } in
      test_from_pipes pol pkt ["right"]
end

let fix_port pol =
  Seq(Filter(Test(Location(Physical 0l))), pol)

let gen_pkt =
  let open QuickCheck in
  let open QuickCheck_gen in
  let open NetKAT_Arbitrary in
  let open Arbitrary_Packet in
  let open Packet in
  testable_fun (NetKAT_Arbitrary.arbitrary_tcp >>= fun pkt -> ret_gen pkt)
    (fun pkt -> NetKAT_Types.(HeadersValues.to_string pkt.headers))
    testable_bool

let gen_pol_1 =
  let open QuickCheck in
  let open QuickCheck_gen in
  let open NetKAT_Arbitrary in
  let open Arbitrary_Packet in
  let open Packet in
  testable_fun
    (arbitrary_lf_pol >>= fun p ->
     NetKAT_Arbitrary.arbitrary_tcp >>= fun packet ->
     ret_gen (fix_port p, packet))
    (fun (p,_) -> string_of_policy p)
    testable_bool

let gen_pol_2 =
  let open QuickCheck in
  let open QuickCheck_gen in
  let open NetKAT_Arbitrary in
  let open Arbitrary_Packet in
  let open Packet in
    testable_fun
      (arbitrary_lf_pol >>= fun p ->
       arbitrary_lf_pol >>= fun q ->
       NetKAT_Arbitrary.arbitrary_tcp >>= fun packet ->
       ret_gen (fix_port p, fix_port q, packet))
      (fun (p,q,_) -> (string_of_policy p) ^ " " ^ (string_of_policy q))
      testable_bool

let gen_pol_3 =
  let open QuickCheck in
  let open QuickCheck_gen in
  let open NetKAT_Arbitrary in
  let open Arbitrary_Packet in
  let open Packet in
  testable_fun
    (arbitrary_lf_pol >>= fun p ->
     arbitrary_lf_pol >>= fun q ->
     arbitrary_lf_pol >>= fun r ->
     NetKAT_Arbitrary.arbitrary_tcp >>= fun packet ->
     ret_gen (fix_port p, fix_port q, fix_port r, packet))
    (fun (p,q,r,_) ->
      (string_of_policy p) ^ " " ^ (string_of_policy q) ^ " "
      ^ (string_of_policy r))
    testable_bool

let compare_eval_output p q pkt =
  let open NetKAT_Semantics in
  PacketSet.compare (eval pkt p) (eval pkt q) = 0

let compare_compiler_output p q pkt =
  PacketSet.compare
    (Flowterp.Packet.eval pkt (NetKAT_LocalCompiler.(to_table (compile pkt.switch p))))
    (Flowterp.Packet.eval pkt (NetKAT_LocalCompiler.(to_table (compile pkt.switch q))))
  = 0

let check gen_fn compare_fn =
  let cfg = { QuickCheck.quick with QuickCheck.maxTest = 1000 } in
  match QuickCheck.check gen_fn cfg compare_fn with
        QuickCheck.Success -> true
    | _                  -> false

let get_masking_test =
  let ip1 = Int32.of_int(192 * 256*256*256 + 168 * 256*256 + 0 * 256 + 1 * 1) in
  let ip2 = Int32.of_int(192 * 256*256*256 + 168 * 256*256 + 0 * 256 + 5 * 1) in
  let filter_pol_of_ip ip =
    Seq(Filter(Test(IP4Src(ip, 24l))), Mod(Location(Physical 1l))) in
  let headers =
    { HeadersValues.location = NetKAT_Types.Physical 0l
    ; ethSrc = 0L ; ethDst = 0L ; vlan = 0 ; vlanPcp = 0 ; ethType = 0
    ; ipProto = 0 ; ipSrc = ip1 ; ipDst = 0l ; tcpSrcPort = 0 ; tcpDstPort = 0
    } in
  let payload = SDN_Types.NotBuffered (Cstruct.create 0) in
  let pkt = {switch = 0L; headers = headers; payload = payload} in
  (filter_pol_of_ip ip1, filter_pol_of_ip ip2, pkt)

TEST "ip masking eval" =
  let (pol1, pol2, pkt) = get_masking_test in
  compare_eval_output pol1 pol2 pkt

TEST "ip masking compile" =
  let (pol1, pol2, pkt) = get_masking_test in
  compare_compiler_output pol1 pol2 pkt

(* regression test for bug in flowterp handling of patterns with IP mask 0 *)
TEST "zero mask" =
  let prop_compile_ok (pkt) =
    let pol = Seq(Filter(Test(Location(Physical 0l))),
                  Filter(Test(IP4Dst(0l,0l)))) in
    PacketSet.compare
      (NetKAT_Semantics.eval pkt (Optimize.specialize_policy pkt.switch pol))
      (Flowterp.Packet.eval pkt
         (NetKAT_LocalCompiler.(to_table (compile pkt.switch pol)))) = 0 in
  check gen_pkt prop_compile_ok

TEST "semantics agree with flowtable" =
  let prop_compile_ok (p, pkt) =
    (* XXX(seliopou): Because flowtables are not pipe-aware, policies that set
     * the packet location to a pipe will not pass this test. To side-step the
     * problem, set the location to physical port 0 on the way out.
     *)
    let p' = Seq(p, Mod(Location(Physical(0l)))) in
    PacketSet.compare
      (NetKAT_Semantics.eval pkt (Optimize.specialize_policy pkt.switch p'))
      (Flowterp.Packet.eval pkt
        (NetKAT_LocalCompiler.(to_table (compile pkt.switch p'))))
    = 0 in
  check gen_pol_1 prop_compile_ok

TEST "quickcheck ka-plus-assoc compiler" =
  let prop_compile_ok (p, q, r, pkt) =
    compare_compiler_output
      (Union(p,(Union(q,r))))
      (Union((Union(p,q)), r))
      pkt in
  check gen_pol_3 prop_compile_ok

TEST "quickcheck ka-plus-assoc eval" =
  let prop_compile_ok (p, q, r, pkt) =
    compare_eval_output
      (Union(p, (Union (q, r))))
      (Union((Union(p, q)), r))
      pkt in
  check gen_pol_3 prop_compile_ok

TEST "quickcheck ka-plus-comm compiler" =
  let prop_compile_ok (p, q, pkt) =
    compare_compiler_output (Union(p, q)) (Union(q, p)) pkt in
  check gen_pol_2 prop_compile_ok

TEST "quickcheck ka-plus-comm eval" =
  let prop_compile_ok (p, q, pkt) =
    compare_eval_output (Union(p, q)) (Union(q, p)) pkt in
  check gen_pol_2 prop_compile_ok

TEST "quickcheck ka-plus-zero compiler" =
  let prop_compile_ok (pol, pkt) =
    compare_compiler_output pol (Union(pol, drop)) pkt in
  check gen_pol_1 prop_compile_ok

TEST "quickcheck ka-plus-zero eval" =
  let prop_compile_ok (pol, pkt) =
    compare_eval_output pol (Union(pol, drop)) pkt in
  check gen_pol_1 prop_compile_ok

TEST "quickcheck ka-plus-idem compiler" =
  let prop_compile_ok (pol, pkt) =
    compare_compiler_output (Union(pol, pol)) pol pkt in
  check gen_pol_1 prop_compile_ok

TEST "quickcheck ka-plus-idem eval" =
  let prop_compile_ok (pol, pkt) =
    compare_eval_output (Union(pol, pol)) pol pkt in
  check gen_pol_1 prop_compile_ok

TEST "quickcheck ka-seq-assoc compiler" =
  let prop_compile_ok (p, q, r, pkt) =
    compare_compiler_output (Seq(p, (Seq (q, r)))) (Seq((Seq(p, q)), r)) pkt in
  check gen_pol_3 prop_compile_ok

TEST "quickcheck ka-seq-assoc eval" =
  let prop_compile_ok (p, q, r, pkt) =
    compare_eval_output (Seq(p, (Seq (q, r)))) (Seq((Seq(p, q)), r)) pkt in
  check gen_pol_3 prop_compile_ok

TEST "quickcheck ka-one-seq compiler" =
  let prop_compile_ok (pol, pkt) =
    compare_compiler_output pol (Seq(id, pol)) pkt in
  check gen_pol_1 prop_compile_ok

TEST "quickcheck ka-one-seq eval" =
  let prop_compile_ok (pol, pkt) =
    compare_eval_output pol (Seq(id, pol)) pkt in
  check gen_pol_1 prop_compile_ok

TEST "quickcheck ka-seq-one compiler" =
  let prop_compile_ok (pol, pkt) =
    compare_compiler_output pol (Seq(pol, id)) pkt in
  check gen_pol_1 prop_compile_ok

TEST "quickcheck ka-seq-one eval" =
  let prop_compile_ok (pol, pkt) =
    compare_eval_output pol (Seq(pol, id)) pkt in
  check gen_pol_1 prop_compile_ok

TEST "quickcheck ka-seq-dist-l compiler" =
  let prop_compile_ok (p, q, r, pkt) =
    compare_compiler_output
      (Seq(p, (Union (q, r)))) (Union ((Seq(p, q)), (Seq(p, r)))) pkt in
  check gen_pol_3 prop_compile_ok

TEST "quickcheck ka-seq-dist-l eval" =
  let prop_compile_ok (p, q, r, pkt) =
    compare_eval_output
      (Seq(p, (Union (q, r)))) (Union ((Seq(p, q)), (Seq(p, r)))) pkt in
  check gen_pol_3 prop_compile_ok

TEST "quickcheck ka-seq-dist-r compiler" =
  let prop_compile_ok (p, q, r, pkt) =
    compare_compiler_output
      (Seq (Union(p, q), r)) (Union (Seq(p, r), Seq(q, r))) pkt in
  check gen_pol_3 prop_compile_ok

TEST "quickcheck ka-seq-dist-r eval" =
  let prop_compile_ok (p, q, r, pkt) =
    compare_eval_output
      (Seq (Union(p, q), r)) (Union (Seq(p, r), Seq(q, r))) pkt in
  check gen_pol_3 prop_compile_ok

TEST "quickcheck ka-zero-seq compiler" =
  let prop_compile_ok (pol, pkt) =
    compare_compiler_output drop (Seq(drop, pol)) pkt in
  check gen_pol_1 prop_compile_ok

TEST "quickcheck ka-zero-seq eval" =
  let prop_compile_ok (pol, pkt) =
    compare_eval_output drop (Seq(drop, pol)) pkt in
  check gen_pol_1 prop_compile_ok

TEST "quickcheck ka-seq-zero compiler" =
  let prop_compile_ok (pol, pkt) =
    compare_compiler_output drop (Seq(pol, drop)) pkt in
  check gen_pol_1 prop_compile_ok

TEST "quickcheck ka-seq-zero eval" =
  let prop_compile_ok (pol, pkt) =
    compare_eval_output drop (Seq(pol, drop)) pkt in
  check gen_pol_1 prop_compile_ok

TEST "quickcheck ka-unroll-l compiler" =
  let prop_compile_ok (pol, pkt) =
    compare_compiler_output (Star pol) (Union(id, Seq(pol, Star pol))) pkt in
  check gen_pol_1 prop_compile_ok

TEST "quickcheck ka-unroll-l eval" =
  let prop_compile_ok (pol, pkt) =
    compare_eval_output (Star pol) (Union(id, Seq(pol, Star pol))) pkt in
  check gen_pol_1 prop_compile_ok

TEST "quickcheck ka-lfp-l compiler" =
  let prop_compile_ok (p, q, r, pkt) =
    not (compare_compiler_output (Union(Union(q, Seq (p, r)), r)) r pkt)
    ||  (compare_compiler_output (Union(Seq(Star p, q), r)) r pkt) in
  check gen_pol_3 prop_compile_ok

TEST "quickcheck ka-lfp-l eval" =
  let prop_compile_ok (p, q, r, pkt) =
    not (compare_eval_output (Union(Union(q, Seq (p, r)), r)) r pkt)
    ||  (compare_eval_output (Union(Seq(Star p, q), r)) r pkt) in
  check gen_pol_3 prop_compile_ok

TEST "quickcheck ka-unroll-r compiler" =
  let prop_compile_ok (pol, pkt) =
    compare_compiler_output (Star pol) (Union(id, Seq(Star pol, pol))) pkt in
  check gen_pol_1 prop_compile_ok

TEST "quickcheck ka-unroll-r eval" =
  let prop_compile_ok (pol, pkt) =
    compare_eval_output (Star pol) (Union(id, Seq(Star pol, pol))) pkt in
  check gen_pol_1 prop_compile_ok

TEST "quickcheck ka-lfp-r compiler" =
  let prop_compile_ok (p, q, r, pkt) =
    not (compare_compiler_output (Union(Union(p, Seq (q, r)), q)) q pkt)
    ||  (compare_compiler_output (Union(Seq(p, Star r), q)) q pkt) in
  check gen_pol_3 prop_compile_ok

TEST "quickcheck ka-lfp-r eval" =
  let prop_compile_ok (p, q, r, pkt) =
    not (compare_eval_output (Union(Union(p, Seq (q, r)), q)) q pkt)
    ||  (compare_eval_output (Union(Seq(p, Star r), q)) q pkt) in
  check gen_pol_3 prop_compile_ok
