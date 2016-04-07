open Frenetic_OpenFlow
open Frenetic_NetKAT
open Frenetic_NetKAT_Pretty
open Frenetic_NetKAT_Compiler

let%test "Can test locations, even when they are set to pipes" =
  let p = Filter (Test (Location (Pipe "web"))) in
  let opt = { default_compiler_options with remove_tail_drops = false } in
  List.length (to_table 0L ~options:opt (compile_local ~options:opt p)) == 1 (* that drops everything *)

let%test "clearing cache fails" =
  let a = Test (IPProto 1) in
  let b = Test (EthType 0x800) in
  let fdd1 = compile_local (Filter a) in
  let compiler_options_empty = { default_compiler_options with cache_prepare = `Empty } in
  let compiler_options_keep = { default_compiler_options with cache_prepare = `Keep } in
  let fdd2 = compile_local ~options:compiler_options_empty (Filter b) in
  try
    seq fdd1 fdd2 != compile_local ~options:compiler_options_keep (Filter (And (a, b)))
  with
    Not_found -> true

let%test "keeping cache_prepare works" =
  let a = Test (IPProto 1) in
  let b = Test (EthType 0x800) in
  let fdd1 = compile_local (Filter a) in
  let compiler_options_keep = { default_compiler_options with cache_prepare = `Keep } in
  let fdd2 = compile_local ~options:compiler_options_keep (Filter b) in
  seq fdd1 fdd2 = compile_local ~options:compiler_options_keep (Filter (And (a, b)))

let%test "keeping reachable nodes in cache_prepare works" =
  let a = Test (IPProto 1) in
  let b = Test (EthType 0x800) in
  let fdd1 = compile_local (Filter a) in
  let compiler_options_keep = { default_compiler_options with cache_prepare = `Keep } in
  let compiler_options_preserve = { default_compiler_options with cache_prepare = `Preserve fdd1 } in
  let fdd2 = compile_local ~options:compiler_options_preserve (Filter b) in
  seq fdd1 fdd2 = compile_local ~options:compiler_options_keep (Filter (And (a, b)))

let ite (pred : pred) (then_pol : policy) (else_pol : policy) : policy =
  Union (Seq (Filter pred, then_pol), Seq (Filter (Neg pred), else_pol))

let testSrc n = Test (EthSrc (Int64.of_int n))
let testDst n = Test (EthDst (Int64.of_int n))
let modSrc n = Mod (EthSrc (Int64.of_int n))
let modDst n = Mod (EthDst (Int64.of_int n))

let test_compile lhs rhs =
  let tbl = Frenetic_NetKAT_Compiler.(restrict (Switch 0L) (compile_local lhs)) in
  let rhs' = Frenetic_NetKAT_Compiler.to_local_pol tbl in
  if rhs' = rhs then
    true
  else
    (Format.printf "compile @,%a@, produced %a@,,@,expected %a\n%!%s\n%!"
       format_policy lhs format_policy rhs' format_policy rhs
       (Frenetic_NetKAT_Compiler.to_string tbl);
     false)

let test_compile_table pol tbl =
  let open Frenetic_NetKAT_Compiler in
  let tbl' = to_table 0L (compile_local pol) in
  if string_of_flowTable tbl = string_of_flowTable tbl' then
    true
  else
    (Format.printf "compile @,%a@, produced\n%s@,\n@,expected\n%s%!\n"
       format_policy pol (string_of_flowTable tbl') (string_of_flowTable tbl);
     false)

let%test "compile drop" =
  test_compile (Filter False) (Filter False)

let%test "compile test" =
  let pr = testSrc 0 in
  test_compile (Filter pr) (Filter pr)

let%test "compile negation" =
  let pr = testSrc 0 in
  test_compile (Filter (Neg pr)) (Filter (Neg pr))

let%test "compile negation of conjunction" =
  let pr1 = testSrc 0 in
  let pr2 = testDst 0 in
  let pr = And (pr1, pr2) in
  test_compile
    (Filter (Neg pr))
    (Filter (Or(And(pr1, Neg pr2), Neg pr1)))

let%test "commute test annihilator" =
  test_compile
    (Seq (modSrc 1 , Filter (testSrc 0)))
    (Filter False)

let%test "commute test different fields" =
  test_compile
    (Seq (modSrc 1, Filter (testDst 0)))
    (Seq (Filter (testDst 0), modSrc 1))

(* trivial optimization possible *)
let%test "commute same field" =
  test_compile
    (Seq (modSrc 1, Filter (testSrc 1)))
    (modSrc 1)

(* trivial optimization possible *)
let%test "same field, two values = drop" =
  let pr1 = testSrc 1 in
  let pr2 = testSrc 0 in
  test_compile
    (Filter (And (pr1, pr2)))
    (Filter False)

let%test "par1" =
  test_compile
    (Union(modSrc 1,
         ite
           (testSrc 1)
           (modSrc 2)
           (modSrc 3)))
    (ite
       (testSrc 1)
       (Union (modSrc 2, modSrc 1))
       (Union (modSrc 3, modSrc 1)))

let%test "star id" =
  test_compile
    (Star (Filter True))
    (Filter True)

let%test "star drop" =
  test_compile
    (Star (Filter False))
    (Filter True)

let%test "star modify1" =
  test_compile
    (Star (modSrc 1))
    (Union (modSrc 1, Filter True))

let%test "star modify2" =
  test_compile
    (Star (Union (modSrc 0,
                ite (testSrc 0) (modSrc 1) (modSrc 2))))
     (Union (modSrc 2, Union(modSrc 1, Union(modSrc 0, Filter True))))

(*
let%test "policy that caused stack overflow on 10/16/2013" =
  test_compile
    (Union (Seq (Filter (Or (Test (Dst, 1), And (Test (Dst, 1), Test (Src, 0)))),
            Union (Mod (Dst, 0), Filter (And (Or (Test (Src, 2), Test (Dst, 1)),
                                          Test (Dst, 0))))),
         Seq (drop, Mod (Src, 1))))
    id *)

(*  Src -> A ; (filter Src = C + Dst -> C) *)
let%test "quickcheck failure on 10/16/2013" =
  test_compile
    (Seq (modSrc 0, Union (Filter (testSrc 2), modDst 2)))
    (Seq (modDst 2, modSrc 0))

(* If a policy has a pipe location in a predicate, it should fail to compile. *)
(* JNF: unless that pipe is determined elsewhere (here the earlier filter port = 0) *)
(* let%test "quickcheck failure on 8/25/2014" = *)
(*   let b = "filter port = 0; (ethDst := fb:40:e5:6b:a8:f8; (ipSrc := 126.42.191.208 | ethTyp := 0x1464 | (filter ipDst = 155.173.129.111/22 | id); filter ipDst = 121.178.114.15/11 and port = __))" in *)
(*   try *)
(*     let _ = Frenetic_NetKAT_Compiler.(to_table (compile 0L *)
(*       (NetKAT_Parser.program NetKAT_Lexer.token (Lexing.from_string b)))) in *)
(*     false *)
(*   with _ -> true *)

(* If a policy has a pipe location in a predicate, it should fail to compile. *)
(* JNF: unless that pipe is determined elsewhere (here the earlier filter port = 0) *)
(* Reimplement when parser is fixed
let%test "indeterminate pipe" =
  let b = "filter port = __; ethDst := fb:40:e5:6b:a8:f8" in
  try
    let _ = Frenetic_NetKAT_Compiler.(to_table 0L (compile
      (NetKAT_Parser.program NetKAT_Lexer.token (Lexing.from_string b)))) in
    false
  with _ -> true
*)
let%test "vlan" =
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
      (Seq (mod_port1, mod_vlan_none)) in
  test_compile pol pol'

(* Regression test for bug in expand_rules fixed on
   03/18/2014. The bug in the helper function that computes a
   cross product of the boolean tables produced by expanding each
   pattern -- the accumulator was being ignored, which is
   bogus. This test tickles the bug by simply compiling a
   predicate with two negated tests. *)
let%test "expand_rules" =
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
    type t = string [@@deriving sexp]
    let compare = String.compare
  end)

  let test_from_pipes pol pkt pipes =
    let ps, _, _ = Frenetic_NetKAT_Semantics.eval_pipes pkt pol in
    PipeSet.(equal (of_list pipes) (of_list (List.map ~f:fst ps)))

  let default_headers =
    let open Frenetic_NetKAT_Semantics.HeadersValues in
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
    let open Frenetic_NetKAT_Semantics in
    { switch = 0L;
      headers;
      payload = Frenetic_OpenFlow.NotBuffered (Cstruct.create 0)
    }

let%test "all to controller" =
  let pol = Mod(Location(Pipe("all"))) in
  let pkt = default_packet default_headers in
  test_from_pipes pol pkt ["all"]

let%test "all to controller, twice" =
  let pol = Union(
    Mod(Location(Pipe("all1"))),
    Mod(Location(Pipe("all2")))) in
  let pkt = default_packet default_headers in
  test_from_pipes pol pkt ["all1"; "all2"]

let%test "ambiguous pipes" =
  let pol = Seq(Filter(Test(EthDst 2L)),
                Union(Seq(Mod(EthDst 3L),
                          Mod(Location(Pipe("pipe1")))),
                      Seq(Mod(EthSrc 3L),
                          Mod(Location(Pipe("pipe2")))))) in
  let open Frenetic_NetKAT_Semantics.HeadersValues in
  let pkt = default_packet { default_headers
                             with ethDst = 2L } in
  test_from_pipes pol pkt ["pipe2"; "pipe1"]

let%test "left side" =
  let pol = Union(
    Seq(Filter(Test(EthSrc 1L)),
        Mod(Location(Pipe("left")))),
    Seq(Filter(Test(EthSrc 2L)),
        Mod(Location(Pipe("right"))))) in
  let open Frenetic_NetKAT_Semantics.HeadersValues in
  let pkt = default_packet { default_headers
                             with ethSrc = 1L } in
  test_from_pipes pol pkt ["left"]

let%test "right side" =
  let pol = Union(
    Seq(Filter(Test(EthSrc 1L)),
        Mod(Location(Pipe("left")))),
    Seq(Filter(Test(EthSrc 2L)),
        Mod(Location(Pipe("right"))))) in
  let open Frenetic_NetKAT_Semantics.HeadersValues in
      let pkt = default_packet { default_headers
                                 with ethSrc = 2L } in
      test_from_pipes pol pkt ["right"]
end

let fix_port pol =
  Seq(Filter(Test(Location(Physical 0l))), pol)

let gen_pkt =
  let open QuickCheck in
  let open QuickCheck_gen in
  let open Arbitrary_Frenetic_NetKAT in
  let open Arbitrary_Frenetic_Packet in
  let open Frenetic_Packet in
  testable_fun (Arbitrary_Frenetic_NetKAT.arbitrary_tcp >>= fun pkt -> ret_gen pkt)
    (fun pkt -> Frenetic_NetKAT_Semantics.(HeadersValues.to_string pkt.headers))
    testable_bool

let gen_pol_1 =
  let open QuickCheck in
  let open QuickCheck_gen in
  let open Arbitrary_Frenetic_NetKAT in
  let open Arbitrary_Frenetic_Packet in
  let open Frenetic_Packet in
  testable_fun
    (arbitrary_lf_pol >>= fun p ->
     Arbitrary_Frenetic_NetKAT.arbitrary_tcp >>= fun packet ->
     ret_gen (fix_port p, packet))
    (fun (p,_) -> string_of_policy p)
    testable_bool

let gen_pol_2 =
  let open QuickCheck in
  let open QuickCheck_gen in
  let open Arbitrary_Frenetic_NetKAT in
  let open Arbitrary_Frenetic_Packet in
  let open Frenetic_Packet in
    testable_fun
      (arbitrary_lf_pol >>= fun p ->
       arbitrary_lf_pol >>= fun q ->
       Arbitrary_Frenetic_NetKAT.arbitrary_tcp >>= fun packet ->
       ret_gen (fix_port p, fix_port q, packet))
      (fun (p,q,_) -> (string_of_policy p) ^ " " ^ (string_of_policy q))
      testable_bool

let gen_pol_3 =
  let open QuickCheck in
  let open QuickCheck_gen in
  let open Arbitrary_Frenetic_NetKAT in
  let open Arbitrary_Frenetic_Packet in
  let open Frenetic_Packet in
  testable_fun
    (arbitrary_lf_pol >>= fun p ->
     arbitrary_lf_pol >>= fun q ->
     arbitrary_lf_pol >>= fun r ->
     Arbitrary_Frenetic_NetKAT.arbitrary_tcp >>= fun packet ->
     ret_gen (fix_port p, fix_port q, fix_port r, packet))
    (fun (p,q,r,_) ->
      (string_of_policy p) ^ " " ^ (string_of_policy q) ^ " "
      ^ (string_of_policy r))
    testable_bool

let compare_eval_output p q pkt =
  let open Frenetic_NetKAT_Semantics in
  PacketSet.compare (eval pkt p) (eval pkt q) = 0

let compare_compiler_output p q pkt =
  let open Frenetic_NetKAT_Semantics in
  PacketSet.compare
    (Flowterp.Packet.eval pkt (Frenetic_NetKAT_Compiler.(to_table pkt.switch (compile_local p))))
    (Flowterp.Packet.eval pkt (Frenetic_NetKAT_Compiler.(to_table pkt.switch (compile_local q))))
  = 0

let check gen_fn compare_fn =
  let cfg = { QuickCheck.quick with QuickCheck.maxTest = 100 } in
  match QuickCheck.check gen_fn cfg compare_fn with
        QuickCheck.Success -> true
    | _                  -> false


(* let%test "quickcheck NetKAT <-> JSON" =
  let open Frenetic_NetKAT_Json in
  let open Frenetic_NetKAT_Optimize in
  let generate_policy_json =
    let open QuickCheck in
    let open QuickCheck_gen in
    let open Arbitrary_Frenetic_NetKAT in
    testable_fun
      arbitrary_lf_pol
      (fun p -> string_of_policy p ^ "\n" ^ policy_to_json_string p)
      testable_bool in
  let prop_parse_ok pol =
    try
      norm_policy (policy_from_json (policy_to_json pol)) = norm_policy pol
    with _ -> false in
  check generate_policy_json prop_parse_ok
let%test "json" =
  let open Frenetic_NetKAT_Optimize in
  let m1 = mk_mod
  let pol =
 *)

let get_masking_test =
  let ip1 = Int32.of_int(192 * 256*256*256 + 168 * 256*256 + 0 * 256 + 1 * 1) in
  let ip2 = Int32.of_int(192 * 256*256*256 + 168 * 256*256 + 0 * 256 + 5 * 1) in
  let filter_pol_of_ip ip =
    Seq(Filter(Test(IP4Src(ip, 24l))), Mod(Location(Physical 1l))) in
  let open Frenetic_NetKAT_Semantics in
  let headers =
    { HeadersValues.location = Frenetic_NetKAT.Physical 0l
    ; ethSrc = 0L ; ethDst = 0L ; vlan = 0 ; vlanPcp = 0 ; ethType = 0
    ; ipProto = 0 ; ipSrc = ip1 ; ipDst = 0l ; tcpSrcPort = 0 ; tcpDstPort = 0
    } in
  let payload = Frenetic_OpenFlow.NotBuffered (Cstruct.create 0) in
  let pkt = {switch = 0L; headers = headers; payload = payload} in
  (filter_pol_of_ip ip1, filter_pol_of_ip ip2, pkt)

let%test "ip masking eval" =
  let (pol1, pol2, pkt) = get_masking_test in
  compare_eval_output pol1 pol2 pkt

let%test "ip masking compile" =
  let (pol1, pol2, pkt) = get_masking_test in
  compare_compiler_output pol1 pol2 pkt

(* regression test for bug in Flowterp handling of patterns with IP mask 0 *)

let%test "zero mask" =
  let prop_compile_ok (pkt) =
    let open Frenetic_NetKAT_Semantics in
    let pol = Seq(Filter(Test(Location(Physical 0l))),
                  Filter(Test(IP4Dst(0l,0l)))) in
    let pol' = Frenetic_NetKAT_Optimize.specialize_policy pkt.switch pol in
    PacketSet.compare
      (Frenetic_NetKAT_Semantics.eval pkt pol')
      (Flowterp.Packet.eval pkt
         (Frenetic_NetKAT_Compiler.(to_table pkt.switch (compile_local pol)))) = 0 in
  check gen_pkt prop_compile_ok


let%test "semantics agree with flowtable" =
  let prop_compile_ok (p, pkt) =
    (* XXX(seliopou): Because flowtables are not pipe-aware, policies that set
     * the packet location to a pipe will not pass this test. To side-step the
     * problem, set the location to physical port 0 on the way out.
     *)
    let p' = Seq(p, Mod(Location(Physical(0l)))) in
    let open Frenetic_NetKAT_Semantics in
    PacketSet.compare
      (Frenetic_NetKAT_Semantics.eval pkt (Frenetic_NetKAT_Optimize.specialize_policy pkt.switch p'))
      (Flowterp.Packet.eval pkt
        (Frenetic_NetKAT_Compiler.(to_table pkt.switch (compile_local p'))))
    = 0 in
  check gen_pol_1 prop_compile_ok

let%test "quickcheck ka-plus-assoc compiler" =
  let prop_compile_ok (p, q, r, pkt) =
    compare_compiler_output
      (Union(p,(Union(q,r))))
      (Union((Union(p,q)), r))
      pkt in
  check gen_pol_3 prop_compile_ok

let%test "quickcheck ka-plus-assoc eval" =
  let prop_compile_ok (p, q, r, pkt) =
    compare_eval_output
      (Union(p, (Union (q, r))))
      (Union((Union(p, q)), r))
      pkt in
  check gen_pol_3 prop_compile_ok

let%test "quickcheck ka-plus-comm compiler" =
  let prop_compile_ok (p, q, pkt) =
    compare_compiler_output (Union(p, q)) (Union(q, p)) pkt in
  check gen_pol_2 prop_compile_ok

let%test "quickcheck ka-plus-comm eval" =
  let prop_compile_ok (p, q, pkt) =
    compare_eval_output (Union(p, q)) (Union(q, p)) pkt in
  check gen_pol_2 prop_compile_ok

let%test "quickcheck ka-plus-zero compiler" =
  let prop_compile_ok (pol, pkt) =
    compare_compiler_output pol (Union(pol, drop)) pkt in
  check gen_pol_1 prop_compile_ok

let%test "quickcheck ka-plus-zero eval" =
  let prop_compile_ok (pol, pkt) =
    compare_eval_output pol (Union(pol, drop)) pkt in
  check gen_pol_1 prop_compile_ok

let%test "quickcheck ka-plus-idem compiler" =
  let prop_compile_ok (pol, pkt) =
    compare_compiler_output (Union(pol, pol)) pol pkt in
  check gen_pol_1 prop_compile_ok

let%test "quickcheck ka-plus-idem eval" =
  let prop_compile_ok (pol, pkt) =
    compare_eval_output (Union(pol, pol)) pol pkt in
  check gen_pol_1 prop_compile_ok

let%test "quickcheck ka-seq-assoc compiler" =
  let prop_compile_ok (p, q, r, pkt) =
    compare_compiler_output (Seq(p, (Seq (q, r)))) (Seq((Seq(p, q)), r)) pkt in
  check gen_pol_3 prop_compile_ok

let%test "quickcheck ka-seq-assoc eval" =
  let prop_compile_ok (p, q, r, pkt) =
    compare_eval_output (Seq(p, (Seq (q, r)))) (Seq((Seq(p, q)), r)) pkt in
  check gen_pol_3 prop_compile_ok

let%test "quickcheck ka-one-seq compiler" =
  let prop_compile_ok (pol, pkt) =
    compare_compiler_output pol (Seq(id, pol)) pkt in
  check gen_pol_1 prop_compile_ok

let%test "quickcheck ka-one-seq eval" =
  let prop_compile_ok (pol, pkt) =
    compare_eval_output pol (Seq(id, pol)) pkt in
  check gen_pol_1 prop_compile_ok

let%test "quickcheck ka-seq-one compiler" =
  let prop_compile_ok (pol, pkt) =
    compare_compiler_output pol (Seq(pol, id)) pkt in
  check gen_pol_1 prop_compile_ok

let%test "quickcheck ka-seq-one eval" =
  let prop_compile_ok (pol, pkt) =
    compare_eval_output pol (Seq(pol, id)) pkt in
  check gen_pol_1 prop_compile_ok

let%test "quickcheck ka-seq-dist-l compiler" =
  let prop_compile_ok (p, q, r, pkt) =
    compare_compiler_output
      (Seq(p, (Union (q, r)))) (Union ((Seq(p, q)), (Seq(p, r)))) pkt in
  check gen_pol_3 prop_compile_ok

let%test "quickcheck ka-seq-dist-l eval" =
  let prop_compile_ok (p, q, r, pkt) =
    compare_eval_output
      (Seq(p, (Union (q, r)))) (Union ((Seq(p, q)), (Seq(p, r)))) pkt in
  check gen_pol_3 prop_compile_ok

let%test "quickcheck ka-seq-dist-r compiler" =
  let prop_compile_ok (p, q, r, pkt) =
    compare_compiler_output
      (Seq (Union(p, q), r)) (Union (Seq(p, r), Seq(q, r))) pkt in
  check gen_pol_3 prop_compile_ok

let%test "quickcheck ka-seq-dist-r eval" =
  let prop_compile_ok (p, q, r, pkt) =
    compare_eval_output
      (Seq (Union(p, q), r)) (Union (Seq(p, r), Seq(q, r))) pkt in
  check gen_pol_3 prop_compile_ok

let%test "quickcheck ka-zero-seq compiler" =
  let prop_compile_ok (pol, pkt) =
    compare_compiler_output drop (Seq(drop, pol)) pkt in
  check gen_pol_1 prop_compile_ok

let%test "quickcheck ka-zero-seq eval" =
  let prop_compile_ok (pol, pkt) =
    compare_eval_output drop (Seq(drop, pol)) pkt in
  check gen_pol_1 prop_compile_ok

let%test "quickcheck ka-seq-zero compiler" =
  let prop_compile_ok (pol, pkt) =
    compare_compiler_output drop (Seq(pol, drop)) pkt in
  check gen_pol_1 prop_compile_ok

let%test "quickcheck ka-seq-zero eval" =
  let prop_compile_ok (pol, pkt) =
    compare_eval_output drop (Seq(pol, drop)) pkt in
  check gen_pol_1 prop_compile_ok

let%test "quickcheck ka-unroll-l compiler" =
  let prop_compile_ok (pol, pkt) =
    compare_compiler_output (Star pol) (Union(id, Seq(pol, Star pol))) pkt in
  check gen_pol_1 prop_compile_ok

let%test "quickcheck ka-unroll-l eval" =
  let prop_compile_ok (pol, pkt) =
    compare_eval_output (Star pol) (Union(id, Seq(pol, Star pol))) pkt in
  check gen_pol_1 prop_compile_ok

let%test "quickcheck ka-lfp-l compiler" =
  let prop_compile_ok (p, q, r, pkt) =
    not (compare_compiler_output (Union(Union(q, Seq (p, r)), r)) r pkt)
    ||  (compare_compiler_output (Union(Seq(Star p, q), r)) r pkt) in
  check gen_pol_3 prop_compile_ok

let%test "quickcheck ka-lfp-l eval" =
  let prop_compile_ok (p, q, r, pkt) =
    not (compare_eval_output (Union(Union(q, Seq (p, r)), r)) r pkt)
    ||  (compare_eval_output (Union(Seq(Star p, q), r)) r pkt) in
  check gen_pol_3 prop_compile_ok

let%test "quickcheck ka-unroll-r compiler" =
  let prop_compile_ok (pol, pkt) =
    compare_compiler_output (Star pol) (Union(id, Seq(Star pol, pol))) pkt in
  check gen_pol_1 prop_compile_ok

let%test "quickcheck ka-unroll-r eval" =
  let prop_compile_ok (pol, pkt) =
    compare_eval_output (Star pol) (Union(id, Seq(Star pol, pol))) pkt in
  check gen_pol_1 prop_compile_ok

let%test "quickcheck ka-lfp-r compiler" =
  let prop_compile_ok (p, q, r, pkt) =
    not (compare_compiler_output (Union(Union(p, Seq (q, r)), q)) q pkt)
    ||  (compare_compiler_output (Union(Seq(p, Star r), q)) q pkt) in
  check gen_pol_3 prop_compile_ok

let%test "quickcheck ka-lfp-r eval" =
  let prop_compile_ok (p, q, r, pkt) =
    not (compare_eval_output (Union(Union(p, Seq (q, r)), q)) q pkt)
    ||  (compare_eval_output (Union(Seq(p, Star r), q)) q pkt) in
  check gen_pol_3 prop_compile_ok

let ip_of_str n = Ipaddr.V4.(to_int32 (of_string_exn n))

let test_ipDst ipAddress = Test(IP4Dst(ip_of_str ipAddress, 32l))

let is_arp = Test(EthType 0x806)

let is_ip = Test(EthType 0x800)

let modify_outport p = Mod (Location (Physical p))

let flow p a = { pattern = p; action = [a]; cookie = 0L; idle_timeout = Permanent; hard_timeout= Permanent }

let%test "adds rules to drop packets not meeting ip4Src dependencies" =
  let open Pattern in
  let pol = 
    Union(
      Seq(Filter(And(test_ipDst "192.168.56.1", is_arp)), modify_outport 9l ),
      Seq(Filter(And(test_ipDst "192.168.57.1", is_ip)), modify_outport 10l )
    ) in
  test_compile_table pol [
    flow { match_all with dlTyp = Some 0x806; nwDst = Some (ip_of_str "192.168.56.1", 32l) } [[ Output(Physical 9l) ]];
    flow { match_all with dlTyp = Some 0x800; nwDst = Some (ip_of_str "192.168.56.1", 32l) } [[]];
    flow { match_all with dlTyp = Some 0x800; nwDst = Some (ip_of_str "192.168.57.1", 32l) } [[ Output(Physical 10l) ]];
    flow match_all []
  ]

let%test "adds matches for ethTyp on ipProto dependencies" =
  let open Pattern in
  let pol = 
    Union(
      Seq(Filter(Test(IPProto(6))), modify_outport 9l ),
      Seq(Filter(Test(IPProto(1))), modify_outport 10l )
    ) in
  test_compile_table pol [
    flow { match_all with dlTyp = Some 0x800; nwProto = Some 0x1 } [[ Output(Physical 10l) ]];
    flow { match_all with dlTyp = Some 0x800; nwProto = Some 0x6 } [[ Output(Physical 9l) ]];
    flow match_all []
  ]

let%test "adds matches for ethTyp and ipProto on tcp/udp dependencies" =
  let open Pattern in
  let pol = 
    Union(
      Seq(Filter(Test(TCPDstPort(22))), modify_outport 9l ),
      Seq(Filter(Test(TCPDstPort(443))), modify_outport 10l )
    ) in
  test_compile_table pol [
    flow { match_all with dlTyp = Some 0x800; nwProto = Some 0x6; tpDst = Some 22 } [[ Output(Physical 9l) ]];
    flow { match_all with dlTyp = Some 0x800; nwProto = Some 0x11; tpDst = Some 22 } [[ Output(Physical 9l) ]];
    flow { match_all with dlTyp = Some 0x800; nwProto = Some 0x6; tpDst = Some 443 } [[ Output(Physical 10l) ]];
    flow { match_all with dlTyp = Some 0x800; nwProto = Some 0x11; tpDst = Some 443 } [[ Output(Physical 10l) ]];
    flow match_all []
  ]

