open Packet
open Printf
open NetCore_Types
open NetCore_Semantics
open NetCore_Pattern
open NetCore_Pretty
open OUnit
open NetKAT_Test
(* open Verify_Tests *)

let string_of_list f lst = String.concat "," (List.map f lst)

let pay = OpenFlow0x01_Core.Buffered (0l, Cstruct.create 10)

module NetCoreCompiler = NetCore_Compiler.NetCoreCompiler

module TestClassifier = struct

  module C = NetCore_Classifier.Make (NetCore_Action.Output)
  open C
  open NetCore_Action.Output
  open NetCore_Pattern

  let test0 =
    "action sequence test" >::
      fun () ->
        assert_equal ~printer:string_of_action
          (forward 5l)
          (seq_action (forward 3l) (forward 5l))

  let test1 =
    "forward action domain should be all" >::
      fun () ->
        assert_equal ~printer:NetCore_Pretty.string_of_pattern
          (domain (List.hd (atoms (forward 1l)))) all

  let test2 =
    "pattern restriction test" >::
      fun () ->
        assert_equal ~printer:NetCore_Pretty.string_of_pattern
          (sequence_range
             (List.hd (atoms (forward 1l)))
             (inPort (Physical 1l)))
          all

  let classifier_tests group_label lst =
    group_label >::: 
      (List.map 
         (fun (label, expected, calculated) ->
            label >:: fun () -> 
              assert_equal expected calculated)
         lst)

  let eval_tests group_label lst = 
    group_label >:::
      (List.map
         (fun (label, expected_action, input_pol, input_val) ->
            label >:: fun () -> 
              assert_equal
                (NetCore_Semantics.eval_action expected_action input_val)
                (NetCore_Semantics.eval input_pol input_val))
         lst)

  let netcore_tests group_label lst = 
    group_label >:::
      (List.map
         (fun (label, expected_tbl, input_sw, input_pol) ->
            label >:: fun () -> 
              assert_equal
                expected_tbl
                (NetCoreCompiler.compile_pol input_pol input_sw))
         lst)

  let lst = 
    [("composition test 1",
      [(NetCore_Pattern.inter (dlDst 0xdeadbeefL) (inPort (Physical 5l)),
        forward 2l);
       (all, drop)],
      let tbl1 =
        [(NetCore_Pattern.inter (dlDst 0xdeadbeefL) (inPort (Physical 5l)),
          forward 1l);
         (all, drop)] in
      let tbl2 =
        [(inPort (Physical 1l), forward 2l);
         (all, drop)] in
      (sequence tbl1 tbl2));
     ("composition test 2",
      [(inter (dlSrc 0xFFFFL) (inPort (Physical 10l)), forward 20l);
       (inter (dlSrc 0xFFFFL) (inPort (Physical 300l)), forward 400l);
       (all, drop)],
      sequence
        [(dlSrc 0xFFFFL, pass); (all, drop)]
        [(inPort (Physical 10l), forward 20l);
         (inPort (Physical 300l), forward 400l);
         (all, drop)]);
     ("composition test 3",
      [(inter (dlSrc 0xDDDDL) (dlDst 0xEEEEL), forward 2l);
       (all, drop)],
      sequence
        [(dlSrc 0xDDDDL, forward 1l); (all, drop)]
        [(dlDst 0xEEEEL, forward 2l); (all, drop)]);
     ("sequencing test 4",
      [(inter (dlSrc 0xDDDDL) (dlDst 0xEEEEL), forward 2l);
       (all, drop)],
      sequence
        [(dlSrc 0xDDDDL, pass); (all, drop)]
        [(dlDst 0xEEEEL, forward 2l); (all, drop)]);
     ("sequencing test 5",
      [(inter (dlSrc 0xDDDDL) (dlDst 0xEEEEL), forward 2l);
       (all, drop)],
      sequence
        [(dlSrc 0xDDDDL, forward 2l); (all, drop)]
        [(dlDst 0xEEEEL, pass); (all, drop)]);
     ("union regression 1",
      [(inter (dlSrc 0xDDDDL) (dlDst 0xEEEEL),
        par_action (forward 2l) (forward 30l));
       (dlSrc 0xDDDDL, forward 2l);
       (dlDst 0xEEEEL, forward 30l);
       (all, drop)],
      union
        [(dlSrc 0xDDDDL, forward 2l);
         (all, drop)]
        [(dlDst 0xEEEEL, forward 30l);
         (all, drop)]);
     ("union overlap 1",
      [(dlSrc 0xDDDDL, par_action (forward 2l) (forward 30l));
       (all, forward 30l)],
      union
        [(dlSrc 0xDDDDL, forward 2l); (all, drop)]
        [(all, forward 30l)]);
     ("union overlap 2",
      [(dlSrc 0xDDDDL, par_action (forward 2l) (forward 2l));
       (all, forward 2l)],
      union
        [(dlSrc 0xDDDDL, forward 2l); (all, drop)]
        [(all, forward 2l)])]

  let lst2 =
    [("NAT module with *wrong* forwarding shim",
      [(inPort (Physical 1l), forward 2l);
       (all, drop)],
      1L,
      Union
        (Seq
           (Seq
              (Filter (And (OnSwitch 1L, Hdr (inPort (Physical 1l)))), 
               Action (forward 5001l)),
            Action (forward 2l)),
         Seq
           (Seq
              (Filter (And (OnSwitch 1L, Hdr (inPort (Physical 2l)))),
               Filter (Nothing)),
            Action (forward 1l))));
     ("NAT debugging 1",
      [],
      1L,
      ITE
        (And (OnSwitch 1L, Hdr (inPort (Physical 2l))),
         Seq
           (ITE
              (And (Hdr (ipSrc 0xffffl), Hdr (tcpSrcPort 2000)),
               Seq
                 (Action (updateSrcIP 0xffffl 0xaaaal),
                  Action (updateSrcPort 2000 43072)),
               Filter (Nothing)),
            ITE
              (Hdr (inPort (Physical 2l)),
               Action (forward 1l),
               Action pass)),
         Action pass));
     ("sequencing 1",
      [(dlVlan None, updateDlVlan None (Some 1));
       (all, drop)],
      1L,
      Seq (Action (updateDlVlan None (Some 1)),
           Filter (Hdr (dlVlan (Some 1)))))]

  let pk = {
    dlSrc = 0L;
    dlDst = 0L;
    dlVlan = None;
    dlVlanPcp = 0;
    nw = Unparsable (0, Cstruct.create 8) 
  }

  let inp = Pkt (1L, Physical 1l, pk, pay)

  let lst3 =
    [("sequencing 1",
      updateDlVlan None (Some 1),
      Seq (Action (updateDlVlan None (Some 1)),
           Filter (Hdr (dlVlan (Some 1)))),
      inp);
     ("filtering 1",
      pass,
      Filter (Hdr (dlVlan (Some 1))),
      Pkt (1L, 
           Physical 1l, 
           { pk with dlVlan = Some 1 },
           pay));
     ("updating 1",
      updateDlVlan None (Some 1),
      Action (updateDlVlan None (Some 1)),
      Pkt (1L, Physical 1l, pk, pay))]


  let go = TestList 
      [ test0; test1; test2; 
        classifier_tests "classifier tests" lst;
        netcore_tests "NetCore tests" lst2;
        eval_tests "NetCore semantics tests" lst3;
        ("eval_action update" >::
           fun () ->
             assert_equal
               [Pkt (1L, Physical 1l, 
                     { pk with dlVlan = Some 1 }, 
                     pay)]
               (eval_action 
                  (updateDlVlan None (Some 1))
                  (Pkt (1L, 
                        Physical 1l, 
                        pk, 
                        pay))))
      ]

end

module TestNetCore = struct

  open NetCore_Types
  open NetCore_Action.Output
  open NetCore_Pattern
  module C = NetCore_Classifier.Make (NetCore_Action.Output)

  let test1_pol1 =
    Seq 
      (Filter 
         (Not 
            (Or (And 
                   (And (OnSwitch 1L, 
                         Hdr (inPort (Physical 1l))), 
                    Hdr (dlSrc 0x0ab75f2211d4L)), Nothing))),
       Action (forward 3l))

  let test1_pol2 = 
    Seq 
      (Filter
         (And 
            (OnSwitch 1L, Hdr (dlDst 0x0ab75f2211d4L))), 
       Action (forward 1l))

  let test1_pol3 =
    Seq 
      (Filter 
         (Not 
            (Or (And (OnSwitch 1L,
                      Hdr (dlDst 0x0ab75f2211d4L)),
                 Nothing))), Action to_all)

  let test1 = 
    "maclearning regression 1" >::
      fun () ->
        assert_equal  
          (NetCoreCompiler.compile_pol test1_pol1 1L) []

  let test2 = 
    "maclearning regression 2" >::
      fun () ->
        assert_equal  
          (NetCoreCompiler.compile_pol test1_pol2 1L) []

  let test3 = 
    "maclearning regression 3" >::
      fun () ->
        assert_equal  
          (NetCoreCompiler.compile_pol test1_pol3 1L) []

  let test4 = 
    "maclearning regression 2 || 3" >::
      fun () ->
        assert_equal  
          (NetCoreCompiler.compile_pol
             (Union (test1_pol2, test1_pol3))
             1L)
          []

  let test5 = 
    "maclearning regression 1 || 2 || 3" >::
      fun () ->
        assert_equal  
          (NetCoreCompiler.compile_pol
             (Union (test1_pol1, Union (test1_pol2, test1_pol3)))
             1L)
          []

  let test6 =
    "sequencing regression 1" >::
      fun () ->
        assert_equal 
          (NetCoreCompiler.compile_pol 
             (Filter
                (And 
                   (OnSwitch 1L, Hdr (dlDst 0x0ab75f2211d4L))))
             1L)
          [(dlDst 0x0ab75f2211d4L, pass); 
           (all, drop)]

  let pol2_query =
    Seq
      (Filter
         (Not
            (Or
               (And
                  (And (OnSwitch 1L, Hdr (inPort (Physical 1l))),
                   Hdr (dlSrc 0x92f0fea53b3fL)),
                Or
                  (And
                     (And (OnSwitch 1L, Hdr (inPort (Physical 2l))),
                      Hdr (dlSrc 0x1274ccafe0eaL)),
                   Nothing)))),
       Action (forward 5000l))

  let pol2_fwd1 = 
    Seq
      (Filter (And (OnSwitch 1L, Hdr (dlDst 0x92f0fea53b3fL))),
       Action (forward 1l))

  let pol2_fwd2 = 
    Seq
      (Filter (And (OnSwitch 1L, Hdr (dlDst 0x1274ccafe0eaL))),
       Action (forward 2l))

  let pol2_fwd_rest =
    Seq
      (Filter
         (Not
            (Or
               (And (OnSwitch 1L, Hdr (dlDst 0x92f0fea53b3fL)),
                Or
                  (And (OnSwitch 1L, Hdr (dlDst 0x1274ccafe0eaL)),
                   Nothing)))),
       Action to_all)

  let pol2 = 
    Union
      (pol2_query,
       Union
         (pol2_fwd1,
          Union
            (pol2_fwd2, pol2_fwd_rest)))

  let test7 =
    "maclearning regression pol2_query" >::
      fun () ->
        assert_equal 
          (NetCoreCompiler.compile_pol pol2_query 1L)
          []

  let test8 =
    "maclearning regression pol2_fwd1" >::
      fun () ->
        assert_equal 
          (NetCoreCompiler.compile_pol pol2_fwd1 1L)
          []

  let test9 =
    "maclearning regression pol2_fwd_rest" >::
      fun () ->
        assert_equal 
          (NetCoreCompiler.compile_pol pol2_fwd_rest 1L)
          []

  let test10 =
    "maclearning regression pol2" >::
      fun () ->
        assert_equal 
          (NetCoreCompiler.compile_pol pol2 1L)
          []

  let pol3_1 = 
    And (Hdr (inPort (Physical 1l)), Hdr (dlSrc 0x92f0fea53b3fL))

  let pol3_2 =
    And
      (Hdr (inPort (Physical 2l)),
       Hdr (dlSrc 0x1274ccafe0eaL))

  let pol3 =
    Filter pol3_2

  let test11 =
    "predicate compilation regression" >::
      fun () ->
        assert_equal 
          (NetCoreCompiler.compile_pol pol3 1L)
          []

  let go = TestList [test11; test10; test9; test8; test7; test6; test5;
                     test4; test3; test2; test1]

end



module Helper = struct

  module C = NetCore_Classifier.Make (NetCore_Action.Output)
  open NetCore_Types
  open NetCore_Types
  open Packet

  let desugar_policy pol =
    let vlan_cell = ref 0 in
    let genvlan () = incr vlan_cell; Some !vlan_cell in
    NetCore_Desugar.desugar genvlan pol

  let in_tp = 
    let flags = 
      let open Tcp.Flags in
      { ns = true
      ; cwr = true
      ; ece = true
      ; urg = true
      ; ack = true
      ; psh = true
      ; rst = true
      ; syn = true
      ; fin = true } in
    let open Tcp in
    Ip.Tcp
      { src = 0
      ; dst = 0
      ; seq = 0l
      ; ack = 0l
      ; offset = 0
      ; flags = flags
      ; window = 0
      ; chksum = 0
      ; urgent = 0
      ; payload = Cstruct.create 0 }

  let in_nw =
    let open Ip in
    Ip { tos = 0
       ; ident = 0
       ; flags = { Flags.df = false; Flags.mf = false }
       ; frag = 0
       ; ttl = 10
       ; chksum = 0 (* TODO(cole) generate checksum. *)
       ; src = 0l
       ; dst = 0l
       ; tp = in_tp }

  let in_pkt =
    { dlSrc = Int64.zero
    ; dlDst = Int64.zero
    ; dlVlan = None
    ; dlVlanPcp = 0
    ; nw = in_nw }

  let in_val =
    Pkt ( Int64.one
        , (Physical 1l)
        , in_pkt
        , pay)

  let mkEvalTest 
      name 
      ?debug:(dbg=false) 
      (pol : NetCore_Desugar.policy) 
      in_val 
      expected_vals = 
    let ds_pol = desugar_policy pol in

    if dbg then
      printf "Internal policy:\n%s\n" (string_of_pol ds_pol)
    else
      ();

    let Pkt (in_sid, in_port, _, _) = in_val in
    let expected_pkts =
      List.map (fun (Pkt (sw, pr, p, _)) -> (sw, pr, p)) expected_vals in

    (* Test the semantic interpretation. *)
    let vals = eval ds_pol in_val in
    let sem_test = 
      (name ^ " (semantic) test") >:: fun () ->
        assert_equal
          ~printer:(string_of_list string_of_value)
          expected_vals vals in

    (* Test the classifier interpretation. *)
    let classifier = NetCoreCompiler.compile_pol ds_pol in_sid in

    if dbg then
      let _ = printf "Classifier:\n" in
      List.iter 
        (fun (m,a) -> printf " %s => %s\n"
            (NetCore_Pretty.string_of_pattern m)
            (NetCore_Pretty.string_of_action a))
        classifier
    else
      ();

    let act = C.scan classifier in_port in_pkt in
    let pkts = NetCore_Action.Output.apply_action act 
        (in_sid, in_port, in_pkt) in
    let classifier_test =
      (name ^ " (classifier) test") >:: fun () ->
        assert_equal
          ~printer:(string_of_list
                      (fun (_, pt, pk) ->
                         Format.sprintf "(%s,%s)"
                           (string_of_port pt)
                           (Packet.to_string pk)))
          expected_pkts pkts in

    TestList [ sem_test; classifier_test ]

end

module TestFilters = struct

  open NetCore_Types
  open Helper

  let test1 =
    let policy = NetCore_Desugar.Filter NetCore_Desugar.All in
    mkEvalTest "filter true" policy in_val [in_val]

  let test2 =
    let policy = NetCore_Desugar.Filter NetCore_Desugar.NoPackets in
    mkEvalTest "filter false" policy in_val []

  let go = TestList [ test1; test2 ]

end

module TestMods = struct

  open NetCore_Desugar
  open Packet
  open Helper

  let test1 =
    let policy = Act (UpdateDlVlan (None, Some 1)) in
    let Pkt (sid, port, pkt, payload) = in_val in
    let expected_pkt = {pkt with dlVlan = Some 1} in
    let expected_val = Pkt ( sid, port, expected_pkt, payload) in
    mkEvalTest "mod vlan" policy in_val [expected_val]

  let test2a =
    let policy = Seq ( Act (UpdateDlVlan (None, (Some 1)))
                     , Act (UpdateDlVlan ((Some 1), None))) in
    mkEvalTest "vlan: act; undo act == pass " policy in_val [in_val]

  let test2b =
    let policy = Seq ( Act (UpdateSrcIP (0l, 1l))
                     , Act (UpdateSrcIP (1l, 0l))) in
    mkEvalTest "srcip: act; undo act == pass" policy in_val [in_val]

  let test2c =
    let policy = Seq ( Act (UpdateDstIP (0l, 1l))
                     , Act (UpdateDstIP (1l, 0l))) in
    mkEvalTest "dstip: act; undo act == pass" policy in_val [in_val]

  let test2d =
    let policy = Seq ( Act (UpdateSrcPort (0, 1))
                     , Act (UpdateSrcPort (1, 0))) in
    mkEvalTest "tpsrc: act; undo act == pass" policy in_val [in_val]

  let test2e =
    let policy = Seq ( Act (UpdateDstPort (0, 1))
                     , Act (UpdateDstPort (1, 0))) in
    mkEvalTest "tpdst: act; undo act == pass" policy in_val [in_val]

  let test2f =
    let policy = Seq ( Act (UpdateDlSrc (0L, 1L))
                     , Act (UpdateDlSrc (1L, 0L))) in
    mkEvalTest "dlSrc: act; undo act == pass" policy in_val [in_val]

  let test2g =
    let policy = Seq ( Act (UpdateDlDst (0L, 1L))
                     , Act (UpdateDlDst (1L, 0L))) in
    mkEvalTest "dlDst: act; undo act == pass" policy in_val [in_val]

  let test3 =
    let policy =
      Seq ( Act (UpdateDlVlan (None, (Some 1)))
          , Seq ( Act ToAll
                , Act (UpdateDlVlan ((Some 1), None)))) in
    let Pkt (sid, port, pkt, payload) = in_val in
    let expected_vals = [Pkt (sid, NetCore_Pattern.All, pkt, payload)] in
    mkEvalTest "mod no effect 2" policy in_val expected_vals

  let test4 = 
    let policy = 
      Seq ( Act (UpdateDlVlan (None, Some 1))
          , Filter (DlVlan (Some 1))) in
    let Pkt (sid, port, pkt, payload) = in_val in
    let expected_pkt = {pkt with dlVlan = Some 1} in
    let expected_val = Pkt ( sid, port, expected_pkt, payload) in
    mkEvalTest "seq mod filter" policy in_val [expected_val]

  let test5 = 
    let policy =
      Seq ( Act (UpdateDlVlan (None, Some 1))
          , Seq ( Filter (DlVlan (Some 1))
                , Act (UpdateDlVlan (Some 1, None)))) in
    mkEvalTest "seq mod seq" policy in_val [in_val]

  let go = 
    TestList 
      [ test1
      ; test2a
      ; test2b
      ; test2c
      ; test2d
      ; test2e
      ; test2f
      ; test2g
      ; test3
      ; test4
      ; test5 ]

end

module TestSlices = struct

  open NetCore_Desugar
  open Packet
  open Helper

  let test1 =
    let policy = Slice (NetCore_Desugar.All, Act ToAll,
                        NetCore_Desugar.All) in
    let Pkt (sid, port, expected_pkt, payload) = in_val in
    let expected_val = Pkt ( sid, NetCore_Pattern.All, expected_pkt, payload) in
    mkEvalTest "slice repeater" policy in_val [expected_val]

  let test1' =
    let policy =
      Seq ( Seq (Filter (DlVlan None), Act (UpdateDlVlan (None, Some 1))),
            Seq ( Act ToAll
                , Par ( Seq (Filter (DlVlan (Some 1)), Act (UpdateDlVlan (Some 1, None)))
                      , Filter (Not (DlVlan (Some 1)))))) in
    let Pkt (sid, port, expected_pkt, payload) = in_val in
    let expected_val = Pkt ( sid, NetCore_Pattern.All, expected_pkt, payload) in
    mkEvalTest "slice' repeater" policy in_val [expected_val]

  let test1'' =
    let policy =
      Seq (Seq (Filter (DlVlan None), Act (UpdateDlVlan (None, Some 1))),
           Seq (Act ToAll,
                Seq (Filter (DlVlan (Some 1)), Act (UpdateDlVlan (Some 1, None))))) in
    let Pkt (sid, port, expected_pkt, payload) = in_val in
    let expected_val = Pkt ( sid, NetCore_Pattern.All, expected_pkt, payload) in
    mkEvalTest "slice'' repeater" policy in_val [expected_val]

  let go = TestList [ test1; test1'; test1'' ]

end

(* TODO(cole): how to test the parser without exposing parsing functions. *)
(*
module TestParser = struct



  (* For each parsable type, test that parse(marshal(v)) == v for some value v.
   *)
  let test_match = "match marshal/parse test" >:: fun () ->
    let v = match_all in
    let bits = Cstruct.create (Match.size_of v) in
    let _ = Match.marshal v bits in
    let v' = Match.parse bits in
    assert_equal ~printer:Match.to_string v v'

  let go = TestList [ test_match ]

end
*)

let tests =
  TestList [ TestFilters.go
           ; TestMods.go
           ; TestSlices.go
           ; TestClassifier.go
           (*         ; TestParser.go *)
           (*         ; TestNetCore.go *)
           ]

