open Packet
open Printf
open NetCore_Types
open NetCore_Semantics
open NetCore_Desugar
open NetCore_Pretty
open OUnit

module TestClassifier = struct

  module C = NetCore_Classifier.Make (NetCore_Action.Output)
  open C
  open NetCore_Action.Output
  open NetCore_Pattern

  let test0 =
    "action sequence test" >::
      fun () ->
        assert_equal ~printer:action_to_string
          (forward 5)
          (seq_action (forward 3) (forward 5))

  let test1 =
    "forward action domain should be NetCore_Pattern.all" >::
      fun () ->
        assert_equal ~printer:NetCore_Pretty.pattern_to_string
          (domain (List.hd (atoms (forward 1)))) NetCore_Pattern.all

  let test2 =
    "pattern restriction test" >::
      fun () ->
        assert_equal ~printer:NetCore_Pretty.pattern_to_string
          (sequence_range
             (List.hd (atoms (forward 1)))
             (NetCore_Pattern.inPort (Physical 1)))
          NetCore_Pattern.all

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
             assert_equal ~printer:action_to_string
               expected_action
               (NetCore_Semantics.eval input_pol input_val))
         lst)

  let netcore_tests group_label lst = 
    group_label >:::
      (List.map
         (fun (label, expected_tbl, input_sw, input_pol) ->
           label >:: fun () -> 
             assert_equal
               expected_tbl
               (NetCore_Compiler.compile_pol input_pol input_sw))
         lst)

  let lst = 
    [("composition test 1",
      [(NetCore_Pattern.inter (dlDst 0xdeadbeefL) (inPort (Physical 5)),
        forward 2);
       (NetCore_Pattern.all, drop)],
      let tbl1 =
        [(NetCore_Pattern.inter (dlDst 0xdeadbeefL) (inPort (Physical 5)),
          forward 1);
         (NetCore_Pattern.all, drop)] in
      let tbl2 =
        [(inPort (Physical 1), forward 2);
         (NetCore_Pattern.all, drop)] in
      (sequence tbl1 tbl2));
     ("composition test 2",
      [(inter (dlSrc 0xFFFFL) (inPort (Physical 10)), forward 20);
       (inter (dlSrc 0xFFFFL) (inPort (Physical 300)), forward 400);
       (all, drop)],
      sequence
        [(dlSrc 0xFFFFL, pass); (all, drop)]
        [(inPort (Physical 10), forward 20);
         (inPort (Physical 300), forward 400);
         (all, drop)]);
     ("composition test 3",
      [(inter (dlSrc 0xDDDDL) (dlDst 0xEEEEL), forward 2);
       (all, drop)],
      sequence
        [(dlSrc 0xDDDDL, forward 1); (all, drop)]
        [(dlDst 0xEEEEL, forward 2); (all, drop)]);
     ("sequencing test 4",
      [(inter (dlSrc 0xDDDDL) (dlDst 0xEEEEL), forward 2);
       (all, drop)],
      sequence
        [(dlSrc 0xDDDDL, pass); (all, drop)]
        [(dlDst 0xEEEEL, forward 2); (all, drop)]);
     ("sequencing test 5",
      [(inter (dlSrc 0xDDDDL) (dlDst 0xEEEEL), forward 2);
       (all, drop)],
      sequence
        [(dlSrc 0xDDDDL, forward 2); (all, drop)]
        [(dlDst 0xEEEEL, pass); (all, drop)]);
     ("union regression 1",
      [(inter (dlSrc 0xDDDDL) (dlDst 0xEEEEL),
        par_action (forward 2) (forward 30));
       (dlSrc 0xDDDDL, forward 2);
       (dlDst 0xEEEEL, forward 30);
       (all, drop)],
      union
        [(dlSrc 0xDDDDL, forward 2);
         (all, drop)]
        [(dlDst 0xEEEEL, forward 30);
         (all, drop)]);
     ("union overlap 1",
      [(dlSrc 0xDDDDL, par_action (forward 2) (forward 30));
       (all, forward 30)],
      union
        [(dlSrc 0xDDDDL, forward 2); (all, drop)]
        [(all, forward 30)]);
     ("union overlap 2",
      [(dlSrc 0xDDDDL, par_action (forward 2) (forward 2));
       (all, forward 2)],
      union
        [(dlSrc 0xDDDDL, forward 2); (all, drop)]
        [(all, forward 2)])]

  let lst2 =
    [("NAT module with *wrong* forwarding shim",
      [(inPort (Physical 1), forward 2);
       (all, drop)],
      1L,
      PoUnion
        (PoSeq
           (PoSeq
              (PoFilter (PrAnd (PrOnSwitch 1L, PrHdr (inPort (Physical 1)))), 
               PoAction (forward 5001)),
            PoAction (forward 2)),
         PoSeq
           (PoSeq
              (PoFilter (PrAnd (PrOnSwitch 1L, PrHdr (inPort (Physical 2)))),
               PoFilter (PrNone)),
            PoAction (forward 1))));
     ("NAT debugging 1",
      [],
      1L,
      PoITE
        (PrAnd (PrOnSwitch 1L, PrHdr (inPort (Physical 2))),
         PoSeq
           (PoITE
              (PrAnd (PrHdr (ipSrc 0xffffl), PrHdr (tcpSrcPort 2000)),
               PoSeq
                 (PoAction (updateSrcIP 0xffffl 0xaaaal),
                  PoAction (updateSrcPort 2000 43072)),
               PoFilter (PrNone)),
            PoITE
              (PrHdr (inPort (Physical 2)),
               PoAction (forward 1),
               PoAction pass)),
         PoAction pass));
     ("sequencing 1",
      [(dlVlan None, updateDlVlan None (Some 1));
       (all, drop)],
      1L,
      PoSeq (PoAction (updateDlVlan None (Some 1)),
             PoFilter (PrHdr (dlVlan (Some 1)))))]

  let pk = {
    dlSrc = 0L;
    dlDst = 0L;
    dlTyp = 0x800;
    dlVlan = None;
    dlVlanPcp = 0;
    nw = Unparsable (Cstruct.create 8) 
  }

  let inp = Pkt (1L, Physical 1, pk, OpenFlow0x01.PacketOut.Buffer 0l)

  let lst3 =
    [("sequencing 1",
      updateDlVlan None (Some 1),
      PoSeq (PoAction (updateDlVlan None (Some 1)),
             PoFilter (PrHdr (dlVlan (Some 1)))),
      inp);
     ("filtering 1",
      pass,
      PoFilter (PrHdr (dlVlan (Some 1))),
      Pkt (1L, Physical 1, { pk with dlVlan = Some 1 }, OpenFlow0x01.PacketOut.Buffer 0l));
     ("updating 1",
      updateDlVlan None (Some 1),
      PoAction (updateDlVlan None (Some 1)),
      Pkt (1L, Physical 1, pk, OpenFlow0x01.PacketOut.Buffer 0l))]
      

  let go = TestList 
    [ test0; test1; test2; 
      classifier_tests "classifier tests" lst;
      netcore_tests "NetCore tests" lst2;
      eval_tests "NetCore semantics tests" lst3;
      ("eval_action update" >::
          fun () ->
            assert_equal
              [Pkt (1L, Physical 1, { pk with dlVlan = Some 1 }, OpenFlow0x01.PacketOut.Buffer 0l)]
              (eval_action (Pkt (1L, Physical 1, pk, OpenFlow0x01.PacketOut.Buffer 0l))
                 (updateDlVlan None (Some 1))))
    ]

end

module TestNetCore = struct

  open NetCore_Types
  open NetCore_Action.Output
  open NetCore_Pattern
  module C = NetCore_Classifier.Make (NetCore_Action.Output)

  let test1_pol1 =
    PoSeq 
      (PoFilter 
         (PrNot 
            (PrOr (PrAnd 
                     (PrAnd (PrOnSwitch 1L, 
                             PrHdr (NetCore_Pattern.inPort (Physical 1))), 
                      PrHdr (NetCore_Pattern.dlSrc 0x0ab75f2211d4L)), PrNone))),
       PoAction (forward 3))

  let test1_pol2 = 
    PoSeq 
      (PoFilter
         (PrAnd 
            (PrOnSwitch 1L, PrHdr (NetCore_Pattern.dlDst 0x0ab75f2211d4L))), 
       PoAction (forward 1))

  let test1_pol3 =
    PoSeq 
      (PoFilter 
         (PrNot 
            (PrOr (PrAnd (PrOnSwitch 1L,
                          PrHdr (NetCore_Pattern.dlDst 0x0ab75f2211d4L)),
                   PrNone))), PoAction to_all)

  let test1 = 
      "maclearning regression 1" >::
        fun () ->
          assert_equal  
            (NetCore_Compiler.compile_pol test1_pol1 1L) []

  let test2 = 
      "maclearning regression 2" >::
        fun () ->
          assert_equal  
            (NetCore_Compiler.compile_pol test1_pol2 1L) []

  let test3 = 
      "maclearning regression 3" >::
        fun () ->
          assert_equal  
            (NetCore_Compiler.compile_pol test1_pol3 1L) []

  let test4 = 
      "maclearning regression 2 || 3" >::
        fun () ->
          assert_equal  
            (NetCore_Compiler.compile_pol
               (PoUnion (test1_pol2, test1_pol3))
            1L)
            []

  let test5 = 
      "maclearning regression 1 || 2 || 3" >::
        fun () ->
          assert_equal  
            (NetCore_Compiler.compile_pol
               (PoUnion (test1_pol1, PoUnion (test1_pol2, test1_pol3)))
            1L)
            []

  let test6 =
    "sequencing regression 1" >::
      fun () ->
        assert_equal 
          (NetCore_Compiler.compile_pol 
             (PoFilter
                (PrAnd 
                   (PrOnSwitch 1L, PrHdr (dlDst 0x0ab75f2211d4L))))
             1L)
          [(dlDst 0x0ab75f2211d4L, pass); 
           (all, drop)]

  let pol2_query =
    PoSeq
      (PoFilter
         (PrNot
            (PrOr
               (PrAnd
                  (PrAnd (PrOnSwitch 1L, PrHdr (inPort (Physical 1))),
                   PrHdr (dlSrc 0x92f0fea53b3fL)),
                PrOr
                  (PrAnd
                     (PrAnd (PrOnSwitch 1L, PrHdr (inPort (Physical 2))),
                      PrHdr (dlSrc 0x1274ccafe0eaL)),
                   PrNone)))),
       PoAction (forward 5000))

  let pol2_fwd1 = 
    PoSeq
      (PoFilter (PrAnd (PrOnSwitch 1L, PrHdr (dlDst 0x92f0fea53b3fL))),
       PoAction (forward 1))

  let pol2_fwd2 = 
    PoSeq
      (PoFilter (PrAnd (PrOnSwitch 1L, PrHdr (dlDst 0x1274ccafe0eaL))),
       PoAction (forward 2))

  let pol2_fwd_rest =
    PoSeq
      (PoFilter
         (PrNot
            (PrOr
               (PrAnd (PrOnSwitch 1L, PrHdr (dlDst 0x92f0fea53b3fL)),
                PrOr
                  (PrAnd (PrOnSwitch 1L, PrHdr (dlDst 0x1274ccafe0eaL)),
                   PrNone)))),
       PoAction to_all)

  let pol2 = 
    PoUnion
      (pol2_query,
       PoUnion
         (pol2_fwd1,
          PoUnion
            (pol2_fwd2, pol2_fwd_rest)))

  let test7 =
    "maclearning regression pol2_query" >::
      fun () ->
        assert_equal 
          (NetCore_Compiler.compile_pol pol2_query 1L)
          []

  let test8 =
    "maclearning regression pol2_fwd1" >::
      fun () ->
        assert_equal 
          (NetCore_Compiler.compile_pol pol2_fwd1 1L)
          []

  let test9 =
    "maclearning regression pol2_fwd_rest" >::
      fun () ->
        assert_equal 
          (NetCore_Compiler.compile_pol pol2_fwd_rest 1L)
          []

  let test10 =
    "maclearning regression pol2" >::
      fun () ->
        assert_equal 
          (NetCore_Compiler.compile_pol pol2 1L)
          []

  let pol3_1 = 
    PrAnd (PrHdr (inPort (Physical 1)), PrHdr (dlSrc 0x92f0fea53b3fL))

  let pol3_2 =
    PrAnd
      (PrHdr (inPort (Physical 2)),
       PrHdr (dlSrc 0x1274ccafe0eaL))

  let pol3 =
    PoFilter pol3_2

  let test11 =
    "predicate compilation regression" >::
      fun () ->
        assert_equal 
          (NetCore_Compiler.compile_pol pol3 1L)
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
    desugar genvlan pol

  let in_pkt =
    { dlSrc = Int64.zero
    ; dlDst = Int64.zero
    ; dlTyp = 0x90
    ; dlVlan = None
    ; dlVlanPcp = 0
    ; nw = Unparsable (Cstruct.create 8) }

  let in_val =
    Pkt ( Int64.one
        , (Physical 1)
        , in_pkt
        , (OpenFlow0x01.PacketOut.Buffer Int32.zero))

  let mkEvalTest name ?debug:(dbg=false) pol in_val expected_vals = 
    let ds_pol = desugar_policy pol in

    if dbg then
      printf "Internal policy:\n%s\n" (pol_to_string ds_pol)
    else
      ();

    let Pkt (in_sid, in_port, _, _) = in_val in
    let expected_pkts =
      List.map (fun (Pkt (sw, pr, p, _)) -> (sw, pr, p)) expected_vals in

    (* Test the semantic interpretation. *)
    let vals = classify ds_pol in_val in
    let sem_test = 
      (name ^ " (semantic) test") >:: fun () ->
        assert_equal
          ~printer:(Frenetic_Misc.string_of_list value_to_string)
          expected_vals vals in

    (* Test the classifier interpretation. *)
    let classifier = NetCore_Compiler.compile_pol ds_pol in_sid in

(*
    if dbg then
      printf "Classifier:\n%s\n" (C.to_string classifier)
    else
      ();
*)

    let act = C.scan classifier in_port in_pkt in
    let pkts = NetCore_Action.Output.apply_action act 
      (in_sid, in_port, in_pkt) in
    let classifier_test =
      (name ^ " (classifier) test") >:: fun () ->
        assert_equal
          ~printer:(Frenetic_Misc.string_of_list
                      (fun (_, pt, pk) ->
                        (Frenetic_Misc.string_of_pair port_to_string packet_to_string) (pt,pk)))
          expected_pkts pkts in
    TestList [ sem_test; classifier_test ]

end

module TestFilters = struct

  open NetCore_Types
  open Helper

  let test1 =
    let policy = Filter NetCore_Desugar.All in
    mkEvalTest "filter true" policy in_val [in_val]

  let test2 =
    let policy = Filter NoPackets in
    mkEvalTest "filter false" policy in_val []

  let go = TestList [ test1; test2 ]

end

module TestMods = struct

  open NetCore_Types
  open Packet
  open Helper

  let test1 =
    let policy = Act (UpdateDlVlan (None, Some 1)) in
    let Pkt (sid, port, pkt, payload) = in_val in
    let expected_pkt = {pkt with dlVlan = Some 1} in
    let expected_val = Pkt ( sid, port, expected_pkt, payload) in
    mkEvalTest "mod vlan" policy in_val [expected_val]

  let test2 =
    let policy = Seq ( Act (UpdateDlVlan (None, (Some 1)))
                     , Act (UpdateDlVlan ((Some 1), None))) in
    mkEvalTest "mod no effect" policy in_val [in_val]

  let test3 =
    let policy =
      Seq ( Act (UpdateDlVlan (None, (Some 1)))
          , Seq ( Act ToAll
                , Act (UpdateDlVlan ((Some 1), None)))) in
    let Pkt (sid, port, pkt, payload) = in_val in
    let expected_vals = [Pkt (sid, NetCore_Types.All, pkt, payload)] in
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

  let go = TestList [ test1; test2; test3; test4; test5 ]

end

module TestSlices = struct

  open NetCore_Types
  open Packet
  open Helper

  let test1 =
    let policy = Slice (NetCore_Desugar.All, Act ToAll,
                        NetCore_Desugar.All) in
    let Pkt (sid, port, expected_pkt, payload) = in_val in
    let expected_val = Pkt ( sid, All, expected_pkt, payload) in
    mkEvalTest "slice repeater" policy in_val [expected_val]

  let test1' =
    let policy =
      Seq ( Seq (Filter (DlVlan None), Act (UpdateDlVlan (None, Some 1))),
      Seq ( Act ToAll
          , Par ( Seq (Filter (DlVlan (Some 1)), Act (UpdateDlVlan (Some 1, None)))
                , Filter (Not (DlVlan (Some 1)))))) in
    let Pkt (sid, port, expected_pkt, payload) = in_val in
    let expected_val = Pkt ( sid, All, expected_pkt, payload) in
    mkEvalTest "slice' repeater" policy in_val [expected_val]

  let test1'' =
    let policy =
      Seq (Seq (Filter (DlVlan None), Act (UpdateDlVlan (None, Some 1))),
      Seq (Act ToAll,
      Seq (Filter (DlVlan (Some 1)), Act (UpdateDlVlan (Some 1, None))))) in
    let Pkt (sid, port, expected_pkt, payload) = in_val in
    let expected_val = Pkt ( sid, All, expected_pkt, payload) in
    mkEvalTest "slice'' repeater" policy in_val [expected_val]

  let go = TestList [ test1; test1'; test1'' ]

end

module TestParser = struct

  open OpenFlow0x01

  (* For each parsable type, test that parse(marshal(v)) == v for some value v.
   *)
  let test_match = "match marshal/parse test" >:: fun () ->
    let v = Match.all in
    let bits = Cstruct.create (Match.size_of v) in
    let _ = Match.marshal v bits in
    let v' = Match.parse bits in
    assert_equal ~printer:Match.to_string v v'

  let go = TestList [ test_match ]

end


let tests =
  TestList [ TestFilters.go
           ; TestMods.go
           ; TestSlices.go
           ; TestClassifier.go
           ; TestParser.go
(*           ; TestNetCore.go *)
           ]

let _ = run_test_tt_main tests
