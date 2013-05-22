open Printf
open NetCore_Types.External
open OUnit

module TestClassifier = struct

  module C = NetCore_Classifier.Make (NetCore_Action.Output)
  open C
  open NetCore_Action.Output
  open NetCore_Pattern

  let test0 =
    "action sequence test" >::
      fun () ->
        assert_equal ~printer:NetCore_Action.Output.to_string
          (forward 5)
          (seq_action (forward 3) (forward 5))

  let test1 =
    "forward action domain should be NetCore_Pattern.all" >::
      fun () ->
        assert_equal ~printer:NetCore_Pattern.to_string
          (domain (List.hd (atoms (forward 1)))) NetCore_Pattern.all

  let test2 =
    "pattern restriction test" >::
      fun () ->
        assert_equal ~printer:NetCore_Pattern.to_string
          (restrict_range
             (List.hd (atoms (forward 1)))
             (NetCore_Pattern.inPort (Physical 1)))
          NetCore_Pattern.all

  let test3 =
    "classifier composition test 1" >::
      fun () ->
        let tbl1 =
          [(NetCore_Pattern.inter (dlDst 0xdeadbeefL) (inPort (Physical 5)),
            forward 1);
           (NetCore_Pattern.all, drop)] in
        let tbl2 =
          [(inPort (Physical 1), forward 2);
           (NetCore_Pattern.all, drop)] in
        assert_equal ~printer:C.to_string
          (sequence tbl1 tbl2)
          [(NetCore_Pattern.inter (dlDst 0xdeadbeefL) (inPort (Physical 5)),
            forward 2);
           (NetCore_Pattern.all, drop)]

  let test4 =
    "classifier composition test 2" >::
      fun () ->
        assert_equal ~printer:C.to_string
          (sequence
             [(dlSrc 0xFFFFL, pass);
              (all, drop)]
             [(inPort (Physical 10), forward 20);
              (inPort (Physical 300), forward 400);
              (all, drop)])
          [(inter (dlSrc 0xFFFFL) (inPort (Physical 10)), forward 20);
           (inter (dlSrc 0xFFFFL) (inPort (Physical 300)), forward 400);
           (dlSrc 0xFFFFL, drop); (* redundant, but our optimizer sucks *)
           (all, drop)]

  let test5 =
    "classifier composition test 3" >::
      fun () ->
        assert_equal ~printer:C.to_string
          (sequence
             [(dlSrc 0xDDDDL, forward 1);
              (all, drop)]
             [(dlDst 0xEEEEL, forward 2);
              (all, drop)])
          [(inter (dlSrc 0xDDDDL) (dlDst 0xEEEEL), forward 2);
           (dlSrc 0xDDDDL, drop); (* redundant *)
           (all, drop)]

  let test6 =
    "classifier sequencing test 4" >::
      fun () ->
        assert_equal ~printer:C.to_string
          (sequence
             [(dlSrc 0xDDDDL, pass);
              (all, drop)]
             [(dlDst 0xEEEEL, forward 2);
              (all, drop)])
          [(inter (dlSrc 0xDDDDL) (dlDst 0xEEEEL), forward 2);
           (dlSrc 0xDDDDL, drop); (* redundant *)
           (all, drop)]


  let test7 =
    "classifier sequencing test 5" >::
      fun () ->
        assert_equal ~printer:C.to_string
          (sequence
             [(dlSrc 0xDDDDL, forward 2);
              (all, drop)]
             [(dlDst 0xEEEEL, pass);
              (all, drop)])
          [(inter (dlSrc 0xDDDDL) (dlDst 0xEEEEL), forward 2);
           (dlSrc 0xDDDDL, drop); (* redundant *)
           (all, drop)]

  let test8 =
    "classifier union regression 1" >::
      fun () ->
        assert_equal ~printer:C.to_string
          (union
             [(dlSrc 0xDDDDL, forward 2);
              (all, drop)]
             [(dlDst 0xEEEEL, forward 30);
              (all, drop)])
          [(inter (dlSrc 0xDDDDL) (dlDst 0xEEEEL), 
            par_action (forward 2) (forward 30));
           (dlSrc 0xDDDDL, forward 2);
           (dlDst 0xEEEEL, forward 30);
           (all, drop)]

  let go =
    TestList [test0; test1; test2; test3; test4; test5; test6; test7; test8]

end

module TestNetCore = struct

  open NetCore_Types.Internal
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
          assert_equal  ~printer:C.to_string
            (NetCore_Compiler.compile_pol test1_pol1 1L) []

  let test2 = 
      "maclearning regression 2" >::
        fun () ->
          assert_equal  ~printer:C.to_string
            (NetCore_Compiler.compile_pol test1_pol2 1L) []

  let test3 = 
      "maclearning regression 3" >::
        fun () ->
          assert_equal  ~printer:C.to_string
            (NetCore_Compiler.compile_pol test1_pol3 1L) []

  let test4 = 
      "maclearning regression 2 || 3" >::
        fun () ->
          assert_equal  ~printer:C.to_string
            (NetCore_Compiler.compile_pol
               (PoUnion (test1_pol2, test1_pol3))
            1L)
            []

  let test5 = 
      "maclearning regression 1 || 2 || 3" >::
        fun () ->
          assert_equal  ~printer:C.to_string
            (NetCore_Compiler.compile_pol
               (PoUnion (test1_pol1, PoUnion (test1_pol2, test1_pol3)))
            1L)
            []

  let test6 =
    "sequencing regression 1" >::
      fun () ->
        assert_equal ~printer:C.to_string
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
        assert_equal ~printer:C.to_string
          (NetCore_Compiler.compile_pol pol2_query 1L)
          []

  let test8 =
    "maclearning regression pol2_fwd1" >::
      fun () ->
        assert_equal ~printer:C.to_string
          (NetCore_Compiler.compile_pol pol2_fwd1 1L)
          []

  let test9 =
    "maclearning regression pol2_fwd_rest" >::
      fun () ->
        assert_equal ~printer:C.to_string
          (NetCore_Compiler.compile_pol pol2_fwd_rest 1L)
          []

  let test10 =
    "maclearning regression pol2" >::
      fun () ->
        assert_equal ~printer:C.to_string
          (NetCore_Compiler.compile_pol pol2 1L)
          []

  let go = TestList [test10; test9; test8; test7; test6; test5;
                     test4; test3; test2; test1]

end

        
        

module Test1 = struct

  module Network = OpenFlow0x01_TestPlatform.Network
  module Controller = NetCore_Controller.Make (OpenFlow0x01_TestPlatform)

  let network_script =
    Network.connect_switch 100L >>
    lwt msg = Network.recv_from_controller 100L in
    Lwt.return ()

  let controller_script =
    Controller.start_controller (Lwt_stream.of_list [Act (To 0)])

  let body = Lwt.pick [controller_script; network_script]

  let go =
    "repeater test" >::
      (bracket
         (fun () -> ())
         (fun () -> Lwt_main.run body)
         (fun () -> Network.tear_down ()))

end

module Helper = struct

  module C = NetCore_Classifier.Make (NetCore_Action.Output)
  open NetCore_Types
  open NetCore_Types.Internal
  open Packet

  let desugar_policy pol =
    let bucket_cell = ref 0 in
    let vlan_cell = ref 0 in
    let genbucket () = incr bucket_cell; !bucket_cell in
    let genvlan () = incr vlan_cell; Some !vlan_cell in
    let get_pkt_handlers : (int, get_packet_handler) Hashtbl.t =
      Hashtbl.create 200 in
    desugar genbucket genvlan pol get_pkt_handlers

  let in_pkt =
    { pktDlSrc = Int64.zero
    ; pktDlDst = Int64.zero
    ; pktDlTyp = 0x90
    ; pktDlVlan = None
    ; pktDlVlanPcp = 0
    ; pktNwHeader = NwUnparsable (0x90, Cstruct.create 8) }

  let in_val =
    Pkt ( Int64.one
        , (NetCore_Pattern.Physical 1)
        , in_pkt
        , (Buf Int32.zero))

  let mkEvalTest name ?debug:(dbg=false) pol in_val expected_vals = 
    let ds_pol = desugar_policy pol in

    if dbg then
      printf "Internal policy:\n%s\n" (pol_to_string ds_pol)
    else
      ();

    let Pkt (in_sid, in_port, _, _) = in_val in
    let expected_pkts =
      List.map (fun (Pkt (_, pr, p, _)) -> (pr, p)) expected_vals in

    (* Test the semantic interpretation. *)
    let vals = classify ds_pol in_val in
    let sem_test = 
      (name ^ " (semantic) test") >:: fun () ->
        assert_equal
          ~printer:(Frenetic_Misc.string_of_list value_to_string)
          expected_vals vals in

    (* Test the classifier interpretation. *)
    let classifier = NetCore_Compiler.compile_pol ds_pol in_sid in

    if dbg then
      printf "Classifier:\n%s\n" (C.to_string classifier)
    else
      ();

    let act = C.scan classifier in_port in_pkt in
    let pkts = NetCore_Action.Output.apply_action act (in_port, in_pkt) in
    let classifier_test =
      (name ^ " (classifier) test") >:: fun () ->
        assert_equal
          ~printer:(Frenetic_Misc.string_of_list
            (Frenetic_Misc.string_of_pair NetCore_Pattern.string_of_port packet_to_string))
          expected_pkts pkts in
    TestList [ sem_test; classifier_test ]

end

module TestFilters = struct

  open NetCore_Types.Internal
  open Helper

  let test1 =
    let policy = Filter All in
    mkEvalTest "filter true" policy in_val [in_val]

  let test2 =
    let policy = Filter NoPackets in
    mkEvalTest "filter false" policy in_val []

  let go = TestList [ test1; test2 ]

end

module TestMods = struct

  open NetCore_Types.Internal
  open Packet
  open Helper

  let test1 =
    let policy = Act (UpdateDlVlan (None, Some 1)) in
    let Pkt (sid, port, pkt, payload) = in_val in
    let expected_pkt = {pkt with pktDlVlan = Some 1} in
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
    let expected_vals = [Pkt (sid, NetCore_Pattern.All, pkt, payload)] in
    mkEvalTest "mod no effect 2" policy in_val expected_vals

  let test4 = 
    let policy = 
      Seq ( Act (UpdateDlVlan (None, Some 1))
          , Filter (DlVlan (Some 1))) in
    let Pkt (sid, port, pkt, payload) = in_val in
    let expected_pkt = {pkt with pktDlVlan = Some 1} in
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

  open NetCore_Types.Internal
  open Packet
  open Helper

  let test1 =
    let policy = Slice (All, Act ToAll, All) in
    let Pkt (sid, port, expected_pkt, payload) = in_val in
    let expected_val = Pkt ( sid, (NetCore_Pattern.All), expected_pkt, payload) in
    mkEvalTest "slice repeater" policy in_val [expected_val]

  let test1' =
    let policy =
      Seq ( Seq (Filter (DlVlan None), Act (UpdateDlVlan (None, Some 1))),
      Seq ( Act ToAll
          , Par ( Seq (Filter (DlVlan (Some 1)), Act (UpdateDlVlan (Some 1, None)))
                , Filter (Not (DlVlan (Some 1)))))) in
    let Pkt (sid, port, expected_pkt, payload) = in_val in
    let expected_val = Pkt ( sid, (NetCore_Pattern.All), expected_pkt, payload) in
    mkEvalTest "slice' repeater" policy in_val [expected_val]

  let test1'' =
    let policy =
      Seq (Seq (Filter (DlVlan None), Act (UpdateDlVlan (None, Some 1))),
      Seq (Act ToAll,
      Seq (Filter (DlVlan (Some 1)), Act (UpdateDlVlan (Some 1, None))))) in
    let Pkt (sid, port, expected_pkt, payload) = in_val in
    let expected_val = Pkt ( sid, (NetCore_Pattern.All), expected_pkt, payload) in
    mkEvalTest "slice'' repeater" policy in_val [expected_val]

  let go = TestList [ test1; test1'; test1'' ]

end

module TestParser = struct

  open OpenFlow0x01
  open OpenFlow0x01_Parser

  (* For each parsable type, test that parse(marshal(v)) == v for some value v.
   *)

  let go = TestList [ ]

end


let tests =
  TestList [ Test1.go
           ; TestFilters.go
           ; TestMods.go
           ; TestSlices.go
           ; TestClassifier.go
           ; TestNetCore.go
           ]

let _ = run_test_tt_main tests

(*
module Test2 = struct

  open NetCore_Types.Internal
  open NetCoreCompiler

  let pt1 = 1
  let pt2 = 2

  let pol1 = PoUnion (PoAtom (PrAll, [Forward (unmodified, PhysicalPort pt1)]),
                      PoAtom (PrAll, [Forward (unmodified, PhysicalPort pt2)]))

  let go =
    TestList [ (*
"optimizer test" >::
    (fun () ->
      let ft = compile_opt pol1 0L in
      Printf.printf "length is %d\n" (List.length ft);
      assert_equal (List.length ft) 2)
               *) ]

end

module Test3 = struct

  open Util

  let test0  = "get_byte test" >::
    (fun () ->
      assert_equal (get_byte 0x112233445566L 5) 0x11)

  let test1 = "manual-check for big-endian encoding" >::
    (fun () ->
      assert_equal (bytes_of_mac 0x112233445566L)
        "\x66\x55\x44\x33\x22\x11")

  let test2 = "mac address parsing test" >::
      (fun () ->
        let k = 0x1234567890abL in
        assert_equal (mac_of_bytes (bytes_of_mac k)) k)

  let go =
    TestList [test0; test1; test2]


end

module OpenFlow0x04SizeTest = struct
  open OpenFlow0x04Parser
  open OpenFlow0x04Types
  open Util

  let test_ofp_header  = "sizeof_ofp_header" >::
    (fun () ->
      assert_equal sizeof_ofp_header 8)

  let test_ofp_flow_mod  = "sizeof_ofp_flow_mod" >::
    (fun () ->
      assert_equal sizeof_ofp_flow_mod 40)

  let test_ofp_match  = "sizeof_ofp_match" >::
    (fun () ->
      assert_equal sizeof_ofp_match 4)

  let test_ofp_instruction_actions  = "sizeof_ofp_instruction_actions" >::
    (fun () ->
      assert_equal sizeof_ofp_instruction_actions 8)

  let test_ofp_action_output  = "sizeof_ofp_action_output" >::
    (fun () ->
      assert_equal sizeof_ofp_action_output 16)

  let go =
    TestList [
      test_ofp_header;
      test_ofp_flow_mod;
      test_ofp_match;
      test_ofp_instruction_actions;
      test_ofp_action_output]
end

module Test4 = struct
  open OpenFlow0x04Parser
  open OpenFlow0x04Types
  open Cstruct

  let test_script () =
    printf "test serialization\n";
    let oc = open_out "test-msg-1.3-msg1" in
    let msg1 = Hello in
    let str = Message.serialize 0l msg1 in
    fprintf oc "%s" str;
    close_out oc;
    let oc = open_out "test-msg-1.3-msg2" in
    let msg2 = FlowMod {
      cookie = ({value = 0xaabbccL; mask = None} : uint64 mask);
      table_id = (0x12 : tableId);
      command = AddFlow;
      idle_timeout = Permanent;
      hard_timeout = Permanent;
      priority = (0x3456 : uint16);
      buffer_id = None;
      out_port = Some (PhysicalPort (0x7890l));
      out_group = None;
      flags = {send_flow_rem = false; check_overlap = false; reset_counts = false; no_pkt_counts = false; no_byt_counts = false};
      ofp_match = [OxmInPort (0x7891l)];
      instructions = [WriteActions ([Output (PhysicalPort (0x7890l))])]
    } in
    let str = Message.serialize 0l msg2 in
    fprintf oc "%s" str;
    close_out oc;
    let oc = open_out "test-msg-1.3-msg3" in
    let action1 : action = SetField (OxmVlanVId ({value = 0x1234; mask = None})) in
    let action2 : action = Output (PhysicalPort (0x2345l)) in
    let bucket1 : bucket = {weight = 0; watch_port = Some (0x67l); watch_group = None; actions = [action1; action2]} in
    let msg3 = GroupMod (AddGroup (FF, 0x12l,
      [bucket1])) in
    let str = Message.serialize 0l msg3 in
    fprintf oc "%s" str;
    close_out oc;
    ()

  let go =
    "serialize test" >::
      (fun () -> test_script())
end

module Test5 = struct
  open OpenFlow0x04Parser
  open OpenFlow0x04Types
  open Cstruct

  let test_script () =
    printf "test OF parser\n";
    let str = "\x04\x0a\x00\x2a\x00\x00\x00\x00\x00\x00\x23\x45\x00\xa0\x00\x05\xcc\xbb\xaa\x00\x00\x00\x00\x00\x00\x01\x00\x0c\x80\x00\x00\x04\x91\x78\x00\x00\x01\x00\x00\x00\x00\x00" in
    let buf = Cstruct.of_string str in
    let msg5 = Message.parse buf in
    ()

  let go =
    "parse test" >::
      (fun () -> test_script())
end

module PacketParser = struct
  open PacketParser
  open Packet
  let eth1 : packet =
    { pktDlSrc = Util.mac_of_bytes "ABCDEF";
      pktDlDst = Util.mac_of_bytes "123456";
      pktDlTyp = 0x0042;
      pktDlVlan = 0x7;
      pktDlVlanPcp = 0;
      pktNwHeader = NwUnparsable (0x0042, Cstruct.of_string "XYZ") }

  let f eth = assert_equal (Some eth) (parse_packet (serialize_packet eth))

  let test1 = "Ethernet parser test" >:: (fun () -> f eth1)

  let go = TestList [test1]

end

let _ = run_test_tt_main
  (TestList [ OpenFlow0x04SizeTest.go;
              Test1.go;
              Test2.go;
              Test3.go;
              Test4.go;
              Test5.go;
	      PacketParser.go])
*)
