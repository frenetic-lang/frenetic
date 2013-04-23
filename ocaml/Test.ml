open OUnit
open Printf
open Unix
open OpenFlow0x01Types
open NetCore
open Packet
open TestPlatform
open Lwt

module Test1 = struct

  module Controller = Modules.Repeater.Make (TestPlatform)

  let test_script () = 
    connect_switch 100L >>
    connect_switch 200L >>
    lwt msg = recv_from_controller 100L in
    assert_equal
      ~msg:"delete all flows from switch 100"
      (0l, FlowModMsg NetCoreController.delete_all_flows)
      msg;
    lwt (_, msg) = recv_from_controller 100L in
    begin
      match msg with
        | FlowModMsg fm ->
          assert_equal ~msg:"add flow" AddFlow fm.mfModCmd

        | _ -> assert_failure "expected flow mod"
    end;
    lwt msg = recv_from_controller 200L in
    assert_equal
      ~msg:"delete all flows from switch 200"
      (0l, FlowModMsg NetCoreController.delete_all_flows)
      msg;
    lwt (_, msg) = recv_from_controller 200L in
    begin
      match msg with
        | FlowModMsg fm ->
          assert (fm.mfModCmd = AddFlow)
        | _ -> failwith "expected add from 100"
    end;
    return ()

  let body () = 
    Lwt.async (fun () -> (Controller.start ())); 
    test_script ()
      
    

  let go = 
    "repeater test" >::
      (bracket 
         (fun () -> ())
         (fun () ->
           Lwt_main.run (body ()))
         (fun () -> tear_down ()))

end 

module Test2 = struct

  open NetCoreEval
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
      Printf.Misc.Log.printf "length is %d\n" (List.length ft);
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
