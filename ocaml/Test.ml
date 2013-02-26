open OUnit
open Printf
open Unix
open MessagesDef
open NetCore
open Packet
open TestPlatform
open Lwt

module Z = FaultTolerantCompiler

module Test1 = struct

  module Controller = Repeater.Make (TestPlatform)

  let test_script () = 
    eprintf "[test] started test script...\n%!";
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
      Printf.eprintf "length is %d\n" (List.length ft);
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
        printf "bytes is %s, str is %Lx\n" (bytes_of_mac k) (mac_of_bytes (bytes_of_mac k));
        assert_equal (mac_of_bytes (bytes_of_mac k)) k)
    
  let go = 
    TestList [test0; test1; test2]


end

module Test4 = struct
  open OpenFlow0x04Parser
  open OpenFlow0x04Types
  open Cstruct

  let test_script () =
    let oc = open_out "test-msg-1.3" in
    let msg1 = Hello in
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
    let str1 = Message.serialize 0l msg1 in
    let str2 = Message.serialize 0l msg2 in
    printf "test serialization";
    fprintf oc "%s" str1;
    close_out oc;
    ()

  let go =
    "serialize test" >::
      (fun () -> test_script())
end

module PacketParser = struct
  open PacketParser
  open Packet
  let eth : packet = 
    { pktDlSrc = Util.mac_of_bytes "ABCDEF";
      pktDlDst = Util.mac_of_bytes "123456";
      pktDlTyp = 0x999; 
      pktDlVlan = 0;
      pktDlVlanPcp = 0;
      pktNwHeader = NwUnparsable (0x999, Cstruct.of_string "") }

  let string_of_eth pkt = 
    sprintf "\n{ pktDlSrc = %s;\n  pktDlDst = %s\n  pktDlTyp : %d\n  pktDlVlan : %d\n  pktDlVlanPcp : %d }" 
      (Util.string_of_mac pkt.pktDlSrc)
      (Util.string_of_mac pkt.pktDlDst)
      (pkt.pktDlTyp)
      (pkt.pktDlVlan)
      (pkt.pktDlVlanPcp)
    
  let test1 = "Ethernet parser test" >::
    (fun () -> 
      let etho = Some eth in 
      let etho' = parse_packet (marshal_packet eth) in 
      match etho, etho' with 
      | Some e, Some e' -> 
	Printf.printf "\nETH  : %s\nETH' : %s\n" 
	  (string_of_eth e) (string_of_eth e')
      | _ -> 
	Printf.sprintf "OOPS\n";
      assert_equal etho' etho)
  
  let go = TestList [test1]

end 

let _ = run_test_tt_main 
  (TestList [ Test1.go; 
              Test2.go;
              Test3.go;
              Test4.go;
	      PacketParser.go])
