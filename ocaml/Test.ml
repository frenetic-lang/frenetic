open OUnit
open Printf
open Unix
open MessagesDef
open NetCore
open Packet
open TestPlatform
open Lwt
(* module Z = OpenFlow0x04Parser *)

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

  let pol1 = PoUnion (PoAtom (PrAll, [Forward (PhysicalPort pt1)]),
                      PoAtom (PrAll, [Forward (PhysicalPort pt2)]))

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


let _ = run_test_tt_main 
  (TestList [ Test1.go; 
              Test2.go;
              Test3.go ])
