open OUnit
open Printf
open Openflow1_0
open Unix
open MonadicController
open MessagesDef
open NetCoreSemantics
open NetCore
open Packet
open TestPlatform
open Lwt


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

let _ = run_test_tt_main Test1.go
