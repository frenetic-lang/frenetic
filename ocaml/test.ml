open OUnit
open Printf
open Openflow1_0
open Platform 
open Unix
open MonadicController
open MessagesDef
open NetCoreSemantics
open NetCore
open Packet

open TestPlatform

module Test1 = struct

  module Controller = Repeater.Make (TestPlatform)

  let test_script () = 
    connect_switch 100L;
    connect_switch 200L;
    assert_equal
      ~msg:"delete all flows from switch 100"
      (0l, FlowModMsg NetCoreController.delete_all_flows)
      (recv_from_controller 100L);
    let (_, msg) = recv_from_controller 100L in
    begin
      match msg with
        | FlowModMsg fm ->
          assert_equal ~msg:"add flow" AddFlow fm.mfModCmd

        | _ -> assert_failure "expected flow mod"
    end;
    assert_equal
      ~msg:"delete all flows from switch 200"
      (0l, FlowModMsg NetCoreController.delete_all_flows)
      (recv_from_controller 200L);
    let (_, msg) = recv_from_controller 200L in
    begin
      match msg with
        | FlowModMsg fm ->
          assert (fm.mfModCmd = AddFlow)
        | _ -> failwith "expected add from 100"
    end

  let go = 
    "repeater test" >::
      (bracket 
         (fun () -> Thread.create Controller.start ())
         (fun _ -> test_script ())
         (fun h -> Thread.kill h; tear_down ()))
      
end

let _ = run_test_tt_main Test1.go
