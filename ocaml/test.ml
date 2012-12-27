open Printf
open Word
open Openflow1_0
open Platform 
open Unix
open MonadicController
open MessagesDef
open NetCore

module Test1 = struct
  module Platform = TestPlatform
  module Controller = Repeater.Make (Platform)

  open Platform

  let test_script () = 
    connect_switch (Word64.from_int64 100L);
    connect_switch (Word64.from_int64 200L);
    let (_, msg) = recv_from_controller (Word64.from_int64 100L) in
    begin
      match msg with
        | FlowModMsg fm ->
          assert (fm = NetCoreController.delete_all_flows)
        | _ -> failwith "expected delete"
    end;
    let (_, msg) = recv_from_controller (Word64.from_int64 100L) in
    begin
      match msg with
        | FlowModMsg fm ->
          assert (fm.mfModCmd = AddFlow)
        | _ -> failwith "expected add"
    end;
    let (_, msg) = recv_from_controller (Word64.from_int64 200L) in
    begin
      match msg with
        | FlowModMsg fm ->
          assert (fm = NetCoreController.delete_all_flows)
        | _ -> failwith "expected delete from 200"
    end;
    let (_, msg) = recv_from_controller (Word64.from_int64 200L) in
    begin
      match msg with
        | FlowModMsg fm ->
          assert (fm.mfModCmd = AddFlow)
        | _ -> failwith "expected add from 100"
    end;    
    eprintf "Terminating test script in style\n%!";
    assert false;
    ()    

  let t1 = Thread.create test_script ()
  let t2 = Thread.create Controller.start ()

  let _ = 
    Thread.join t1;
    Thread.join t2

end



    

(*
module Test1 = struct
  module Platform = TestPlatform
  module Controller = DummyController.Make (Platform)

  open Platform

  let test_script () = 
    connect_switch (Word64.from_int64 100L);
    connect_switch (Word64.from_int64 200L)
    

  let t1 = Thread.create test_script ()
  let t2 = Thread.create Controller.start ()

  let _ = 
    Thread.join t1;
    Thread.join t2


end
*)
