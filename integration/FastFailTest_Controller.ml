open Core.Std
open Async.Std

open Frenetic_OpenFlow0x04_Controller
open Frenetic_NetKAT_Local_Compiler
open Frenetic_OpenFlow
open Frenetic_NetKAT
open Frenetic_NetKAT_Pretty

let polAtoB = 
  let a_to_b = Filter (And ((Test (EthSrc 1L)), (Test (EthDst 2L)))) in
  let sw0 = Seq ((Filter (Test (Switch 0L))), (Mod (Location (FastFail [1l;2l])))) in
  let sw1 = Seq ((Filter (Test (Switch 1L))), (Mod (Location (Physical 1l)))) in
  let sw2 = Seq ((Filter (Test (Switch 2L))), (Mod (Location (Physical 1l)))) in
  let sw3 = Seq ((Filter (Test (Switch 3L))), (Mod (Location (Physical 3l)))) in
  (Seq (a_to_b, (Union (sw0, (Union (sw1, (Union (sw2, sw3)))))))) 

let polBtoA = 
  let b_to_a = Filter (And ((Test (EthSrc 2L)), (Test (EthDst 1L)))) in
  let sw3 = Seq ((Filter (Test (Switch 4L))), (Mod (Location (Physical 2l)))) in
  let sw2 = Seq ((Filter (Test (Switch 3L))), (Mod (Location (Physical 2l)))) in
  let sw0 = Seq ((Filter (Test (Switch 1L))), (Mod (Location (Physical 3l)))) in
  (Seq (b_to_a, (Union (sw0, (Union (sw2, sw3)))))) 

let pol = (Union (polAtoB, polBtoA))

let main () =
  Frenetic_Log.info "Starting controller";
  let layout = Frenetic_Fdd.Field.all_fields in 
  let fdd = compile pol ~order:(`Static layout) in
  let _ = Tcp.Server.create ~on_handler_error:`Raise (Tcp.on_port 6633)
    (fun _ reader writer -> 
      let message_sender = send_message writer in
      let flow_sender = implement_flow writer fdd [layout] in
      client_handler reader message_sender flow_sender) 
  in ()

let () =
  main ();
  never_returns (Scheduler.go ())
