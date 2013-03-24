open ControllerInterface
open MessagesDef
open Packet
open Platform
open Printf
open NetCoreSyntax

module CoqCtrl = FwOFSimpleExtractableController

module type POLICY = sig
  val policy : policy
  (* Necessary due to static compilation in FwOF. *)
  val switches : switchId list
end

(* Topology is not relevant for execution, only the proof. See the note below.
*)
module MakePolTopo (Policy : POLICY) : CoqCtrl.POLICY_AND_TOPOLOGY = struct

  (* Note: this is not relevant for execution. It is only needed for
     verification. The signatures in Coq (FwOFSignatures.v) should be better
     factored so that this cruft is not neeeded. *)
  let topo _ = None  

  let handlers = Hashtbl.create 0

  let pol = desugar_policy Policy.policy handlers

  let get_pkt out = 
    let open NetCoreEval in
    match out with
      | OutPkt (sw,PhysicalPort pt, pk, Datatypes.Coq_inl buf) ->
        Some (pt, (pk, buf))
      | _ -> None
        

  let abst_func sw pt (pk,buf) = 
    let open NetCoreEval in
    Types.filter_map get_pkt (classify pol (InPkt (sw,pt,pk, Some buf)))

end


module Make (Platform : PLATFORM) (Policy : POLICY) = struct

  module Controller = CoqCtrl.Make (MakePolTopo (Policy))

  type state = Controller.state

  let init_packet_out () = {
    Controller.pktsToSend = []; 
    Controller.switchStates = []
  }

  let pending_switches : (switchId, bool) Hashtbl.t = Hashtbl.create 100

  let rec accept_switches () = 
    Lwt.bind (Platform.accept_switch ())
      (fun feats -> 
        (if Hashtbl.mem pending_switches feats.switch_id then
          Hashtbl.remove pending_switches feats.switch_id
        else
          eprintf "[VerifiedNetCore.ml]: unexpected connection from %Ld"
            feats.switch_id);
        if Hashtbl.length pending_switches > 0 then
          begin
            eprintf "[VerifiedNetCore.ml]: waiting for next switch.";
            accept_switches ()
          end
        else
          Lwt.return ())

  let start (init_state : state) = 
    List.iter (fun sw -> Hashtbl.add pending_switches sw true) Policy.switches;
    accept_switches ()

end

(*
  (Handlers : HANDLERS) = struct

  (* The monad is written in OCaml *)
  module NetCoreMonad = MakeNetCoreMonad (Platform) (Handlers)
  (* The controller is written in Coq *)
  module Controller = NetCoreController.Make (NetCoreMonad)

  let start_controller policy_stream =
    let init_state = { 
      NetCoreController.policy = drop_all_packets; 
      NetCoreController.switches = []
    } in
    let policy_stream = Lwt_stream.map (fun v -> Policy v) policy_stream in
    let event_stream = Lwt_stream.map (fun v -> Event v)
      (Lwt_channel.to_stream NetCoreMonad.events) in
    let event_or_policy_stream = Lwt_stream.choose 
      [ event_stream ; policy_stream ] in
    let body = fun state ->
      Lwt.bind (Lwt_stream.next event_or_policy_stream)
        (fun v -> match v with
          | Event ev -> 
            printf "[NetCore.ml] new event, calling handler\n%!";
            Controller.handle_event ev state
          | Policy pol ->
            printf "[NetCore.ml] new policy\n%!";
            Controller.set_policy pol state) in
    let main = NetCoreMonad.forever body in
    NetCoreMonad.run init_state main

end

module Make (Policy : POLICY) (Platform : PLATFORM) = struct

  let get_pkt_handlers : (int, get_packet_handler) Hashtbl.t = Hashtbl.create 0

  let start () 

    Hashtbl.create 200

  module Handlers : HANDLERS = struct
      
    let get_packet_handler queryId switchId portId packet = 
      printf "[NetCore.ml] Got packet from %Ld\n" switchId;
        (Hashtbl.find get_pkt_handlers queryId) switchId portId packet
  end

  module Controller = MakeDynamic (Platform) (Handlers)

  let start_controller (pol : policy Lwt_stream.t) : unit Lwt.t = 
    Controller.start_controller
      (Lwt_stream.map 
         (fun pol -> 
           printf "[NetCore.ml] got a new policy%!\n";
           NetCoreSyntax.desugar_policy pol get_pkt_handlers)
         pol)

end
*)
