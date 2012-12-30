open MessagesDef
open WordInterface

open Platform
open NetCore
open Packet

module Learning = struct
    
  (** Maps to ports that hosts are attached on each switch *)
  let learned_hosts : (switchId * Word48.t, portId) Hashtbl.t = 
    Hashtbl.create 100

  let (policy, push) = Lwt_stream.create ()

  let make_unknown_predicate () = 
    Not
      (Hashtbl.fold 
         (fun (sw,eth) pt pol -> 
           Or (And (And (Switch sw, InPort pt), DlSrc eth), pol))
         learned_hosts
         None)

  let rec make_learning_policy () = 
    Pol (make_unknown_predicate (),
         [GetPacket learn_host])

  and learn_host sw pt pk : unit = 
    Printf.printf "Got packet from %Ld on %d" sw pt;
    Hashtbl.replace learned_hosts (sw, pk.pktDlSrc) pt;
    push (Some (make_learning_policy ()))
  
  let _ = push (Some (Pol (All, [GetPacket learn_host])))

end 

module Routing = struct

  let make_routing_policy () = 
    Hashtbl.fold
      (fun (sw, src) _ pol ->
        Hashtbl.fold
          (fun (sw', dst) port pol ->
            if sw = sw' then
              Par (Pol (And (DlSrc src, DlDst dst), [To port]),  pol)
            else
              pol)
          Learning.learned_hosts
          pol)
      Learning.learned_hosts
      (Pol (Learning.make_unknown_predicate (), [ToAll]))

  let policy = Lwt_stream.map (fun learning_pol ->
    Par (learning_pol, make_routing_policy ()))
      Learning.policy
end


module Make (Platform : PLATFORM) = struct

  module Controller = NetCore.Make (Platform)

  let start () = Controller.start_controller Routing.policy

end

