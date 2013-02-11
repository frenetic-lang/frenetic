open MessagesDef
open WordInterface

open Platform
open NetCore
open Packet

module H = Hashtbl

module Routing = struct

  let s101 = Int64.of_int 101
  let s102 = Int64.of_int 102
  let s103 = Int64.of_int 103
  open Regex
    (* Simple linear topo 101 <-> 102 <-> 103 *)
  let make_topo () = 
    let topo = Hashtbl.create 3 in
    let ports = Hashtbl.create 3 in
    let s101Ports = Hashtbl.create 3 in
    let s102Ports = Hashtbl.create 3 in
    let s103Ports = Hashtbl.create 3 in
    let () = H.add ports s101 s101Ports;
      H.add ports s102 s102Ports;
      H.add ports s103 s103Ports;
      H.add topo s101 [s102];
      H.add s101Ports s102 0;
      H.add topo s102 [s101;s103];
      H.add s102Ports s101 0;
      H.add s102Ports s103 1;
      H.add topo s103 [s102];
      H.add s103Ports s102 0 in
    (topo, ports)

  let (policy, push) = Lwt_stream.create ()
    
  let test_regex = RegPol (All, Sequence (Hop (Int64.of_int 101), Sequence (Star, Hop (Int64.of_int 103))))

  (** Composes learning and routing policies, which together form
      mac-learning. *)      
  let () = push (Some (compile_regex test_regex (make_topo ())))
end

module Make (Platform : PLATFORM) = struct

  module Controller = NetCore.Make (Platform)

  let start () = Controller.start_controller Routing.policy

end

