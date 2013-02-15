open MessagesDef
open WordInterface

open Platform
open NetCore
open Packet

module H = Hashtbl

module Routing = struct

  let s101 = Int64.of_int 1
  let s102 = Int64.of_int 2
  let s103 = Int64.of_int 3
  let h4 = Int64.of_int 4
  let h6 = Int64.of_int 6
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
      H.add topo s101 [s102; h4];
      H.add s101Ports s102 2;
      H.add s101Ports h4 0;
      H.add topo s102 [s101;s103];
      H.add s102Ports s101 2;
      H.add s102Ports s103 3;
      H.add topo s103 [s102; h6];
      H.add s103Ports s102 2;
      H.add s103Ports h6 0;
      H.add topo h4 [s101];
      H.add topo h6 [s103] in
    (topo, ports)

  let (policy, push) = Lwt_stream.create ()
    
  let test_regex = RegPar (RegPol (All, Sequence (Host h4, (Sequence (Hop s101, Sequence (Star, (Sequence (Hop s103, Host h6))))))),
			   RegPol (All, Sequence (Host h6, Sequence (Hop s103, Sequence (Star, (Sequence (Hop s101, Host h4)))))))

  (** Composes learning and routing policies, which together form
      mac-learning. *)      
  let () = let pol = (compile_regex test_regex (make_topo ())) in
	   Printf.printf "%s\n" (policy_to_string pol);
	   push (Some pol)
end

module Make (Platform : PLATFORM) = struct

  module Controller = NetCore.Make (Platform)

  let start () = Controller.start_controller Routing.policy

end

