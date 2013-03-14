open WordInterface

open Platform0x04
open NetCoreFT
open FaultTolerance

module G = Graph.Graph

module Routing = struct

  let s101 = Int64.of_int 1
  let s102 = Int64.of_int 2
  let s103 = Int64.of_int 3
  let h4 = Int64.of_int 4
  let h5 = Int64.of_int 5
  let h6 = Int64.of_int 6
  open Regex
    (* Simple linear topo 101 <-> 102 <-> 103 *)
  let make_topo () = 
    let topo = G.create () in
    let () = G.add_node topo s101;
      G.add_node topo s102;
      G.add_node topo s103;
      G.add_node topo h4;
      G.add_node topo h5;
      G.add_node topo h6;
      G.add_edge topo s101 (Int32.of_int 1) h4 (Int32.of_int 0);
      G.add_edge topo s101 (Int32.of_int 2) s102 (Int32.of_int 2);
      G.add_edge topo s102 (Int32.of_int 1) h5 (Int32.of_int 0);
      G.add_edge topo s102 (Int32.of_int 2) s101 (Int32.of_int 2);
      G.add_edge topo s102 (Int32.of_int 3) s103 (Int32.of_int 2);
      G.add_edge topo s103 (Int32.of_int 1) h6 (Int32.of_int 0);
      G.add_edge topo s103 (Int32.of_int 2) s102 (Int32.of_int 3);
      G.add_edge topo h4 (Int32.of_int 0) s101 (Int32.of_int 1);
      G.add_edge topo h5 (Int32.of_int 0) s102 (Int32.of_int 1);
      G.add_edge topo h6 (Int32.of_int 0) s103 (Int32.of_int 1) in
    topo

  let (policy, push) = Lwt_stream.create ()
    
  let test_regex = RegPar (RegPol (All, Sequence (Host h4, (Sequence (Hop s101, Sequence (Star, (Sequence (Hop s103, Host h6))))))),
			   RegPol (All, Sequence (Host h6, Sequence (Hop s103, Sequence (Star, (Sequence (Hop s101, Host h4)))))))

  (** Composes learning and routing policies, which together form
      mac-learning. *)      
  let () = let pol = (compile_regex test_regex (make_topo ())) in
	   Printf.printf "%s\n" (policy_to_string pol);
	   push (Some (pol, Pol (All, []), Pol (All, [])))
end

module Make (Platform : PLATFORM) = struct

  module Controller = NetCoreFT.Make (Platform)

  let start () = Controller.start_controller Routing.policy

end

