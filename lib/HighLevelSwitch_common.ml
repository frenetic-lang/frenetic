module SDN = SDN_Types

module type VERSION_SPECIFIC = sig

  (* Atomic actions for some version of OpenFlow. *)
  type of_action

  (* The type of ports for some version of OpenFlow. *)
  type of_portId

  val from_action : of_portId option -> SDN.action ->
    ModComposition.t * of_action

end

module type S = sig

  type of_action
  type of_portId

  val flatten_par : of_portId option -> SDN.par -> of_action list

 end

 module Make (OF : VERSION_SPECIFIC) 
   : S with type of_action = OF.of_action
        and type of_portId = OF.of_portId = struct

  module Mod = ModComposition

  type of_action = OF.of_action
  type of_portId = OF.of_portId


	(* Converts an abstract action sequence to an OpenFlow action sequence *)
	let rec from_seq (inPort : OF.of_portId option) (seq : SDN.seq) 
	  : Mod.t * OF.of_action list =
	  let f act (mods, of_seq) = 
	    let (mods', of_act) = OF.from_action inPort act in
	    (Mod.seq mods' mods, of_act :: of_seq) in
	  List.fold_right f seq (Mod.none, [])

	(* Converts abstract action union to an OF 1.0 action sequence. This may
	   trigger exceptions if the parallel composition is unrealizable. *)
	let rec from_par (inPort : OF.of_portId option) (par : SDN.par) :
	  Mod.t * OF.of_action list =
	  let f act (mods, of_seq) =
	    let (mods', of_act) = from_seq inPort act in
	    (Mod.par mods' mods, of_act @ of_seq) in
	  List.fold_right f par (Mod.none, [])

	let rec flatten_par (inPort : OF.of_portId option) (par : SDN.par) 
	  : OF.of_action list =
	  let (_, acts) = from_par inPort par in
	  acts

end