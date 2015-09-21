open Async.Std
open Frenetic_NetKAT
module Log = Frenetic_Log
       
module type Controller = sig
  val update_policy : policy -> unit Deferred.t
  val start : int -> unit
end

module Make : Controller = struct

  module OF = Frenetic_OpenFlow0x04
  module Controller = Frenetic_OpenFlow0x04_Controller
  module Compiler = Frenetic_NetKAT_Compiler
  module Field = Frenetic_Fdd.Field

  (* TODO(jcollard): Type should probalby be changed to use `Heuristic *)
  let layout : Compiler.flow_layout ref = ref [Field.all_fields]

  let options = {Compiler.default_compiler_options with field_order = `Static (List.concat !layout)}

  type switch =
    {
      id : OF.switchId
    ; send_message : Frenetic_OpenFlow_Header.xid -> OF.Message.t -> unit
    ; writer : Writer.t
    }

  (* TODO(jcollard): Needs to support an arbitrary number of switches *)
  let switches : (OF.switchId, switch) Hashtbl.t = Hashtbl.create 100

  let update_switch fdd layout sw_id switch =
    Controller.implement_flow switch.writer fdd layout sw_id

  let update_switches ( p : policy ) : unit Deferred.t =
    let layout = !layout in    
    let compiler_opts = {Compiler.default_compiler_options with field_order = `Static (List.concat layout)} in
    let fdd = Compiler.compile_local p ~options:compiler_opts in
    Hashtbl.iter (update_switch fdd layout) switches;
    Deferred.unit
			      
			      
  let (pol_reader, pol_writer) = Pipe.create ()			      
  let update_policy (pol : policy) : unit Deferred.t =
    Pipe.write pol_writer pol

  let start ( of_port : int ) : unit =
    Log.info "Starting OpenFlow 1.3 controller";
    Log.info "Using flow tables: %s" (Compiler.layout_to_string !layout);
    let layout = !layout in
    let pol = drop in
    let compiler_opts = {Compiler.default_compiler_options with field_order = `Static (List.concat layout)} in
    let fdd = Compiler.compile_local pol ~options:compiler_opts in
    let _ = Tcp.Server.create ~on_handler_error:`Raise (Tcp.on_port of_port)
	    (fun _ reader writer -> 
	     Log.info "Connection.";
	     let message_sender = Controller.send_message writer in
	     let flow_sender =
	       (fun sw_id ->
		let sw =
		  {
		    id = sw_id
		  ; writer = writer
		  ; send_message = Controller.send_message writer
		  }
		in Hashtbl.add switches sw_id sw;
		   Controller.implement_flow writer fdd layout sw_id)
	     in
	     Controller.client_handler reader message_sender flow_sender) 
    in don't_wait_for (Pipe.iter pol_reader ~f:update_switches)	     
end
