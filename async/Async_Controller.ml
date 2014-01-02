open Core.Std
open Async.Std 

module Platform = Async_Highlevel
module SDN = SDN_Types
module NetKAT = Types

module Log = Async_OpenFlow.Log

let _ = Log.set_level `Debug

let _ = Log.set_output 
          [Log.make_colored_filtered_output 
             [("openflow", "socket");
              ("openflow", "serialization")]]

let switch local_stream () feats : unit Deferred.t = 
  let sw_id = feats.SDN.switch_id in
  Log.info "setting up switch %d" (VInt.get_int sw_id);
  let next () local = Platform.setup_flow_table sw_id (local sw_id) in 
  Pipe.fold local_stream ~init:() ~f:next

let rec start ~f ~port ~pols =
  let local_stream = Pipe.map pols ~f:f in  
  Platform.accept_switches port >>= fun switch_feats -> 
  Log.info "got switch features"; 
  Pipe.fold switch_feats ~init:() ~f:(switch local_stream)
