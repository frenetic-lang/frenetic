open Core.Std
open Async.Std 

module Platform = Async_Highlevel
module SDN = SDN_Types
module NetKAT = Types

let switch local_stream () feats : unit Deferred.t = 
  let sw_id = feats.SDN.switch_id in
  let next () local = Platform.setup_flow_table sw_id (local sw_id) in 
  Pipe.fold local_stream ~init:() ~f:next

let rec start ~f ~port ~pols =
  let local_stream = Pipe.map pols ~f:f in  
  Platform.accept_switches port >>= fun switch_feats -> 
  Pipe.fold switch_feats ~init:() ~f:(switch local_stream)
