open Core.Std
open Async.Std

module Log = Async_OpenFlow.Log
let tags = [("netkat", "arp")]

module SwitchMap = Map.Make(Int64)

module PortSet = Set.Make(Int32)

let pred = NetKAT_Types.(Test(EthType 0x806))

let create () =
  let open NetKAT_Types in 
  let open Async_NetKAT in 
  let state = ref SwitchMap.empty in

  let gen_pol () =
    let flood switch_id = 
      match SwitchMap.find !state switch_id with 
      | None -> 
	 drop
      | Some ports -> 
	 PortSet.fold ports ~init:drop ~f:(fun acc port_id -> 
    	   Union(Mod(Location(Physical port_id)), acc)) in 
    SwitchMap.fold !state ~init:drop ~f:(fun ~key:switch_id ~data:ports acc ->
      Union(Seq(Filter(And(Test(Switch switch_id), pred)), flood switch_id), acc)) in 

  let default = drop in 

  let handler t w () e = match e with
    | SwitchUp(switch_id) ->
       Log.of_lazy ~tags ~level:`Info (lazy (Printf.sprintf 
         "[arp] ↑ { switch = %Lu }" switch_id));
       state := SwitchMap.add !state ~key:switch_id ~data:PortSet.empty;
      return (Some(gen_pol ()))
    | SwitchDown(switch_id) ->
       Log.of_lazy ~tags ~level:`Info (lazy (Printf.sprintf 
         "[arp] ↓ { switch = %Lu }" switch_id));
       state := SwitchMap.remove !state switch_id;
       return (Some(gen_pol ()))
    | PortUp(switch_id, port_id) -> 
       Log.of_lazy ~tags ~level:`Info (lazy (Printf.sprintf 
         "[arp] ↑ { switch = %Lu; port = %lu }" switch_id port_id));
       (match SwitchMap.find !state switch_id with 
	| None -> 
	   ()
	| Some ports -> 
	   state := SwitchMap.add !state ~key:switch_id ~data:(PortSet.add ports port_id));
       return (Some (gen_pol ()))
    | PortDown(switch_id, port_id) -> 
       Log.of_lazy ~tags ~level:`Info (lazy (Printf.sprintf 
         "[arp] ↓ { switch = %Lu; port = %lu }" switch_id port_id));
       (match SwitchMap.find !state switch_id with 
	| None -> 
	   ()
	| Some ports -> 
	   state := SwitchMap.add !state ~key:switch_id ~data:(PortSet.remove ports port_id));       
       return (Some(gen_pol()))
    | _ -> 
       return None in
      
  create ~pipes:PipeSet.empty default handler
