module Platform = SDN
module SDN = SDN_Types
module NetKAT = NetKAT_Types
module Stream = NetCore_Stream

(* Keeps the switch configured with the latest policy on onf_stream. *)
let switch_thread 
  (local_stream : NetKAT_Compiler.SwitchCompiler.RunTime.i Stream.t)
  (feats : SDN.switchFeatures) : unit Lwt.t =
  let sw_id = feats.SDN.switch_id in
  let config_switch local = 
    let table = NetKAT_Compiler.SwitchCompiler.RunTime.to_table sw_id local in 
    Platform.setup_flow_table sw_id table in 
  lwt () = config_switch (Stream.now local_stream) in
  Lwt_stream.iter_s config_switch (Stream.to_stream local_stream)

let rec start ~port ~pols =
  let local_stream = Stream.map NetKAT_Compiler.SwitchCompiler.RunTime.compile pols in
  lwt (stop_accept, new_switches) = Platform.accept_switches port  in
  Lwt_stream.iter_p (switch_thread local_stream) new_switches
