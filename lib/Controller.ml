module Platform = SDN
module SDN = SDN_Types
module NetKAT = NetKAT_Types
module Stream = NetCore_Stream
module ONF = NetKAT_Compiler.Local

(* Keeps the switch configured with the latest policy on onf_stream. *)
let switch_thread 
  (onf_stream : ONF.local Stream.t)
  (feats : SDN.switchFeatures) : unit Lwt.t =
  let sw_id = feats.SDN.switch_id in
  let config_switch onf = 
    Platform.setup_flow_table sw_id (ONF.local_to_table sw_id onf) in
  lwt () = config_switch (Stream.now onf_stream) in
  Lwt_stream.iter_s config_switch (Stream.to_stream onf_stream)

let rec start ~port ~pols =
  let onf_stream = Stream.map ONF.compile pols in
  lwt (stop_accept, new_switches) = Platform.accept_switches port  in
  Lwt_stream.iter_p (switch_thread onf_stream) new_switches
