module Platform = SDN
module SDN = SDN_Types
module NetKAT = Types
module Stream = NetCore_Stream
module Log = Lwt_log

let section = Log.Section.make "Controller"


(* Keeps the switch configured with the latest policy on onf_stream. *)
let switch_thread 
  (local_stream : LocalCompiler.RunTime.i Stream.t)
  (feats : SDN.switchFeatures) : unit Lwt.t = 
  let sw_id = feats.SDN.switch_id in
  Lwt_log.info_f ~section "switch connected" >>
  let config_switch local = 
    lwt () = Lwt_log.info_f ~section "About to here" in
    let table = LocalCompiler.RunTime.to_table sw_id local in
    Format.eprintf "Flow table is\n%a\n%!" SDN.format_flowTable table;
    Platform.setup_flow_table sw_id table in 
  lwt () = config_switch (Stream.now local_stream) in
  Lwt_stream.iter_s config_switch (Stream.to_stream local_stream)

let rec start ~port ~pols =
  let local_stream = 
    Stream.map 
      (fun p -> 
	let () = Printf.printf "p: %s\n%!" (Pretty.string_of_policy p) in 
	let d = (Dehop.dehop_policy_opt p) in
	let () = Printf.printf "d: %s\n%!" (Pretty.string_of_policy d) in 
        let l = LocalCompiler.RunTime.compile d in
        let () = Printf.printf "l: %s\n%!" (Pretty.string_of_policy (LocalCompiler.RunTime.decompile l)) in 
        l)
      pols in
  lwt (stop_accept, new_switches) = Platform.accept_switches port  in
  Lwt_stream.iter_p (switch_thread local_stream) new_switches
