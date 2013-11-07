module Platform = SDN
module SDN = SDN_Types
module NetKAT = Types
module Log = Lwt_log

let section = Log.Section.make "Controller"


(* Keeps the switch configured with the latest policy on onf_stream. *)
let switch_thread 
  (local_stream : LocalCompiler.RunTime.i NetKAT_Stream.t)
  (feats : SDN.switchFeatures) : unit Lwt.t = 
  let sw_id = feats.SDN.switch_id in
  Lwt_log.info_f ~section "switch connected" >>
  let config_switch local = 
    let table = LocalCompiler.RunTime.to_table sw_id local in
    Platform.setup_flow_table sw_id table in 
  lwt () = config_switch (NetKAT_Stream.now local_stream) in
  Lwt_stream.iter_s config_switch (NetKAT_Stream.to_stream local_stream)

let rec start ~port ~pols =
  let local_stream =
    NetKAT_Stream.map 
      (fun p -> 
        Printf.printf "p: %s\n%!" (Pretty.string_of_policy p);
        let p' = Dehop.dehop_policy p in
        Printf.printf "p': %s\n%!" (Pretty.string_of_policy p');
        let l = LocalCompiler.RunTime.compile p' in
        Printf.printf "l: %s\n%!" (Pretty.string_of_policy (LocalCompiler.RunTime.decompile l));
        l)
      pols in
  lwt (stop_accept, new_switches) = Platform.accept_switches port  in
  Lwt_stream.iter_p (switch_thread local_stream) new_switches

let rec start_no_dehop ~port ~pols =
  let local_stream =
    NetKAT_Stream.map
      (fun p ->
        Printf.printf "p: %s\n%!" (Pretty.string_of_policy p);
        let l = LocalCompiler.RunTime.compile p in
        Printf.printf "l: %s\n%!" (Pretty.string_of_policy (LocalCompiler.RunTime.decompile l));
        l)
      pols in
  lwt (stop_accept, new_switches) = Platform.accept_switches port  in
  Lwt_stream.iter_p (switch_thread local_stream) new_switches
