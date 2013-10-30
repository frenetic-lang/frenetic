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

(* We analyze the topology (currently from 't'), and compute the edge
   switches. For edge locations L_e, the policy is (filter L_e;i | filter ~L_e); (p | e). But, it is probably better to instead use the internal locations L_i, making it (filter ~L_i;i | filter L_i); (p | e)
*)

let rec extract_internal_locs t = 
  let open Types in
      match t with
        | Seq(p,q) -> Or (extract_internal_locs p, extract_internal_locs q)
        | Par(p,q) -> Or (extract_internal_locs p, extract_internal_locs q)
        | Choice(p,q) -> Or (extract_internal_locs p, extract_internal_locs q)
        | Link(sw,pt,sw',pt') -> Or (And (Test (Switch, sw), Test (Switch, pt)),
                                     And (Test (Switch, sw'), Test (Switch, pt')))
        | _ -> False

let rec start ~port ~pols =
  let local_stream = 
    Stream.map 
      (fun p -> 
        Printf.printf "p: %s\n%!" (Pretty.string_of_policy p);
	let i,s,t,e = Dehop.dehop_policy p in
        Printf.printf "i: %s\n%!" (Pretty.string_of_policy i);
	Printf.printf "s: %s\n%!" (Pretty.string_of_policy s);
	Printf.printf "t: %s\n%!" (Pretty.string_of_policy t);
        Printf.printf "e: %s\n%!" (Pretty.string_of_policy e);
        let l_i = extract_internal_locs t in
        let open Types in
            let p' = (Seq (Par (Seq (Filter (Neg l_i), i),
                                Filter l_i),
                           Par (s,e))) in
        Printf.printf "p': %s\n%!" (Pretty.string_of_policy p');
        let l = LocalCompiler.RunTime.compile p' in
        Printf.printf "l: %s\n%!" (Pretty.string_of_policy (LocalCompiler.RunTime.decompile l));
        l)
      pols in
  lwt (stop_accept, new_switches) = Platform.accept_switches port  in
Lwt_stream.iter_p (switch_thread local_stream) new_switches

let rec start_no_dehop ~port ~pols =
  let local_stream = 
    Stream.map 
      (fun p -> 
        Printf.printf "p: %s\n%!" (Pretty.string_of_policy p);
        let l = LocalCompiler.RunTime.compile p in
        Printf.printf "l: %s\n%!" (Pretty.string_of_policy (LocalCompiler.RunTime.decompile l));
        l)
      pols in
  lwt (stop_accept, new_switches) = Platform.accept_switches port  in
Lwt_stream.iter_p (switch_thread local_stream) new_switches
