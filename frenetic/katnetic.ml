module Run = struct
  open Core.Std
  open Async.Std

  let main update learn policy_queue_size filename =
    let open NetKAT_Types in 
    let open NetKAT_LocalCompiler in
    let open Async_NetKAT in 
    let main () =
      let static = match filename with
	| None   -> create_static id 
	| Some f -> create_from_file f in 
      let app = 
	if learn then 
	  let arp = 
	    seq (create_static (Filter (Test(EthType 0x806))))
		(Flood.create ()) in 
	  let learning = Learning.create () in 
	  seq static (union arp learning)
	  (* JNF: why does this not work? *)
	  (* seq static (slice (Test (EthType 0x806)) arp learning) *)
        else 
	  static in 
      Async_NetKAT_Controller.start ~update ?policy_queue_size app () in
    never_returns (Scheduler.go_main ~max_num_open_file_descrs:4096 ~main ())
end

module Dump = struct
  open Core.Std
  open Async.Std

  type level = All | Policies | Flowtables | Stats

  let with_channel f chan =
    f (NetKAT_Parser.program NetKAT_Lexer.token (Lexing.from_channel chan))

  let with_file f filename =
    In_channel.with_file filename ~f:(with_channel f)

  let profile f =
    let t1 = Unix.gettimeofday () in
    let r = f () in
    let t2 = Unix.gettimeofday () in
    (t2 -. t1, r)

  module Local = struct

    let with_compile (sw : SDN_Types.switchId) (p : NetKAT_Types.policy) =
      let open NetKAT_LocalCompiler in
      let _ = 
        Format.printf "@[Compiling switch %Ld [size=%d]...@]%!"
          sw (NetKAT_Semantics.size p) in
      let c_time, i = profile (fun () -> compile sw p) in
      let t_time, t = profile (fun () -> to_table i) in
      let _ = Format.printf "@[Done [ctime=%fs ttime=%fs tsize=%d]@\n@]%!"
        c_time t_time (List.length t) in
      t

    let flowtable (sw : SDN_Types.switchId) t =
      if List.length t > 0 then
        Format.printf "@[flowtable for switch %Ld:@\n%a@\n@\n@]%!"
          sw
          SDN_Types.format_flowTable t

    let policy p =
      Format.printf "@[%a@\n@\n@]%!" NetKAT_Pretty.format_policy p

    let local f num_switches p =
      let rec loop switch_id =
        if switch_id > num_switches then ()
        else begin
          let swL = Int64.of_int32 switch_id in
          let sw_p = NetKAT_Types.(Seq(Filter(Test(Switch swL)), p)) in
          let t = with_compile swL sw_p in
          f swL t; loop Int32.(switch_id + 1l)
        end in
      loop 0l

    let all sw_num p =
      policy p;
      local flowtable sw_num p

    let stats sw_num p =
      local (fun x y -> ()) sw_num p

    let main level num_switches filename =
      match level with
        | All -> with_file (all num_switches) filename
        | Policies -> with_file policy filename
        | Flowtables -> with_file (local flowtable num_switches) filename
        | Stats -> with_file (stats num_switches) filename
  end
end

open Cmdliner

let run_cmd : unit Cmdliner.Term.t * Cmdliner.Term.info =
  let update =
    let strategy = Arg.enum
      [ ("best-effort", `BestEffort)
      ; ("per-packet-consistent", `PerPacketConsistent) ] in

    let doc = "specify network strategy. $(docv) can either be
    `per-packet-consistent' or `best-effort', which provides no consistentcy
    guarentees." in
    Arg.(value & opt strategy `BestEffort & info ["update"] ~docv:"STRATEGY" ~doc)
  in
  let learn =
    let doc = "enable per-switch L2 learning" in
    Arg.(value & flag & info ["learn"] ~doc)
  in
  let policy =
    let doc = "file containing a static NetKAT policy" in
    Arg.(value & (pos 0 (some file) None) & info [] ~docv:"FILE" ~doc)
  in
  let policy_queue_size =
    let doc = "maximum number of policies to queue before the controller
    modifies the network" in
    Arg.(value & opt (some int) None & info ["policy-queue-size"] ~docv:"SIZE" ~doc)
  in
  let doc = "start a controller that will serve the static policy" in
  Term.(pure Run.main $ update $ learn $ policy_queue_size $ policy),
  Term.info "run" ~doc

let dump_cmd : unit Cmdliner.Term.t * Cmdliner.Term.info =
  let doc = "dump per-switch compiler results and statistics" in
  let switch_id =
    let doc = "the maximum switch id in the policy" in
    Arg.(required & (pos 0 (some int32) None) & info [] ~docv:"NUM_SWITCHES" ~doc)
  in
  let level =
    let doc = "Dump all compiler information (default)" in
    let all = Dump.All, Arg.info ["all"] ~doc in

    let doc = "Dump per-switch policy" in
    let policies = Dump.Policies, Arg.info ["policies"] ~doc in

    let doc = "Dump per-switch flowtables" in
    let flowtables = Dump.Flowtables, Arg.info ["flowtables"] ~doc in

    let doc = "Dump per-switch profiling statistics" in
    let stats = Dump.Stats, Arg.info ["stats"] ~doc in

    Arg.(last & vflag_all [Dump.All] [all;policies;flowtables;stats])
  in
  let policy =
    let doc = "file containing a static NetKAT policy" in
    Arg.(required & (pos 1 (some file) None) & info [] ~docv:"FILE" ~doc)
  in

  Term.(pure Dump.Local.main $ level $ switch_id $ policy),
  Term.info "dump" ~doc

let default_cmd : unit Cmdliner.Term.t * Cmdliner.Term.info =
  let doc = "an sdn controller platform" in
  Term.(ret (pure (`Help(`Plain, None)))),
  Term.info "katnetic" ~version:"1.6.1" ~doc

let cmds = [run_cmd; dump_cmd]

let () = match Term.eval_choice default_cmd cmds with
  | `Error _ -> exit 1 | _ -> exit 0
