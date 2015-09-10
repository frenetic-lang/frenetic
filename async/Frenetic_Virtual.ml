open Frenetic_NetKAT_Virtual_Compiler
module Log = Frenetic_Log

let fmt = Format.formatter_of_out_channel stderr

let print_tables policy : Frenetic_NetKAT_Compiler.t =
  let print_table (sw, t) =
    Format.fprintf fmt "@[%s@]@\n@\n" (Frenetic_OpenFlow.string_of_flowTable ~label:(Int64.to_string sw) t) in
  let fdd = Frenetic_NetKAT_Compiler.compile_global policy in
  let switches = Frenetic_NetKAT_Semantics.switches_of_policy policy in
  let local_pol = Frenetic_NetKAT_Compiler.to_local_pol fdd in
  Frenetic_NetKAT_Compiler.to_dotfile fdd "automaton2.dot";
  Format.fprintf
    fmt "[local] Local Policy:@\n@[%a@]@\n@\n"
    Frenetic_NetKAT_Pretty.format_policy local_pol;
  let tables = List.map (fun sw -> (sw, Frenetic_NetKAT_Compiler.to_table ?pc:(Some Frenetic_Fdd.Field.Vlan) sw fdd)) switches in
  List.iter print_table tables;
  fdd

let policy_from_file fname = Core.Std.In_channel.read_all fname |>
			       Frenetic_NetKAT_Parser.policy_from_string

let pred_from_file fname = Core.Std.In_channel.read_all fname|>
			     Frenetic_NetKAT_Parser.pred_from_string

let setup () =
  let () = Format.pp_set_margin fmt 120 in
  let () = Frenetic_Fdd.Field.set_order
             [ Switch; Location; VSwitch; VPort; IP4Dst; Vlan; TCPSrcPort; TCPDstPort; IP4Src;
               EthType; EthDst; EthSrc; VlanPcp; IPProto; VFabric ] in
  let () = Frenetic_Fdd.Field.set_order
             [ Switch; Location; VSwitch; VPort; IP4Dst; Vlan; TCPSrcPort; TCPDstPort; IP4Src;
               EthType; EthDst; EthSrc; VlanPcp; IPProto; VFabric] in
  ()

let main vpolicy_file vrel_file vtopo_file ving_pol_file ving_file veg_file ptopo_file ping_file peg_file : unit =
  let () = setup () in
  let vpolicy = policy_from_file vpolicy_file in
  let vrel = pred_from_file vrel_file in
  let vtopo = policy_from_file vtopo_file in
  let ving_pol = policy_from_file ving_pol_file in
  let ving = pred_from_file ving_file in
  let veg = pred_from_file veg_file in
  let ptopo = policy_from_file ptopo_file in
  let ping = pred_from_file ping_file in
  let peg = pred_from_file peg_file in
  let global_pol = compile ~log:true vpolicy vrel vtopo ving_pol ving veg ptopo ping peg in
  let opt_pol = Frenetic_NetKAT_Optimize.(mk_big_seq [mk_filter ping; global_pol; mk_filter peg]) in
  let _ =
    begin
      Format.fprintf
	fmt "@\n[global] Parsed: @[%s@] @[%s@] @[%s@] @[%s@] @[%s@] @[%s@] @[%s@] @[%s@] @[%s@] @\n@\n"
	vpolicy_file vrel_file vtopo_file ving_pol_file ving_file veg_file ptopo_file ping_file peg_file;
      Format.fprintf
	fmt "[global] Global Policy:@\n@[%a@]@\n@\n"
	Frenetic_NetKAT_Pretty.format_policy global_pol;
    end in
  let _ = print_tables opt_pol in
  ()

let main2 policy_file : unit =
  let () = setup () in
  let policy_from_file fname = Core.Std.In_channel.read_all fname |>
				 Frenetic_NetKAT_Parser.policy_from_string in
  let policy = policy_from_file policy_file in
  let _ = print_tables policy in
  ()
