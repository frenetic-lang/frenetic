open Frenetic_NetKAT_Virtual_Compiler
module Log = Frenetic_Log

let fmt = Format.formatter_of_out_channel stderr 

let print_table (sw, t) =
  Format.fprintf fmt "@[%s@]@\n@\n" (Frenetic_OpenFlow.string_of_flowTable ~label:(Int64.to_string sw) t)

let main vpolicy_file vrel_file vtopo_file ving_pol_file ving_file veg_file ptopo_file ping_file peg_file : unit = 
  let () = Format.pp_set_margin fmt 120 in
  let () = Frenetic_Fdd.Field.set_order
             [ Switch; Location; VSwitch; VPort; IP4Dst; Vlan; TCPSrcPort; TCPDstPort; IP4Src;
               EthType; EthDst; EthSrc; VlanPcp; IPProto ] in
  let policy_from_file fname = Core.Std.In_channel.read_all fname |>
				 Frenetic_NetKAT_Parser.policy_from_string in
  let pred_from_file fname = Core.Std.In_channel.read_all fname|>
			       Frenetic_NetKAT_Parser.pred_from_string in
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
  let fdd = Frenetic_NetKAT_Local_Compiler.compile_global opt_pol in
  let _ = 
    begin
      Format.fprintf 
	fmt "@\n[global] Parsed: @[%s@] @[%s@] @[%s@] @[%s@] @[%s@] @[%s@] @[%s@] @[%s@] @[%s@] @\n@\n"
	vpolicy_file vrel_file vtopo_file ving_pol_file ving_file veg_file ptopo_file ping_file peg_file;
      Format.fprintf 
	fmt "[global] Global Policy:@\n@[%a@]@\n@\n"
	Frenetic_NetKAT_Pretty.format_policy global_pol;
    end in
  let switches = 
    Frenetic_NetKAT_Semantics.switches_of_policy 
      (Frenetic_NetKAT_Optimize.mk_seq (Frenetic_NetKAT.Filter ping) global_pol) in
  let tables = List.map (fun sw -> (sw, Frenetic_NetKAT_Local_Compiler.to_table sw fdd)) switches in
  List.iter print_table tables;
  ()
    
let main2 policy_file : unit = 
  let fmt = Format.formatter_of_out_channel stderr in
  let () = Format.pp_set_margin fmt 120 in
  let () = Frenetic_Fdd.Field.set_order
             [ Switch; Location; VSwitch; VPort; IP4Dst; Vlan; TCPSrcPort; TCPDstPort; IP4Src;
               EthType; EthDst; EthSrc; VlanPcp; IPProto ] in
  let policy_from_file fname = Core.Std.In_channel.read_all fname |>
				 Frenetic_NetKAT_Parser.policy_from_string in
  let policy = policy_from_file policy_file in
  let switches = Frenetic_NetKAT_Semantics.switches_of_policy policy in 
  let fdd = Frenetic_NetKAT_Local_Compiler.compile_global policy in 
  Printf.printf "*** FDD ***\n\n%s\n\n" (Frenetic_NetKAT_Local_Compiler.to_string fdd);
  let tables = List.map (fun sw -> (sw, Frenetic_NetKAT_Local_Compiler.to_table ?pc:(Some Frenetic_Fdd.Field.Vlan) sw fdd)) switches in 
  List.iter print_table tables;
  ()
