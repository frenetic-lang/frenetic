open Frenetic_NetKAT_Virtual_Compiler
open Frenetic_NetKAT_Pretty
module Log = Frenetic_Log

let main vpolicy_file vrel_file vtopo_file ving_pol_file ving_file veg_file ptopo_file ping_file peg_file : unit = 
    let fmt = Format.formatter_of_out_channel stderr in
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
    (* let _ = generate_fabrics ~log:true  vrel vtopo ving veg ptopo ping peg in  *)
    let global_pol = compile ~log:true vpolicy vrel vtopo ving_pol ving veg ptopo ping peg in
    Printf.printf "global_pol: %s\n\n%!" (string_of_policy global_pol);
    let fdks = NetKAT_GlobalFDDCompiler.of_policy global_pol ~dedup:false in
    let fdks_deduped = NetKAT_GlobalFDDCompiler.of_policy global_pol ~dedup:true in
    let fdd =
      NetKAT_GlobalFDDCompiler.to_local NetKAT_FDD.Field.Vlan (NetKAT_FDD.Value.of_int 0xffff)
      fdks_deduped in
    let switches =
      NetKAT_Misc.switches_of_policy (Optimize.mk_seq (NetKAT_Types.Filter ingress) global_pol) in
    let tables =
      List.map switches ~f:(fun sw -> (sw, NetKAT_LocalCompiler.to_table sw fdd)) in
    let print_table (sw, t) =
      Format.fprintf fmt "@[%s@]@\n@\n"
        (SDN_Types.string_of_flowTable ~label:(Int64.to_string sw) t) in
    Format.fprintf fmt "@\n";
    Format.fprintf fmt "[global] Ingress:@\n@[%a@]@\n@\n" NetKAT_Pretty.format_pred ingress;
    Format.fprintf fmt "[global] Egress:@\n@[%a@]@\n@\n" NetKAT_Pretty.format_pred egress;
    Format.fprintf fmt "[global] Input Policy:@\n@[%a@]@\n@\n" NetKAT_Pretty.format_policy global_pol;
    List.iter tables ~f:print_table;
    Out_channel.write_all "fdd.dot" ~data:(NetKAT_FDD.T.to_dot fdd);
    Out_channel.write_all "fdks.dot" ~data:(NetKAT_GlobalFDDCompiler.to_dot fdks);
    Out_channel.write_all "fdks-deduped.dot" ~data:(NetKAT_GlobalFDDCompiler.to_dot fdks_deduped);
    ()

