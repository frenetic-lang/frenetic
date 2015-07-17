open Frenetic_NetKAT_Virtual_Compiler

let main vpolicy_file vrel_file vtopo_file ving_pol_file ving_file veg_file ptopo_file ping_file peg_file () : unit = 
    let fmt = Format.formatter_of_out_channel stderr in
    let () = Format.pp_set_margin fmt 120 in
    let () = Frenetic_Fdd.Field.set_order
             [ Switch; Location; VSwitch; VPort; IP4Dst; Vlan; TCPSrcPort; TCPDstPort; IP4Src;
                EthType; EthDst; EthSrc; VlanPcp; IPProto ] in
    let vpolicy =
      Core.Std.In_channel.with_file vpolicy_file ~f:(fun chan ->
        Frenetic_NetKAT_Parser.policy_from_string (Frenetic_NetKAT_Lexer.token (Lexing.from_channel chan))) in
    let vrel =
      Core.Std.In_channel.with_file vrel_file ~f:(fun chan ->
        Frenetic_NetKAT_Parser.pred_program Frenetic_NetKAT_Lexer.token (Lexing.from_channel chan)) in
    let vtopo =
      Core.Std.In_channel.with_file vtopo_file ~f:(fun chan ->
        Frenetic_NetKAT_Parser.program Frenetic_NetKAT_Lexer.token (Lexing.from_channel chan)) in
    let ving_pol =
      Core.Std.In_channel.with_file ving_pol_file ~f:(fun chan ->
        Frenetic_NetKAT_Parser.program Frenetic_NetKAT_Lexer.token (Lexing.from_channel chan)) in
    let ving =
      Core.Std.In_channel.with_file ving_file ~f:(fun chan ->
        Frenetic_NetKAT_Parser.pred_program Frenetic_NetKAT_Lexer.token (Lexing.from_channel chan)) in
    let veg =
      Core.Std.In_channel.with_file veg_file ~f:(fun chan ->
        Frenetic_NetKAT_Parser.pred_program Frenetic_NetKAT_Lexer.token (Lexing.from_channel chan)) in
    let ptopo =
      Core.Std.In_channel.with_file ptopo_file ~f:(fun chan ->
        Frenetic_NetKAT_Parser.program Frenetic_NetKAT_Lexer.token (Lexing.from_channel chan)) in
    let ping =
      Core.Std.In_channel.with_file ping_file ~f:(fun chan ->
        Frenetic_NetKAT_Parser.pred_program Frenetic_NetKAT_Lexer.token (Lexing.from_channel chan)) in
    let peg =
      Core.Std.In_channel.with_file peg_file ~f:(fun chan ->
        Frenetic_NetKAT_Parser.pred_program Frenetic_NetKAT_Lexer.token (Lexing.from_channel chan)) in
    let _ = generate_fabrics ~log ~record_paths vrel vtopo ving peg ptopo ping peg in 
    ()

