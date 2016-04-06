open Frenetic_OpenFlow
open Frenetic_NetKAT
open Frenetic_NetKAT_Pretty
open Frenetic_NetKAT_Compiler

let%test "Can test locations, even when they are set to pipes" =
  let p = Filter (Test (Location (Pipe "web"))) in
  let opt = { default_compiler_options with remove_tail_drops = false } in
  List.length (to_table 0L ~options:opt (compile_local ~options:opt p)) == 1 (* that drops everything *)

let%test "clearing cache fails" =
  let a = Test (IPProto 1) in
  let b = Test (EthType 0x800) in
  let fdd1 = compile_local (Filter a) in
  let compiler_options_empty = { default_compiler_options with cache_prepare = `Empty } in
  let compiler_options_keep = { default_compiler_options with cache_prepare = `Keep } in
  let fdd2 = compile_local ~options:compiler_options_empty (Filter b) in
  try
    seq fdd1 fdd2 != compile_local ~options:compiler_options_keep (Filter (And (a, b)))
  with
    Not_found -> true

let%test "keeping cache_prepare works" =
  let a = Test (IPProto 1) in
  let b = Test (EthType 0x800) in
  let fdd1 = compile_local (Filter a) in
  let compiler_options_keep = { default_compiler_options with cache_prepare = `Keep } in
  let fdd2 = compile_local ~options:compiler_options_keep (Filter b) in
  seq fdd1 fdd2 = compile_local ~options:compiler_options_keep (Filter (And (a, b)))

let%test "keeping reachable nodes in cache_prepare works" =
  let a = Test (IPProto 1) in
  let b = Test (EthType 0x800) in
  let fdd1 = compile_local (Filter a) in
  let compiler_options_keep = { default_compiler_options with cache_prepare = `Keep } in
  let compiler_options_preserve = { default_compiler_options with cache_prepare = `Preserve fdd1 } in
  let fdd2 = compile_local ~options:compiler_options_preserve (Filter b) in
  seq fdd1 fdd2 = compile_local ~options:compiler_options_keep (Filter (And (a, b)))

let test_compile_table pol tbl =
  let open Frenetic_NetKAT_Compiler in
  let tbl' = to_table 0L (compile_local pol) in
  if string_of_flowTable tbl = string_of_flowTable tbl' then
    true
  else
    (Format.printf "compile @,%a@, produced\n%s@,\n@,expected\n%s%!\n"
       format_policy pol (string_of_flowTable tbl') (string_of_flowTable tbl);
     false)

let ip_of_str n = Ipaddr.V4.(to_int32 (of_string_exn n))

let test_ipDst ipAddress = Test(IP4Dst(ip_of_str ipAddress, 32l))

let is_arp = Test(EthType 0x806)

let is_ip = Test(EthType 0x800)

let modify_outport p = Mod (Location (Physical p))

let flow p a = { pattern = p; action = [a]; cookie = 0L; idle_timeout = Permanent; hard_timeout= Permanent }

let%test "adds rules to drop packets not meeting ip4Src dependencies" =
  let open Pattern in
  let pol = 
    Union(
      Seq(Filter(And(test_ipDst "192.168.56.1", is_arp)), modify_outport 9l ),
      Seq(Filter(And(test_ipDst "192.168.57.1", is_ip)), modify_outport 10l )
    ) in
  test_compile_table pol [
    flow { match_all with dlTyp = Some 0x806; nwDst = Some (ip_of_str "192.168.56.1", 32l) } [[ Output(Physical 9l) ]];
    flow { match_all with dlTyp = Some 0x800; nwDst = Some (ip_of_str "192.168.56.1", 32l) } [[]];
    flow { match_all with dlTyp = Some 0x800; nwDst = Some (ip_of_str "192.168.57.1", 32l) } [[ Output(Physical 10l) ]];
    flow match_all []
  ]

let%test "adds matches for ethTyp on ipProto dependencies" =
  let open Pattern in
  let pol = 
    Union(
      Seq(Filter(Test(IPProto(6))), modify_outport 9l ),
      Seq(Filter(Test(IPProto(1))), modify_outport 10l )
    ) in
  test_compile_table pol [
    flow { match_all with dlTyp = Some 0x800; nwProto = Some 0x1 } [[ Output(Physical 10l) ]];
    flow { match_all with dlTyp = Some 0x800; nwProto = Some 0x6 } [[ Output(Physical 9l) ]];
    flow match_all []
  ]

let%test "adds matches for ethTyp and ipProto on tcp/udp dependencies" =
  let open Pattern in
  let pol = 
    Union(
      Seq(Filter(Test(TCPDstPort(22))), modify_outport 9l ),
      Seq(Filter(Test(TCPDstPort(443))), modify_outport 10l )
    ) in
  test_compile_table pol [
    flow { match_all with dlTyp = Some 0x800; nwProto = Some 0x6; tpDst = Some 22 } [[ Output(Physical 9l) ]];
    flow { match_all with dlTyp = Some 0x800; nwProto = Some 0x11; tpDst = Some 22 } [[ Output(Physical 9l) ]];
    flow { match_all with dlTyp = Some 0x800; nwProto = Some 0x6; tpDst = Some 443 } [[ Output(Physical 10l) ]];
    flow { match_all with dlTyp = Some 0x800; nwProto = Some 0x11; tpDst = Some 443 } [[ Output(Physical 10l) ]];
    flow match_all []
  ]
