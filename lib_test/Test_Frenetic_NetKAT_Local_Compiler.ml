open OUnitHack
open Frenetic_OpenFlow
open Frenetic_NetKAT
open Frenetic_NetKAT_Pretty
open Frenetic_NetKAT_Local_Compiler

(* TODO: This fails, but it's unclear how to fix just yet.   
  Basically they compile down to Open Flow tables:
  tbl1 = { dlTyp = 2048 : Output <- InPort}
    { Else : Drop }
  tbl2 = { dlType = 2048 + nwProto = 1 : Drop }
    { dlType = 2048 : Output <- InPort }
    { Else : Drop }


TEST "even with IPProto < EthType, should check EthType & IPProto in OpenFlow" =
  let p = Seq (Filter (Test (EthType 0x800)),
               Union (Seq (Filter (Test (IPProto 1)), Filter False),
                      Seq (Filter (Neg (Test (IPProto 1))), Filter True))) in
  let order1 =
    `Static (Field.[ IPProto; EthType; Switch; Location; EthSrc; EthDst; Vlan; VlanPcp;
      IP4Src; IP4Dst; TCPSrcPort; TCPDstPort ]) in
  let tbl1 = to_table 0L (compile ~order:order1 p) in
  let order2 =
    `Static (Field.[ EthType; IPProto; Switch; Location; EthSrc; EthDst; Vlan; VlanPcp;
      IP4Src; IP4Dst; TCPSrcPort; TCPDstPort ]) in
  let tbl2 = to_table 0L (compile ~order:order2 p) in
  tbl1 = tbl2
*)
TEST "Can test locations, even when they are set to pipes" =
  let p = Filter (Test (Location (Pipe "web"))) in
  List.length (to_table 0L (compile p)) == 1 (* that drops everything *)

TEST "clearing cache fails" =
  let a = Test (IPProto 1) in
  let b = Test (EthType 0x800) in
  let fdd1 = compile (Filter a) in
  let fdd2 = compile ~cache:`Empty (Filter b) in
  try
    seq fdd1 fdd2 != compile ~cache:`Keep (Filter (And (a, b)))
  with
    Not_found -> true

TEST "keeping cache works" =
  let a = Test (IPProto 1) in
  let b = Test (EthType 0x800) in
  let fdd1 = compile (Filter a) in
  let fdd2 = compile ~cache:`Keep (Filter b) in
  seq fdd1 fdd2 = compile ~cache:`Keep (Filter (And (a, b)))

TEST "keeping reachable nodes in cache works" =
  let a = Test (IPProto 1) in
  let b = Test (EthType 0x800) in
  let fdd1 = compile (Filter a) in
  let fdd2 = compile ~cache:(`Preserve fdd1) (Filter b) in
  seq fdd1 fdd2 = compile ~cache:`Keep (Filter (And (a, b)))
