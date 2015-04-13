open OUnitHack
open SDN_Types
open NetKAT_Types
open NetKAT_Pretty
open NetKAT_LocalCompiler

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

TEST "Can test locations, even when they are set to pipes" =
  let p = Filter (Test (Location (Pipe "web"))) in
  List.length (to_table 0L (compile p)) == 1 (* that drops everything *)