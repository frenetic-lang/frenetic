open OUnitHack

TEST_MODULE = struct
  open Frenetic_Fdd.Field

  (* Field.hash intentionally not tested, as it just uses the default implementation *)

  TEST "Field.all_fields returns default ordering" =
    List.hd all_fields = Switch

  TEST "Field.compare returns comparison value between two fields in the current ordering" =
    compare VlanPcp Location = -1     

  TEST "Field.of_string converts field string to abstract field" =
    of_string "Vlan" = Vlan

  TEST "Field.of_string throws exception for unrecognized field strings" =
    try 
      of_string "Not a Valid Field String" = Vlan
    with Assert_failure _ -> true | _ -> false

  TEST "Field.to_string converts field to string rep" =
    to_string VPort = "VPort"

  let all_fields_alpha_order =
    [ EthDst; EthSrc; EthType; IP4Src; IP4Dst; IPProto; Location; Switch; TCPSrcPort; 
      TCPDstPort; Vlan; VlanPcp; VFabric; VPort; VSwitch ]

  TEST "Field.set_order mutably sets field order" =
    let () = set_order all_fields_alpha_order in
    List.nth (get_order ()) 5 = IPProto 

  TEST "Field.set_order mutably changes the order array so compare works" =
    let () = set_order all_fields_alpha_order in
    compare VlanPcp Location = 1     

  TEST "Field.set_order rejects list with missing fields" =
    try 
      let () = set_order (List.tl all_fields_alpha_order) in (* Chop off first field *)
      false
    with Assert_failure _ -> true | _ -> false

  TEST "Field.set_order rejects list with duplicate fields" =
    try 
      let () = set_order (VSwitch :: List.tl all_fields_alpha_order) in (* VSwitch is now listed twice *)
      false
    with Assert_failure _ -> true | _ -> false

  TEST "Field.get_order gets currently stored field order" =
    let () = set_order all_fields in  (* Set back to default order *)
    List.nth (get_order ()) 6 = IPProto 

  TEST "Field.auto_order sorts referenced Test field to top" = 
    let open Frenetic_NetKAT in 
    let policy = Filter(Test(TCPSrcPort(5))) in
    let () = auto_order policy in
    List.hd (get_order ()) = TCPSrcPort  
end