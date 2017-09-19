open Core

let%test_module _ = (module struct
  open Frenetic_Fdd.Field

  (* Field.hash intentionally not tested, as it just uses the default implementation *)

  let%test "Field.all returns default ordering" =
    List.hd_exn all = Switch

  (* The "order" field is dependent on Obj.magic returning Constructors starting from 0 in the
    order that they're defined.  That's a pretty big assumption, and Obj.magic is not a public function,
    so test it here to make sure a future OCaml release doesn't break it.  *)
  type test_type = One | Two | Three
  let%test "Obj.magic returns cardinal numbers of constructors in correct order" =
    Obj.magic One = 0 && Obj.magic Two = 1 && Obj.magic Three = 2

  let%test "Field.compare returns comparison value between two fields in the current ordering" =
    compare Location VlanPcp = -1

  let%test "Field.of_string converts field string to abstract field" =
    of_string "Vlan" = Vlan

  let%test "Field.of_string throws exception for unrecognized field strings" =
    Exn.does_raise (fun () -> of_string "Not a Valid Field String")

  let%test "Field.to_string converts field to string rep" =
    to_string VPort = "VPort"

  let all_fields_alpha_order =
    [ EthDst; EthSrc; EthType; IP4Src; IP4Dst; IPProto; Location; Switch; TCPSrcPort;
      TCPDstPort; Vlan; VlanPcp; VFabric; VPort; VSwitch; Meta0; Meta1; Meta2; Meta3; Meta4 ]

  let string_of_list to_string l =
    let strs = List.map l to_string in
    "[" ^ (String.concat ~sep:", " strs) ^ "]"

  let%test "Field.set_order mutably sets field order" =
    let () = set_order all_fields_alpha_order in
    List.nth_exn (get_order ()) 5 = IPProto

  let%test "Field.set_order mutably changes the order array so compare works" =
    let () = set_order all_fields_alpha_order in
    compare VlanPcp Location = 1

  let%test "Field.set_order rejects list with missing fields" =
    Exn.does_raise (fun () ->
      set_order (List.tl_exn all_fields_alpha_order)) (* Chop off first field *)

  let%test "Field.set_order rejects list with duplicate fields" =
    Exn.does_raise (fun () ->
     set_order (VSwitch :: List.tl_exn all_fields_alpha_order)) (* VSwitch is now listed twice *)

  let%test "Field.get_order gets currently stored field order" =
    let () = set_order all in  (* Set back to default order *)
    List.nth_exn (get_order ()) 12 = IPProto

  let%test "Field.auto_order sorts referenced Test field to top" =
    let open Frenetic_NetKAT in
    let policy = Filter(Test(TCPSrcPort(5))) in
    let () = auto_order policy in
    List.hd_exn (get_order ()) = TCPSrcPort

  let auto_order_on_file_policy pol_file =
    let open Frenetic_NetKAT in
    let open Frenetic_NetKAT_Parser in
    let () = set_order all in  (* Set back to default order *)
    let nk_str = In_channel.read_all pol_file in
    let pol = pol_of_string nk_str in
    auto_order pol

  let%test "Field.auto_order on example places Location at the top" =
    let () = auto_order_on_file_policy "examples/example1.kat" in
    List.hd_exn (get_order()) = Location

  let%test "Field.auto_order on fall through optimization example places Vlan at the top" =
    let () = auto_order_on_file_policy "examples/fall-through-optimization.kat" in
    List.hd_exn (get_order()) = VlanPcp

  (* TODO(cr396): More examples ... once I've figured out the actual heuristic function of auto-order *)
end)

let%test_module _ = (module struct
  open Frenetic_Fdd.Value

  (* subset_eq *)

  let%test "Value.subset_eq returns true if two constants without masks are equal" =
    subset_eq (Const 5L) (Const 5L)

  let%test "Value.subset_eq returns false if two constants without masks are unequal" =
    not (subset_eq (Const 5L) (Const 6L))

  let%test "Value.subset_eq returns true if constant has mask 64 and values are equal" =
    subset_eq (Mask(5L, 64)) (Const 5L)

  let%test "Value.subset_eq returns true if pipes have equal names" =
    subset_eq (Pipe "Hello World") (Pipe "Hello World")

  let%test "Value.subset_eq returns false if values are different types" =
    not (subset_eq (Pipe "Hello World") (FastFail []))

  let%test "Value.subset_eq returns true if masked values agree in all the bits except for the masked ones" =
    subset_eq (Mask(0x1020304000FFFFFFL, 40)) (Mask(0x1020304000000000L, 40))

  let%test "Value.subset_eq returns false if there are more mask bits in the second value" =
    not (subset_eq (Mask(0x10200000L, 8)) (Mask(0x1020ffffL, 16)))

  (* meet *)

  let%test "Value.meet returns either if two constants without masks are equal" =
    meet (Const 5L) (Const 5L) = Some (Const 5L)

  let%test "Value.meet returns None if two constants without masks are unequal" =
    meet (Const 5L) (Const 6L) = None

  let%test "Value.meet returns constant if constant has mask 64 and values are equal" =
    meet (Mask(5L, 64)) (Const 5L) = Some (Const 5L)

  let%test "Value.meet returns either if queries have equal names" =
    meet (Query "Hello World") (Query "Hello World") = Some (Query "Hello World")

  let%test "Value.meet returns false if values are different types" =
    meet (Query "Hello World") (Const 5L) = None

  let%test "Value.meet returns value with most one bits if masked values agree in all the bits except for the masked ones" =
    meet (Mask(0x1020304000FFFFFFL, 40)) (Mask(0x1020304000000000L, 40)) = Some (Mask(0x1020304000FFFFFFL, 40))

  let%test "Value.meet returns None if values do not agree in masked bits" =
    meet (Mask(0x1020FF4000000000L, 40)) (Mask(0x1020304000000000L, 40)) = None

  (* join *)

  let%test "Value.join returns either if two constants without masks are equal" =
    join (Const 5L) (Const 5L) = Some (Const 5L)

  let%test "Value.join returns None if two constants without masks are unequal" =
    join (Const 5L) (Const 6L) = None

  let%test "Value.join returns constant if constant has mask 64 and values are equal" =
    join (Mask(5L, 64)) (Const 5L) = Some (Const 5L)

  let%test "Value.join returns either if queries have equal names" =
    join (Query "Hello World") (Query "Hello World") = Some (Query "Hello World")

  let%test "Value.join returns false if values are different types" =
    join (Query "Hello World") (Const 5L) = None

  let%test "Value.join returns value with most one bits if masked values agree in all the bits except for the masked ones" =
    join (Mask(0x1020304000FFFFFFL, 40)) (Mask(0x1020304000000000L, 40)) = Some (Mask(0x1020304000FFFFFFL, 40))

  let%test "Value.join returns None if values do not agree in masked bits" =
    join (Mask(0x1020FF4000000000L, 40)) (Mask(0x1020304000000000L, 40)) = None

  (* Misc *)

  let%test "Value.to_string returns human readable value" =
    to_string (Mask(1162209354204577792L, 40)) = "1162209354204577792/40"

  let%test "Value.of_int converts int to abstract constant" =
    of_int 5 = Const 5L

  let%test "Value.of_int64 converts int64 to abstract constant" =
    of_int64 5L = Const 5L

  let%test "Value.to_int_exn returns int from abstract constant, provided it fits in an int" =
    to_int_exn (Const 5L) = 5

  (* Note that to_int_exn always succeeds on 64-bit architectures because both int's are 64 bits  *)

  let%test "Value.to_int_exn throws exception if not a constant" =
    Exn.does_raise (fun () -> to_int_exn (Pipe ("Hello")))

end)

let%test_module _ = (module struct
  open Frenetic_Fdd.Field
  open Frenetic_Fdd.Value
  open Frenetic_Fdd.Pattern

  let%test "Pattern.compare returns comparison of fields if they're different" =
    compare (TCPSrcPort, Const 0L) (Vlan, Const 1305L) = 1

  let%test "Pattern.compare uses value as tiebreaker if fields are the same" =
    compare (TCPSrcPort, Const 80L) (TCPSrcPort, Const 8080L) = -1

  let%test "Pattern.of_hv converts NetKAT HeaderValue to Pattern" =
    of_hv (Frenetic_NetKAT.Switch 1L) = (Switch, Const 1L)

  let%test "Pattern.of_hv converts NetKAT 32-bit based mask patterns to 64-bit" =
    (* TODO(cr396): I think this is wrong.  If you expand the mask, it seems that you should right-pad the value with 32 bits of 0's *)
    of_hv (Frenetic_NetKAT.IP4Src(0x10203040l, 24l)) = (IP4Src, Mask(0x10203040L, 56))

  let%test "Pattern.to_hv converts Pattern to NetKAT HeaderValue" =
    to_hv (VFabric, Const 7L) = Frenetic_NetKAT.VFabric 7L

  let%test "Pattern.to_hv converts Pattern constant values in places where mask is required to a full 32-bit mask" =
    to_hv (IP4Src, Const 0x10203040L) = (Frenetic_NetKAT.IP4Src(0x10203040l, 32l))

  let%test "Pattern.to_hv converts Pattern 64-bit based mask patterns to 32-bit" =
    (* TODO(cr396): I think this is wrong.  If you have a 56-bit mask, the number should be greater than a 56-bit number, which won't
       fit in the NetKAT 32-bit constant.  The constant should be left-shifted 32 bits *)
    to_hv (IP4Src, Mask(0x10203040L, 56)) = (Frenetic_NetKAT.IP4Src(0x10203040l, 24l))

  let%test "Pattern.to_pred converts pattern to NetKAT predicate" =
    let module NetKAT = Frenetic_NetKAT in
    to_pred (TCPSrcPort, Const 80L) = NetKAT.Test( NetKAT.TCPSrcPort 80 )

  let%test "Pattern.to_sdn returns a function that replaces an Fdd pattern in a SDN Pattern" =
    let module SDN_Pattern = Frenetic_OpenFlow.Pattern in
    let new_sp_fn = to_sdn (Vlan, Const(1305L)) in
    new_sp_fn SDN_Pattern.match_all = SDN_Pattern.{match_all with dlVlan = Some 1305 }

  let%test "Pattern.to_sdn returns a function that replaces an Fdd pattern in an SDN Pattern, converting constant values in places where mask is required to a full 32-bit mask" =
    let module SDN_Pattern = Frenetic_OpenFlow.Pattern in
    let new_sp_fn = to_sdn (IP4Src, Const 0x10203040L) in
    new_sp_fn SDN_Pattern.match_all = SDN_Pattern.{match_all with nwSrc = Some (0x10203040l, 32l) }

  let%test "Pattern.to_sdn fails when given a non-OpenFlow field in a pattern" =
    Exn.does_raise (fun () ->
      let module SDN_Pattern = Frenetic_OpenFlow.Pattern in
      let new_sp_fn = to_sdn (VPort, Const 0L) in
      new_sp_fn SDN_Pattern.match_all)

  let%test "Pattern.to_sdn fails when given a non-OpenFlow construct like a Pipe or Query" =
    Exn.does_raise (fun () ->
      let module SDN_Pattern = Frenetic_OpenFlow.Pattern in
      let new_sp_fn = to_sdn (Location, Pipe("Hello")) in
      new_sp_fn SDN_Pattern.match_all = SDN_Pattern.match_all)

end)
