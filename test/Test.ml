open OpenFlow0x01
open OpenFlow0x01_Stats

open QuickCheck

module Gen = QuickCheck_gen

(* Test that `parse` is the left inverse of `marshal` *)
let prop_roundtrip parse marshal e =
    parse (marshal e) = e

(* Setup a quickCheck for a serlalizable OpenFlow datatype *)
let openflow_quickCheck arbitrary show parse marshal = 
    let test = testable_fun arbitrary show testable_bool in
    match quickCheck test (prop_roundtrip parse marshal) with
      | Success -> true
      | Failure _ -> failwith "No failure expected"
      | Exhausted _ -> failwith "No exhaustion expected"

(* arbitrary instance for usigned integers. Still uses the `int` type. *)
let arbitrary_uint = Gen.sized (fun n -> Gen.choose_int (0, n))

module OpenFlow0x01_Wildcards = struct
    let gen_wildcards =
        let open Gen in
        let open Wildcards in
        arbitrary_bool >>= fun in_port ->
        arbitrary_bool >>= fun dl_vlan ->
        arbitrary_bool >>= fun dl_src ->
        arbitrary_bool >>= fun dl_dst ->
        arbitrary_bool >>= fun dl_type ->
        arbitrary_bool >>= fun nw_proto ->
        arbitrary_bool >>= fun tp_src ->
        arbitrary_bool >>= fun tp_dst ->
        arbitrary_uint >>= fun nw_src ->
        arbitrary_uint >>= fun nw_dst ->
        arbitrary_bool >>= fun dl_vlan_pcp ->
        arbitrary_bool >>= fun nw_tos ->
            ret_gen {
                in_port = in_port;
                dl_vlan = dl_vlan;
                dl_src = dl_src;
                dl_dst = dl_dst;
                dl_type = dl_type;
                nw_proto = nw_proto;
                tp_src = tp_src;
                tp_dst = tp_dst;
                nw_src = nw_src;
                nw_dst = nw_dst;
                dl_vlan_pcp = dl_vlan_pcp;
                nw_tos = nw_tos
            }

    TEST "OpenFlow0x01 Wildcards RoundTrip" =
        (openflow_quickCheck gen_wildcards 
            Wildcards.to_string Wildcards.parse Wildcards.marshal)
end

module RoundTripping = struct
  TEST "OpenFlow Hello Test 1" = 
    let open Message in 
    let bs = Cstruct.create 101 in
    let m = Hello bs in 
    let x = 42l in 
    let s = marshal x m in  
    let h = Header.parse s in 
    let s' = String.sub s Header.size (Header.len h - Header.size) in 
    let x',m' = parse h s' in 
    let xid_ok = x = x' in 
    let msg_ok = 
      match m',m with 
	    | Hello bs', Hello bs ->
	      Cstruct.to_string bs = Cstruct.to_string bs'
	    | _ -> 
	      false in 
          xid_ok && msg_ok 
  
  TEST "OpenFlow Vendor Test 1" =
    let open Message in
    let bs = Cstruct.create 101 in
    let bs' = Cstruct.create ((Cstruct.len bs) + 4) in
    let body = "42 is the answer" in
    Cstruct.blit_from_string body 0 bs 0 (String.length body);
    let m = (42l, bs) in
    let _ = Vendor.marshal m bs' in
    let m' = Vendor.parse bs' in    
    match m, m' with
    | (42l, bs), (42l, bs') ->
      Cstruct.to_string bs = Cstruct.to_string bs'
    | _ ->
      false
  
  TEST "OpenFlow StatsReply DescriptionReply Test 1" =
    let open Message in
    let bs' = Cstruct.create 1060 in
    let content = {  
      manufacturer = String.create 256
      ; hardware = String.create 256
      ; software = String.create 256
      ; serial_number = String.create 32
      ; datapath = String.create 256} in
    let m = DescriptionRep content in
    let _ = StatsReply.marshal m bs' in
    let m' = StatsReply.parse bs' in
    match m, m' with
    | DescriptionRep rep, DescriptionRep rep' ->
      rep.manufacturer = rep'.manufacturer &&
      rep.hardware = rep'.hardware &&
      rep.serial_number = rep'.serial_number &&
      rep.datapath = rep'.datapath
    | _ -> 
      false
end
