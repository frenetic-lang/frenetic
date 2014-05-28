open NetKAT_Types
open Packet_Arbitrary

module AB = Arbitrary_Base

let arbitrary_id =
  let open QuickCheck_gen in
  let shared = [
    map_gen (fun i -> Char.chr i) (choose_int (0x41, 0x5a));
    map_gen (fun i -> Char.chr i) (choose_int (0x61, 0x7a));
    ret_gen (Char.chr 0x5f);
  ] in
  let schr = oneof shared in
  let chr = oneof ([
    map_gen (fun i -> Char.chr i) (choose_int (0x30,0x39));
  ] @ shared) in
  schr >>= fun c ->
  choose_int (0, 10) >>= fun l ->
  QuickCheck.arbitrary_listN l chr >>= fun cs ->
    ret_gen (QuickCheck_util.charlist_to_string (c::cs))


let masks = 
  List.map QuickCheck_gen.ret_gen [ 0l; 8l; 16l; 24l; 32l ] 

let arbitrary_test, arbitrary_mod =
  let open QuickCheck_gen in
  let open NetKAT_Types in
  let shared = [
    map_gen (fun i -> Location (Physical i)) AB.arbitrary_uint32;
    map_gen (fun s -> Location (Pipe s)) arbitrary_id;
    map_gen (fun i -> EthSrc i) AB.arbitrary_uint48;
    map_gen (fun i -> EthDst i) AB.arbitrary_uint48;
    map_gen (fun i -> EthType i) AB.arbitrary_uint16;
    (* XXX(seliopou): Currently not being tested:
     *
     *   vlan, vlanPcp
     * *)
    map_gen (fun i -> IPProto i) AB.arbitrary_uint8;
    (AB.arbitrary_uint32 >>= fun i -> 
     oneof masks >>= fun j -> ret_gen (IP4Src(i,j)));
    (AB.arbitrary_uint32 >>= fun i -> 
     oneof masks >>= fun j -> ret_gen (IP4Dst(i,j)));
    map_gen (fun i -> TCPSrcPort i) AB.arbitrary_uint16;
    map_gen (fun i -> TCPDstPort i) AB.arbitrary_uint16;
  ] in
  (oneof ([
    (* XXX(seliopou): Switch has a restricted range due to the printing bug
     * mentioned in a comment below
     * *)
    map_gen (fun i -> Switch i) AB.arbitrary_uint48;
  ] @ shared),
  oneof ([
  ] @ shared))

let arbitrary_portId = Arbitrary_Base.arbitrary_uint32

let treesize n x =
  if n <= 0
    then x
    else QuickCheck_gen.resize (n / (1 + Random.int n)) x

let gen_atom_pred : pred QuickCheck_gen.gen = 
  let open QuickCheck_gen in 
      arbitrary_test >>= fun hv ->
        ret_gen (Test hv)

let rec gen_composite_pred () : pred QuickCheck_gen.gen =
  let open QuickCheck_gen in
    sized (fun n -> treesize n
      (frequency [
          (3, gen_pred_ctor () >>= fun pr1 ->
                gen_pred_ctor () >>= fun pr2 ->
                  ret_gen (And (pr1, pr2)));
          (3, gen_pred_ctor () >>= fun pr1 ->
                gen_pred_ctor () >>= fun pr2 ->
                  ret_gen (Or (pr1, pr2)));
          (1, gen_pred_ctor () >>= fun pr ->
                ret_gen (Neg (pr)))
        ]))

and gen_pred_ctor () : pred QuickCheck_gen.gen =
  let open QuickCheck_gen in
    sized (fun n -> treesize n
      (frequency [ (1, gen_atom_pred);
                   (max 0 (n - 1), gen_composite_pred ())
                 ]))

let gen_pred : pred QuickCheck_gen.gen =
  let open QuickCheck_gen in
    frequency [
      (1, ret_gen (True));
      (1, ret_gen (False));
      (* XXX(seliopou) this should be a function of the size. *)
      (3, gen_pred_ctor ())
      ]

let arbitrary_link : policy QuickCheck_gen.gen = 
  let open QuickCheck_gen in
  let open Arbitrary_Base in
  (* XXX(seliopou): The range of switch ids is currently limited in tests
   * because of a bug in OCaml's Scanf library, reported here:
   *
   *   http://caml.inria.fr/mantis/view.php?id=6316
   *)
  arbitrary_uint48 >>= fun sw1 ->
  arbitrary_portId >>= fun pt1 ->
  arbitrary_uint48 >>= fun sw2 ->
  arbitrary_portId >>= fun pt2 ->
    ret_gen (Link(sw1,pt1,sw2,pt2))

let gen_lf_atom_pol : policy QuickCheck_gen.gen  =
  let open QuickCheck_gen in
  oneof [
    (arbitrary_mod >>= fun hv ->
         ret_gen (Mod hv));
    (gen_pred >>= fun pr ->
        ret_gen (Filter (pr))) ]

let gen_atom_pol : policy QuickCheck_gen.gen = 
  let open QuickCheck_gen in
  oneof [
    (arbitrary_mod >>= fun hv ->
        ret_gen (Mod hv));
    (gen_pred >>= fun pr ->
        ret_gen (Filter (pr)));
    arbitrary_link ]

let rec gen_composite_pol arbitrary_atom : policy QuickCheck_gen.gen =
  let open QuickCheck_gen in 
      sized (fun n -> treesize n
       (frequency [
          (3, gen_pol arbitrary_atom >>= fun p1 ->
              gen_pol arbitrary_atom >>= fun p2 ->
              ret_gen (Union (p1, p2)));
          (3, gen_pol arbitrary_atom>>= fun p1 ->
              gen_pol arbitrary_atom >>= fun p2 ->
              ret_gen (Seq (p1, p2)));
          (1, gen_pol arbitrary_atom >>= fun p ->
              ret_gen (Star p))
        ]))

and gen_pol arbitrary_atom : policy QuickCheck_gen.gen =
  let open QuickCheck_gen in
    sized (fun n ->
      frequency [
          (1, arbitrary_atom);
          (max 0 (n - 1), gen_composite_pol arbitrary_atom) ])


let arbitrary_policy = gen_pol gen_atom_pol

let arbitrary_lf_pol = gen_pol gen_lf_atom_pol

let arbitrary_tcp : packet QuickCheck_gen.gen =
  let open QuickCheck_gen in
  let open QuickCheck in
  let open NetKAT_Types.Headers in
  let open Packet in
  let module Parb = Packet_Arbitrary in
  let payload = Parb.arbitrary_payload 64 in
  let tcp = map_gen (fun i -> Packet.Ip.Tcp i) (Parb.arbitrary_tcp payload) in
  let ip = map_gen (fun i -> Packet.Ip i) (Parb.arbitrary_ip tcp) in
  Parb.arbitrary_packet ip >>= fun pkt ->
    arbitrary_int32 >>= fun port_id ->
      let headers =
        { HeadersValues.location = NetKAT_Types.Physical port_id
        ; ethSrc = pkt.dlSrc
        ; ethDst = pkt.dlDst
        ; vlan = (match pkt.dlVlan with Some(x) -> x | None -> 0)
        ; vlanPcp = pkt.dlVlanPcp
        ; ethType = dlTyp pkt
        ; ipProto = (try nwProto pkt with Invalid_argument(_) -> 0)
        ; ipSrc = (try nwSrc pkt with Invalid_argument(_) -> 0l)
        ; ipDst = (try nwDst pkt with Invalid_argument(_) -> 0l)
        ; tcpSrcPort = (try tpSrc pkt with Invalid_argument(_) -> 0)
        ; tcpDstPort = (try tpDst pkt with Invalid_argument(_) -> 0)
        } in
      arbitrary_int64 >>= fun switch_id ->
        payload >>= fun payload ->
          ret_gen
            { switch = switch_id
            ; headers = headers
            ; payload = (SDN_Types.NotBuffered payload)
            }

let arbitrary_packet : packet QuickCheck_gen.gen = 
  QuickCheck_gen.Gen 
    (fun _ -> failwith "arbitrary_packet: not yet implemented")    
  (* let open QuickCheck_gen in *)
  (* let open QuickCheck in *)
  (* listN num_hdrs arbitrary_headerval >>= fun vals -> *)
  (*   Arbitrary_SDN_Types.arbitrary_payload >>= fun payload -> *)
  (*   ret_gen { *)
  (*     headers = List.fold_right2 HeaderMap.add all_headers vals HeaderMap.empty; *)
  (*     payload = payload *)
  (*   } *)
