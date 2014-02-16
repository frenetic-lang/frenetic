open NetKAT_Types

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
    map_gen (fun i -> IP4Src (i,32)) AB.arbitrary_uint32;
    map_gen (fun i -> IP4Dst (i,32)) AB.arbitrary_uint32;
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

let arbitrary_portId =
  let open QuickCheck_gen in
  let open Arbitrary_Base in
  map_gen (fun i -> VInt.Int64 (Int64.of_int i)) arbitrary_uint16

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
