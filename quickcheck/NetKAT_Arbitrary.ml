open NetKAT_Types


let arbitrary_header  = 
  let open QuickCheck_gen in
  elements
    [
      (Header (SDN_Types.InPort));
      (Header (SDN_Types.EthType));
      (Header (SDN_Types.EthSrc));
      (Header (SDN_Types.EthDst));
      (Header (SDN_Types.Vlan));
      (Header (SDN_Types.VlanPcp));
      (Header (SDN_Types.IPProto));
      (Header (SDN_Types.IP4Src));
      (Header (SDN_Types.IP4Dst));
      (Header (SDN_Types.TCPSrcPort));
      (Header (SDN_Types.TCPDstPort));
      (NetKAT_Types.Switch)
    ] 


let arbitrary_headerval = let open QuickCheck_gen in 
                            choose_int0 200 >>= fun rint ->
                              ret_gen (VInt.Int64 (Int64.of_int rint))
                            


let gen_atom_pred : pred QuickCheck_gen.gen = 
  let open QuickCheck_gen in 
    oneof [
      ret_gen (True);
      ret_gen (False);
      arbitrary_header >>= fun h ->
        arbitrary_headerval >>= fun v ->
          ret_gen (Test (h, v));
          ]


let rec gen_composite_pred (size : int) : pred QuickCheck_gen.gen =
  let open QuickCheck_gen in
    oneof [
        gen_pred (size - 1) >>= fun pr1 ->
          gen_pred (size - 1) >>= fun pr2 ->
            ret_gen (And (pr1, pr2));
        gen_pred (size - 1) >>= fun pr1 ->
          gen_pred (size - 1) >>= fun pr2 ->
            ret_gen (Or (pr1, pr2));
        gen_pred (size - 1) >>= fun pr ->
          ret_gen (Neg (pr))
      ]

and gen_pred (size : int) : pred QuickCheck_gen.gen =
  if size < 1
  then gen_atom_pred
  else QuickCheck_gen.oneof [gen_atom_pred; gen_composite_pred size]


let gen_atom_pol : policy QuickCheck_gen.gen = 
  let open QuickCheck_gen in
  oneof [
    arbitrary_header >>= fun h -> 
      arbitrary_headerval >>= fun v ->
        ret_gen (Mod (h, v));
    gen_pred (Random.int 20) >>= fun pr ->
      ret_gen (Filter (pr))
        ]

let rec gen_composite_pol (size : int) : policy QuickCheck_gen.gen =
  let open QuickCheck_gen in 
      oneof [
        gen_pol (size - 1) >>= fun p1 ->
          gen_pol (size - 1) >>= fun p2 ->
            ret_gen (Par (p1, p2));
        gen_pol (size - 1) >>= fun p1 ->
          gen_pol (size - 1) >>= fun p2 ->
            ret_gen (Seq (p1, p2));
        gen_pol (size - 1) >>= fun p ->
          ret_gen (Star p)
        ]

and gen_pol (size : int ) : policy QuickCheck_gen.gen =
  if size < 1
  then gen_atom_pol
  else QuickCheck_gen.oneof [gen_atom_pol; gen_composite_pol size]


let arbitrary_pol = QuickCheck_gen.sized gen_pol
