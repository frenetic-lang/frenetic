open NetKAT_Types

(** {2 Semantics}

  [eval pkt pol] raises [Not_found] if it tests or updates a header that  [pkt]
  does not have. This behavior is different from OpenFlow, which fails  silently
  in both cases. *)

let size_pred (pr:pred) : int =
  let rec size_pred (pr:pred) f : int =
    match pr with
      | True -> f 1
      | False -> f 1
      | Test(_) -> f 3
      | And(pr1,pr2)
      | Or(pr1,pr2) -> size_pred pr1 (fun spr1 -> size_pred pr2 (fun spr2 -> f (1 + spr1 + spr2)))
      | Neg(pr) -> size_pred pr (fun spr -> f (1 + spr)) in
  size_pred pr (fun spr -> spr)

let size (pol:policy) : int =
  let rec size (pol:policy) f : int =
    match pol with
      | Filter pr -> f (size_pred pr + 1)
      | Mod(_) -> f 3
      | Union(pol1, pol2)
      | Seq(pol1, pol2) -> size pol1 (fun spol1 -> size pol2 (fun spol2 -> f (1 + spol1 + spol2)))
      | Star(pol) -> size pol (fun spol -> f (1 + spol))
      | Link(_,_,_,_) -> f 5 in
  size pol (fun spol -> spol)

let rec eval_pred (pkt : packet) (pr : pred) : bool = match pr with
  | True -> true
  | False -> false
  | Test hv ->
    begin
      let open HeadersValues in
      match hv with
      | Switch n -> pkt.switch = n
      | Location l -> pkt.headers.location = l
      | EthSrc n -> pkt.headers.ethSrc = n
      | EthDst n -> pkt.headers.ethDst = n
      | Vlan n -> pkt.headers.vlan = n
      | VlanPcp n -> pkt.headers.vlanPcp = n
      | EthType n -> pkt.headers.ethType = n
      | IPProto n -> pkt.headers.ipProto = n
      | IP4Src (n,m) -> Int32TupleHeader.lessthan (pkt.headers.ipSrc,32l) (n,m)
      | IP4Dst (n,m) -> Int32TupleHeader.lessthan (pkt.headers.ipDst,32l) (n,m)
      | TCPSrcPort n -> pkt.headers.tcpSrcPort = n
      | TCPDstPort n -> pkt.headers.tcpDstPort = n
    end
  | And (pr1, pr2) -> eval_pred pkt pr1 && eval_pred pkt pr2
  | Or (pr1, pr2) -> eval_pred pkt pr1 || eval_pred pkt pr2
  | Neg pr1 -> not (eval_pred pkt pr1)

let rec eval (pkt : packet) (pol : policy) : PacketSet.t = match pol with
  | Filter pr ->
    if eval_pred pkt pr then
      (PacketSet.singleton pkt)
    else
      PacketSet.empty
  | Mod hv ->
    let open HeadersValues in
    let pkt' = match hv with
      | Switch n -> { pkt with switch = n }
      | Location l -> { pkt with headers = { pkt.headers with location = l }}
      | EthSrc n -> { pkt with headers = { pkt.headers with ethSrc = n }}
      | EthDst n -> { pkt with headers = { pkt.headers with ethDst = n }}
      | Vlan n -> { pkt with headers = { pkt.headers with vlan = n }}
      | VlanPcp n -> { pkt with headers = { pkt.headers with vlanPcp = n }}
      | EthType n -> { pkt with headers = { pkt.headers with ethType = n }}
      | IPProto n -> { pkt with headers = { pkt.headers with ipProto = n }}
      | IP4Src(n,m) ->
        (* JNF: assert m = 32? *)
        { pkt with headers = { pkt.headers with ipSrc = n }}
      | IP4Dst(n,m) ->
        (* JNF: assert m = 32? *)
        { pkt with headers = { pkt.headers with ipDst = n }}
      | TCPSrcPort n ->
        { pkt with headers = { pkt.headers with tcpSrcPort = n }}
      | TCPDstPort n ->
        { pkt with headers = { pkt.headers with tcpDstPort = n }} in
    PacketSet.singleton pkt'
  | Union (pol1, pol2) ->
    PacketSet.union (eval pkt pol1) (eval pkt pol2)
  | Seq (pol1, pol2) ->
    PacketSet.fold (eval pkt pol1) ~init:PacketSet.empty
      ~f:(fun set pkt' -> PacketSet.union set (eval pkt' pol2))
  | Star pol ->
    let rec loop acc =
      let f set pkt' = PacketSet.union (eval pkt' pol) set in
      let acc' = PacketSet.fold acc ~init:PacketSet.empty ~f in
      let acc'' = PacketSet.union acc acc' in
      if PacketSet.equal acc acc'' then acc else loop acc'' in
      loop (PacketSet.singleton pkt)
  | Link(sw,pt,sw',pt') ->
    PacketSet.empty (* JNF *)

let eval_pipes (packet:NetKAT_Types.packet) (pol:NetKAT_Types.policy)
  : (string * NetKAT_Types.packet) list *
    (string * NetKAT_Types.packet) list *
    NetKAT_Types.packet list =
  let open NetKAT_Types in
  (* Determines the locations that the packet belongs to. Note that a packet may
   * belong to several pipes for several reasons:
   *
   *   1. Multiple points of a single application wish to inspect the packet;
   *   2. Multiple applications wish to inspect the packet; and
   *   3. The switch makes modifications to the packet before it is sent to
   *      the controller, making it impossible to disambiguate the pipe it
   *      came from, e.g.:
   *
   *        (filter ethDst = 2; ethDst := 3; controller pipe1)
   *        | (filter ethDst = 3; controller pipe2)
   *
   * The return value may contain duplicate pipe names.
   *
   * Since Local.t is switch-specific, this function assumes but does not
   * check that the packet came from the same switch as the given Local.t *)
  let packets = eval packet pol in
  PacketSet.fold packets ~init:([],[],[]) ~f:(fun (pi,qu,phy) pkt ->
    (* Running the packet through the switch's policy will label the resultant
     * packets with the pipe or query they belong to, if any. All that's left to
     * do is pick out packets in the PacketSet.t that have a pipe location, and
     * return those packets (with the location cleared) along with the pipe they
     * belong to. *)
    match pkt.headers.HeadersValues.location with
      | Physical _ -> (            pi,             qu, pkt :: phy)
      | Pipe     p -> ((p, pkt) :: pi,             qu,        phy)
      | Query    q -> (            pi, (q, pkt) :: qu,        phy))
