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
      let open Headers in 
      match hv with 
      | Switch n -> pkt.switch = n
      | Location l -> pkt.headers.location = Some l
      | EthSrc n -> pkt.headers.ethSrc = Some n 
      | EthDst n -> pkt.headers.ethDst = Some n 
      | Vlan n -> pkt.headers.vlan = Some n 
      | VlanPcp n -> pkt.headers.vlanPcp = Some n 
      | EthType n -> pkt.headers.ethType = Some n 
      | IPProto n -> pkt.headers.ipProto = Some n 
      | IP4Src (n,m) -> 
        (match pkt.headers.ipSrc with 
          | Some (n',_) -> 
            let b = max 0 (32 - m) in 
            Int32.shift_right n b = Int32.shift_right n' b
          | None -> false)
      | IP4Dst (n,m) -> 
        (match pkt.headers.ipDst with 
          | Some (n',_) -> 
            let b = max 0 (32 - m) in 
            Int32.shift_right n b = Int32.shift_right n' b
          | None -> false)
      | TCPSrcPort n -> pkt.headers.tcpSrcPort = Some n 
      | TCPDstPort n -> pkt.headers.tcpDstPort = Some n 
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
    let open Headers in 
    let pkt' = match hv with 
      | Switch n -> { pkt with switch = n }
      | Location l -> { pkt with headers = { pkt.headers with location = Some l }}
      | EthSrc n -> { pkt with headers = { pkt.headers with ethSrc = Some n }}
      | EthDst n -> { pkt with headers = { pkt.headers with ethDst = Some n }}
      | Vlan n -> { pkt with headers = { pkt.headers with vlan = Some n }}
      | VlanPcp n -> { pkt with headers = { pkt.headers with vlanPcp = Some n }}
      | EthType n -> { pkt with headers = { pkt.headers with ethType = Some n }}
      | IPProto n -> { pkt with headers = { pkt.headers with ipProto = Some n }}
      | IP4Src(n,m) -> 
        (* JNF: assert m = 32? *)
        { pkt with headers = { pkt.headers with ipSrc = Some (n,m) }}
      | IP4Dst(n,m) -> 
        (* JNF: assert m = 32? *)
        { pkt with headers = { pkt.headers with ipDst = Some (n,m) }}
      | TCPSrcPort n -> { pkt with headers = { pkt.headers with tcpSrcPort = Some n }}
      | TCPDstPort n -> { pkt with headers = { pkt.headers with tcpDstPort = Some n }} in 
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
      if PacketSet.equal acc acc' then acc else loop acc' in 
      loop (PacketSet.singleton pkt)
  | Link(sw,pt,sw',pt') -> 
    PacketSet.empty (* JNF *)
