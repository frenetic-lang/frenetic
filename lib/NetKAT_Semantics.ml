open Core.Std

open NetKAT_Types
open Packet


(** A map keyed by header names. *)
module HeadersValues = struct

  type t =
    { location : location sexp_opaque
    ; ethSrc : dlAddr
    ; ethDst : dlAddr
    ; vlan : int16
    ; vlanPcp : dlVlanPcp
    ; ethType : dlTyp
    ; ipProto : nwProto
    ; ipSrc : nwAddr
    ; ipDst : nwAddr
    ; tcpSrcPort : tpPort
    ; tcpDstPort : tpPort
    } with sexp, fields

  let compare x y =
    (* N.B. This is intentionally unrolled for performance purposes, as the
     * comparison should short circuit as soon as possible. In light of that
     * fact, it may be beneficial to reorder some of these checks in the
     * future.
     * *)
    let c = Pervasives.compare x.location y.location in
    if c <> 0 then c else
    let c = Int64.compare x.ethSrc y.ethSrc in
    if c <> 0 then c else
    let c = Int64.compare x.ethDst y.ethDst in
    if c <> 0 then c else
    let c = Int.compare x.vlan y.vlan in
    if c <> 0 then c else
    let c = Int.compare x.vlanPcp y.vlanPcp in
    if c <> 0 then c else
    let c = Int.compare x.ethType y.ethType in
    if c <> 0 then c else
    let c = Int.compare x.ipProto y.ipProto in
    if c <> 0 then c else
    let c = Int32.compare x.ipSrc y.ipSrc in
    if c <> 0 then c else
    let c = Int32.compare x.ipDst y.ipDst in
    if c <> 0 then c else
    let c = Int.compare x.tcpSrcPort y.tcpSrcPort in
    if c <> 0 then c else
    let c = Int.compare x.tcpDstPort y.tcpDstPort in
    c

  let to_string (x:t) : string =
    let g to_string acc f =
      Printf.sprintf "%s%s=%s"
        (if acc = "" then "" else acc ^ "; ")
        (Field.name f) (to_string (Field.get f x))
    in
    Fields.fold
      ~init:""
      ~location:(g (function
        | Physical n -> Printf.sprintf "%lu" n
        | Pipe x     -> Printf.sprintf "pipe(%s)" x
        | Query x    -> Printf.sprintf "query(%s)" x))
      ~ethSrc:Int64.(g to_string)
      ~ethDst:Int64.(g to_string)
      ~vlan:Int.(g to_string)
      ~vlanPcp:Int.(g to_string)
      ~ethType:Int.(g to_string)
      ~ipProto:Int.(g to_string)
      ~ipSrc:Int32.(g to_string)
      ~ipDst:Int32.(g to_string)
      ~tcpSrcPort:Int.(g to_string)
      ~tcpDstPort:Int.(g to_string)

  let to_hvs (t:t) : NetKAT_Types.header_val list =
    let open NetKAT_Types in
    let conv to_hv = fun acc f -> (to_hv (Field.get f t)) :: acc in
    Fields.fold
      ~init:[]
      ~location:(conv (fun x -> Location x))
      ~ethSrc:(conv (fun x -> EthSrc x))
      ~ethDst:(conv (fun x -> EthDst x))
      ~vlan:(conv (fun x -> Vlan x))
      ~vlanPcp:(conv (fun x -> VlanPcp x))
      ~ethType:(conv (fun x -> EthType x))
      ~ipProto:(conv (fun x -> IPProto x))
      ~ipSrc:(conv (fun x -> IP4Src(x, 32l)))
      ~ipDst:(conv (fun x -> IP4Dst(x, 32l)))
      ~tcpSrcPort:(conv (fun x -> TCPSrcPort x))
      ~tcpDstPort:(conv (fun x -> TCPDstPort x))

end

type packet = {
  switch : switchId;
  headers : HeadersValues.t;
  payload : payload
}

module PacketSet = Set.Make (struct
  type t = packet sexp_opaque with sexp

  (* First compare by headers, then payload. The payload comparison is a
     little questionable. However, this is safe to use in eval, since
     all output packets have the same payload as the input packet. *)
  let compare x y =
    let cmp = HeadersValues.compare x.headers y.headers in
    if cmp <> 0 then
      cmp
    else
      Pervasives.compare x.payload y.payload
end)

(** {2 Semantics}

  [eval pkt pol] raises [Not_found] if it tests or updates a header that  [pkt]
  does not have. This behavior is different from OpenFlow, which fails  silently
  in both cases. *)

let size_pred (pr:pred) : int =
  let rec size_pred (pr:pred) f : int =
    match pr with
      | True -> f 1
      | False -> f 1
      | Test(_) -> f 1
      | And(pr1,pr2)
      | Or(pr1,pr2) -> size_pred pr1 (fun spr1 -> size_pred pr2 (fun spr2 -> f (1 + spr1 + spr2)))
      | Neg(pr) -> size_pred pr (fun spr -> f (1 + spr)) in
  size_pred pr (fun spr -> spr)

let size (pol:policy) : int =
  let rec size (pol:policy) f : int =
    match pol with
      | Filter pr -> f (size_pred pr + 1)
      | Mod(_) -> f 1
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
      | IP4Src (n, m) ->
        SDN_Types.Pattern.Ip.less_eq (pkt.headers.ipSrc, 32l) (n, m)
      | IP4Dst (n, m) ->
        SDN_Types.Pattern.Ip.less_eq (pkt.headers.ipDst, 32l) (n, m)
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

let eval_pipes (packet:packet) (pol:NetKAT_Types.policy)
  : (string * packet) list *
    (string * packet) list *
    packet list =
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


let switches_of_policy (pol : policy) : switchId list =
  let ids : (switchId, unit) Hashtbl.Poly.t = Hashtbl.Poly.create () in
  let rec count_pred (pred : pred) (k : unit -> 'a) : 'a = match pred with
    | Test (Switch sw) ->
      Hashtbl.Poly.set ids ~key:sw ~data:();
      k ()
    | True | False | Test _ -> k ()
    | And (a, b) | Or (a, b) ->
      count_pred a (fun () -> count_pred b (fun () -> k ()))
    | Neg a -> count_pred a k in
  let rec count_pol (pol : policy) (k : unit -> 'a) : 'a = match pol with
    | Filter a -> count_pred a k
    | Mod _ -> k ()
    | Union (p, q) | Seq (p, q) ->
       count_pol p (fun () -> count_pol q (fun () -> k ()))
    | Star p -> count_pol p k
    | Link (sw1, _, sw2, _) ->
      Hashtbl.Poly.set ids ~key:sw1 ~data:();
      Hashtbl.Poly.set ids ~key:sw2 ~data:();
      k () in
  count_pol pol ident;
  Hashtbl.Poly.keys ids
