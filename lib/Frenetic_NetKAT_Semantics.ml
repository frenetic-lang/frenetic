open Core.Std

open Frenetic_NetKAT
open Frenetic_Packet

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
    } [@@deriving sexp, fields, compare]

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
        | FastFail n_lst -> Printf.sprintf "%s" (string_of_fastfail n_lst)
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

  let to_hvs (t:t) : Frenetic_NetKAT.header_val list =
    let open Frenetic_NetKAT in
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
  type t = packet sexp_opaque [@@deriving sexp]

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
      | Link(_,_,_,_) -> f 5
      | VLink(_,_,_,_) -> f 5 in
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
        Frenetic_OpenFlow.Pattern.Ip.less_eq (pkt.headers.ipSrc, 32l) (n, m)
      | IP4Dst (n, m) ->
        Frenetic_OpenFlow.Pattern.Ip.less_eq (pkt.headers.ipDst, 32l) (n, m)
      | TCPSrcPort n -> pkt.headers.tcpSrcPort = n
      | TCPDstPort n -> pkt.headers.tcpDstPort = n
      | VSwitch n | VPort n | VFabric n -> true (* SJS *)
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
        { pkt with headers = { pkt.headers with tcpDstPort = n }}
      | VSwitch n | VPort n | VFabric n -> pkt (* SJS *) in
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
    PacketSet.empty (* TODO(JNF): yeah no *)
  | VLink(vsw,vpt,vsw',vpt') ->
    PacketSet.empty (* SJS *)

let eval_pipes (packet:packet) (pol:Frenetic_NetKAT.policy)
  : (string * packet) list *
    (string * packet) list *
    packet list =
  let open Frenetic_NetKAT in
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
  let () = eprintf "Found %d packets" (PacketSet.length packets) in
  PacketSet.fold packets ~init:([],[],[]) ~f:(fun (pi,qu,phy) pkt ->
    (* Running the packet through the switch's policy will label the resultant
     * packets with the pipe or query they belong to, if any. All that's left to
     * do is pick out packets in the PacketSet.t that have a pipe location, and
     * return those packets (with the location cleared) along with the pipe they
     * belong to. *)
    match pkt.headers.HeadersValues.location with
      | Physical _ -> (            pi,             qu, pkt :: phy)
      (* TODO(grouptable) *)
      | FastFail _ -> failwith "Not Yet Implemented"
      | Pipe     p -> ((p, pkt) :: pi,             qu,        phy)
      | Query    q -> (            pi, (q, pkt) :: qu,        phy))

let queries_of_policy (pol : policy) : string list =
  let rec loop (pol : policy) (acc : string list) : string list = match pol with
    | Mod (Location (Query str)) ->
      if List.mem acc str then acc else str :: acc
    | Filter _ | Mod _ | Link _ | VLink _ -> acc
    | Union (p, q) | Seq (p, q) -> loop q (loop p acc)
    | Star p -> loop p acc in
  loop pol []

(* JNF: is this dead code? *)
(* SJS: no, we use it for the compilekat benchmarks *)
let switches_of_policy (p:policy) =
  let rec collect' a acc =
    match a with
    | Test (Switch sw) ->
       sw :: acc
    | True | False | Test _ ->
       acc
    | And (b, c) | Or (b, c) ->
       acc |> collect' b |> collect' c
    | Neg b -> collect' b acc in
  let rec collect p acc =
    match p with
    | Filter a ->
       collect' a acc
    | Mod _ ->
       acc
    | Union(q,r) | Seq (q,r) ->
       acc |> collect q |> collect r
    | Star q ->
       collect q acc
    | Link(sw1,_,sw2,_) ->
       sw1 :: sw2 :: acc
    | VLink _ -> acc in
  collect p [] |> List.dedup |> List.to_list
