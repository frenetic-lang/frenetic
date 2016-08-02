module Ast = Frenetic_Decide_Ast
module Net = Frenetic_Network.Net
module NetKAT = Frenetic_NetKAT
module Network = Frenetic_Network
module Util = Frenetic_Decide_Util

module Predicate = struct
  type t =
    | One
    | Zero
    | Test of Util.Field.t * Util.Value.t
    | Or of t * t
    | And of t * t
    | Not of t

  let rec to_string pred =
    match pred with
    | One         -> "1"
    | Zero        -> "0"
    | Test (f, v) ->
        let fs = Util.Field.to_string f in
        let vs = Util.Value.to_string v in
        Printf.sprintf "(%s = %s)" fs vs
    | Or   (a, b) -> Printf.sprintf "(%s ∨ %s)" (to_string a) (to_string b)
    | And  (a, b) -> Printf.sprintf "(%s ∧ %s)" (to_string a) (to_string b)
    | Not   a     -> Printf.sprintf "¬(%s)" (to_string a)

  let rec compile (pred: t) : Ast.Term.t =
    match pred with
    | One         -> Ast.Term.one
    | Zero        -> Ast.Term.zero
    | Test (f, v) -> Ast.Term.test f v
    | Or   (a, b) -> Ast.Term.plus (Ast.TermSet.of_list [compile a; compile b])
    | And  (a, b) -> Ast.Term.times [compile a; compile b]
    | Not   a     -> Ast.Term.not (compile a)
end

module Query = struct
  type t =
    | Pred of Predicate.t * Predicate.t
    | Plus of t * t
    | Times of t * t
    | Star of t

  let rec to_string query =
    match query with
    | Pred  (a, b)  -> Printf.sprintf "(%s, %s)"
                       (Predicate.to_string a) (Predicate.to_string b)
    | Plus  (q, q') -> Printf.sprintf "(%s + %s)" (to_string q) (to_string q')
    | Times (q, q') -> Printf.sprintf "(%s ; %s)" (to_string q) (to_string q')
    | Star   q      -> Printf.sprintf "(%s)*" (to_string q)

  let rec compile (p: Ast.Term.t) (t: Ast.Term.t) (query: t) : Ast.Term.t =
    let c = compile p t in
    match query with
    | Pred  (a, b)  -> Ast.Term.times [p; Predicate.compile a; t; Predicate.compile b]
    | Plus  (q, q') -> Ast.Term.plus (Ast.TermSet.of_list [c q; c q'])
    | Times (q, q') -> Ast.Term.times [c q; c q']
    | Star   q      -> Ast.Term.star (c q)
end

let uncurry f (a, b) =
  f a b

let term_of_field_value ((f, v): (string * string)) : (Util.Field.t * Util.Value.t) =
  (Util.Field.of_string f, Util.Value.of_string v)

let term_of_location (l: Frenetic_NetKAT.location) : (string * string) =
  let open Frenetic_NetKAT in
  match l with
  | Physical i  -> ("port",  Int32.to_string i)
  | FastFail is -> ("ports", string_of_fastfail is)
  | Pipe s      -> ("port",  "pipe(" ^ s ^ ")")
  | Query s     -> ("port",  "query(" ^ s ^ ")")

let term_of_header_val (h: Frenetic_NetKAT.header_val) : (string * string) =
  let open Frenetic_NetKAT in
  let open Frenetic_Packet in
  match h with
  | Switch i         -> ("switch",     Int64.to_string i)
  | Location l       -> term_of_location l
  | EthSrc addr      -> ("ethSrc",     string_of_mac addr)
  | EthDst addr      -> ("ethDst",     string_of_mac addr)
  | Vlan i           -> ("vlanId",     string_of_int i)
  | VlanPcp i        -> ("vlanPcp",    string_of_int i)
  | EthType i        -> ("ethType",    string_of_int i)
  | IPProto i        -> ("ipProto",    string_of_int i)
  | IP4Src (addr, i) -> ("ip4src",     string_of_ip addr ^ "/" ^ Int32.to_string i)
  | IP4Dst (addr, i) -> ("ip4dst",     string_of_ip addr ^ "/" ^ Int32.to_string i)
  | TCPSrcPort i     -> ("tcpSrcPort", string_of_int i)
  | TCPDstPort i     -> ("tcpDstPort", string_of_int i)
  | VSwitch i        -> ("vswitch",    Int64.to_string i)
  | VPort i          -> ("vport",      Int64.to_string i)
  | VFabric i        -> ("vfabric",    Int64.to_string i)

let term_of_pred (p: Frenetic_NetKAT.pred) : Ast.Term.t =
  let open Ast in
  let open Frenetic_NetKAT in

  (* this function is implemented using continuation passing style to avoid
   * stack overflows on large inputs *)
  let rec help p k =
    match p with
    | True ->
        k (Term.one)
    | False ->
        k (Term.zero)
    | Test h ->
        k (uncurry Term.test (term_of_field_value (term_of_header_val h)))
    | And (a, b) ->
        help a @@ fun a_term ->
        help b @@ fun b_term ->
        k (Term.times [a_term; b_term])
    | Or (a, b) ->
        help a @@ fun a_term ->
        help b @@ fun b_term ->
        k (Term.plus (TermSet.of_list [a_term; b_term]))
    | Neg a ->
        help a @@ fun a_term ->
        k (Term.not (a_term))
  in
  help p (fun x -> x)

let term_of_policy p =
  let open Ast in
  let open Frenetic_NetKAT in

  (* this function is implemented using continuation passing style to avoid
   * stack overflows on large inputs *)
  let rec help p k =
    match p with
    | Filter a ->
        k (term_of_pred a)
    | Mod h ->
        k (uncurry Term.assg (term_of_field_value (term_of_header_val h)))
    | Union (p, q) ->
        help p @@ fun p_term ->
        help q @@ fun q_term ->
        k (Term.plus (TermSet.of_list [p_term; q_term]))
    | Seq (p, q) ->
        help p @@ fun p_term ->
        help q @@ fun q_term ->
        k (Term.times [p_term; q_term])
    | Star p ->
        help p @@ fun p_term ->
        k (Term.star (p_term))
    | Link  (_s1, _p1, _s2, _p2) -> failwith "Link not yet implemented"
    | VLink (_s1, _p1, _s2, _p2) -> failwith "VLink not yet implemented"
  in
  help p (fun x -> x)

module StrMap = Map.Make(String)
let map_append key v m =
  if StrMap.mem key m then
    let lst = StrMap.find key m in
    StrMap.add key (Frenetic_NetKAT.Union(lst, v)) m
  else StrMap.add key v m

let rec policy_to_map p m =
  let open Ast in
  let open Frenetic_NetKAT in
  let open Frenetic_Packet in

  let rec pred_to_ipdst pred =
    match pred with
    | Test h -> begin
      match h with
      | IP4Dst (n, _) -> Some (string_of_ip n)
      | _ -> None
    end
    | And (p, q) -> begin
      match pred_to_ipdst p with
      | Some dst -> Some dst
      | None -> pred_to_ipdst q
    end
    | _ -> None
  in

  let rec get_ipdst rule =
    match rule with
    | Filter a -> pred_to_ipdst a
    | Mod h -> None
    | Seq (p, q) -> begin
      match get_ipdst p with
      | Some dst -> Some dst
      | None -> get_ipdst q
    end
    | _ -> failwith "Should not happen"
  in

  match p with
  | Union (p, q) ->
    policy_to_map p (policy_to_map q m)
  | Seq _ -> begin
    match get_ipdst p with
    | Some dst ->
      map_append dst p m
    | None -> failwith "Policy rule does not have destination"
  end
  | _ -> failwith "Should not happen"

let terms_of_policy_ipdst p =
  let dst_map = policy_to_map p StrMap.empty in
  StrMap.fold (fun _ v acc -> (term_of_policy v)::acc) dst_map []

let is_switch topo v =
  match Network.Node.device (Net.Topology.vertex_to_label topo v) with
  | Network.Node.Switch -> true
  | _ -> false

let is_host topo v =
  match Network.Node.device (Net.Topology.vertex_to_label topo v) with
  | Network.Node.Host -> true
  | _ -> false

let filter header_val =
  NetKAT.(Filter (Test header_val))

let term_of_topology t =
  let open Net in
  let open NetKAT in
  let open Network in

  let f term edge =
    let vsrc, psrc = Topology.edge_src edge in
    let vdst, pdst = Topology.edge_dst edge in
    let lsrc = Topology.vertex_to_label t vsrc in
    let ldst = Topology.vertex_to_label t vdst in
    if is_switch t vsrc && is_switch t vdst then
      let s1 = filter @@ Switch (Node.id lsrc) in
      let p1 = filter @@ Location (Physical psrc) in
      let s2 = Mod (Switch (Node.id ldst)) in
      let p2 = Mod (Location (Physical pdst)) in
      let link = Ast.Term.times [
        term_of_policy s1;
        term_of_policy p1;
        term_of_policy s2;
        term_of_policy p2;
      ] in
      Ast.Term.plus (Ast.TermSet.of_list [term; link])
    else
      term
  in
  Topology.EdgeSet.fold (Topology.edges t) ~init:Ast.Term.zero ~f

let in_of_topology t =
  let open Net in
  let open NetKAT in
  let open Network in

  let f term edge =
    let vsrc, _ = Topology.edge_src edge in
    let vdst, pdst = Topology.edge_dst edge in
    let ldst = Topology.vertex_to_label t vdst in
    if is_host t vsrc && is_switch t vdst then
      let s2 = filter @@ Switch (Node.id ldst) in
      let p2 = filter @@ Location (Physical pdst) in
      let host = Ast.Term.times [term_of_policy s2; term_of_policy p2] in
      Ast.Term.plus (Ast.TermSet.of_list [term; host])
    else
      term
  in
  Topology.EdgeSet.fold (Topology.edges t) ~init:Ast.Term.zero ~f

let out_of_topology t =
  let open Net in
  let open NetKAT in
  let open Network in

  let f term edge =
    let vsrc, psrc = Topology.edge_src edge in
    let vdst, _ = Topology.edge_dst edge in
    let lsrc = Topology.vertex_to_label t vsrc in
    if is_switch t vsrc && is_host t vdst then
      let s1 = filter @@ Switch (Node.id lsrc) in
      let p1 = filter @@ Location (Physical psrc) in
      let host = Ast.Term.times [term_of_policy s1; term_of_policy p1] in
      Ast.Term.plus (Ast.TermSet.of_list [term; host])
    else
      term
  in
  Topology.EdgeSet.fold (Topology.edges t) ~init:Ast.Term.zero ~f

type network = {
  ingress:  Ast.Term.t;
  outgress: Ast.Term.t;
  p:        Ast.Term.t;
  t:        Ast.Term.t;
}

let compile {ingress; outgress; p; t} q =
  Ast.Term.times [ingress; (Query.compile p t q); p; outgress]
