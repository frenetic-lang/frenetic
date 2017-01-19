open Core.Std
open Frenetic_NetKAT
open Frenetic_OpenFlow

module Action = Frenetic_Fdd.Action
module Fabric = Frenetic_Fabric
module Dyad = Fabric.Dyad
module Condition = Fabric.Condition

type fiber = Frenetic_Topology.CoroNet.Waypath.fiber
type assemblage = Fabric.Assemblage.t

type place = Fabric.place
type path  = Dyad.t * ( place list )
type spread = Dyad.t list * int

let seq     = Frenetic_NetKAT_Optimize.mk_big_seq
let union   = Frenetic_NetKAT_Optimize.mk_big_union

type topology = {
  topo  : policy
; preds : (place, place) Hashtbl.t
; succs : (place, place) Hashtbl.t }

type timings = (string * Int64.t) list
type result = policy * timings


module type SYNTHESIZER = sig
  type input
  type solution
  val synthesize : input -> input -> policy -> solution
end

module type MATCHER = SYNTHESIZER with
  type input = Dyad.t list and type solution = result

module type STITCHER = SYNTHESIZER with
  type input = path list and type solution = result

module type DIVERSIFIER = SYNTHESIZER with
  type input = spread list and type solution = result


module Path = struct

  open MParser
  module Tokens = MParser_RE.Tokens

  type t = pred * place list

  let symbol = Tokens.symbol

  let pred : (pred, bytes list) MParser.t =
    char '[' >>
    many_chars_until (none_of "]") (char ']') >>= fun s ->
    return (Frenetic_NetKAT_Parser.pred_of_string s)

  let location : (place , bytes list) MParser.t =
    many_chars_until digit (char '@') >>= fun swid ->
    many_chars digit >>= fun ptid ->
    return ((Int64.of_string swid),
            (Int32.of_string ptid))

  let path : (t, bytes list) MParser.t =
    pred >>= fun p ->
    spaces >> (char ':') >> spaces >>
    location >>= fun start ->
    (many_until (spaces >> symbol "==>" >> spaces >> location >>= fun l ->
                 return l)
       (char ';')) >>= fun ls ->
    return (p, start::ls)

  let program : (t list, bytes list) MParser.t =
    many_until (spaces >> path) eof

  let of_string (s:string) : (t list, string) Result.t =
    match (MParser.parse_string program s []) with
    | Success paths -> Ok paths
    | Failed (msg, e) -> Error msg

end

(** Start graph-based retargeting. *)

module OverNode = struct
  type t = (switchId * portId)

  let default = (0L, 0l)

  let compare l1 l2 = Pervasives.compare l1 l2
  let hash l1       = Hashtbl.hash l1

  let equal (sw,pt) (sw',pt') =
    sw = sw' && pt = pt'

  let to_string (sw,pt) = sprintf "(%Ld:%ld)" sw pt
end

module OverEdge = struct
  open Frenetic_NetKAT

  type t =
    | Internal                            (* Between ports on the same switch *)
    | Exact of Condition.t * Action.t       (* The fabric delivers between those exact point *)
    | Adjacent of Condition.t * Action.t    (* Fabric delivers between locations attached to
                                             these in the topology *)

  let default = Internal

  let compare = Pervasives.compare
  let hash    = Hashtbl.hash
  let equal   = (=)
end

module Overlay = struct
  module G = Graph.Persistent.Digraph.ConcreteBidirectionalLabeled
      (OverNode)
      (OverEdge)
  include G
end

module Weight = struct
  type t = int
  type edge = Overlay.E.t
  let weight e = 1
  let compare = Int.compare
  let add = (+)
  let zero = 0
end

module OverPath = Graph.Path.Dijkstra(Overlay)(Weight)

let overlay (places:place list) (fabric:Dyad.t list) (topo:policy) : Overlay.t =
  let preds = Fabric.Topo.predecessors topo in
  let succs = Fabric.Topo.successors topo in

  (* Add vertices for all policy locations *)
  let graph = List.fold places ~init:Overlay.empty
      ~f:(Overlay.add_vertex) in

  (* Add edges between all location pairs reachable via the fabric *)
  let graph = ( List.fold fabric ~init:graph
      ~f:(fun g (_,src, sink, condition, action) ->
           let open OverEdge in
           let g' = Overlay.add_edge_e g (src, Exact(condition,action) , sink) in
           let src' = Hashtbl.Poly.find preds src in
           let sink' = Hashtbl.Poly.find succs sink in
           match src', sink' with
           | Some src', Some sink' ->
            Overlay.add_edge_e g' (src', Adjacent(condition, action), sink')
           | _ -> g'
         ) ) in

  (* Add edges between ports of the same switch *)
  Overlay.fold_vertex (fun (sw,pt) g ->
      Overlay.fold_vertex (fun (sw',pt') g ->
          if sw = sw' && not (pt = pt')
          then Overlay.add_edge_e g ((sw,pt), Internal, (sw,pt'))
          else g ) g g) graph graph

(* Given an edge representing a fabric path (hop) and a  *)
let generalize ((_,fabric_path,_):Overlay.E.t) (cond:Condition.t)
  : policy list * policy list =
  let open Condition in
  match fabric_path with
  | Internal -> [], []
  | Exact (cond',_)
  | Adjacent (cond',_) ->
    if places_only cond' then [], []
    else if is_subset cond' cond then
      let mods = satisfy cond' in
      let restore = undo cond' cond in
      (mods, restore)
    else
      let mods = satisfy cond' in
      let encapsulate = Mod( Location( Pipe( "encapsulate" ))) in
      let restore = Mod( Location( Pipe( "decapsulate" ))) in
      (encapsulate::mods, [restore])

(* Given a list of hops consisting of alternating fabric paths and internal
   forwards on edge switches, generate ingress, egress, or bounce rules to
   stitch the hops together, forming a VLAN-tagged path between the naive
   policy endpoints *)
let rec stitch (_,src,sink,condition,action) path tag =
  let open OverEdge in
  let rec aux path restore = match path with
    | [] -> ([], [])
    | (_,Internal,(_,in_pt))::rest ->
      let sw, pt = src in
      let start =
        And( Test( Switch sw ), Test( Location( Physical pt ))) in
      let pred = Condition.to_pred condition in
      let filter = Filter( And( start, pred )) in
      begin match rest with
        | [] ->
          ([Seq(filter,Mod( Location( Physical in_pt))) ], [])
        | hd::_ ->
          let satisfy,restore = generalize hd condition in
          let forward = [ Mod( Vlan tag ); Mod( Location( Physical in_pt)) ] in
          let ingress = seq ( filter::(List.append satisfy forward)) in
          let ins, outs = aux rest restore in
          (ingress::ins, outs)
      end
    | (_,Adjacent _,_)::((out_sw,out_pt),Internal,(_,in_pt))::rest ->
      let test_loc = And( Test( Switch out_sw ),
                          Test( Location( Physical out_pt ))) in
      let filter = Filter( And( Test( Vlan tag ),
                                test_loc )) in
      begin match rest with
        | [] ->
          let mods = Action.to_policy action in
          let out = Mod( Location( Physical (snd sink) )) in
          let egress = seq ( filter::Mod( Vlan 0xffff)::(List.append restore
                                                               [ mods; out]) ) in
          ([], [egress])
        | hd::_ ->
          let out = Mod( Location( Physical in_pt )) in
          let satisfy, restore' = generalize hd condition in
          let modify = List.append restore satisfy in
          let egress = seq ( filter::(List.append modify [out])) in
          let ins, outs = aux rest restore' in
          (ins, egress::outs) end
    | _ -> failwith "Malformed path" in
  aux path []


let generate topo pairs : policy * policy =

  let to_netkat topo
      ((_,src,dst,cond,actions)) ((_,src',dst',cond',actions'))
      (tag:int): policy * policy =
    let open Fabric.Topo in
    let open Fabric.Condition in

    let strip_vlan = 0xffff in
    let to_fabric = go_to topo src src' in
    let to_edge   = Mod( Location (Physical (snd dst))) in
    let in_filter  = Filter (to_pred cond) in
    let out_filter = Filter (And( come_from topo dst' dst,
                                  Test( Vlan tag))) in
    let modify = Frenetic_Fdd.Action.to_policy actions in
    let ingress = seq ([ in_filter; Mod( Vlan tag ); to_fabric ]) in
    let egress  = seq ([ out_filter; Mod( Vlan strip_vlan ); modify; to_edge ]) in
    ingress, egress in


  let ins, outs, _ = List.fold_left pairs ~init:([],[], 0)
      ~f:(fun (ins, outs, tag) (pol, fab) ->
          let ins', outs' = to_netkat topo pol fab tag in
          (ins'::ins, outs'::outs, tag+1)) in
  (union ins, union outs)


module GraphicalMatching = struct
  type input = Dyad.t list
  type solution = result

  let synthesize (policy:input) (fabric:input) (topo:policy) =
    let open Frenetic_Time in
    let start = time () in
    let places = List.fold_left policy ~init:[] ~f:(fun acc (_,src, sink,_,_) ->
        src::sink::acc) in
    let graph = overlay places fabric topo in
    let form_time = from start in

    let start = time () in
    let paths =  List.fold policy ~init:[]
        ~f:(fun acc ((_,src,sink,c,a) as dyad) ->
            try
              let path,_ = OverPath.shortest_path graph src sink in
              (path, dyad)::acc
            with Not_found ->
              let open Frenetic_Fabric in
              printf "No path between %s and %s\n%!"
                (string_of_place src) (string_of_place sink);
              acc) in
    let soln_time = from start in

    let start = time () in
    let ingresses, egresses, _ = List.fold paths ~init:([],[],1)
        ~f:(fun (ins, outs, tag) (path,dyad) ->
            let ingress, egress = stitch dyad path tag in
            ( List.rev_append ingress ins,
              List.rev_append egress  outs,
              tag + 1 )) in
    let gen_time = from start in

    let timings = [ ("Formulation time" , form_time)
                  ; ("Solution time"    , soln_time)
                  ; ("Generation time"  , gen_time) ] in

    Frenetic_NetKAT.Union( union ingresses, union egresses), timings

end

module GraphicalStitching = struct
  type input = path list
  type solution = result

  let implement acc dyad ps graph =
    let open OverEdge in
    let rec mk_ends node path = match path with
      | [] -> []
      | hd::tl -> (node,hd)::(mk_ends hd tl) in
    let endpoints = mk_ends (List.hd_exn ps) (List.tl_exn ps) in
    let cond = Dyad.condition dyad in
    let action = Action.one in
    List.fold_left endpoints ~init:acc ~f:(fun acc (src,sink) ->
        try
          let path,_ = OverPath.shortest_path graph src sink in
          let dyad' = (src, sink, cond, action) in
          (path, dyad)::acc
        with Not_found ->
          let open Frenetic_Fabric in
          let msg = sprintf "No path between %s and %s\n%!"
              (string_of_place src) (string_of_place sink) in
          raise (NonExistentPath msg))


  let synthesize (paths:input) (fabric:input) (topo:policy) =
    let open Frenetic_Time in
    let start = time () in
    let places = List.fold_left paths ~init:[] ~f:(fun acc (_,ps) ->
        List.fold_left ps ~init:acc ~f:(fun acc p -> p::acc)) in
    let fab = List.map fabric ~f:fst in
    let graph = overlay places fab topo in
    let form_time = from start in

    let start = time () in
    let segments = List.fold paths ~init:[] ~f:(fun acc (dyad,ps) ->
        implement acc dyad ps graph) in
    let soln_time = from start in

    let start = time () in
    let ingresses, egresses, _ = List.fold segments ~init:([],[],1)
        ~f:(fun (ins, outs, tag) (path, dyad) ->
            try
              let ing, out = stitch dyad path tag in
              ( List.rev_append ing ins,
                List.rev_append out outs,
                tag + 1 )
            with Fabric.NonExistentPath s ->
              print_endline s;
              (ins, outs, tag)) in
    let gen_time = from start in

    let timings = [ ("Formulation time" , form_time)
                  ; ("Solution time"    , soln_time)
                  ; ("Generation time"  , gen_time ) ] in

    Union( union ingresses, union egresses), timings

end

module LinearMatching = struct
  open Fabric.Topo
  open Frenetic_LP

  type input = Dyad.t list

  type solution = result

  let uid = Frenetic_Fabric.Dyad.uid

  let to_lp policy fabric topo =
    let tbl = Hashtbl.Poly.create ~size:(List.length policy) () in

    let vars, checks = List.fold policy ~init:([],[]) ~f:(fun (vars,checks) pol ->
        List.fold fabric ~init:(vars,checks) ~f:(fun (vars,checks) fab ->
            let var = sprintf "v_%d_%d" (uid pol) (uid fab) in
            Hashtbl.Poly.add_multi tbl (uid pol) var;
            let checks = if not (adjacent topo pol fab ) then
                Constraint( Var var, Eq, 0L)::checks
              else checks in
            var::vars, checks)) in

    let allocs = Hashtbl.Poly.fold tbl ~init:[] ~f:(fun ~key:id ~data:vars acc ->
        let vars = List.map vars ~f:(fun v -> Var v) in
        let expr = sum vars in
        Constraint(expr, Eq, 1L)::acc) in

    let objective = Minimize( [sum (List.map vars ~f:(fun v -> Var v))]) in
    let constraints = checks@allocs in
    let bounds = [] in
    let bools = [ Binary vars ] in
    let sos = NoSos in
    LP (objective, constraints, bounds, bools, sos)

  let pair policy fabric solns =
    List.fold solns ~init:[] ~f:(fun acc soln ->
        match String.split soln ~on:'_' with
        | ["v";p;f] ->
          let p_id = Int.of_string p in
          let f_id = Int.of_string f in
          let pol = List.find_exn policy ~f:(fun d -> (uid d) = p_id) in
          let fab = List.find_exn fabric ~f:(fun d -> (uid d) = f_id) in
          (pol, fab)::acc
        | _ ->
          let msg = sprintf "Variable: %s\n" soln in
          raise (LPParseError msg))


  let synthesize (policy:input) (fabric:input) (topo:policy) =
    let open Frenetic_LP in
    let open Frenetic_Time in
    let lp_file = "synthesis.lp" in
    let sol_file = "synthesis.sol" in

    let preds = Fabric.Topo.predecessors topo in
    let succs = Fabric.Topo.successors topo in
    let topology = {topo; preds; succs} in

    let start = time () in
    let lp = to_lp policy fabric topology in
    let form_time = from start in

    clean sol_file;
    clean lp_file;
    write lp_file (Frenetic_LP.to_string lp);

    let cmd = sprintf "gurobi_cl ResultFile=%s %s > gurobi_output.log"
        sol_file lp_file in
    let start = time () in
    Sys.command_exn cmd;
    let soln_time = from start in

    let soln = read sol_file in
    let pairs = pair policy fabric soln in

    let start = time () in
    let ingress, egress = generate topology pairs in
    let gen_time = from start in

    let timings = [ ("Formulation time" , form_time)
                  ; ("Solution time"    , soln_time)
                  ; ("Generation time"  , gen_time) ] in

    ( Union(ingress, egress), timings )

end

module LinearStitching = struct
  open Fabric.Topo
  open Frenetic_LP

  type input = path list
  type solution = result

  let uid = Frenetic_Fabric.Dyad.uid

  let to_lp policy fabric topo =
    let ptbl = Hashtbl.Poly.create ~size:(List.length policy) () in
    let ftbl = Hashtbl.Poly.create ~size:(List.length fabric) () in
    let paths = Hashtbl.Poly.create ~size:(List.length fabric) () in

    (* Generate a variable Vij if policy dyad i can be implemented on fabric
       dyad j (ie, they share the same endpoints) *)
    let vars, checks = List.fold policy ~init:([],[]) ~f:(fun (vars,checks) (pol,ppath) ->
        List.fold fabric ~init:(vars,checks) ~f:(fun (vars,checks) (fab,fpath) ->
            let fid = uid fab in
            let var = sprintf "v_%d_%d" (uid pol) (uid fab) in
            Hashtbl.Poly.add_multi ptbl (uid pol) var;
            Hashtbl.Poly.set paths fid fpath;

            let checks =
              if not (adjacent topo pol fab ) then
                Constraint( Var var, Eq, 0L)::checks
              else
                (* Generate a constraint Vij=0 if \Sum Fnj < # of wpts for all n
                   wpts of policy dyad i *)
                let pts = List.map ppath ~f:(fun (swid,_) ->
                    Hashtbl.Poly.add_multi ftbl fid swid;
                    Var (sprintf "f_%Ld_%d" swid fid)) in
                let expr = sum pts in
                let bound = (List.length ppath) - 1 in
                Indicator(var, false, expr, Leq, Int64.of_int bound)::checks in
            var::vars, checks)) in

    (* Generate a constraint that chooses one fabric dyad for each policy dyad *)
    let choose = Hashtbl.Poly.fold ptbl ~init:checks ~f:(fun ~key:id ~data:vars acc ->
        let vars = List.map vars ~f:(fun v -> Var v) in
        let expr = sum vars in
        Constraint(expr, Eq, 1L)::acc) in

    (* Generate a constraint Fnj = 0 if node n is NOT on the path for dyad j *)
    let pathpts, fabs = Hashtbl.Poly.fold ftbl ~init:(choose,[])
        ~f:(fun ~key:fid ~data:swids acc ->
            List.fold swids ~init:acc ~f:(fun acc swid ->
                let ps, fabs = acc in
                let path = Hashtbl.Poly.find_exn paths fid in
                if List.exists path ~f:(fun (sw,_) -> sw = swid) then acc
                else
                  let var = sprintf "f_%Ld_%d" swid fid in
                  Constraint(Var var, Eq, 0L)::ps, var::fabs)) in

    let objective = Minimize( [sum (List.map vars ~f:(fun v -> Var v))]) in
    let constraints = pathpts in
    let bounds = [] in
    let bools = [ Binary vars ; Binary fabs] in
    let sos = NoSos in
    LP (objective, constraints, bounds, bools, sos)


  let pair policy fabric solns =
    List.fold solns ~init:[] ~f:(fun acc soln ->
        match String.split soln ~on:'_' with
        | ["v";p;f] ->
          let p_id = Int.of_string p in
          let f_id = Int.of_string f in
          let pol = List.find_exn policy ~f:(fun (d,_) -> (uid d) = p_id) in
          let fab = List.find_exn fabric ~f:(fun (d,_) -> (uid d) = f_id) in
          (fst pol, fst fab)::acc
        | ["f";p;f] -> acc
        | _ ->
          let msg = sprintf "Variable: %s\n" soln in
          raise (LPParseError msg))

  let synthesize (policy:input) (fabric:input) (topo:policy) =
    let open Frenetic_LP in
    let open Frenetic_Time in
    let lp_file = "synthesis.lp" in
    let sol_file = "synthesis.sol" in

    let preds = Fabric.Topo.predecessors topo in
    let succs = Fabric.Topo.successors topo in
    let topology = {topo; preds; succs} in

    let start = time () in
    let lp = to_lp policy fabric topology in
    let form_time = from start in

    clean sol_file;
    clean lp_file;
    write lp_file (Frenetic_LP.to_string lp);

    let cmd = sprintf "gurobi_cl ResultFile=%s %s > gurobi_output.log"
        sol_file lp_file in
    let start = time () in
    Sys.command_exn cmd;
    let soln_time = from start in

    let soln = read sol_file in
    let pairs = pair policy fabric soln in

    let start = time () in
    let ingress, egress = generate topology pairs in
    let gen_time = from start in

    let timings = [ ("Formulation time" , form_time)
                  ; ("Solution time"    , soln_time)
                  ; ("Generation time"  , gen_time) ] in

    ( Union(ingress, egress), timings )

end

(* module GraphicalDiverse = struct end *)
(* module LinearDiverse = struct end *)

