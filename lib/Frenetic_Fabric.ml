open Core.Std
open Frenetic_Network
open Frenetic_OpenFlow

module Compiler = Frenetic_NetKAT_Compiler
module FDK = Frenetic_Fdd.FDK

type policy = Frenetic_NetKAT.policy
type fabric = (switchId, Frenetic_OpenFlow.flowTable) Hashtbl.t
type stream = (policy * policy)

let strip_vlan = Some 0xffff

let mk_flow (pat:Pattern.t) (actions:group) : flow =
  { pattern = pat
  ; action = actions
  ; cookie = 0L
  ; idle_timeout = Permanent
  ; hard_timeout = Permanent
  }

let drop = mk_flow Pattern.match_all [[[]]]

let vlan_per_port (net:Net.Topology.t) : fabric =
  let open Net.Topology in
  let tags = Hashtbl.Poly.create ~size:(num_vertexes net) () in
  iter_edges (fun edge ->
      let src, port = edge_src edge in
      let label = vertex_to_label net src in
      let pattern = { Pattern.match_all with dlVlan =
                                               Some (Int32.to_int_exn port)} in
      let actions = [ [ [ Modify(SetVlan strip_vlan); Output (Physical port) ] ] ] in
      let flow = mk_flow pattern actions in
      match Node.device label with
      | Node.Switch ->
        Hashtbl.Poly.change tags (Node.id label)
          ~f:(fun table -> match table with
              | Some flows -> Some( flow::flows )
              | None -> Some [flow; drop] )
      | _ -> ()) net;
  tags

let shortest_path (net:Net.Topology.t)
    (ingress:switchId list) (egress:switchId list) : fabric =
  let open Net.Topology in
  let vertexes = vertexes net in
  let vertex_from_id swid =
    let vopt = VertexSet.find vertexes (fun v ->
      (Node.id (vertex_to_label net v)) = swid) in
    match vopt with
    | Some v -> v
    | None -> failwith (Printf.sprintf "No vertex for switch id: %Ld" swid )
  in

  let mk_flow_mod (tag:int) (port:int32) : flow =
    let pattern = { Pattern.match_all with dlVlan = Some tag } in
    let actions = [[[ Output (Physical port) ]]] in
    mk_flow pattern actions
  in


  let table = Hashtbl.Poly.create ~size:(num_vertexes net) () in
  let tag = ref 10 in
  List.iter ingress ~f:(fun swin ->
    let src = vertex_from_id swin in
    List.iter egress ~f:(fun swout ->
      if swin = swout then ()
      else
        let dst = vertex_from_id swout in
        tag := !tag + 1;
        match Net.UnitPath.shortest_path net src dst with
        | None -> ()
        | Some p ->
          List.iter p ~f:(fun edge ->
            let src, port = edge_src edge in
            let label = vertex_to_label net src in
            let flow_mod = mk_flow_mod !tag port in
            match Node.device label with
            | Node.Switch ->
              Hashtbl.Poly.change table (Node.id label)
                ~f:(fun table -> match table with
                | Some flow_mods -> Some( flow_mod::flow_mods )
                | None -> Some [flow_mod; drop] )
            | _ -> ())));
  table

let of_local_policy (pol:policy) (sws:switchId list) : fabric =
  let fabric = Hashtbl.Poly.create ~size:(List.length sws) () in
  let compiled = Compiler.compile_local pol in
  List.iter sws ~f:(fun swid ->
      let table = (Compiler.to_table swid compiled) in
      match Hashtbl.Poly.add fabric ~key:swid ~data:table with
      | `Ok -> ()
      | `Duplicate -> printf "Duplicate table for switch %Ld\n" swid
    ) ;
  fabric


let of_global_policy (pol:policy) (sws:switchId list) : fabric =
  let fabric = Hashtbl.Poly.create ~size:(List.length sws) () in
  let compiled = Compiler.compile_global pol in
  List.iter sws ~f:(fun swid ->
      let table = (Compiler.to_table swid compiled) in
      match Hashtbl.Poly.add fabric ~key:swid ~data:table with
      | `Ok -> ()
      | `Duplicate -> printf "Duplicate table for switch %Ld\n" swid
    ) ;
  fabric

let to_string (fab:fabric) : string =
  let buf = Buffer.create (Hashtbl.length fab * 100) in
  Hashtbl.Poly.iteri fab ~f:(fun ~key:swid ~data:mods ->
      Buffer.add_string buf (
        Frenetic_OpenFlow.string_of_flowTable
          ~label:(sprintf "Switch %Ld |\n" swid)
          mods)) ;
  Buffer.contents buf


let rec remove_dups (pol:policy) : policy =
  let open Frenetic_NetKAT in
  let at_location sw pt =
    let sw_test = Test (Switch sw) in
    let pt_test = Test (Location (Physical pt)) in
    let loc_test = Frenetic_NetKAT_Optimize.mk_and sw_test pt_test in
    Filter loc_test in
  let to_location sw pt =
    let sw_mod = Mod (Switch sw) in
    let pt_mod = Mod (Location (Physical pt)) in
    Seq ( sw_mod, pt_mod ) in
  match pol with
  | Filter a    -> Filter a
  | Mod hv      -> Mod hv
  | Union (p,q) -> Union(remove_dups p, remove_dups q)
  | Seq (p,q)   -> Seq(remove_dups p, remove_dups q)
  | Star p      -> Star(remove_dups p)
  | Link (s1,p1,s2,p2) ->
    Seq (at_location s1 p1, to_location s2 p2)
  | VLink _ -> failwith "Fabric: Cannot remove Dups from a policy with VLink"

let extract (pol:policy) : (policy * policy) list =
  let open FDK in
  let module NK = Frenetic_NetKAT in

  (* This returns a list of paths, where the each path is a list of
     policies. The head of each path is the policy form of the leaf node action
     and the remainder is a list of predicates that need to be true to perform
     the action. *)
  let rec get_paths id path =
    let node = unget id in
    match node with
    | Branch ((v,l), t, f) ->
      let true_pred   = NK.Test (Frenetic_Fdd.Pattern.to_hv (v, l)) in
      let true_paths  = get_paths t ( (NK.Filter true_pred)::path ) in
      let false_pred  = NK.Neg true_pred in
      let false_paths = get_paths f ( (NK.Filter false_pred)::path ) in
      List.unordered_append true_paths false_paths
    | Leaf r -> [ (Frenetic_Fdd.Action.to_policy r)::path ]
  in

  (* Partition a path through the FDD into the condition and the
     action. TODO(basus): add checks for either component. *)
  let partition (path: NK.policy list) = match path with
    | head::tail ->
      let action = head in
      let condition = Frenetic_NetKAT_Optimize.mk_big_seq tail in
      (condition, action)
    | _ -> failwith "Path through FDD not long enough to paritition"
  in
  let deduped = remove_dups pol in
  printf "\nDup free policy: \n%s\n"
    (Frenetic_NetKAT_Pretty.string_of_policy deduped);
  let fdd = Compiler.compile_local deduped in
  printf "\nCompiled fdd:\n%s\n" (FDK.to_string fdd);
  let paths = get_paths fdd [] in
  printf "\nNumber of paths %d\n%!" (List.length paths);
  List.map paths ~f:partition


let assemble (pol:policy) (topo:policy) ings egs : policy =
  let open Frenetic_NetKAT in
  let union = Frenetic_NetKAT_Optimize.mk_big_union in
  let seq = Frenetic_NetKAT_Optimize.mk_big_seq in
  let to_filter (sw,pt) = Filter( And( Test(Switch sw),
                                       Test(Location (Physical pt)))) in
  let ingresses = union (List.map ings ~f:to_filter) in
  let egresses  = union (List.map egs ~f:to_filter) in
  seq [ ingresses;
        Star(Seq(pol, topo)); pol;
        egresses ]

let find_predecessors topo sw =
  let open Frenetic_NetKAT in
  let rec find sw pol acc =
    match pol with
    | Union(p1, p2) -> find sw p1 (find sw p2 acc)
    | Link (s1,p1,s2,p2) -> if s2 = sw then (s1,p1,p2)::acc else acc
    | _ -> failwith "Invalid construct in topology" in
  match sw with
  | None -> failwith "Cannot find predecessor for None switch"
  | Some sw -> find sw topo []

let locate_from_header hv =
  let open Frenetic_NetKAT in
  match hv with
  | Switch sw -> (Some sw, None)
  | Location (Physical pt) -> (None, Some pt)
  | _ -> (None, None)

let rec locate_from_endpoint policy : (switchId option * portId option) =
  let open Frenetic_NetKAT in
  let combine p1 p2 = match p1, p2 with
    | (None, None), (None, None) -> (None, None)
    | (Some sw, None), (None, Some pt)
    | (None, Some pt), (Some sw, None)
    | (Some sw, Some pt), (None, None)
    | (None, None), (Some sw, Some pt) ->
      printf " Sw: %Ld pt: %ld" sw pt ;(Some sw, Some pt)
    | _ -> failwith "Clash" in
  match policy with
  | Mod hv -> locate_from_header hv
  | Union (p1, p2) -> combine (locate_from_endpoint p1) (locate_from_endpoint p2)
  | Seq (p1, p2) -> combine (locate_from_endpoint p1) (locate_from_endpoint p2)
  | Star p -> locate_from_endpoint p
  | _ -> (None, None)

let rec locate_from_origin policy =
  let open Frenetic_NetKAT in
  let combine p1 p2 = match p1, p2 with
    | (None, None), (None, None) -> (None, None)
    | (Some sw, None), (None, Some pt)
    | (None, Some pt), (Some sw, None)
    | (Some sw, Some pt), (None, None)
    | (None, None), (Some sw, Some pt) -> (Some sw, Some pt)
    | _ -> failwith "Clash" in
  let rec locate_from_filter f = match f with
    | Test hv -> locate_from_header hv
    | True | False | Neg _ -> (None, None)
    | And(p1, p2) -> combine (locate_from_filter p1) (locate_from_filter p2)
    | Or (p1, p2) -> combine (locate_from_filter p1) (locate_from_filter p2) in
  match policy with
  | Filter f -> locate_from_filter f
  | Union (p1, p2) -> combine (locate_from_origin p1) (locate_from_origin p2)
  | Seq (p1, p2) -> combine (locate_from_origin p1) (locate_from_origin p2)
  | Star p -> locate_from_origin p
  | _ -> (None, None)


let mk_ingress ideal fabric topo =
  let open Frenetic_NetKAT in
  let matching sw preds = match sw with
    | None -> failwith "mk_ingress: None switch cannot be matched"
    | Some sw -> begin
        match List.hd ( List.filter preds ~f:(fun (s,p,_) -> s = sw) ) with
        | Some (sw,pt,_) -> pt
        | None -> failwith (sprintf "Nowhere to go from %Ld " sw) end
  in
  let sw_fab, pt_fab = locate_from_origin fabric in
  let sw_ideal, _ = locate_from_origin ideal in
  let preds = find_predecessors topo sw_fab in
  let port = matching sw_ideal preds in
  let action = Mod( Location( Physical port)) in
  Seq(ideal,action)

(* let match_actions pol_table fab_table topo = *)
(*   let open Frenetic_NetKAT in *)
(*   let fab_outs = Hashtbl.keys fab_table in *)
(*   (\* For each (switch,port) endpoint in the ideal policy there are three options: *)
(*      1. The fabric reaches that endpoint *)
(*      2. The fabric reaches the ideal switch, but not the same port *)
(*      3. The fabric reaches a port that connects to the ideal switch *\) *)
(*   Hashtbl.fold pol_table ~init:[] ~f:(fun ~key:(sw,pt) ~data:conds acc -> *)
(*           printf "Inside match_actions outer loop\n%!" ; *)
(*       List.fold conds ~init:acc ~f:(fun acc cond -> *)
(*           printf "Inside match_actions inner loop\n%!" ; *)
(*       match Hashtbl.find fab_table (sw,pt) with *)
(*       | Some conds' -> List.fold conds' ~init:acc ~f:(fun acc cond' -> *)
(*           ((sw,pt), mk_ingress cond cond' topo, Filter True)::acc) *)
(*       | None -> *)
(*         let conds = List.map *)
(*             (List.filter fab_outs ~f:(fun (sw',_) -> sw = sw')) *)
(*             ~f:(fun key -> Hashtbl.find_exn fab_table key) in *)
(*         begin match conds with *)
(*           | head::_ -> ((sw,pt),mk_ingress cond (List.hd_exn head) topo, Filter True)::acc *)
(*           | _ -> *)
(*             (\* TODO(basus): this should be a list of acceptable predecessors *\) *)
(*             printf "In the final clause\n%!"; *)
(*             let sw', pt',pt'' = List.hd_exn (find_predecessors topo sw)in *)
(*             let cond' = Hashtbl.find_exn fab_table (Some sw', Some pt') in *)
(*             printf "Found predecessors\n%!"; *)
(*             let end_sw = begin match sw with *)
(*               | Some sw -> sw *)
(*               | None -> failwith "Cannot test against none switch" end in *)
(*             let end_pt = begin match pt with *)
(*               | Some pt -> pt *)
(*               | None -> failwith "Cannot test against none port" end in *)
(*             printf "Right before final collection\n%!"; *)
(*             ((sw, pt), *)
(*              mk_ingress cond (List.hd_exn cond') topo, *)
(*              Seq(Filter (And (Test( Location(Physical pt'')), *)
(*                               Test( Switch end_sw))), *)
(*                 (Mod (Location (Physical end_pt)))))::acc *)
(*         end )) *)

let find_upto_predecessor (sw,pt) table topo : policy list =
    let predecessors = find_predecessors topo sw in
  List.fold predecessors ~init:[] ~f:(fun acc (sw,pt,_) ->
      let conds = Hashtbl.Poly.find table (Some sw, Some pt) in
      match conds with
      | Some conds -> List.rev_append conds acc
      | None -> acc)

let end_stream fab_conds cond topo : policy option =
  let open Frenetic_NetKAT in
  let ideal_sw_opt, _ = locate_from_origin cond in
  match ideal_sw_opt with
  | None -> None
  | Some ideal_sw ->
    List.fold fab_conds ~init:None
      ~f:(fun acc cond' -> match acc with
          | Some _ -> acc
          | None ->
            let fab_sw, fab_pt = locate_from_origin cond' in
            let preds = find_predecessors topo fab_sw in
            List.fold preds ~init:None ~f:(fun acc (sw,pt,_) ->
                match acc with
                | Some _ -> acc
                | None   ->
                  if sw = ideal_sw
                  then Some (Seq (cond, (Mod (Location (Physical pt)))))
                  else acc))

let preceding_stream fab_conds cond =
  [],[]

let retarget (ideal:stream list) (fabric:stream list) (topology:policy) =
  (* Collect fabric streams by endpoint *)
  let fab_table = Hashtbl.Poly.create ~size:(List.length fabric) () in
  List.iter fabric (fun (cond, actions) ->
      let loc = (locate_from_endpoint actions) in
      if loc = (None, None)
      then ()
      else Hashtbl.Poly.add_multi fab_table loc cond);

  List.fold ideal ~init:([],[]) ~f:(fun (ins,outs) (cond, actions) ->
      let end_point = locate_from_endpoint actions in
      match Hashtbl.Poly.find fab_table end_point with
      | Some streams ->
        let ingress = end_stream streams cond topology in
        begin match ingress with
          | Some ing -> (ing::ins, outs)
          | None -> printf "No ingress found"; (ins,outs)
        end
      | None ->
        let fab_conds = find_upto_predecessor end_point fab_table in
        preceding_stream fab_conds cond
    )

let print_partition (cond, act) =
  printf "Condition: %s\n%!" (Frenetic_NetKAT_Pretty.string_of_policy cond);
  printf "Action: %s\n\n%!" (Frenetic_NetKAT_Pretty.string_of_policy act)
