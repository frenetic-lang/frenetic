open Core.Std
open Frenetic_Network
open Frenetic_OpenFlow

module Compiler = Frenetic_NetKAT_Compiler

type fabric = (switchId, Frenetic_OpenFlow.flowTable) Hashtbl.t

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

let of_local_policy (pol:Frenetic_NetKAT.policy) (sws:switchId list) : fabric =
  let fabric = Hashtbl.Poly.create ~size:(List.length sws) () in
  let compiled = Compiler.compile_local pol in
  List.iter sws ~f:(fun swid ->
      let table = (Compiler.to_table swid compiled) in
      match Hashtbl.Poly.add fabric ~key:swid ~data:table with
      | `Ok -> ()
      | `Duplicate -> printf "Duplicate table for switch %Ld\n" swid
    ) ;
  fabric


let of_global_policy (pol:Frenetic_NetKAT.policy) (sws:switchId list) : fabric =
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
