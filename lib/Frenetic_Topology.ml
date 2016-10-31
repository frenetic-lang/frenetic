(* Topology utility functions. This module should eventually be replaced with a
 * Frenetic-specific topology module that includes the ocaml-topology module.
 *)

open Core.Std

module Net = Frenetic_NetKAT_Net.Net
module SDN = Frenetic_OpenFlow
type circuit = Frenetic_Circuit_NetKAT.circuit
type portId = Frenetic_NetKAT.portId

let switch_ids (t : Net.Topology.t) : SDN.switchId list =
  let open Net.Topology in
  fold_vertexes (fun v acc ->
    match vertex_to_label t v with
    | Frenetic_NetKAT_Net.Switch id -> id::acc
    | _ -> acc)
  t []

(* Topology detection doesn't really detect hosts. So, I treat any
   port not connected to a known switch as an edge port *)
let internal_ports (t : Net.Topology.t) (sw_id : SDN.switchId) =
  let open Net.Topology in
  let switch = vertex_of_label t (Switch sw_id) in
  PortSet.fold  (vertex_to_ports t switch) ~init:PortSet.empty ~f:(fun acc p ->
    match next_hop t switch p with
    | Some e ->
       let node, _ = edge_dst e in
       begin match vertex_to_label t node with
       | Switch _ -> PortSet.add acc p
       | _ -> acc
       end
    | _ -> acc)

let in_edge (t : Net.Topology.t) (sw_id : SDN.switchId) (pt_id : SDN.portId) =
  let open Net.Topology in
  let switch = vertex_of_label t (Switch sw_id) in
  match next_hop t switch pt_id with
  | None    -> true
  | Some(_) -> false

let edge (t: Net.Topology.t) =
  let open Net.Topology in
  fold_vertexes (fun v acc ->
    match vertex_to_label t v with
    | Frenetic_NetKAT_Net.Switch sw_id ->
      PortSet.fold (vertex_to_ports t v) ~init:acc ~f:(fun acc pt_id ->
        match next_hop t v pt_id with
        | None   -> (sw_id, pt_id)::acc
        | Some _ -> acc)
    | _ -> acc)
  t []


module CoroNode = struct

  open Frenetic_Packet
  open Frenetic_NetKAT

  type t =
    | Switch of string * switchId
    | Repeater of switchId
    | Host of string * dlAddr * nwAddr
  [@@deriving sexp, compare]


  let id t = match t with
    | Switch(name, id) -> id
    | Repeater id      -> id
    | Host(name, dlAddr, nwAddr) -> dlAddr

  let to_string t = match t with
    | Switch(name, id) -> name
    | Repeater id      -> sprintf "r%Lu" id
    | Host(name, dlAddr, nwAddr) -> name

  let parse_dot _ _ = failwith "Cannot parse a Coronet node from DOT format"
  let parse_gml _   = failwith "Cannot parse a Coronet node from GML format"

  let to_dot t = match t with
    | Switch(name, id) ->
      sprintf "switch %s [label=%Lu]" name id
    | Repeater id ->
      sprintf "repeater %Lu" id
    | Host(name, dlAddr, nwAddr) ->
      sprintf "%s [label=%s]" (to_string t) (string_of_nwAddr nwAddr)

  let to_mininet n = match n with
    | Switch(name, id) ->
      sprintf "%s = net.addSwitch(\'s%Ld\', dpid=\'%s\', cls=LINCSwitch)\n"
        name id (string_of_mac id)
    | Repeater(id) ->
      sprintf "r%Ld = net.addSwitch(\'r%Ld\', cls=LINCSwitch)\n" id id
    | Host(name, mac, ip) ->
      (* Mininet doesn't like underscores in host names *)
      let mnname = Str.global_replace (Str.regexp "_") "" name in
      sprintf "%s = net.addHost(\'%s\', mac=\'%s\', ip=\'%s\')\n"
        name mnname
        (string_of_mac mac) (string_of_ip ip)

end

module CoroLink = struct
  type t = float [@@deriving sexp, compare]

  let to_string = string_of_float

  let default = 0.0

  let parse_dot _ = failwith "Cannot parse a Coronet link from DOT format"
  let parse_gml _ = failwith "Cannot parse a Coronet link from GML format"

  let to_dot = to_string

end

module Distance = struct
  type edge = CoroLink.t [@@deriving sexp, compare]
  type t = float [@@deriving sexp, compare]

  let weight l = l
  let add = (+.)
  let zero = 0.
end

type name_table = (string, CoroNode.t) Hashtbl.t
type port_table = (string, Frenetic_NetKAT.portId) Hashtbl.t

module CoroNet = struct
  exception NonexistentNode of string

  include Frenetic_Network.Make(CoroNode)(CoroLink)

  module CoroPath = Path(Distance)
  type path = CoroPath.t * int
  type pathset = { src : Topology.vertex
                 ; dst : Topology.vertex
                 ; shortest : path option
                 ; local    : path option
                 ; across   : path option
                 }

  type waypath = { path : CoroPath.t
                 ; start : Topology.vertex * portId
                 ; stop  : Topology.vertex * portId
                 ; waypoints : Topology.vertex list
                 ; channel : int
                 }
  type wptable = ((string * string), waypath list) Hashtbl.t

  let string_of_path (p,ch) =
    sprintf "%d@[%s]" ch ( CoroPath.to_string p )

  let string_of_pathset net ps =
    let show = function
      | Some (p,ch) ->
        sprintf "%d@[%s]" ch ( CoroPath.to_string p )
      | None -> "" in
    sprintf "From:%s\nTo:%s\nShortest:\n%s\nLocal First:\n%s\nAcross First:\n%s\n\n"
      (Topology.vertex_to_string net ps.src)
      (Topology.vertex_to_string net ps.dst)
      (show ps.shortest) (show ps.local) (show ps.across)

  module Debug = struct
    let vertex = Topology.vertex_to_string

    let closest net tbl =
      Topology.VertexHash.iteri tbl ~f:(fun ~key:src ~data:(dst, weight, path) ->
          printf "%s => %s ; weight=%f; %s\n%!"
            ( vertex net src ) ( vertex net dst )
            weight ( CoroPath.to_string path ))

    let all_closest net east west cross =
      print_endline "\nEast paths:";
      closest net east;
      print_endline "\nWest paths:";
      closest net west;
      print_endline "\nCross paths:";
      closest net cross;

  end

  let find_label tbl name =
    match Hashtbl.find tbl name with
        | Some l -> l
        | None -> raise (NonexistentNode name)

  let find_vertex net tbl name =
    let label = find_label tbl name in
    Topology.vertex_of_label net label

  let from_path_file net filename =
    let channel = In_channel.create filename in
    let vertexes = Topology.vertexes net in

    let name_tbl = String.Table.create ~size:(Topology.VertexSet.length vertexes) () in
    Topology.VertexSet.iter vertexes ~f:(fun v ->
        let label = Topology.vertex_to_label net v in
        Hashtbl.add_exn name_tbl (CoroNode.to_string label) v);

    In_channel.fold_lines channel ~init:[] ~f:(fun acc line ->
        let words = String.split line ~on:';' in
        let vertices = List.map words ~f:(fun w ->
            Hashtbl.find_exn name_tbl (String.strip w)) in
        (CoroPath.from_vertexes net vertices)::acc)

  let get_label tbl name = match Hashtbl.find tbl name with
    | None ->
      let next = Int64.of_int ( (Hashtbl.length tbl) + 1 ) in
      let sw = CoroNode.Switch(name, next) in
      Hashtbl.add_exn tbl name sw;
      sw
    | Some l -> l

  (* Start ports at 1. *)
  let get_port tbl name = match Hashtbl.find tbl name with
    | None ->
      Hashtbl.add_exn tbl name 1l;
      1l
    | Some p ->
      Hashtbl.set tbl name (Int32.succ p);
      Int32.succ p

  let from_csv_file filename =
    let net = Topology.empty () in
    let channel = In_channel.create filename in

    let name_table = String.Table.create () in
    let port_table = String.Table.create () in

    let starts_with s c = match String.index s c with
      | Some i -> i = 0
      | None -> false in

    let net = In_channel.fold_lines channel ~init:net ~f:(fun net line ->
        if starts_with line '#' then net
        else
          match String.split line ~on:',' with
          | sname::dname::[dist] ->
            let slabel = get_label name_table sname in
            let sport = get_port port_table sname in
            let net,src = Topology.add_vertex net slabel in

            let dlabel = get_label name_table dname in
            let dport = get_port port_table dname in
            let net,dst = Topology.add_vertex net dlabel in

            let distance = float_of_string dist in
            (* Add edges in both directions because the topology structure is directional *)
            let net,_ = Topology.add_edge net src sport distance dst dport in
            let net,_ = Topology.add_edge net dst dport distance src sport in
            net
          | _ -> failwith "Expected each line in CSV to have structure `src,dst,distance`"
      ) in
    (net, name_table, port_table)

  let surround net names ports east west paths =
    let range i = List.range ~start:`inclusive ~stop:`exclusive 0
        (List.length paths * i) in
    let mk_host name id =
      let hostname = sprintf "h%s" name in
      let host = CoroNode.Host(hostname, id, Int32.of_int64_exn id) in
      host in

    let aux net nodes range =
      List.fold nodes ~init:net ~f:(fun net e ->
          let label = Hashtbl.Poly.find_exn names e in
          let optcl = Topology.vertex_of_label net label in

          (* Add a packet switch to this optical node *)
          let port = Int32.succ (Hashtbl.Poly.find_exn ports e) in
          let name  = sprintf "sw%s" e in
          let pktsw = get_label names name in
          let net,pkt = Topology.add_vertex net pktsw in

          (* Add a host to the packet switch *)
          let host = mk_host e (CoroNode.id label) in
          let net,hv = Topology.add_vertex net host in
          let net,_ = Topology.add_edge net hv 0l 0.0 pkt 0l in
          let net,_ = Topology.add_edge net pkt 0l 0.0 hv 0l in

          (* Add one edge between the optical and packet switch for each channel
             connecting this optical switch to another. Assumes that `range`
             starts from 0. *)
          List.fold range ~init:net ~f:(fun net i ->
              let open Int32 in
              let i = of_int_exn i in
              let net,_ = Topology.add_edge net pkt (i+1l) 0.0 optcl (port+i) in
              fst ( Topology.add_edge net optcl (port+i) 0.0 pkt (i+1l)) ))
    in

    let westward = range (List.length west) in
    let eastward = range (List.length east) in
    let net' = aux net east westward in
    aux net' west eastward

  let find_paths net local across channel src dst =
    let shortest = match CoroPath.shortest_path net src dst with
      | Some p -> Some (p, channel)
      | None -> None in
    let local_next,_,p = Topology.VertexHash.find_exn local src in
    let local_across = match CoroPath.shortest_path net local_next dst with
      | Some p' ->
        Some ( CoroPath.join p p', channel + 1)
      | None ->
        None in

    let across_next,_,p = Topology.VertexHash.find_exn across src in
    let across_local = match CoroPath.shortest_path net across_next dst with
      | Some p' ->
        Some ( CoroPath.join p p', channel + 2)
      | None ->
        printf "No path between %s and %s"
          (Debug.vertex net across_next) (Debug.vertex net dst);
        None in

    shortest, local_across, across_local

  let closest tbl =
    let open Topology in
    let tbl' = VertexHash.create ~size:(VertexPairHash.length tbl) () in
    VertexPairHash.iteri tbl ~f:(fun ~key:(src,dst) ~data:(w,p) ->
        VertexHash.update tbl' src ~f:(fun value -> match value with
            | None -> (dst,w,p)
            | Some(dst',w',p') ->
              if w < w' then (dst,w,p) else (dst',w',p')));
    tbl'

  let pack src dst (s,l,a) =
    { src = src; dst = dst; shortest = s; local = l; across = a}

  let cross_connect (net:Topology.t) (tbl:name_table)
      (e:string list) (w:string list) =
    let module VS = Topology.VertexSet in
    let east = List.fold e ~init:VS.empty ~f:(fun acc name ->
        let v = find_vertex net tbl name in
        VS.add acc v) in
    let west = List.fold w ~init:VS.empty ~f:(fun acc name ->
        let v = find_vertex net tbl name in
        VS.add acc v) in

    let east_paths = CoroPath.all_pairs_shortest_paths ~topo:net
        ~f:(fun v1 v2 -> ( VS.mem east v1 && VS.mem east v2 ) &&
                         (not ( v1 = v2 ))) |> closest in
    let west_paths = CoroPath.all_pairs_shortest_paths ~topo:net
        ~f:(fun v1 v2 -> ( VS.mem west v1 && VS.mem west v2) &&
                         (not ( v1 = v2 ))) |> closest in

    let cross_paths = CoroPath.all_pairs_shortest_paths ~topo:net
        ~f:(fun v1 v2 -> ( VS.mem west v1 && VS.mem east v2 ) ||
                         ( VS.mem west v2 && VS.mem east v1 )) |> closest in

    let east_to_west = find_paths net east_paths cross_paths in
    let west_to_east = find_paths net west_paths cross_paths in

    let paths,_ = VS.fold east ~init:([], 1) ~f:(fun (acc,ch) e ->
        VS.fold west ~init:(acc,ch) ~f:(fun (acc,ch) w ->
            let wtoe = pack w e (west_to_east ch w e) in
            let etow = pack e w ( east_to_west (ch+3) e w ) in
            ( etow::wtoe::acc, ch+6 ))) in
    paths

  (* Asumme that the cross country paths are east to west *)
  let path_connect (net:Topology.t) (ntbl:name_table) (ptbl:port_table)
      (e:string list) (w:string list) (ps:CoroPath.t list) =
    let module VS = Topology.VertexSet in
    let show = Topology.vertex_to_string net in
    let east = List.fold e ~init:VS.empty ~f:(fun acc name ->
        let v = find_vertex net ntbl name in
        VS.add acc v) in
    let west = List.fold w ~init:VS.empty ~f:(fun acc name ->
        let v = find_vertex net ntbl name in
        VS.add acc v) in

    let wptbl = Hashtbl.Poly.create ~size:((List.length e) * (List.length w)) () in
    let ptbl' = String.Table.create ~size:(String.Table.length ptbl) () in
    (* This could be optimized by precomputing the prefix and suffixes *)
    let paths,_ = VS.fold east ~init:([],1) ~f:(fun (acc, ch) e ->
        let e_name = show e in
        String.Table.update ptbl' e_name ~f:(fun popt -> match popt with
                | None -> ( String.Table.find_exn ptbl e_name )
                | Some p -> p);

        VS.fold west ~init:(acc,ch) ~f:(fun (acc,ch) w ->
            let w_name = show w in
            String.Table.update ptbl' w_name ~f:(fun popt -> match popt with
                | None -> ( String.Table.find_exn ptbl w_name)
                | Some p -> p);

            List.foldi ps ~init:(acc,ch) ~f:(fun i (acc,ch) path ->
                (* These are the transceiver ports on optical nodes that
                   connect to the edge packet nodes *)
                let e_port = get_port ptbl' e_name in
                let w_port = get_port ptbl' w_name in
                let e' = (e, e_port) in
                let w' = (w, w_port) in

                (* Find the paths connecting the optical edge nodes to the
                   predetermined disjoint physical paths*)
                let start = CoroPath.start path in
                let stop  = CoroPath.stop  path in
                let prefix = CoroPath.shortest_path net e start in
                let suffix = CoroPath.shortest_path net stop w in

                match prefix, suffix with
                | Some p, Some s ->
                  let path' = CoroPath.join p (CoroPath.join path s) in
                  let rpath' = CoroPath.reverse net path' in
                  let waypath = { path = path'; start = e'; stop = w';
                                  waypoints = [ start; stop ]; channel = ch } in
                  let waypath' = { path = rpath'; start = w'; stop = e';
                                   waypoints = [ stop; start ]; channel = ch } in
                  Hashtbl.Poly.add_multi wptbl (e_name, w_name) waypath;
                  Hashtbl.Poly.add_multi wptbl (w_name, e_name) waypath';
                  (waypath::waypath'::acc, succ ch)
                | None, _ -> failwith (sprintf "No path between %s and %s"
                                         (show e) (show start))
                | _, None -> failwith (sprintf "No path between %s and %s"
                                         (show stop) (show w))))) in
    paths, wptbl


  (* Construct dup-free policies that connect up packet switches connected to
     the optical endpoints of waypaths, using the specified predicates. The
     number of predicates should match the number of disjoint cross-country
     physical paths. *)
  let policies (net:Topology.t) (wptbl:wptable) (preds:Frenetic_NetKAT.pred list) =
    Hashtbl.Poly.fold wptbl ~init:[] ~f:(fun ~key:(s,d) ~data:wps pols ->
        List.fold2_exn wps preds ~init:pols ~f:(fun pols wp pred ->
            let open Frenetic_NetKAT in
            (* The waypath gives the optical node endpoints of a fabric path *)
            let fsv, fspt = wp.start in
            let fdv, fdpt = wp.stop  in

            (* Use the topology to find the packet switch and ports connected
               the optical fabric endpoints *)
            let sedge = match Topology.next_hop net fsv fspt with
              | Some e -> e | None -> failwith "No src edge to connect fabric" in
            let dedge = match Topology.next_hop net fdv fdpt with
              | Some e -> e | None -> failwith "No dst edge to connect fabric" in
            let psv, psppt = Topology.edge_dst sedge in
            let pdv, pdppt = Topology.edge_dst dedge in

            (* Generate a entry predicate using the packet switch and one of the
               provided predicates. Send traffic to the packet port on the other
               end. Connect ports 0 because they are reservered for hosts. This
               may need to be changed. *)
            let pred' = Frenetic_NetKAT_Optimize.mk_big_and
                [pred;
                 Test( Switch (Topology.vertex_to_id net psv));
                 Test( Location (Physical 0l) ) ] in
            let modify = [ Mod( Switch( Topology.vertex_to_id net pdv));
                           Mod( Location( Physical 0l)) ] in
            let policy = Frenetic_NetKAT_Optimize.mk_big_seq (( Filter pred')::modify ) in
            policy::pols
          ))

  let circuit_of_path net ( src, sport ) ( dst, dport ) (p:path) =
    let open Frenetic_Circuit_NetKAT in
    let source = ( Topology.vertex_to_id net src, sport ) in
    let sink   = ( Topology.vertex_to_id net dst, dport ) in
    let channel = snd p in
    let path = List.map (fst p) ~f:(fun edge ->
        let sv,spt = Topology.edge_src edge in
        let ssw    = (Topology.vertex_to_id net sv) in
        let dv,dpt = Topology.edge_dst edge in
        let dsw    = (Topology.vertex_to_id net dv) in
        (ssw,spt,dsw,dpt)) in
    { source; sink; path; channel }

  let circuits_of_pathset net sport dport ps =
    let (>>|) = Option.(>>|) in
    let to_circuit = circuit_of_path net ( ps.src,sport ) ( ps.dst,dport ) in
    ( ps.shortest >>| to_circuit,
      ps.local    >>| to_circuit,
      ps.across   >>| to_circuit )

end
