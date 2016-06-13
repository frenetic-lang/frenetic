open Core.Std
open Frenetic_Network
open Net
open Topology
open Frenetic_ProbNetKAT_Interpreter

include Interp(Hist)(PreciseProb)

open Pol

let parse_topology (dot_file : string) =
  (* Topology dot file to ProbNetKAT program *)
  let topo = Parse.from_dotfile dot_file in
  let host_set = VertexSet.filter (Topology.vertexes topo)
  ~f:(fun v ->
    let label = Topology.vertex_to_label topo v in
    Node.device label = Node.Host) in

  let num_hosts = Topology.VertexSet.length host_set in
  let num_vertices = (Topology.num_vertexes topo) in
  let num_edges = (Topology.num_edges topo) in

  let p1 = ?@[ !!(Switch 0) , 1/2
           ; !!(Switch 1) , 1/2 ] in

  let get_swid switch_name =
        let regexp = Str.regexp "\\([A-Za-z]+\\)\\([0-9]+\\)" in
        let _ = Str.string_match regexp switch_name 0 in
        int_of_string (Str.matched_group 2 switch_name) in

  let get_portid port =
    match Int32.to_int port with
    | None -> failwith "Invalid port"
    | Some x -> x in

  let is_host node =
     Node.device (vertex_to_label topo node) = Node.Host in

  let pol = fold_edges (fun edge pol_acc ->
    (* Ignore hosts, consider switch-switch links *)
    let src_sw, src_port = edge_src edge in
    let dst_sw, dst_port = edge_dst edge in
    if is_host src_sw || is_host dst_sw then
      pol_acc
    else
      let src_sw_id = get_swid (Node.name (vertex_to_label topo src_sw)) in
      let dst_sw_id = get_swid (Node.name (vertex_to_label topo dst_sw)) in
      let edge_src_test = Seq (Test (Switch src_sw_id), Test (Port (get_portid src_port))) in
      let edge_dest_mod = Seq (Mod (Switch dst_sw_id), Mod (Port (get_portid dst_port))) in
      let edge_prog = Seq (edge_src_test, Seq(Dup, Seq(edge_dest_mod, Dup))) in
      if pol_acc == Drop then edge_prog
      else Union (edge_prog, pol_acc)
    ) topo Drop
  in pol


let () = begin
  Printf.printf "%s\n" (Pol.to_string (parse_topology "3cycle.dot"))
end
