open Util
open DOT_Types
open Topology
open Graph

module TopoDot = struct
  let node (i:Dot_ast.node_id) (ats:Dot_ast.attr list) : Topology.V.label =
    Node.Host("none")

  let edge (ats:Dot_ast.attr list) : Topology.E.label =
    Link.default
end


module P = Graph.Dot.Parse(Graph.Builder.P(Topology))(TopoDot)

exception ParseError of info * string
      (* Private utility functions *)
let get_int s =
  let _ = try Str.search_forward (Str.regexp "[0123456789]+") s 0
    with Not_found -> failwith
      (Printf.sprintf "Cannot guess switch identifier for %s\n" s)
  in
  let num = Str.matched_string s in
  try
    Int64.of_string num
  with Failure "int_of_string" ->
    failwith (Printf.sprintf "Invalid ID for %s\n" s)

let mk_host n m =
  try StringMap.find n m
  with Not_found ->
    if n.[0] = 'h' then Node.Host n
    else if n.[0] = 's' then Node.Switch(n, get_int n)
    else if n.[0] = 'm' then Node.Mbox (n, [])
    else failwith (Printf.sprintf "Invalid host type for %s\n" n)

let mk_edges n1 n2 attrs m =
  let h1 = mk_host n1 m in
  let h2 = mk_host n2 m in
  let l = {Link.srcport = attrs.sport; Link.dstport = attrs.dport;
           Link.cost = attrs.cost; Link.capacity = attrs.capacity} in
  let l' = {Link.srcport = attrs.dport; Link.dstport = attrs.sport;
            Link.cost = attrs.cost; Link.capacity = attrs.capacity} in
  let fwd = Link.mk_edge h1 h2 l in
  let bwd = Link.mk_edge h2 h1 l' in
  (fwd,bwd)


(* Create a topology from the AST of a DOT file *)
let topo_from_ast (dg:dotgraph) : Topology.t =
  let add_stmt (g,m) stmt =
    match stmt with
      | DotNode (n, attrs) ->
        let node = if attrs.kind = "host" then Node.Host n
          else if attrs.kind = "switch" then Node.Switch(n, attrs.id)
          else if attrs.kind = "mbox" then Node.Mbox (n, [])
          else failwith (Printf.sprintf "Invalid host type for %s\n" n)
        in
        (Topology.add_vertex g node, StringMap.add n node m)
      | DotEdge (n1,n2,attrs) ->
        let fwd,bwd = mk_edges n1 n2 attrs m in
        let g' = Topology.add_edge_e g fwd in
        (* Add the reverse direction only if it's not a self-loop *)
        let g' = if not (n1 = n2) then Topology.add_edge_e g' bwd
          else g' in
        (g',m)
      | DotDiedge (n1,n2,attrs) ->
        let fwd,bwd = mk_edges n1 n2 attrs m in
        let g' = Topology.add_edge_e g fwd in
        (g',m)
  in
  let g = Topology.empty in
  let g',_ = match dg with
    | DotGraph(n,ds) -> List.fold_left add_stmt (g, StringMap.empty) ds
    | DotDigraph(n,ds) -> List.fold_left add_stmt (g, StringMap.empty) ds in
  g'

let parse channel target =
  let lexbuf = Lexing.from_channel channel in
  let ast =
    try target Lexer.main lexbuf
    with Parsing.Parse_error ->
      let i = Lexer.info lexbuf in
      let t = Lexer.lexeme lexbuf in
      raise (ParseError (i, t))
  in
  ast

let parse_dotfile fname =
  try parse (open_in fname) Parser.graph
  with ParseError(i,t) ->
    Printf.eprintf "%s: parse error at file %s at %s\n" (string_of_info i) fname t;
    exit 1
