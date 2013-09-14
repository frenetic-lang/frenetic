open Types
open Parser
open Topology

(* Create a topology from the AST of a DOT file *)
let topo_from_dot (dg:dotgraph) : Topology.t =
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
  in
  let mk_host n m =
    try StringMap.find n m
    with Not_found ->
      if n.[0] = 'h' then Node.Host n
      else if n.[0] = 's' then Node.Switch(n, get_int n)
      else if n.[0] = 'm' then Node.Mbox (n, [])
      else failwith (Printf.sprintf "Invalid host type for %s\n" n)
  in
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
        let h1 = mk_host n1 m in
        let h2 = mk_host n2 m in
        let l = {Link.srcport = attrs.sport; Link.dstport = attrs.dport;
                 Link.cost = attrs.cost; Link.capacity = attrs.capacity} in
        let l' = {Link.srcport = attrs.dport; Link.dstport = attrs.sport;
                  Link.cost = attrs.cost; Link.capacity = attrs.capacity} in
        let g' = Topology.add_edge_e g (Link.mk_edge h1 h2 l) in
        let g' = if not (n1 = n2) then Topology.add_edge_e g' (Link.mk_edge h2 h1 l')
          else g' in
        (g',m)
  in
  let g = Topology.empty in
  let DotGraph(n,ds) = dg in
  let g',_ = List.fold_left add_stmt (g, StringMap.empty) ds in
  g'

let parse_file target file  =
  let lexbuf = Lexing.from_channel (open_in file) in
  let ast =
    try target Lexer.main lexbuf
    with Parsing.Parse_error ->
      let i = Lexer.info lexbuf in
      let t = Lexer.lexeme lexbuf in
      Printf.eprintf "%s: parse error at %s\n" (string_of_info file i) t;
      exit 1
  in
  ast

let _ =
  let fname = Sys.argv.(1) in
  Printf.printf "Attempting to topology from file: %s\n%!" fname;
  let ast = parse_file Parser.graph fname in
  let topo = topo_from_dot ast in
  Printf.printf "Topology: \n%s\n" (Topology.to_dot topo);
  topo

