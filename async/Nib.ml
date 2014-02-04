open Core.Std

module State = struct

  type host = unit
  type switch = SDN_Types.switchId

  type port = int64
    with sexp

  type node =
    | Host of host sexp_opaque
    | Switch of switch sexp_opaque
    with sexp

  module NodeSet = Set.Make(struct
    type t = node with sexp
    let compare = Pervasives.compare
  end)

  module PortMap = Map.Make(struct
    type t = (node * port) with sexp
    let compare = Pervasives.compare
  end)

  type t =
    { nodes : NodeSet.t
    ; ends : (node * port) PortMap.t
    }

  let empty =
    { nodes = NodeSet.empty; ends = PortMap.empty }

  let add_node (t : t) (n : node) : t =
    assert (not (NodeSet.mem t.nodes n));
    { t with nodes = NodeSet.add t.nodes n }

  let add_link (t : t) n1 p1 n2 p2 : t =
    assert (NodeSet.mem t.nodes n1);
    assert (NodeSet.mem t.nodes n2);

    let nodes = NodeSet.(union t.nodes (of_list [n1; n2])) in
    let ends = PortMap.add t.ends (n1, p1) (n2, p2) in
    let ends = PortMap.add ends   (n2, p2) (n1, p1) in
    { nodes; ends }

  let remove_node (t : t) (n : node) : t =
    assert (NodeSet.mem t.nodes n);
    { nodes = NodeSet.remove t.nodes n
    ; ends = PortMap.filter t.ends ~f:(fun ~key:(n0, p0) ~data:(n1, p0) ->
        not ((n0 = n) || (n1 = n))) }

  let remove_port (t : t) (n : node) (p : port) : t =
    match PortMap.find t.ends (n, p) with
      | Some(l) ->
        { t with ends = PortMap.(remove (remove t.ends (n, p)) l) }
      | None ->
        t

  let has_port (t : t) (n : node) (p : port) : (node * port) option =
    PortMap.find t.ends (n, p)

end
