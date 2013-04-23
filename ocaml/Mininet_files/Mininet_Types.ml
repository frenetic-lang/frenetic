open Format

type switchId = Int64.t
type hostAddr = Int64.t
type portId = int

type node =
  | Host of hostAddr
  | Switch of switchId

let string_of_node node = match node with
  | Host mac -> sprintf "Host %Ld" mac
  | Switch dpid -> sprintf "Sw %Ld" dpid

let string_of_portId = string_of_int

type edge_label = portId

let string_of_edge_label = string_of_portId

let node_compare (x : node) (y : node) = match (x, y) with
  | Host _, Switch _ -> -1
  | Switch _, Host _ -> 1
  | Host m, Host n -> Int64.compare m n
  | Switch m, Switch n -> Int64.compare m n
