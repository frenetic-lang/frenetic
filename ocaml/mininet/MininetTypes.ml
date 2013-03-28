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
