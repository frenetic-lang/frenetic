open Core.Std

module SDN = Frenetic_OpenFlow

type node =
  | Switch of SDN.switchId
  | Host of Frenetic_Packet.dlAddr * Frenetic_Packet.nwAddr
  [@@deriving sexp, compare]

module Node = struct
  type t = node [@@deriving sexp, compare]

  let to_string t = match t with
    | Switch(sw_id)       -> Printf.sprintf "switch %Lu" sw_id
    | Host(dlAddr, nwAddr) -> Printf.sprintf "host %s/%s"
        (Frenetic_Packet.string_of_nwAddr nwAddr)
        (Frenetic_Packet.string_of_dlAddr dlAddr)

  let parse_dot _ _ = failwith "NYI: Node.parse_dot"
  let parse_gml _ = failwith "NYI: Node.parse_dot"

  let to_dot t = match t with
    | Switch(sw_id) -> Printf.sprintf "%s [label=SW%Lu]" (to_string t) sw_id
    | Host(dlAddr, nwAddr) -> Printf.sprintf "%s [label=%s]" (to_string t) (Frenetic_Packet.string_of_nwAddr nwAddr)

  let to_mininet _ = failwith "NYI: Node.to_mininet"
end

module Link = struct
  type t = unit [@@deriving sexp, compare]

  let to_string () = "()"
  let default = ()

  let parse_dot _ = failwith "NYI: Link.parse_dot"
  let parse_gml _ = failwith "NYI: Link.parse_dot"

  let to_dot = to_string
  let to_mininet _ = failwith "NYI: Link.to_mininet"
end

module Net = Frenetic_Network.Make(Node)(Link)
