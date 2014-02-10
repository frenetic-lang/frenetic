open Topology_Types
val from_dotfile_tbl : string -> (Topology_Core.Topology.t *
                                    (string, attributes) Hashtbl.t *
                                    (Topology_Core.switchId, attributes) Hashtbl.t)
val from_dotfile : string -> Topology_Core.Topology.t
val from_gmlfile : string -> Topology_Core.Topology.t
