type nattr = {
  ntype: string
  ; name : string
  ; id : int64
  ; ip : string
  ; mac : string
}

val from_dotfile_tbl : string -> (Topology_Core.Topology.t *
                                    (string, nattr) Hashtbl.t *
                                    (Topology_Core.switchId, nattr) Hashtbl.t)
val from_dotfile : string -> Topology_Core.Topology.t
val from_gmlfile : string -> Topology_Core.Topology.t
