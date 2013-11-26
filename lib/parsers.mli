
type nattr = {
  ntype: string
  ; name : string
  ; id : int64
  ; ip : string
  ; mac : string
}

val from_dotfile_tbl : string -> (Core.Topology.t *
                                    (string, nattr) Hashtbl.t *
                                    (VInt.t, nattr) Hashtbl.t)
val from_dotfile : string -> Core.Topology.t
val from_gmlfile : string -> Core.Topology.t
