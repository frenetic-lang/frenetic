open Types
val from_dotfile_tbl : string -> (Network.t *
                                    (string, attributes) Hashtbl.t *
                                    (switchId, attributes) Hashtbl.t)
val from_dotfile : string -> Network.t
val from_gmlfile : string -> Network.t
