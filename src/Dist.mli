open Core

module Make : functor (Dom: Vlr.HashCmp) -> Dist_intf.S with module Dom = Dom
