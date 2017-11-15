(** Module for generating ProbNetKAT encoding of topology with certain failure
    model.
*)
module Net = Frenetic.Network.Net


let parse : string -> Net.Topology.t =
  Net.Parse.from_dotfile

