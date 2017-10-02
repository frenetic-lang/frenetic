
(** Compiles a portless policy to a portful policy using the provided topology*)
val compile: Syntax.policy -> Frenetic_base.Network.Net.Topology.t -> Syntax.policy
