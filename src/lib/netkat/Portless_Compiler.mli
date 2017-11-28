
(** Compiles a portless policy to a portful policy using the provided topology*)
val compile: Syntax.policy -> Frenetic_kernel.Network.Net.Topology.t -> Syntax.policy
