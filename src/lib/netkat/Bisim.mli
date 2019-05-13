(** Checks if the given automata are bisimilar using Hopcroft-Karp.  *)
val check : Global_compiler.Automaton.t -> Global_compiler.Automaton.t -> bool
