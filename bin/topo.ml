open! Core
open Probnetkat

module Parameters = struct
  (* switch field *)
  let sw = "sw"

  (* port field *)
  let pt = "pt"

  (* up bit associated with link *)
  let up sw pt = sprintf "up_%d" pt
end

module Topo = Topology.Make(Parameters)

let () = begin
  let topo = Topo.parse (Sys.argv.(1)) in
  let topo_prog = Topo.to_probnetkat topo ~guard_links:true in
  Format.printf "%a\n" Syntax.pp_policy topo_prog;
  Util.timed "topo to Fdd" (fun () -> Symbolic.Fdd.of_pol topo_prog; ())
end
