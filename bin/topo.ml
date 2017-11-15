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

module Topology = struct
  include Topology
  include Topology.Make(Parameters)
end



let () = begin

let topo = Topology.parse (Sys.argv.(1)) in
let topo_prog = Topology.to_probnetkat topo ~guard_links:true in
Format.printf "%a\n" Syntax.pp_policy topo_prog;
Util.timed "topo to Fdd" (fun () -> Symbolic.Fdd.of_pol topo_prog; ())
end
