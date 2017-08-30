open Core
open Frenetic_NetKAT_Compiler

module NetKAT_Auto = Frenetic_NetKAT_Compiler.Automaton
module Par = Action.Par
module Seq = Action.Seq

module Automaton = struct

  let x = 2

(*   let constants auto : Int64.Set.t =
    let of_fdd fdd =
      FDD.fold fdd ~f:(fun par -> )
 *)

  let of_netkat_auto (auto : NetKAT_Auto.t) =
    let constants auto : Int64.Set.t =
      NetKAT_Auto.fold_reachable auto ~init:Int64.Set.empty ~f:(fun acc _ (e,d) -> acc)
    in
    constants auto
end
