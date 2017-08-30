open Core
open Frenetic_NetKAT_Compiler

module NetKAT_Auto = Frenetic_NetKAT_Compiler.Automaton
module Par = Action.Par
module Seq = Action.Seq

module Automaton = struct

  let x = 2

  (* fold over all FDDs in an automaton *)
  let auto_fold ~init ~f ~g auto =
    NetKAT_Auto.fold_reachable auto ~init ~f:(fun init _ (e,d) ->
      let init = FDD.fold' ~init ~f ~g e in
      FDD.fold' ~init ~f ~g d)

  (* set of constants mentioned by automaton, either in tests or modifications *)
  let constants auto : Int64.Set.t =
    auto_fold auto ~init:Int64.Set.empty
      ~f:(fun ~init par ->
        Par.fold par ~init ~f:(fun init seq ->
          Seq.fold seq ~init ~f:(fun ~key:f ~data:v init ->
            match f,v with
            | Action.K, _ -> init
            | _, Value.Const v -> Int64.Set.add init v
            | _ -> failwith "not supported")))
      ~g:(fun ~init (f,v) ->
            match v with
            | Value.Const v -> Int64.Set.(add init v)
            | _ -> failwith "not supported")


  let of_netkat_auto (auto : NetKAT_Auto.t) =
    let constants auto : Int64.Set.t =
      NetKAT_Auto.fold_reachable auto ~init:Int64.Set.empty ~f:(fun acc _ (e,d) -> acc)
    in
    constants auto
end
