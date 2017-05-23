open Core
open ProbNetKAT

type pk = value Field.Map.t

let test (f : field) (v : value) (pk : pk) : bool =
  Option.value_map (Map.find pk f) ~f:(Value.equal v) ~default:false

let modify (f : field) (v : value) (pk : pk) : pk =
  Map.add pk ~key:f ~data:v
