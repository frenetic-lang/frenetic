open Core
open Probnetkat

type pk = value Field.Map.t

let test (f : field) (v : value) (pk : pk) : bool =
  Option.value_map (Map.find pk f) ~f:(Value.equal v) ~default:false

let modify (f : field) (v : value) (pk : pk) : pk =
  Map.add pk ~key:f ~data:v

let pp fmt pk =
  Format.fprintf fmt "@[";
  if Map.is_empty pk then Format.fprintf fmt "*@ " else
  Map.iteri pk ~f:(fun ~key ~data -> Format.fprintf fmt "@[%s=%d@]@ " key data);
  Format.fprintf fmt "@]";
  ()
