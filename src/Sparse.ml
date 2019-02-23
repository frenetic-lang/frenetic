open! Core

type entry = { 
  i : int;
  j : int;
  mutable v : float 
}

type mat = {
  n : int;
  m : int;
  mutable entries : entry list
}

let zeros n m = { n; m; entries = [] }

let set m i j v =
  if v <> 0.0 then m.entries <- { i; j; v} :: m.entries

let foldi_nz mat ~init ~f =
  List.fold mat.entries ~init ~f:(fun acc { i; j; v} -> f i j acc v)

let row mat i =
  { mat with entries = List.filter mat.entries ~f:(fun e -> e.i = i) }
