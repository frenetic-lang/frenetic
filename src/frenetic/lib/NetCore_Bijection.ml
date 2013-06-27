type 'a t = ('a, 'a) Hashtbl.t

let create capacity = Hashtbl.create capacity

let del t x = 
  try 
    Hashtbl.remove t (Hashtbl.find t x);
    Hashtbl.remove t x
  with Not_found -> ()

let add t x y = 
  del t x;
  del t y;
  Hashtbl.add t x y;
  Hashtbl.add t y x

let lookup t x = 
  try
    Some (Hashtbl.find t x)
  with Not_found ->
    None

let members t =
  Hashtbl.fold (fun x _ lst -> x :: lst) t []
