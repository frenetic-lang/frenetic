let map_option f = function
  | None -> None
  | Some x -> Some (f x)

let string_of_list to_string l =
  let strs = List.map to_string l in
  "[" ^ (String.concat ", " strs) ^ "]"

let string_of_option to_string opt =
  match opt with
  | Some v -> "Some " ^ to_string v
  | None -> "None"

let string_of_pair to_string1 to_string2 (p1,p2) =
  Printf.sprintf "(%s, %s)" (to_string1 p1) (to_string2 p2)
