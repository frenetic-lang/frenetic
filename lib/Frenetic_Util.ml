let string_of_option to_string opt =
  match opt with
  | Some v -> "Some " ^ to_string v
  | None -> "None"

let string_of_list to_string l =
  let strs = List.map to_string l in
  "[" ^ (String.concat ", " strs) ^ "]"
