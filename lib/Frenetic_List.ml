let intercalate f s = function
  | [] -> 
    ""
  | h::t -> 
    List.fold_left (fun acc x -> acc ^ s ^ f x) (f h) t

let intersperse v lst =
  List.fold_right (fun x xs -> x :: (v :: xs)) [] lst

let concat_map f lst =
  List.fold_right (fun a bs -> List.append (f a) bs) lst []
    
let rec filter_map f xs = match xs with
  | [] -> []
  | x :: xs' -> match f x with
    | Some y -> y :: (filter_map f xs')
    | None -> filter_map f xs'

let filter_none lst = filter_map (fun x -> x) lst
