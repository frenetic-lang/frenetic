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

let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = String.create n in
  really_input ic s 0 n;
  close_in ic;
  (s)

let write_to_file filename text_to_write =
  try
    let cout = open_out filename in
    let co = Format.formatter_of_out_channel cout in
    Format.fprintf co "%s\n" text_to_write;
    close_out cout
  with Sys_error _ as e ->
    Format.printf "Cannot open file \"%s\": %s\n" filename (Printexc.to_string e)


let list_intercalate f sep l =
  List.fold_left
    (fun acc li -> Printf.sprintf "%s%s%s" acc (if acc = "" then "" else sep) (f li)) "" l

(* JNF-ENHANCED SETS AND MAPS *)

module type OrderedType = Set.OrderedType

module Mapplus =
struct
  module type S = sig
    include Map.S
    val map : 'a t -> ('a -> 'b) -> 'b t
    val keys : 'a t -> key list
    val values : 'a t -> 'a list
  end
  module Make (Ord:OrderedType) =
  struct
    include Map.Make(Ord)
    let map f m = fold (fun k v acc -> add k (f v) acc) m empty
    let keys m = let ks,_ = List.split (bindings m) in ks
    let values m = let _,vs = List.split (bindings m) in vs
  end
end

module Setplus =
struct
  module type S = sig
    include Set.S
    val map : (elt -> elt) -> t -> t
    val intercalate : (elt -> string) -> string -> t -> string
    val of_list : elt list -> t
  end
  module Make (Ord:OrderedType) =
  struct
    include Set.Make(Ord)
    let map f s = fold (fun v acc -> add (f v) acc) s empty
    let intercalate f sep s =
      fold
        (fun si acc -> Printf.sprintf "%s%s%s" acc (if acc = "" then "" else sep) (f si))
        s ""

    let of_list ls =
      List.fold_left (fun acc e -> add e acc) empty ls
  end
end
