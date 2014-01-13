let map_option f = function
  | None -> None
  | Some x -> Some (f x)

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

let make_string_of formatter x =
  let open Format in
  let buf = Buffer.create 100 in
  let fmt = formatter_of_buffer buf in
  pp_set_margin fmt 80;
  formatter fmt x;
  fprintf fmt "@?";
  Buffer.contents buf
