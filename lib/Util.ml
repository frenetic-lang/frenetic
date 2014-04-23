(*
 *   (¯`·._.·(¯`·._.· PEOPLE, READ THIS BEFORE YOU EDIT ·._.·´¯)·._.·´¯)
 *
 * At times it is necessary to write a function that's super-general and
 * reusable, but really has no proper place within the codebase. Hence the Util
 * module. Welcome to the sin bin.
 *
 * Before you start to write that function, take a minute to search Core to see
 * if there is a function there that does what you want to do. Here's convenient
 * link to its documentation page:
 *
 *  https://ocaml.janestreet.com/ocaml-core/latest/doc/
 *
 * Next, check this library's dependencies to see if that function's already
 * been written elsewhere. Finally, give this library a quick grep to see if the
 * function might have been written in some other module as a one-off. In that
 * case, for the sake of posterity, move it in here and reuse it.
 *
 * If after all that you're still without the function you need, have at it
 * below.
 *)

let intercalate f s = function
  | [] ->
    ""
  | h::t ->
    List.fold_left (fun acc x -> acc ^ s ^ f x) (f h) t

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
