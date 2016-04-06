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
open Core.Std

let make_string_of formatter x =
  let open Format in
  let buf = Buffer.create 100 in
  let fmt = formatter_of_buffer buf in
  pp_set_margin fmt 80;
  formatter fmt x;
  fprintf fmt "@?";
  Buffer.contents buf

let string_of_option to_string opt =
  match opt with
  | Some v -> "Some " ^ to_string v
  | None -> "None"

let string_of_list to_string l =
  let strs = List.map l to_string in
  "[" ^ (String.concat ~sep:", " strs) ^ "]"

module IntPair = struct
  type t = (int * int) [@@deriving sexp, compare]
  let hash (t1, t2) = 617 * t1 +  619 * t2
end

module IntPairTbl = Hashtbl.Make(IntPair)
