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

(* prevent core from overwriting this *)
module Printexn = Printexc
open Core

let make_string_of formatter x =
  let open Format in
  let buf = Buffer.create 100 in
  let fmt = formatter_of_buffer buf in
  pp_set_margin fmt 80;
  formatter fmt x;
  fprintf fmt "@?";
  Buffer.contents buf

let pp_exceptions () =
  Printexn.register_printer (fun exn -> Option.try_with (fun () ->
    Location.report_exception Format.str_formatter exn;
    Format.flush_str_formatter ()))

let map_fst lst ~f =
  List.map lst ~f:(fun (x,y) -> (f x, y))

let map_snd lst ~f =
  List.map lst ~f:(fun (x,y) -> (x, f y))
