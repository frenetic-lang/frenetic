open! Core

module type Job = sig
  val number_processes : int

  (** assumption: it is safe to marshal input/output  *)
  type input
  type output
  val map : input -> output
  val reduce : output -> output -> output
end

module Make(J:Job) = struct
(*   let to_worker = Array.init J.number_processes ~f:(fun () ->
    let (in_ch, out_ch) = Unix.pipe () in
    let from_caml = Unix.in_channel_of_descr in_ch in
    let to_parent = Unix.out_channel_of_descr out_ch in
    failwith "todo"
  ) *)
end
