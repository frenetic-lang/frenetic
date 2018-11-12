module type Job = sig
  val number_processes : int

  (** assumption: it is safe to marshal input/output  *)
  type input
  type output
  val map : input -> output
  val reduce : output -> output -> output
end

module Make : functor (J : Job) -> sig
  (* val map_reduce : J.input list -> J.output *)
end
