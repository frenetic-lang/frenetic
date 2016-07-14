(** Lexical analyzer for NetKAT

  Mostly the definitions here are used to placate the Camlp4 parser, which wants parsing functions in a
  certain way.   So see camlp4 documentation for descriptions of the functions here.
*)

module Loc = Camlp4.PreCast.Loc

module Error : sig
  type t
  exception E of t

  val print : Format.formatter -> t -> unit
  val to_string : t -> string
end

type token =
  | KEYWORD  of string
  | INT of string
  | INT32 of string
  | INT64 of string
  | IP4ADDR of string
  | ANTIQUOT of string
  | STRING_CONSTANT of string
  | METAID of string
  | EOI

module Token : sig
  module Loc = Loc
  module Error = Error

  type t = token

  module Filter : sig
    type t = unit
    type token_filter = (token, Loc.t) Camlp4.Sig.stream_filter

    val keyword_removed : t -> bytes -> t
    val keyword_added : t -> bytes -> bool -> t
    val filter : t -> token_filter
    val define_filter : t -> (token_filter -> token_filter) -> t
    val mk : (bytes -> bool) -> t
  end


  val to_string : t -> string
  val print : Format.formatter -> t -> unit
  val match_keyword : bytes -> t -> bool
  val extract_string : t -> string
end

val mk : unit -> Loc.t -> char Stream.t -> (token * Loc.t) Stream.t
