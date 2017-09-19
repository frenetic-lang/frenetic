(** The first fields of every OpenFlow message, no matter what the version, have
    the same shape.  This is unlikely to change in subsequent OpenFlow versions
    because there needs to be some standard way to determine the version of
    each message.  So we define that here. *)

open Core

(** {6 Types and exceptions} *)

type xid = Int32.t [@@deriving sexp]

type t = {
  version: int;
  type_code: int;
  length: int;
  xid: xid
} [@@deriving sexp]

(** {6 Accessors} *)

(** [size] returns size of standard OpenFlow header in bytes *)
val size : int

(** {6 Conversion} *)

(** [parse pkt] takes a message buffer and returns a OpenFlow_Header.t.  The message buffer
     can be a complete OpenFlow raw message yanked from the wire - not just the header.  *)
val parse : Cstruct.t -> t

(** [marshal pkt hdr] fills a message buffer with the header fields of a OpenFlow_Header.t.
     The message buffer can be a complete OpenFlow raw message, with the other stuff possibly filled in.  *)
val marshal : Cstruct.t -> t -> unit

(** [to_string hdr] gives a human-readable, printable rep of a OpenFlow_Header.t  *)
val to_string : t -> string

