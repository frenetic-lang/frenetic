open Core.Std
open NetKAT_Types
(*
 * Note that this module uses the fieldslib library from Jane Street, a syntax
 * extension that generates several convenience functions, .e.g, fold, map,
 * iter, getters, setters, etc., specialized for the record type.  More
 * documentation can be found here:
 *
 *   https://github.com/janestreet/fieldslib
 * *)

module type HEADER = sig
  type t with sexp
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val to_string : t -> string
  val top : t
  val is_top : t -> bool
  val join : t -> t -> t option
  val meet : t -> t -> t option
  val lessthan : t -> t -> bool
end

module Make =
  functor(Location:HEADER) ->
  functor(DlAddr:HEADER) ->
  functor(Vlan:HEADER) ->
  functor(VlanPcp:HEADER) ->
  functor(DlType:HEADER) ->
  functor(NwProto:HEADER) ->
  functor(NwAddr:HEADER) ->
  functor(TcpPort:HEADER) ->
struct
  type t =
      { location : Location.t sexp_opaque;
        ethSrc : DlAddr.t sexp_opaque;
        ethDst : DlAddr.t sexp_opaque;
        vlan : Vlan.t sexp_opaque;
        vlanPcp : VlanPcp.t sexp_opaque;
        ethType : DlType.t sexp_opaque;
        ipProto : NwProto.t sexp_opaque;
        ipSrc : NwAddr.t sexp_opaque;
        ipDst : NwAddr.t sexp_opaque;
        tcpSrcPort : TcpPort.t sexp_opaque;
        tcpDstPort : TcpPort.t sexp_opaque
      } with sexp, fields

  let compare x y =
    (* N.B. This is intentionally unrolled for performance purposes, as the
     * comparison should short circuit as soon as possible. In light of that
     * fact, it may be beneficial to reorder some of these checks in the
     * future.
     * *)
    let c = Location.compare x.location y.location in
    if c <> 0 then c else
    let c = DlAddr.compare x.ethSrc y.ethSrc in
    if c <> 0 then c else
    let c = DlAddr.compare x.ethDst y.ethDst in
    if c <> 0 then c else
    let c = Vlan.compare x.vlan y.vlan in
    if c <> 0 then c else
    let c = VlanPcp.compare x.vlanPcp y.vlanPcp in
    if c <> 0 then c else
    let c = DlType.compare x.ethType y.ethType in
    if c <> 0 then c else
    let c = NwProto.compare x.ipProto y.ipProto in
    if c <> 0 then c else
    let c = NwAddr.compare x.ipSrc y.ipSrc in
    if c <> 0 then c else
    let c = NwAddr.compare x.ipDst y.ipDst in
    if c <> 0 then c else
    let c = TcpPort.compare x.tcpSrcPort y.tcpSrcPort in
    if c <> 0 then c else
    let c = TcpPort.compare x.tcpDstPort y.tcpDstPort in
    c

  let to_string ?(init="") ?(sep="=") (x:t) : string =
    let g is_top to_string acc f =
      let v = Field.get f x in
      if is_top v then acc
      else
        Printf.sprintf "%s%s%s%s"
          (if acc = init then "" else acc ^ "; ")
          (Field.name f) sep (to_string (Field.get f x)) in
    Fields.fold
      ~init
      ~location:Location.(g is_top to_string)
      ~ethSrc:DlAddr.(g is_top to_string)
      ~ethDst:DlAddr.(g is_top to_string)
      ~vlan:Vlan.(g is_top to_string)
      ~vlanPcp:VlanPcp.(g is_top to_string)
      ~ethType:DlType.(g is_top to_string)
      ~ipProto:NwProto.(g is_top to_string)
      ~ipSrc:NwAddr.(g is_top to_string)
      ~ipDst:NwAddr.(g is_top to_string)
      ~tcpSrcPort:TcpPort.(g is_top to_string)
      ~tcpDstPort:TcpPort.(g is_top to_string)

  let is_top (x:t) : bool =
    let g is_top f = is_top (Field.get f x) in
    Fields.for_all
      ~location:Location.(g is_top)
      ~ethSrc:DlAddr.(g is_top)
      ~ethDst:DlAddr.(g is_top)
      ~vlan:Vlan.(g is_top)
      ~vlanPcp:VlanPcp.(g is_top)
      ~ethType:DlType.(g is_top)
      ~ipProto:NwProto.(g is_top)
      ~ipSrc:NwAddr.(g is_top)
      ~ipDst:NwAddr.(g is_top)
      ~tcpSrcPort:TcpPort.(g is_top)
      ~tcpDstPort:TcpPort.(g is_top)

end

module LocationHeader = struct
  type t = location with sexp
  let compare = Pervasives.compare
  let equal = (=)
  let to_string l =
    match l with
      | Physical n -> Printf.sprintf "%lu" n
      | Pipe x     -> Printf.sprintf "pipe(%s)" x
      | Query x    -> Printf.sprintf "query(%s)" x
  let top = Physical 0l
  let is_top l = false
  let lessthan l1 l2 = equal l1 l2
  let meet l1 l2 =
    if equal l1 l2 then Some l1 else None
  let join l1 l2 =
    if equal l1 l2 then Some l1 else None
end
module IntHeader = struct
  include Int
  let top = 0
  let is_top _ = false
  let lessthan l1 l2 = equal l1 l2
  let meet l1 l2 =
    if equal l1 l2 then Some l1 else None
  let join l1 l2 =
    if equal l1 l2 then Some l1 else None
end
module Int32Header = struct
  include Int32
  let top = 0l
  let is_top _ = false
  let lessthan l1 l2 = equal l1 l2
  let meet l1 l2 =
    if equal l1 l2 then Some l1 else None
  let join l1 l2 =
    if equal l1 l2 then Some l1 else None
end
module Int64Header = struct
  include Int64
  let top = 0L
  let is_top _ = false
  let to_string = Packet.string_of_mac
  let lessthan l1 l2 = equal l1 l2
  let meet l1 l2 =
    if equal l1 l2 then Some l1 else None
  let join l1 l2 =
    if equal l1 l2 then Some l1 else None
end
module Int32TupleHeader = struct
  (* Represents an (ip_address, mask) tuple. *)
  type t = Int32Header.t * Int32Header.t with sexp

  module Ip = SDN_Types.Pattern.Ip

  let equal x1 x2 = Ip.eq x1 x2

  let compare ((p,m):t) ((p',m'):t) : int =
    let c = Pervasives.compare p p' in
    if c <> 0 then c else Pervasives.compare m m'

  let top = Ip.match_all

  let is_top x1 = equal x1 top

  let meet (x1:t) (x2:t) : t option =
    Ip.intersect x1 x2

  let lessthan (x1:t) (x2:t) : bool =
    Ip.less_eq x1 x2

  (* Given two Ip.t's x1 and x2, attempt to combine them into a single Ip.t x
   * with the following property:
   *
   *   ∀y, subseteq y x <=> subseteq y x1 || subseteq y x2 || eq y x
   *
   * In other words, if combine x1 x2 exists, then it is an upper-bound on
   * x1 and x2 that bounds elements transitively through x1 and x2 and no
   * other elements, besides itself.
   *
   * Examples:
   *
   *   combine 0.0.0.0/32 0.0.0.1/32 = Some(0.0.0.0/31)
   *   combine 0.0.0.2/31 0.0.0.4/31 = Some(0.0.0.0/30)
   *   combine 0.0.0.2/31 0.0.0.3/32 = Some(0.0.0.2/31)
   *   combine 1.0.0.2/31 2.0.0.2/31 = None
   *)
  let join ((p1,m1) as x1:t) ((p2,m2) as x2:t) : t option =
    if equal x1 x2 then
      Some x1
    else if m1 = m2 then
      let x1', x2' = Int32.(p1, m1 - 1l), Int32.(p2, m2 - 1l) in
      if equal x1' x2' then Some x1' else None
    else if m1 = Int32.(m2 - 1l) && lessthan x2 x1 then
      Some(x1)
    else if m2 = Int32.(m1 - 1l) && lessthan x1 x2 then
      Some(x2)
    else
      None

  let to_string x = Ip.string_of x
end
