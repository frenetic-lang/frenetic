open Core.Std
open Sexplib.Conv
open SDN_Types

type location = NetKAT_Types.location
module Headers = NetKAT_Types.Headers

module type HEADERSCOMMON = sig
  type t = Headers.t with sexp
  module Set : Set.S with type Elt.t = t
  val to_string : ?init:string -> ?sep:string -> t -> string
  val set_to_string : ?init:string -> ?sep:string -> Set.t -> string
  val compare : t -> t -> int    
  val empty : t
  val mk_location : location -> t
  val mk_ethSrc : int48 -> t
  val mk_ethDst : int48 -> t
  val mk_vlan : int16 -> t
  val mk_vlanPcp: int8 -> t
  val mk_ethType : int16 -> t
  val mk_ipProto : int8 -> t
  val mk_ipSrc : int32 * int -> t
  val mk_ipDst : int32 * int -> t
  val mk_tcpSrcPort : int16 -> t
  val mk_tcpDstPort : int16 -> t
  val is_empty : t -> bool
  val seq : t -> t -> t option
  val diff : t -> t -> t
  val subseteq : t -> t -> bool
  val shadow : ?keep:bool -> t -> t -> t
end

module HeadersCommon : HEADERSCOMMON = struct

  type t = Headers.t with sexp

  let compare (x:t) (y:t) : int = Headers.compare x y
  let to_string ?init ?sep (x:t) = Headers.to_string ?init ?sep x

  type this_t = t with sexp

  module Set = Set.Make(struct
    type t = this_t with sexp
    let compare = compare
  end)

  let set_to_string ?init:(init="[]") ?sep:(sep="=") (s:Set.t) : string =
    Printf.sprintf "%s"
      (Set.fold s
         ~init:""
         ~f:(fun acc x ->
           Printf.sprintf "%s%s"
             (if acc = "" then "" else acc ^ ", ")
             (to_string ~init:init ~sep:sep x)))

  let empty : t = Headers.empty

  let is_empty (x:t) : bool =
    x = empty

  let mk_location l = Headers.mk_location l
  let mk_ethSrc n = Headers.mk_ethSrc n
  let mk_ethDst n = Headers.mk_ethDst n
  let mk_vlan n = Headers.mk_vlan n
  let mk_vlanPcp n = Headers.mk_vlanPcp n
  let mk_ethType n = Headers.mk_ethType n
  let mk_ipProto n = Headers.mk_ipProto n
  let mk_ipSrc n = Headers.mk_ipSrc n
  let mk_ipDst n = Headers.mk_ipDst n
  let mk_tcpSrcPort n = Headers.mk_tcpSrcPort n
  let mk_tcpDstPort n = Headers.mk_tcpDstPort n
       
  let rec subseteq (x:t) (y:t) : bool =
    let g f =
      match Field.get f x, Field.get f y with
        | None, Some _ -> false
        | Some _, None -> true
        | Some v1, Some v2 -> v1 = v2
        | None, None -> true in
    Headers.Fields.for_all
      ~location:g
      ~ethSrc:g
      ~ethDst:g
      ~vlan:g
      ~vlanPcp:g
      ~ethType:g
      ~ipProto:g
      ~ipSrc:g
      ~ipDst:g
      ~tcpSrcPort:g
      ~tcpDstPort:g

  let rec seq (x:t) (y:t) : t option =
    let g c acc f =
      match acc with
        | None -> 
          None
        | Some z ->
          (match Field.get f x, Field.get f y with
            | Some v1, Some v2 ->
              c f v1 v2 z
            | (Some v1 as o1), None -> 
              Some (Field.fset f z o1)
            | None, (Some v2 as o2) -> 
              Some (Field.fset f z o2)
            | None, None -> 
              Some z) in
    let c f v1 v2 z = 
      if v1 = v2 then Some (Field.fset f z (Some v1)) else None in 
    let cm f (v1,m1) (v2,m2) z = 
      let b = max 0 (max (32-m1) (32-m2)) in 
      if Int32.shift_right v1 b = Int32.shift_right v2 b then 
        Some (Field.fset f z (Some (v1,b))) 
      else
        None in 
    Headers.Fields.fold
      ~init:(Some empty)
      ~location:(g c)
      ~ethSrc:(g c)
      ~ethDst:(g c)
      ~vlan:(g c)
      ~vlanPcp:(g c)
      ~ethType:(g c)
      ~ipProto:(g c)
      ~ipSrc:(g cm)
      ~ipDst:(g cm)
      ~tcpSrcPort:(g c)
      ~tcpDstPort:(g c)

  let diff (x:t) (y:t) : t =
    let g c acc f =
      match Field.get f x, Field.get f y with
        | Some v1, Some v2 ->
          c f v1 v2 acc
        | _ -> 
          acc in
    let c f v1 v2 acc = 
      if v1 = v2 then Field.fset f acc None else acc in 
    let cm f (v1,m1) (v2,m2) acc = 
      let b = max 0 (max (32-m1) (32-m2)) in 
      if m2 >= m1 && Int32.shift_right v1 b = Int32.shift_right v2 b then 
        Field.fset f acc None 
      else 
        acc in 
    Headers.Fields.fold
      ~init:x
      ~location:(g c)
      ~ethSrc:(g c)
      ~ethDst:(g c)
      ~vlan:(g c)
      ~vlanPcp:(g c)
      ~ethType:(g c)
      ~ipProto:(g c)
      ~ipSrc:(g cm)
      ~ipDst:(g cm)
      ~tcpSrcPort:(g c)
      ~tcpDstPort:(g c)

  let shadow ?(keep=true) (x:t) (y:t) : t =
    let g acc f =
      match Field.get f acc, Field.get f y with
        | Some v1, Some v2 ->
          if v1 = v2
            then acc
            else Field.fset f acc (if keep then Some v2 else None)
        | _, _ -> acc in
    Headers.Fields.fold
      ~init:x
      ~location:g
      ~ethSrc:g
      ~ethDst:g
      ~vlan:g
      ~vlanPcp:g
      ~ethType:g
      ~ipProto:g
      ~ipSrc:g
      ~ipDst:g
      ~tcpSrcPort:g
      ~tcpDstPort:g
end

module type ACTION = sig
  type t = Headers.t
  module Set : Set.S with type Elt.t = t
  val to_string : t -> string
  val set_to_string : Set.t -> string
  val mk_location : location -> t
  val mk_ethSrc : int48 -> t
  val mk_ethDst : int48 -> t
  val mk_vlan : int16 -> t
  val mk_vlanPcp: int8 -> t
  val mk_ethType : int16 -> t
  val mk_ipProto : int8 -> t
  val mk_ipSrc : int32 * int -> t
  val mk_ipDst : int32 * int -> t
  val mk_tcpSrcPort : int16 -> t
  val mk_tcpDstPort : int16 -> t
  val seq : t -> t -> t
  val set_seq : t -> Set.t -> Set.t
  val diff : t -> t -> t
  val set_compare : Set.t -> Set.t -> int
  val id : Set.t
  val drop : Set.t
  val is_id : Set.t -> bool
  val is_drop : Set.t -> bool
  val to_netkat : t -> NetKAT_Types.policy
  val set_to_netkat : Set.t -> NetKAT_Types.policy
end

module Action : ACTION = struct

  type t = Headers.t with sexp

  type this_t = t with sexp

  module SetSet = Set.Make(HeadersCommon.Set)

  module Set = HeadersCommon.Set

  let compare = Headers.compare

  let set_compare = Set.compare

  let to_string : t -> string =
    HeadersCommon.to_string ~init:"id" ~sep:":="

  let set_to_string (s:Set.t) : string =
    if Set.is_empty s then "drop"
    else HeadersCommon.set_to_string ~init:"id" ~sep:":=" s

  let mk_location l = HeadersCommon.mk_location l
  let mk_ethSrc n = HeadersCommon.mk_ethSrc n
  let mk_ethDst n = HeadersCommon.mk_ethDst n
  let mk_vlan n = HeadersCommon.mk_vlan n
  let mk_vlanPcp n = HeadersCommon.mk_vlanPcp n
  let mk_ethType n = HeadersCommon.mk_ethType n
  let mk_ipProto n = HeadersCommon.mk_ipProto n
  let mk_ipSrc (n,m) = HeadersCommon.mk_ipSrc (n,m)
  let mk_ipDst (n,m) = HeadersCommon.mk_ipDst (n,m)
  let mk_tcpSrcPort n = HeadersCommon.mk_tcpSrcPort n
  let mk_tcpDstPort n = HeadersCommon.mk_tcpDstPort n

  let seq (x:t) (y:t) : t =
    let g acc f =
      match Field.get f y with
        | Some _ as o2 ->
          Field.fset f acc o2
        | _ -> acc in
    Headers.Fields.fold
      ~init:x
      ~location:g
      ~ethSrc:g
      ~ethDst:g
      ~vlan:g
      ~vlanPcp:g
      ~ethType:g
      ~ipProto:g
      ~ipSrc:g
      ~ipDst:g
      ~tcpSrcPort:g
      ~tcpDstPort:g

  let set_seq a s =
    Set.map s (seq a)

  let diff : t -> t -> t =
    HeadersCommon.diff

  let id : Set.t =
    Set.singleton (HeadersCommon.empty)

  let drop : Set.t =
    Set.empty

  let is_id (s:Set.t) : bool =
    Set.length s = 1 &&
    match Set.min_elt s with
      | None -> false
      | Some a -> HeadersCommon.is_empty a

  let is_drop (s:Set.t) : bool =
    Set.is_empty s

  let to_netkat (a:t) : NetKAT_Types.policy =
    let open NetKAT_Types in 
    let g h pol f =
      match Field.get f a with 
        | None -> 
          pol
        | Some v -> 
          let pol' = Mod (h v) in
          match pol with 
            | Filter True -> 
              pol'
            | _ -> 
              if Field.name f = "port" then
	        Seq (pol, pol')
              else
	        Seq (pol', pol) in
    Headers.Fields.fold
      ~init:(Filter True)
      ~location:(g (fun l -> Location l))
      ~ethSrc:(g (fun n -> EthSrc n))
      ~ethDst:(g (fun n -> EthDst n))
      ~vlan:(g (fun n -> Vlan n))
      ~vlanPcp:(g (fun n -> VlanPcp n))
      ~ethType:(g (fun n -> EthType n))
      ~ipProto:(g (fun n -> IPProto n))
      ~ipSrc:(g (fun (n,m) -> IP4Src (n,m)))
      ~ipDst:(g (fun (n,m) -> IP4Dst (n,m)))
      ~tcpSrcPort:(g (fun n -> TCPSrcPort n))
      ~tcpDstPort:(g (fun n -> TCPDstPort n))

  let set_to_netkat (s:Set.t) : NetKAT_Types.policy =
    if Set.is_empty s then
      NetKAT_Types.Filter NetKAT_Types.False
    else
      let f pol a = NetKAT_Types.Union (pol, to_netkat a) in
      let a = Set.min_elt_exn s in
      let s' = Set.remove s a in
      Set.fold s' ~f:f ~init:(to_netkat a)
end

module type PATTERN = sig
  type t = Headers.t with sexp
  module Set : Set.S with type Elt.t = t

  val matches : t -> NetKAT_Types.packet -> bool

  val to_string : t -> string
  val set_to_string : Set.t -> string
  val compare : t -> t -> int
  val mk_location  : location -> t
  val mk_ethSrc : int48 -> t
  val mk_ethDst : int48 -> t
  val mk_vlan : int16 -> t
  val mk_vlanPcp: int8 -> t
  val mk_ethType : int16 -> t
  val mk_ipProto : int8 -> t
  val mk_ipSrc : int32 * int -> t
  val mk_ipDst : int32 * int -> t
  val mk_tcpSrcPort : int16 -> t
  val mk_tcpDstPort : int16 -> t
  val seq : t -> t -> t option
  val seq_act : t -> Action.t -> t
  val seq_act_t : t -> Action.t -> t -> t option
  val diff : t -> t -> t
  val subseteq : t -> t -> bool
  val tru : t
  val to_netkat : t -> NetKAT_Types.pred
  val set_to_netkat : Set.t -> NetKAT_Types.pred
end

module Pattern : PATTERN = struct

  type t = Headers.t with sexp

  module Set = HeadersCommon.Set

  let to_string : t -> string =
    HeadersCommon.to_string ~init:"true" ~sep:"="

  let set_to_string (xs:Set.t) : string =
    Printf.sprintf "{%s}"
      (HeadersCommon.set_to_string ~init:"true" ~sep:"=" xs)

  let compare : t -> t -> int =
    HeadersCommon.compare

  let mk_location l = HeadersCommon.mk_location l
  let mk_ethSrc n = HeadersCommon.mk_ethSrc n
  let mk_ethDst n = HeadersCommon.mk_ethDst n
  let mk_vlan n = HeadersCommon.mk_vlan n
  let mk_vlanPcp n = HeadersCommon.mk_vlanPcp n
  let mk_ethType n = HeadersCommon.mk_ethType n
  let mk_ipProto n = HeadersCommon.mk_ipProto n
  let mk_ipSrc n = HeadersCommon.mk_ipSrc n
  let mk_ipDst n = HeadersCommon.mk_ipDst n
  let mk_tcpSrcPort n = HeadersCommon.mk_tcpSrcPort n
  let mk_tcpDstPort n = HeadersCommon.mk_tcpDstPort n

  let matches (t:t) (packet:NetKAT_Types.packet) : bool =
    let open NetKAT_Types in
    let fn acc f =
      acc && (match Field.get f t, Field.get f packet.headers with
        | None, _      -> true
        | Some _, None -> false
        | Some e1, Some e2 -> e1 = e2) in
    Headers.Fields.fold
      ~init:true
      ~location:(fun acc _ -> acc)
      ~ethSrc:fn
      ~ethDst:fn
      ~vlan:fn
      ~vlanPcp:fn
      ~ethType:fn
      ~ipProto:fn
      ~ipSrc:fn
      ~ipDst:fn
      ~tcpSrcPort:fn
      ~tcpDstPort:fn

  let seq : t -> t -> t option =
    HeadersCommon.seq

  (**
     Sequence a pattern with an Action.t, the effect of which is to modify the
     pattern to match any fields that the Action.t assigns. *)
  let seq_act (t:t) (act:Action.t) : t =
    HeadersCommon.shadow t act

  let seq_act_t x a y =
    (* TODO(jnf): can optimize into a single loop *)
    (* Printf.printf "  SEQ_ACT\n  X=%s\n  A=%s\n  Y=%s\n  " *)
    (*   (to_string x) *)
    (*   (Action.to_string a) *)
    (*   (to_string y); *)
    match HeadersCommon.seq a y with
      | None ->
        (* Printf.printf "Z=None\n"; *)
        None
      | Some z ->
        (* Printf.printf "Z=Some (%s)\n  " (to_string z); *)
        (* Printf.printf "D=%s\n  " (to_string (Fields.diff z a)); *)
        (* Printf.printf "R=%s\n" (match (Fields.seq x (Fields.diff z a)) with None -> "None" | Some r -> to_string r); *)
        HeadersCommon.seq x (HeadersCommon.diff z a)
      
  let diff : t -> t -> t =
    HeadersCommon.diff

  let subseteq : t -> t -> bool =
    HeadersCommon.subseteq
      
  let tru : t =
    HeadersCommon.empty

  let to_netkat (a:t) : NetKAT_Types.pred =
    let open NetKAT_Types in 
    let g h pol f =
      match Field.get f a with 
        | None -> 
          pol
        | Some v -> 
          let pol' = Test (h v) in
          match pol with 
            | True -> 
              pol'
            | _ -> 
              if Field.name f = "port" then
	        And (pol, pol')
              else
	        And (pol', pol) in
    Headers.Fields.fold
      ~init:True
      ~location:(g (fun l -> Location l))
      ~ethSrc:(g (fun n -> EthSrc n))
      ~ethDst:(g (fun n -> EthDst n))
      ~vlan:(g (fun n -> Vlan n))
      ~vlanPcp:(g (fun n -> VlanPcp n))
      ~ethType:(g (fun n -> EthType n))
      ~ipProto:(g (fun n -> IPProto n))
      ~ipSrc:(g (fun (n,m) -> IP4Src (n,32)))
      ~ipDst:(g (fun (n,m) -> IP4Dst (n,32)))
      ~tcpSrcPort:(g (fun n -> TCPSrcPort n))
      ~tcpDstPort:(g (fun n -> TCPDstPort n))

  let set_to_netkat (xs:Set.t) : NetKAT_Types.pred =
    match Set.choose xs with
      | None ->
        NetKAT_Types.False
      | Some x ->
        let xs' = Set.remove xs x in
        let f pol x = NetKAT_Types.Or(pol, to_netkat x) in
        Set.fold xs' ~init:(to_netkat x) ~f:f
end

module type ATOM = sig
  type t = Pattern.Set.t * Pattern.t
  module Set : Set.S with type Elt.t = t
  module DepMap : Map.S with type Key.t = t
  module Map : Map.S with type Key.t = t

  val matches : t -> NetKAT_Types.packet -> bool

  val to_string : t -> string
  val compare : t -> t -> int
  val mk : Pattern.t -> t
  val tru : t
  val neg : t -> Set.t
  val seq : t -> t -> t option
  val seq_act : t -> Action.t -> t
  val seq_act_t : t -> Action.t -> t -> t option
end

module Atom : ATOM = struct

  type t = (Pattern.Set.t * Pattern.t) sexp_opaque with sexp

  let compare ((xs1,x1):t) ((xs2,x2):t) : int =
    let cmp = Pattern.Set.compare xs1 xs2 in
    if cmp <> 0 then cmp
    else Pattern.compare x1 x2

  let shadows (xs1,x1) (xs2,x2) =
    let ys =
      Pattern.Set.fold xs1 ~init:Pattern.Set.empty
        ~f:(fun acc xi ->
          match Pattern.seq x1 xi with
            | None -> acc
            | Some x1_xi -> Pattern.Set.add acc x1_xi) in
    Pattern.Set.mem ys x2

  let dep_compare ((xs1,x1) as r1) ((xs2,x2) as r2) =
    let r =
      if shadows r2 r1 then
        -1
      else if shadows r1 r2 then
        1
      else
        let cmp = Pattern.Set.compare xs1 xs2 in
        if cmp = 0 then
          Pattern.compare x1 x2
        else
          cmp in
    (* Printf.printf "COMPARE %s %s = %d\n%!" (to_string (xs1,x1)) (to_string (xs2,x2)) r; *)
    r

  let matches (t:t) (packet:NetKAT_Types.packet) : bool =
    let ps_neg, p_pos = t in
    Pattern.Set.for_all ps_neg ~f:(fun p -> not (Pattern.matches p packet))
      && Pattern.matches p_pos packet

  type this_t = t with sexp

  module Set = Set.Make(struct
    type t = this_t with sexp
    let compare = compare
  end)

  module DepMap = Map.Make(struct
    type t = this_t with sexp
    let compare = dep_compare
  end)

  module Map = Map.Make(struct
    type t = this_t with sexp
    let compare = compare
  end)

  let to_string ((xs,x):t) : string =
    Printf.sprintf "%s,%s"
      (Pattern.set_to_string xs)
      (Pattern.to_string x)

  let mk (x:Pattern.t) : t =
    (Pattern.Set.empty, x)

  let tru : t =
    mk Pattern.tru

  let check ((xs,x):t) : t option =
    if Pattern.Set.exists xs (fun xi -> Pattern.subseteq x xi) then
      None
    else
      let xs' =
        Pattern.Set.filter xs
          ~f:(fun xi -> Pattern.seq x xi <> None) in
      Some (xs',x)
    
  let seq ((xs1,x1):t) ((xs2,x2):t) : t option =
    match Pattern.seq x1 x2 with
      | Some x12 ->
        check (Pattern.Set.union xs1 xs2, x12)
      | None ->
        None

  let seq_act (xs1, x1) act =
    let xs1' = Pattern.Set.map xs1 ~f:(fun x -> HeadersCommon.shadow x act) in
    (xs1', HeadersCommon.shadow x1 act)

  let seq_act_t (xs1,x1) a (xs2,x2) =
    match Pattern.seq_act_t x1 a x2 with
      | None ->
        None
      | Some x1ax2 ->
        let xs =
          Pattern.Set.fold xs2
            ~init:xs1
            ~f:(fun acc xs2i ->
              match Pattern.seq_act_t Pattern.tru a xs2i with
                | Some truaxs2i ->
                  Pattern.Set.add acc truaxs2i
                | None ->
                  acc) in
        check (xs, x1ax2)

  let neg (xs,x) : Set.t =
    let init =
      match check (Pattern.Set.singleton x, Pattern.tru) with
        | None -> Set.empty
        | Some r -> Set.singleton r in
    Pattern.Set.fold xs
      ~init:init
      ~f:(fun acc xi -> Set.add acc (mk xi))
end

module type OPTIMIZE = sig
  open NetKAT_Types
  val mk_and : pred -> pred -> pred
  val mk_or : pred -> pred -> pred
  val mk_not : pred -> pred
  val mk_filter : pred -> policy
  val mk_seq : policy -> policy -> policy
  val mk_par : policy -> policy -> policy
  val mk_star : policy -> policy
  val specialize_pred : switchId -> pred -> pred
  val specialize_policy : switchId -> policy -> policy
end

module Optimize : OPTIMIZE = struct
  let mk_and pr1 pr2 =
    match pr1, pr2 with
      | NetKAT_Types.True, _ ->
        pr2
      | _, NetKAT_Types.True ->
        pr1
      | NetKAT_Types.False, _ ->
        NetKAT_Types.False
      | _, NetKAT_Types.False ->
        NetKAT_Types.False
      | _ ->
        NetKAT_Types.And(pr1, pr2)

  let mk_or pr1 pr2 =
    match pr1, pr2 with
      | NetKAT_Types.True, _ ->
        NetKAT_Types.True
      | _, NetKAT_Types.True ->
        NetKAT_Types.True
      | NetKAT_Types.False, _ ->
        pr2
      | _, NetKAT_Types.False ->
        pr2
      | _ ->
        NetKAT_Types.Or(pr1, pr2)

  let mk_not pat =
    match pat with
      | NetKAT_Types.False -> NetKAT_Types.True
      | NetKAT_Types.True -> NetKAT_Types.False
      | _ -> NetKAT_Types.Neg(pat)

  let mk_filter pr =
    NetKAT_Types.Filter (pr)

  let mk_par pol1 pol2 =
    match pol1, pol2 with
      | NetKAT_Types.Filter NetKAT_Types.False, _ ->
        pol2
      | _, NetKAT_Types.Filter NetKAT_Types.False ->
        pol1
      | _ ->
        NetKAT_Types.Union(pol1,pol2)

  let mk_seq pol1 pol2 =
    match pol1, pol2 with
      | NetKAT_Types.Filter NetKAT_Types.True, _ ->
        pol2
      | _, NetKAT_Types.Filter NetKAT_Types.True ->
        pol1
      | NetKAT_Types.Filter NetKAT_Types.False, _ ->
        pol1
      | _, NetKAT_Types.Filter NetKAT_Types.False ->
        pol2
      | _ ->
        NetKAT_Types.Seq(pol1,pol2)

  let mk_star pol =
    match pol with
      | NetKAT_Types.Filter NetKAT_Types.True ->
        pol
      | NetKAT_Types.Filter NetKAT_Types.False ->
        NetKAT_Types.Filter NetKAT_Types.True
      | NetKAT_Types.Star(pol1) -> pol
      | _ -> NetKAT_Types.Star(pol)
  
  let specialize_pred sw pr =
    let rec loop pr k =
      match pr with
        | NetKAT_Types.True ->
          k pr
        | NetKAT_Types.False ->
          k pr
        | NetKAT_Types.Neg pr1 ->
          loop pr1 (fun pr -> k (mk_not pr))
        | NetKAT_Types.Test (NetKAT_Types.Switch v) -> 
          if v = sw then
            k NetKAT_Types.True
          else
            k NetKAT_Types.False
        | NetKAT_Types.Test _ -> 
          k pr
        | NetKAT_Types.And (pr1, pr2) ->
          loop pr1 (fun p1 -> loop pr2 (fun p2 -> k (mk_and p1 p2)))
        | NetKAT_Types.Or (pr1, pr2) ->
          loop pr1 (fun p1 -> loop pr2 (fun p2 -> k (mk_or p1 p2))) in
    loop pr (fun x -> x)

  let specialize_policy sw pol =
    let rec loop pol k =
      match pol with
        | NetKAT_Types.Filter pr ->
          k (NetKAT_Types.Filter (specialize_pred sw pr))
        | NetKAT_Types.Mod hv -> 
          k pol
        | NetKAT_Types.Union (pol1, pol2) ->
          loop pol1 (fun p1 -> loop pol2 (fun p2 -> k (mk_par p1 p2)))
        | NetKAT_Types.Seq (pol1, pol2) ->
          loop pol1 (fun p1 -> loop pol2 (fun p2 -> k (mk_seq p1 p2)))
        | NetKAT_Types.Star pol ->
          loop pol (fun p -> k (mk_star p))
        | NetKAT_Types.Link(sw,pt,sw',pt') ->
	  failwith "Not a local policy" in
    loop pol (fun x -> x)
end

module type LOCAL = sig
  open NetKAT_Types

  type t = Action.Set.t Atom.Map.t

  val to_string : t -> string
  val of_pred : pred -> t
  val of_policy : policy -> t
  val to_netkat : t -> policy

  val from_pipes : t -> packet -> (string * packet) list
end

module Local : LOCAL = struct

  type t = Action.Set.t Atom.Map.t

  let compare p q =
    Atom.Map.compare Action.Set.compare p q
      
  let to_string (m:t) : string =
    Printf.sprintf "%s"
      (Atom.Map.fold m
         ~init:""
         ~f:(fun ~key:r ~data:g acc ->
             Printf.sprintf "%s(%s) => %s\n"
               acc
               (Atom.to_string r)
               (Action.set_to_string g)))

  let extend (r:Atom.t) (s:Action.Set.t) (m:t) : t =
    if Atom.Map.mem m r then
      failwith "Local.extend: overlap"
    else
      Atom.Map.add m r s

  let intersect (op:Action.Set.t -> Action.Set.t -> Action.Set.t) (p:t) (q:t) : t =
    Atom.Map.fold p
      ~init:Atom.Map.empty
      ~f:(fun ~key:r1 ~data:s1 acc ->
        Atom.Map.fold q
          ~init:acc
          ~f:(fun ~key:r2 ~data:s2 acc ->
            match Atom.seq r1 r2 with
              | None ->
                acc
              | Some r1_r2 ->
                extend r1_r2 (op s1 s2) acc))
    
  let par p q =
    let r = intersect Action.Set.union p q in
    (* debug "### PAR ###\n%s\n%s\n%s" *)
    (*   (to_string p) *)
    (*   (to_string q) *)
    (*   (to_string r); *)
    r
      
  let seq (p:t) (q:t) : t =
    let merge ~key:_ v =
      match v with
        | `Left s1 -> Some s1
        | `Right s2 -> Some s2
        | `Both (s1,s2) -> Some (Action.Set.union s1 s2) in

    let seq_act_t r1 a q =
      Atom.Map.fold q
        ~init:Atom.Map.empty
        ~f:(fun ~key:r2 ~data:s2 acc ->
          match Atom.seq_act_t r1 a r2 with
            | None ->
              acc
            | Some r12 ->
              extend r12 (Action.set_seq a s2) acc) in
    
    let seq_atom_acts_local r1 s1 q =
      if Action.Set.is_empty s1 then
        Atom.Map.singleton r1 s1
      else
        Action.Set.fold s1
          ~init:Atom.Map.empty
          ~f:(fun acc a ->
            let acc' = seq_act_t r1 a q in
            Atom.Map.merge ~f:merge acc acc') in
    
    let r =
      Atom.Map.fold p
        ~init:Atom.Map.empty
        ~f:(fun ~key:r1 ~data:s1 acc ->
          let acc' = seq_atom_acts_local r1 s1 q in
          Atom.Map.merge ~f:merge acc acc') in
    (* debug "### SEQ ###\n%s\n%s\n%s" *)
    (*   (to_string p) *)
    (*   (to_string q) *)
    (*   (to_string r); *)
    r
        
  let neg (p:t) : t=
    let r =
      Atom.Map.map p
        ~f:(fun s ->
          if Action.is_drop s then Action.id
          else if Action.is_id s then Action.drop
          else failwith "neg: not a predicate") in
    (* debug "### NEGATE ###\n%s\n%s" *)
    (*   (to_string p) *)
    (*   (to_string r); *)
    r

  let star p =
    let rec loop acc pi =
      let psucci = seq p pi in
      let acc' = par acc psucci in
      if compare acc acc' = 0 then
        acc
      else
        loop acc' psucci in
    let p0 = Atom.Map.singleton Atom.tru Action.id in
    let r = loop p0 p0 in
    (* debug "### STAR ###\n%s\n%s" *)
    (*   (to_string p) *)
    (*   (to_string r); *)
    r

  let rec of_pred (pr:NetKAT_Types.pred) : t =
    let rec loop pr k =
      match pr with
      | NetKAT_Types.True ->
        k (Atom.Map.singleton Atom.tru Action.id)
      | NetKAT_Types.False ->
        k (Atom.Map.singleton Atom.tru Action.drop)
      | NetKAT_Types.Neg pr ->
        loop pr (fun (p:t) -> k (neg p))
      | NetKAT_Types.Test hv -> 
        let x = match hv with 
          | NetKAT_Types.Switch n -> 
            failwith "Not a local policy"
          | NetKAT_Types.Location l ->  
            Pattern.mk_location l
          | NetKAT_Types.EthType n -> 
            Pattern.mk_ethType n
          | NetKAT_Types.EthSrc n -> 
            Pattern.mk_ethSrc n
          | NetKAT_Types.EthDst n -> 
            Pattern.mk_ethDst n
          | NetKAT_Types.Vlan n -> 
            Pattern.mk_vlan n
          | NetKAT_Types.VlanPcp n -> 
            Pattern.mk_vlanPcp n
          | NetKAT_Types.IPProto n -> 
            Pattern.mk_ipProto n
          | NetKAT_Types.IP4Src (n,m) -> 
            Pattern.mk_ipSrc (n,m)
          | NetKAT_Types.IP4Dst (n,m) -> 
            Pattern.mk_ipDst (n,m)
          | NetKAT_Types.TCPSrcPort n -> 
            Pattern.mk_tcpSrcPort n
          | NetKAT_Types.TCPDstPort n -> 
            Pattern.mk_tcpDstPort n in 
        let r = Atom.mk x in 
        let m =
          Atom.Set.fold (Atom.neg r)
            ~init:(Atom.Map.singleton r Action.id)
            ~f:(fun acc r -> extend r Action.drop acc) in
        k m
      | NetKAT_Types.And (pr1, pr2) ->
        loop pr1 (fun p1 -> loop pr2 (fun p2 -> k (seq p1 p2)))
      | NetKAT_Types.Or (pr1, pr2) ->
        loop pr1 (fun p1 -> loop pr2 (fun p2 -> k (par p1 p2))) in
    loop pr (fun x -> x)

  let of_policy (pol:NetKAT_Types.policy) : t =
    let rec loop pol k =
      match pol with
        | NetKAT_Types.Filter pr ->
          k (of_pred pr)
        | NetKAT_Types.Mod hv -> 
        let a = match hv with 
          | NetKAT_Types.Switch n -> 
            failwith "Not a local policy"
          | NetKAT_Types.Location l ->  
            Action.mk_location l
          | NetKAT_Types.EthType n -> 
            Action.mk_ethType n
          | NetKAT_Types.EthSrc n -> 
            Action.mk_ethSrc n
          | NetKAT_Types.EthDst n -> 
            Action.mk_ethDst n
          | NetKAT_Types.Vlan n -> 
            Action.mk_vlan n
          | NetKAT_Types.VlanPcp n -> 
            Action.mk_vlanPcp n
          | NetKAT_Types.IPProto n -> 
            Action.mk_ipProto n
          | NetKAT_Types.IP4Src (n,m) -> 
            Action.mk_ipSrc (n,m)
          | NetKAT_Types.IP4Dst (n,m) -> 
            Action.mk_ipDst (n,m)
          | NetKAT_Types.TCPSrcPort n -> 
            Action.mk_tcpSrcPort n
          | NetKAT_Types.TCPDstPort n -> 
            Action.mk_tcpDstPort n in 
          let s = Action.Set.singleton a in
          let m = Atom.Map.singleton Atom.tru s in
          k m
        | NetKAT_Types.Union (pol1, pol2) ->
          loop pol1 (fun p1 -> loop pol2 (fun p2 -> k (par p1 p2)))
        | NetKAT_Types.Seq (pol1, pol2) ->
          loop pol1 (fun p1 -> loop pol2 (fun p2 -> k (seq p1 p2)))
        | NetKAT_Types.Star pol ->
          loop pol (fun p -> k (star p))
        | NetKAT_Types.Link(sw,pt,sw',pt') ->
	  failwith "Not a local policy" in
    loop pol (fun p -> p)

  let to_netkat (m:t) : NetKAT_Types.policy =
    let open Optimize in
    let rec loop m =
    match Atom.Map.min_elt m with
      | None ->
        NetKAT_Types.Filter NetKAT_Types.False
      | Some (r,s) ->
        let m' = Atom.Map.remove m r in
        let (xs,x) = r in
        let nc_pred = mk_and (mk_not (Pattern.set_to_netkat xs)) (Pattern.to_netkat x) in
        let nc_pred_acts = mk_seq (NetKAT_Types.Filter nc_pred) (Action.set_to_netkat s) in
        mk_par nc_pred_acts  (loop m') in
    loop m

  let from_pipes (t:t) (packet:NetKAT_Types.packet) : (string * NetKAT_Types.packet) list =
    let open NetKAT_Types in
    (* Determines the pipes that the packet belongs to. Note that a packet may
     * belong to several pipes for several reasons:
     *
     *   1. Multiple points of a single application wish to inspect the packet;
     *   2. Multiple applications wish to inspect the packet; and
     *   3. The switch makes modifications to the packet before it is sent to
     *      the controller, making it impossible to disambiguate the pipe it
     *      came from, e.g.:
     *
     *        (filter ethDst = 2; ethDst := 3; controller pipe1)
     *        | (filter ethDst = 3; controller pipe2)
     *
     * The return value may contain duplicate pipe names.
     *
     * Since Local.t is switch-specific, this function assumes but does not
     * check that the packet came from the same switch as the given Local.t *)
    let packets = Semantics.eval packet (to_netkat t) in
    PacketSet.fold packets ~init:[] ~f:(fun acc pkt ->
      (* Running the packet through the switch's policy will label the resultant
       * packets with the pipe they belong to, if any. All that's left to do is
       * pick out packets in the PacketSet.t that have a pipe location, and return
       * those packets (with the location cleared) along with the pipe they belong
       * to. *)
      match pkt.headers.Headers.location with
        | Some(Pipe p) -> (p, pkt) :: acc
        | _ -> acc)
end

module RunTime = struct

  (* XXX(seliopou): the int32 should be a portId, once portId is no longer a
   * VInt.t *)
  let to_action (a:Action.t) (pto: int32 option) : seq =
    let i8 x = VInt.Int8 x in 
    let i16 x = VInt.Int16 x in 
    let i32m (x,y) = VInt.Int32 x in (* JNF *)
    let i48 x = VInt.Int64 x in 
    (* If an action sets the location to a pipe, ignore all other modifications.
     * They will be applied at the controller by Semantics.eval. Otherwise, the
     * port must be determined either by the pattern or by the action. The pto
     * is the port determined by the pattern, if it exists. If the port is not
     * determinate, we fail though it is technically acceptable to send it out
     * InPort... if SDN_Types exposed that.
     * *)
    match Headers.location a, pto with
      | Some (NetKAT_Types.Pipe(_)), _ ->
        [Controller 128]
      | Some (NetKAT_Types.Physical pt), _
      | None, Some pt ->
        let g h c act f =
          match Field.get f a with
            | None -> act
            | Some v -> SetField(h,c v)::act in
        Headers.Fields.fold
          ~init:[OutputPort (VInt.Int32 pt)]
          ~location:(fun act _ -> act)
          ~ethSrc:(g EthSrc i48)
          ~ethDst:(g EthDst i48)
          ~vlan:(g Vlan i16)
          ~vlanPcp:(g VlanPcp i8)
          ~ethType:(g EthType i16)
          ~ipProto:(g IPProto i8)
          ~ipSrc:(g IP4Src i32m)
          ~ipDst:(g IP4Src i32m)
          ~tcpSrcPort:(g TCPSrcPort i16)
          ~tcpDstPort:(g TCPDstPort i16)
      | None, None ->
        failwith "indeterminate location"

  let set_to_action (s:Action.Set.t) (pto : int32 option) : par =
    let f par a = (to_action a pto)::par in
    Action.Set.fold s ~f:f ~init:[]

  let to_pattern (x:Pattern.t) : pattern =
    let i8 x = VInt.Int8 x in 
    let i16 x = VInt.Int16 x in 
    let il x = match x with
        | NetKAT_Types.Physical pt -> VInt.Int32 pt
        | NetKAT_Types.Pipe _ ->
          (* This is impossble, given the check in to_table *)
          failwith "to_pattern: OpenFlow can't match on pipes" in
    let i32m (x,y) = VInt.Int32 x in (* JNF *)
    let i48 x = VInt.Int64 x in 
    let g h c act f = 
      match Field.get f x with 
        | None -> act
        | Some v -> FieldMap.add h (c v) act in 
    Headers.Fields.fold
      ~init:FieldMap.empty
      ~location:(g InPort il)
      ~ethSrc:(g EthSrc i48)
      ~ethDst:(g EthDst i48)
      ~vlan:(g Vlan i16)
      ~vlanPcp:(g VlanPcp i8)
      ~ethType:(g EthType i16)
      ~ipProto:(g IPProto i8)
      ~ipSrc:(g IP4Src i32m)
      ~ipDst:(g IP4Src i32m)
      ~tcpSrcPort:(g TCPSrcPort i16)
      ~tcpDstPort:(g TCPDstPort i16)

  type i = Local.t

  let compile (sw:switchId) (pol:NetKAT_Types.policy) : i =
    let pol' = Optimize.specialize_policy sw pol in
    let n,n' = Semantics.size pol, Semantics.size pol' in
    Printf.printf " [compression: %d -> %d = %.3f] "
      n n' (Float.of_int n' /. Float.of_int n);
    Local.of_policy pol'

  let decompile (p:i) : NetKAT_Types.policy =
    Local.to_netkat p

  let simpl_flow (p : pattern) (a : par) : flow =
    { pattern = p;
      action = [a];
      cookie = 0L;
      idle_timeout = Permanent;
      hard_timeout = Permanent }

  (* Prunes out rules that apply to other switches. *)
  let to_table (m:i) : flowTable =
    let dm =
      Atom.Map.fold m
        ~init:Atom.DepMap.empty
        ~f:(fun ~key:r ~data:s acc -> Atom.DepMap.add acc r s) in
    let add_flow x s l =
      match Headers.location x with
        | Some (NetKAT_Types.Pipe p) -> l
        | _ ->
          let pat = to_pattern x in
          let pto = match Headers.location x with
            | Some (NetKAT_Types.Physical p) -> Some p
            | _ -> None in
          let act = set_to_action s pto in
          simpl_flow pat act::l in
    let rec loop dm acc cover =
      match Atom.DepMap.min_elt dm with
        | None ->
          acc
        | Some (r,s) ->
          let (xs,x) = r in
          let dm' = Atom.DepMap.remove dm r in
          let ys =
            Pattern.Set.fold
              xs ~init:Pattern.Set.empty
              ~f:(fun acc xi ->
                match Pattern.seq xi x with
                  | None -> acc
                  | Some xi_x -> Pattern.Set.add acc xi_x) in
          let zs =
            Pattern.Set.fold ys
              ~init:Pattern.Set.empty
              ~f:(fun acc yi ->
                if Pattern.Set.exists cover ~f:(Pattern.subseteq yi) then
                  acc
                else
                  Pattern.Set.add acc yi) in
          let acc' =
            Pattern.Set.fold zs
              ~init:acc
              ~f:(fun acc x -> add_flow x Action.drop acc) in
          let acc'' = add_flow x s acc' in
          let cover' = Pattern.Set.add (Pattern.Set.union zs cover) x in
          loop dm' acc'' cover' in
    List.rev (loop dm [] Pattern.Set.empty)

end

(* exports *)
type t = RunTime.i

let of_policy sw pol =
  Local.of_policy (Optimize.specialize_policy sw pol)

let to_netkat =
  Local.to_netkat

let compile =
  RunTime.compile

let decompile =
  RunTime.decompile

let to_table =
  RunTime.to_table

let from_pipes =
  Local.from_pipes
