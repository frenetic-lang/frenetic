open Core.Std
open Sexplib.Conv
open Frenetic_Decide_Util
open Tdk
open Frenetic_Decide_Ast

exception Empty

module PointSet = struct
  include Set.Make (struct
    type t = point [@@deriving sexp, compare]
    end)
  let to_string pts = Printf.sprintf "{%s}" (String.concat ~sep:"," (fold pts ~init:[] ~f:(fun acc pt -> point_to_string pt :: acc)))
end


module type DerivTerm = sig
  type t [@@deriving sexp]

  module EMatrix : sig
    type t [@@deriving sexp]
    val fold : t -> init:'a -> f:('a -> point -> 'a) -> 'a
    val run : t -> point -> bool
    val compare : t -> t -> int
    val empty : t
    val intersection_empty : t -> t -> bool
    val union : t -> t -> t
  end

  module DMatrix : sig
    type t [@@deriving sexp]
    val run : t -> point -> TermSet.t
    val compare : t -> t -> int
    val equivalent : (TermSet.t -> TermSet.t -> bool) -> t -> t -> bool
    val points : t -> EMatrix.t
  end

  val make_term : TermSet.t -> t
  val get_termset : t -> TermSet.t
  (* val to_term : t -> Frenetic_Decide_Ast.Term.t *)
  val get_e : t -> EMatrix.t
  val get_d : t -> DMatrix.t
  val sexp_of_t : t -> Sexplib.Sexp.t
  val compare : t -> t -> int
  val to_string : t -> string
end

module rec BDDDeriv : DerivTerm = struct

  open S

  module PartialPacketSet = struct
    include PacketSet
    (* type t = PacketSet.t [@@deriving sexp] *)
    let pktHash pkt = FieldMap.fold pkt ~init:0 ~f:(fun ~key:k ~data:v acc -> acc * 17 + (Field.hash k) * 13 + (Value.hash v) * 11)
    let hash pkts = PacketSet.fold pkts ~init:0 ~f:(fun acc x -> acc * 13 + pktHash x)
    (* pkt <= pkt' *)
    let partialPacketCompare pkt1 pkt2 = try (FieldMap.fold pkt1 ~init:true ~f:(fun ~key:k ~data:v acc ->
        acc && FieldMap.find_exn pkt2 k = v))
      with Not_found -> false

    let packetJoin pkt1 pkt2 = try Some (FieldMap.merge pkt1 pkt2 ~f:(fun ~key:k v ->
        match v with
        | `Right v -> Some v
        | `Left v -> Some v
        | `Both (_,v) -> raise Not_found))
      with Not_found -> None


    (* let compare p1 p2 = *)
    (*   let pointwiseCompare p p' = PacketSet.for_all p (fun pkt -> PacketSet.exists p' (partialPacketCompare pkt)) in *)
    (*   match pointwiseCompare p1 p2 with *)
    (*   | true -> begin match pointwiseCompare p2 p1 with *)
    (*       | true -> 0 *)
    (*       | false -> -1 *)
    (*     end *)
    (*   | false -> begin match pointwiseCompare p2 p1 with *)
    (*       | true -> 1 *)
    (*       | false -> -1 *)
    (*     end *)
    let compare = PacketSet.compare
    let contains p pkt = PacketSet.exists p (partialPacketCompare pkt)
    (* We could simplify (i.e. {*} U {pkt} -> {*}), but KISS for now *)
    let sum = PacketSet.union
    let prod p1 p2 = PacketSet.fold p1 ~init:PacketSet.empty ~f:(fun acc x ->
        PacketSet.union acc (PacketSet.filter_map p2 (packetJoin x)))
    let zero = PacketSet.empty
    let one = PacketSet.singleton FieldMap.empty
    let union = sum

    let to_string t = Sexplib.Sexp.to_string (sexp_of_t t)
  end

  module PacketDD = Tdk.Vcr.Make(Field)(Value)(PartialPacketSet)

  module EMatrix = struct
    open Term
    open Hashcons

    type t = PacketDD.t

    let cond v t f =
      if PacketDD.equal t f then
        t
      else
        PacketDD.(sum (prod (atom v PartialPacketSet.one PartialPacketSet.zero) t)
                    (prod (atom v PartialPacketSet.zero PartialPacketSet.one) f))

    let mask (t : t) (f,v) = PacketDD.fold (fun r -> PacketDD.const (PartialPacketSet.map r (fun pkt -> match FieldMap.find pkt f with
        | Some v' -> if v = v' then FieldMap.remove pkt f else pkt
        | None -> pkt)))
        (fun v t f -> cond v t f) t
    (* Because (h=v;h<-v) == h=v, we don't have canonical
       representation in DD's. This function canonicalizes by removing
       shadowed modifications
    *)
    let reduce t = t (* PacketDD.fold (fun r -> PacketDD.const r) *)
        (* (fun v t f -> cond v (mask t v) f) t *)

    let rec t_of_sexp sexp = let open Sexplib in
      match sexp with
      | Sexp.List ss -> begin match List.length ss with
          | 3 -> cond (pair_of_sexp Field.t_of_sexp Value.t_of_sexp (List.nth_exn ss 0))
                   (t_of_sexp (List.nth_exn ss 1))
                   (t_of_sexp (List.nth_exn ss 2))
          | 2 -> PacketDD.const (PartialPacketSet.t_of_sexp (List.nth_exn ss 1))
          | _ -> of_sexp_error "Neither a leaf nor a branch node" sexp
        end
      | Sexp.Atom _ -> failwith "This can't happen"

    let sexp_of_t = let open Sexplib in
      PacketDD.fold (fun r -> Sexp.List [Sexp.Atom "leaf"; PartialPacketSet.sexp_of_t r])
        (fun v t f ->
           Sexp.List [sexp_of_pair Field.sexp_of_t Value.sexp_of_t v;
                      t;
                      f])

    let seq_pkt pkt1 pkt2 = FieldMap.merge pkt1 pkt2 ~f:(fun ~key:k v ->
        match v with
        | `Right v -> Some v
        | `Left v -> Some v
        | `Both (_,v) -> Some v)

    let run t (pkt1,pkt2) =
      match PacketDD.peek (PacketDD.restrict (FieldMap.to_alist pkt1) t) with
      | Some p -> PartialPacketSet.contains (PartialPacketSet.map p (seq_pkt pkt1)) pkt2
      | None -> failwith "Frenetic_Decide_Deriv.BDDDeriv.EMatrix.run failed to get a value from the DD on the pkt"

    let empty = PacketDD.const PartialPacketSet.zero

    let one = PacketDD.const PartialPacketSet.one
    let zero = PacketDD.const PartialPacketSet.zero

    let times e1 e2 =
      reduce (PacketDD.fold
                (fun par ->
                   PartialPacketSet.fold par ~init:zero ~f:(fun acc pkt ->
                       let e2' = PacketDD.restrict FieldMap.(to_alist pkt) e2 in
                       PacketDD.(sum (PacketDD.map_r (fun pkts -> PartialPacketSet.map pkts (seq_pkt pkt)) e2') acc)))
                (fun v t f -> cond v t f)
                e1)

    let plus e1 e2 = reduce (PacketDD.sum e1 e2)

    let star e =
      let rec loop acc =
        let acc' = plus one (times e acc) in
        if PacketDD.equal acc acc'
        then acc
        else loop acc'
      in
      reduce (loop e)

    let rec matrix_of_term t =
      let result = match t.node with
        | Plus ts -> TermSet.fold ts ~init:zero ~f:(fun acc v -> plus acc (matrix_of_term v))
        | Dup -> empty
        | Times ts -> List.fold ts ~init:one ~f:(fun acc x -> times acc (matrix_of_term x))
        | Star t -> star (matrix_of_term t)
        | Assg (f,v) -> PacketDD.const (PartialPacketSet.singleton (FieldMap.add FieldMap.empty ~key:f ~data:v))
        | Test (f,v) -> PacketDD.atom (f, v) PartialPacketSet.one PartialPacketSet.zero
        (* Because t should *always* be a predicate, the leaves should only contain either an empty set, or {*} *)
        | Not t -> PacketDD.map_r (fun p -> match PartialPacketSet.equal PartialPacketSet.one p with
            | true -> PartialPacketSet.zero
            | false -> assert (PartialPacketSet.is_empty p); PartialPacketSet.one) (matrix_of_term t)
        | Zero -> zero
        | One -> one in
      reduce result

    let union e1 e2 = reduce (PacketDD.sum e1 e2)
    let intersection e1 e2 = reduce (PacketDD.prod e1 e2)

    let get_points t : PointSet.t =
      let base_points =
        PacketDD.fold
          (fun r -> PartialPacketSet.fold r ~f:(fun acc pkt -> PointSet.add acc (FieldMap.empty, pkt)) ~init:PointSet.empty)
          (fun (h,v) t f ->
             let extra_pkt = FieldMap.add FieldMap.empty ~key:h ~data:Value.extra_val in
             PointSet.union (PointSet.map t (fun (pkt1,pkt2) -> FieldMap.add pkt1 ~key:h ~data:v, pkt2))
               f)
          t
      in
      PointSet.fold base_points ~f:(fun acc pt -> PointSet.union acc (Frenetic_Decide_Util.FieldSet.fold (fun field pts ->
          PointSet.fold pts ~f:(fun acc (a,b) -> PointSet.union acc
          begin
            match FieldMap.find a field, FieldMap.find b field with
            | Some _, Some _ -> PointSet.singleton (a,b)
            | Some x, None -> PointSet.singleton (a, FieldMap.add b ~key:field ~data:x)
            | None, Some _ -> Frenetic_Decide_Util.ValueSet.fold (fun v acc -> PointSet.add acc (FieldMap.add a ~key:field ~data:v, b)) (!Frenetic_Decide_Util.all_values () field) PointSet.empty
            | None, None -> Frenetic_Decide_Util.ValueSet.fold (fun v acc -> PointSet.add acc (FieldMap.add a ~key:field ~data:v, FieldMap.add b ~key:field ~data:v))
                              (!Frenetic_Decide_Util.all_values () field) PointSet.empty
          end) ~init:PointSet.empty) (!Frenetic_Decide_Util.all_fields ()) (PointSet.singleton pt))) ~init:PointSet.empty

    let fold t ~init:init ~f:f =
      let pts = get_points t in
      PointSet.fold pts ~f:f ~init:init

    (* Since PacketDD is not guaranteed to be canonical, we have to semantically compare *)
    let compare t1 t2 = let eq = PacketDD.equal t1 t2 ||
                                 (fold t1 ~init:true ~f:(fun acc pt -> acc &&
                                                                       run t1 pt = run t2 pt)
                                  && fold t2 ~init:true ~f:(fun acc pt -> acc && run t1 pt = run t2 pt)) in
      if eq then 0 else -1
    let intersection_empty e e' = (compare (PacketDD.prod e e') empty) = 0

    let to_string = PacketDD.to_string

    let packet_to_beta pkt = Term.times (FieldMap.fold pkt ~init:[] ~f:(fun ~key:k ~data:v acc ->
        test k v :: acc))

    (* TODO: I'm not sure this handles the negative branch correctly. *)
    let betas = PacketDD.fold (fun pkts -> PartialPacketSet.fold pkts ~init:TermSet.empty
                                  ~f:(fun acc x -> TermSet.union acc (TermSet.singleton (packet_to_beta x))))
        (fun (h,v) t f -> TermSet.union (TermSet.map t (fun beta -> Term.times [test h v; beta]))
                                        (TermSet.map f (fun beta -> Term.times [not (test h v); beta])))

  end

  module DMatrix = struct

    open Hashcons

    type compact_derivative = {
      left_hand : EMatrix.t;
      right_hand : Term.t
    } [@@deriving sexp, compare]

    module CompactDerivSet = Set.Make (struct
      type t = compact_derivative [@@deriving sexp, compare]
      end)

    type t = CompactDerivSet.t [@@deriving sexp, compare]

    (* let compare = t_compare *)

    let compact_derivative_to_string elm = Printf.sprintf "(%s,%s)" (EMatrix.to_string elm.left_hand)
                                                                            (Term.to_string elm.right_hand)
    let to_string t = Printf.sprintf "{%s}" (String.concat ~sep:"; " (List.map (CompactDerivSet.elements t) compact_derivative_to_string))

    let pkt_to_beta pkt = Term.times (FieldMap.fold pkt ~init:[] ~f:(fun ~key ~data acc -> Term.test key data :: acc))

    let run t point =
      CompactDerivSet.fold t ~init:TermSet.empty
        ~f:(fun acc deriv ->
            let result = EMatrix.run deriv.left_hand point in
            (* Printf.printf "Running %s on %s: %b\n" (compact_derivative_to_string deriv) (point_to_string point) result; *)
               if result
             then TermSet.add acc (Term.times [pkt_to_beta (snd point); deriv.right_hand])
             else acc)

    let term_append t e = Term.times [t; e]
    let left_app e d = { d with left_hand = EMatrix.times e d.left_hand }
    let right_app d e = { d with right_hand = term_append d.right_hand e }

    let d_right_app ds e = CompactDerivSet.map ds (fun x -> right_app x e)
    let d_left_app e ds = CompactDerivSet.map ds (left_app e)


    let matrix_of_term t =
      let rec matrix_of_term' t =
        let open Term in
        begin match t.node with
        | Dup -> CompactDerivSet.singleton ({ left_hand = EMatrix.one; right_hand = one })
        | Plus ts -> TermSet.fold ts ~f:(fun acc t -> CompactDerivSet.union (matrix_of_term' t) acc) ~init:CompactDerivSet.empty
        | Times (t::ts) -> CompactDerivSet.union (d_right_app (matrix_of_term' t) (times ts))
                             (d_left_app (EMatrix.matrix_of_term t) (matrix_of_term' (times ts)))
        | Star t -> d_left_app (EMatrix.matrix_of_term (star t)) (d_right_app (matrix_of_term' t) (star t))
        | _ -> CompactDerivSet.empty
        end in
      TermSet.fold t ~init:CompactDerivSet.empty ~f:(fun acc x -> CompactDerivSet.union acc (matrix_of_term' x))

    (*
       a) for each (b, e) \in D(elm1), (b',e') \in D(elm2),
          if b /\ b' != 0, then e bisim e'
       b) \/ b == \/ b'
    *)

    let rec power_set = function
      | [] -> [[]]
      | x :: xs -> let rem = power_set xs in
        rem @ (List.map rem (fun y -> x :: y))

    let d_equivalent bisim d1 d2 =
      let compute_intersections d =
        let dlst = CompactDerivSet.elements d in
        (* We can be smarter and filter out empty-intersections *)
        List.map (power_set dlst) (fun xs -> List.fold xs ~init:(EMatrix.one, TermSet.empty) ~f:(fun (e,ts) t ->
            let e' = EMatrix.intersection e t.left_hand in
            if EMatrix.compare e' EMatrix.zero = 0 then
              (EMatrix.zero, TermSet.empty)
            else
              (e', TermSet.union ts (TermSet.singleton t.right_hand)))) in
        List.for_all (List.cartesian_product (compute_intersections d1) (compute_intersections d2))
          (fun ((e,d),(e',d')) -> match EMatrix.intersection_empty e e' with
             | true -> bisim d d'
             | false -> true)

    let equivalent (bisim : TermSet.t -> TermSet.t -> bool) d1 d2 =
      d_equivalent bisim d1 d2
      && EMatrix.compare (CompactDerivSet.fold d1 ~init:EMatrix.empty ~f:(fun acc x -> EMatrix.union x.left_hand acc))
        (CompactDerivSet.fold d1 ~init:EMatrix.empty ~f:(fun acc x -> EMatrix.union x.left_hand acc)) = 0

    let points t = CompactDerivSet.fold t ~init:EMatrix.zero ~f:(fun acc x -> EMatrix.union acc x.left_hand)

  end

  type t = { desc : TermSet.t;
             mutable e_matrix : EMatrix.t option;
             mutable d_matrix : DMatrix.t option
           } [@@deriving sexp]

  let get_termset t = t.desc

  let get_e t = match t.e_matrix with
    | Some e -> e
    | None -> let e = EMatrix.matrix_of_term (Term.plus t.desc) in
      t.e_matrix <- Some e;
      e

  let get_d t = match t.d_matrix with
    | Some d -> d
    | None -> let d = DMatrix.matrix_of_term t.desc in
      t.d_matrix <- Some d;
      d

  let get_term t = t.desc

  let sexp_of_t t = t.e_matrix <- Some (get_e t);
    t.d_matrix <- Some (get_d t);
    sexp_of_t t

  let make_term terms =
    (* let open HashCons in *)
    (* let terms = match term.node with *)
    (*   | TermBase.Plus ts -> ts *)
    (*   | _ -> TermSet.singleton term in *)
    { desc = terms;
      e_matrix = None;
      d_matrix = None
    }

  let compare t t' = TermSet.compare t.desc t'.desc
  let to_string t = Printf.sprintf "[desc: %s; e_matrix: %s; d_matrix: %s]"
      (TermSet.to_string t.desc)
      (EMatrix.to_string (get_e t))
      (DMatrix.to_string (get_d t))
end
