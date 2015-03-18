(** The signature for a type that can be compared and hashed *)
module type HashCmp = sig
  type t

  val hash : t -> int
  (** [hash t] assigns an interger to each value of type [t]. This assignment
      must be consistent with the {!compare} operation in the following way:

          if [compare a b = 0] then [hash a = hash b] *)

  val compare : t -> t -> int
  (** [compare a b] returns one of three values:

      {ul
      {- [0] when [a] and [b] are equal;}
      {- [1] when [a] is greater than [b]; and}
      {- [-1] when [a] is less than [b].}} *)

  val to_string : t -> string
  (** [to_string t] returns a string representation of the value. *)
end

(** The signature for a type that has a lattice structure. *)
module type Lattice = sig
  include HashCmp

  val subset_eq : t -> t -> bool
  (** [subset_eq a b] returns [true] if [a] and [b] in the partial ordering of
      the lattice. This relation should be reflexive, transitive, and
      antisymmetric. *)

  val meet : ?tight:bool -> t -> t -> t option
  (** [meet ~tight a b] returns the greatest lower bound of the elements [a]
      and [b], if one exists. This operation should be associative, commutative,
      and idempotent. If the optional argument [tight] is set to [true], then
      the result [c] should satisfy the additional property:

          ∀x, [subset_eq c x] <=> [subset_eq a x || subset_eq b x || equal c x].

      In other words, elements related to the greatest lower bound should be
      related transitively through [a] and [b], or be equal to the greatest
      lower bound itself. *)

  val join : ?tight:bool -> t -> t -> t option
  (** [join ~tight a b] returns the least upper bound of the elements [a] and
      [b], if one exists. This operation should be associative, commutative, and
      idempotent. If the optional argument [tight] is set to [true], then the
      result [c] should satisfy the additional property:

          ∀x, [subset_eq x c] <=> [subset_eq x a || subset_eq x b || equal x c].

      In other words, elements related to the least upper bound should be
      related transitively through [a] and [b], or be equal to the least upper
      bound itself. *)

end

(** The type for a result that has a semi-ring structure *)
module type Result = sig
  include HashCmp

  val sum : t -> t -> t
  (** An associative and commutative binary operation over the type [t]. The
      following should hold:

      {ul
      {- [sum a (sum b c)] = [sum (sum a b) c].}
      {- [sum a b] = [sum b a].}} *)

  val prod : t -> t -> t
  (** An associative binary operation over the type [t]. The following should
      hold:

      {ul
      {- [prod a (prod b c)] = [prod (prod a b) c]. }
      {- [prod a (sum b c)] = [sum (prod a b) (prod a c)].}} *)

  val one : t
  (** The identity for the [prod] operation. The following should hold:

      {ul
      {- [prod one t] = [t].}
      {- [prod t one] = [t].}}

      As an example, if [t] where the type [bool] and [prod] and [sum] were [&&]
      and [||], respectively, then [one] should be the value [true]. *)

  val zero : t
  (** The identity for the [sum] operation. The following should hold:

      {ul
      {- [sum zero t] = [t].}
      {- [sum t zero] = [t].}
      {- [prod zero t] = [zero].}
      {- [prod t zero] = [zero].}}

      As an example, if [t] where the type [bool] and [prod] and [sum] were [&&]
      and [||], respectively, then [zero] should be the value [false]. *)
end


module type TABLE = sig
  val clear : Core.Std.Int.Set.t -> unit
  type value
  val get : value -> int
  val unget : int -> value
end

module type TABLE_VALUE = sig
  type t
  val hash : t -> int
  val equal : t -> t -> bool
end

module PersistentTable (Value : TABLE_VALUE) : TABLE
  with type value = Value.t = struct

  module T = Hashtbl.Make (Value)
  (* TODO(arjun): Since these are allocated contiguously, it would be
     better to use a growable array ArrayList<Int> *)
  module U = Hashtbl.Make(struct
    type t = int
    let hash n = n
    let equal x y = x = y
  end)

  type value = Value.t

  let tbl : int T.t = T.create 100
  let untbl : value U.t = U.create 100

  let idx = ref 0

  let clear (preserve : Core.Std.Int.Set.t) : unit =
    let open Core.Std in
    let max_key = ref 0 in
    T.iter (fun key data ->
      max_key := max !max_key data;
      if Int.Set.mem preserve data then
        ()
      else
        (U.remove untbl data;
         T.remove tbl key))
      tbl;
    idx := !max_key + 1

  let gensym () =
    let r = !idx in
    idx := !idx + 1;
    r

  let get (v : value) =
    try
      T.find tbl v
    with Not_found ->
      begin
        let n = gensym () in
        T.add tbl v n;
        U.add untbl n v;
        n
      end

  let unget (idx : int) : value = U.find untbl idx

end

module Make(V:HashCmp)(L:Lattice)(R:Result) = struct
  type v = V.t * L.t
  type r = R.t

  type d
    = Leaf of r
    | Branch of v * int * int

  type t = int
  module T = PersistentTable(struct
      type t = d

      let hash t = match t with
        | Leaf r ->
          (R.hash r) lsl 1
        | Branch((v, l), t, f) ->
          (1021 * (V.hash v) + 1031 * (L.hash l) + 1033 * t + 1039 * f) lor 0x1

      let equal a b = match a, b with
        | Leaf r1, Leaf r2 -> R.compare r1 r2 = 0
        | Branch((vx, lx), tx, fx), Branch((vy, ly), ty, fy) ->
          V.compare vx vy = 0 && tx = ty && fx = fy
            && L.compare lx ly = 0
        | _, _ -> false
    end)

  let get = T.get
  let unget = T.unget

  (* A tree structure representing the decision diagram. The [Leaf] variant
   * represents a constant function. The [Branch(v, l, t, f)] represents an
   * if-then-else. When variable [v] takes on the value [l], then [t] should
   * hold. Otherwise, [f] should hold.
   *
   * [Branch] nodes appear in an order determined first by the total order on
   * the [V.t] value with with ties broken by the total order on [L.t]. The
   * least such pair should appear at the root of the diagram, with each child
   * nodes being strictly greater than their parent node. This invariant is
   * important both for efficiency and correctness.
   * *)

  let equal x y = x = y (* comparing ints *)

  let rec to_string t = "to_string broken" (* match T.get t with
    | Leaf r             -> R.to_string r
    | Branch(v, l, t, f) -> Printf.sprintf "B(%s = %s, %s, %s)"
      (V.to_string v) (L.to_string l) (to_string t)
      (to_string (T.get f))
 *)
  let clear_cache ~(preserve : Core.Std.Int.Set.t) = T.clear preserve

  let mk_leaf r = T.get (Leaf r)


  let mk_branch (v,l) t f =
    (* When the ids of the diagrams are equal, then the diagram will take on the
       same value regardless of variable assignment. The node that's being
       constructed can therefore be eliminated and replaced with one of the
       sub-diagrams, which are identical.

       If the ids are distinct, then the node has to be constructed and assigned
       a new id. *)
    if equal t f then begin
      t
    end else
      T.get (Branch((v, l), t, f))

  (* these need to be functions to avoid cache problems *)
  let mk_id () = mk_leaf (R.one)
  let mk_drop () = mk_leaf (R.zero)

  let rec fold g h t = match T.unget t with
    | Leaf r -> g r
    | Branch((v, l), t, f) ->
      h (v, l) (fold g h t) (fold g h f)

  let const r = mk_leaf r
  let atom (v, l) t f = mk_branch (v,l) (const t) (const f)

  let node_min t1 t2 = match (t1, t2) with
  | Leaf _, Leaf _ -> (t1, t2) (* constants at same rank since they can't be ordered *)
  | Leaf _, _ -> (t2, t1)
  | _, Leaf _ -> (t1, t2)
  | Branch ((v1, l1), _, _), Branch ((v2, l2), _, _) -> match V.compare v1 v2 with
    | -1 -> (t1, t2)
    | 1 -> (t2, t1)
    | 0 -> if L.subset_eq l1 l2 then
             (t1, t2)
           else if L.subset_eq l2 l1 then
             (t2, t1)
           else
             (* The Spiros case. I don't think it matters which we pick *)
             (t1, t2)
    | _ -> assert false


  module H = Core.Std.Hashtbl.Poly

  let rec restrict' ((x1, l1) : v) (is_true : bool) (t : t) : t=
    match unget t with
    | Leaf r -> t
    | Branch ((x2, l2), tru, fls) ->
      match V.compare x1 x2 with
      | 1 -> t
      | -1 ->
        if is_true then mk_branch (x1,l1) t (mk_leaf R.zero)
        else mk_branch (x1,l1) (mk_leaf R.zero) t
      | 0 ->
        if L.subset_eq l2 l1 then
          (if is_true then t else mk_leaf R.zero)
        else if L.subset_eq l1 l2 then
          mk_branch (x2,l2) (restrict' (x1,l1) is_true tru) fls
        (* TODO(arjun): disjoint assumption. Does not work for prefixes *)
        else
          (if is_true then mk_leaf R.zero else tru)
      | _ -> assert false


  let restrict lst =
    let rec loop xs u =
      match xs, T.unget u with
      | []          , _
      | _           , Leaf _ -> u
      | (v,l) :: xs', Branch((v', l'), t, f) ->
        match V.compare v v' with
        |  0 -> if L.subset_eq l l' then loop xs' t else loop xs f
        | -1 -> loop xs' u
        |  1 -> mk_branch (v',l') (loop xs t) (loop xs f)
        |  _ -> assert false
    in
    loop (List.sort (fun (u, _) (v, _) -> V.compare u v) lst)

  let peek t = match T.unget t with
    | Leaf r   -> Some r
    | Branch _ -> None

  let rec map_r g = fold
    (fun r          -> const (g r))
    (fun (v, l) t f -> mk_branch (v,l) t f)

  let sum_generalized f zero x y =
    let tbl : (int * int, int) H.t = H.create () in
    let rec sum x y =
       H.find_or_add tbl (x, y) ~default:(fun () -> sum' x y)
    and sum' x y =
      match T.unget x, T.unget y with
      | Leaf r, _      ->
         if R.compare r zero = 0 then y
        else map_r (f r) y
      | _     , Leaf r ->
        if R.compare zero r = 0 then x
        else map_r (fun x -> f x r) x
      | Branch((vx, lx), tx, fx), Branch((vy, ly), ty, fy) ->
        begin match V.compare vx vy with
        |  0 ->
          begin match L.compare lx ly with
          |  0 -> mk_branch (vx,lx) (sum tx ty) (sum fx fy)
          | -1 -> mk_branch (vx,lx) (sum tx (restrict [(vx, lx)] y)) (sum fx y)
          |  1 -> mk_branch (vy,ly) (sum (restrict [(vy, ly)] x) ty) (sum x fy)
          |  _ -> assert false
          end
        | -1 -> mk_branch (vx,lx) (sum tx y) (sum fx y)
        |  1 -> mk_branch (vy,ly) (sum x ty) (sum x fy)
        |  _ -> assert false
        end
    in sum x y

  let sum = sum_generalized R.sum R.zero

  let prod = sum_generalized R.prod R.one

  let compressed_size (node : t) : int =
    let open Core.Std in
    let rec f (node : t) (seen : Int.Set.t) =
      if Int.Set.mem seen node then
        (0, seen)
      else
        match T.unget node with
        | Leaf _ -> (1, Int.Set.add seen node)
        | Branch (_, hi, lo) ->
          (* Due to variable-ordering, there is no need to add node.id to seen
             in the recursive calls *)
          let (hi_size, seen) = f hi seen in
          let (lo_size, seen) = f lo seen in
          (1 + hi_size + lo_size, Int.Set.add seen node) in
    let (size, _) = f node Int.Set.empty in
    size

  let rec uncompressed_size (node : t) : int = match T.unget node with
    | Leaf _ -> 1
    | Branch (_, hi, lo) -> 1 + uncompressed_size hi + uncompressed_size lo

  module VH = Hashtbl.Make(struct
    type t = v

    let equal (v1, l1) (v2, l2) =
      V.compare v1 v2 = 0 && L.compare l1 l2 = 0

    let hash (v, l) =
      191 * (V.hash v) + 877 * (L.hash l)
  end)

  let to_dot t =
    let open Format in
    let buf = Buffer.create 200 in
    let fmt = formatter_of_buffer buf in
    let seen = Hashtbl.create ~random:true 20 in
    let rank = VH.create 20 in
    pp_set_margin fmt (1 lsl 29);
    fprintf fmt "digraph tdk {@\n";
    let rec loop t =
      if not (Hashtbl.mem seen t) then begin
        Hashtbl.add seen t ();
        match T.unget t with
        | Leaf r ->
          fprintf fmt "%d [shape=box label=\"%s\"];@\n" t (R.to_string r)
        | Branch((v, l), a, b) ->
          begin
            try Hashtbl.add (VH.find rank (v, l)) t ()
            with Not_found ->
              let s = Hashtbl.create ~random:true 10 in
              Hashtbl.add s t ();
              VH.add rank (v, l) s
          end;
          fprintf fmt "%d [label=\"%s = %s\"];@\n"
            t (V.to_string v) (L.to_string l);
          fprintf fmt "%d -> %d;@\n" t a;
          fprintf fmt "%d -> %d [style=\"dashed\"];@\n" t b;
          loop a;
          loop b
      end
    in
    loop t;
    VH.iter
      (fun _ s ->
         fprintf fmt "{rank=same; ";
         Hashtbl.iter (fun x () -> fprintf fmt "%d " x) s;
         fprintf fmt ";}@\n")
      rank;
    fprintf fmt "}@.";
    Buffer.contents buf

  let refs (t : t) : Core.Std.Int.Set.t =
    let open Core.Std in
    let rec f (node : t) (seen : Int.Set.t) =
      if Int.Set.mem seen node then
        seen
      else
        match T.unget node with
        | Leaf _ -> Int.Set.add seen node
        | Branch (_, hi, lo) ->
          Int.Set.add (f lo (f hi seen)) node in
    f t Int.Set.empty

end
