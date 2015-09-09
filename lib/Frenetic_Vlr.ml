module type HashCmp = sig
  type t
  val hash : t -> int
  val compare : t -> t -> int
  val to_string : t -> string
end

module type Lattice = sig
  include HashCmp
  val subset_eq : t -> t -> bool
  val meet : ?tight:bool -> t -> t -> t option
  val join : ?tight:bool -> t -> t -> t option
end

module type Result = sig
  include HashCmp
  val sum : t -> t -> t
  val prod : t -> t -> t
  val one : t
  val zero : t
end

module type S = sig
  type t = int
  type v
  type r
  type d
    = Leaf of r
    | Branch of v * t * t
  val get : d -> t
  val unget : t -> d
  val mk_branch : v -> t -> t -> t
  val mk_leaf : r -> t
  val mk_id : unit -> t
  val mk_drop : unit -> t
  val const : r -> t
  val atom : v -> r -> r -> t
  val restrict : v list -> t -> t
  val peek : t -> r option
  val sum : t -> t -> t
  val sum_generalized : (r -> r -> r) -> r -> t -> t -> t
  val prod : t -> t -> t
  val map_r : (r -> r) -> t -> t
  val fold : (r -> 'a) -> (v -> 'a -> 'a -> 'a) -> t -> 'a
  val equal : t -> t -> bool
  val to_string : t -> string
  val clear_cache : preserve:Core.Std.Int.Set.t -> unit
  val compressed_size : t -> int
  val uncompressed_size : t -> int
  val to_dot : t -> string
  val refs : t -> Core.Std.Int.Set.t

end


module Make(V:HashCmp)(L:Lattice)(R:Result) : 
  S with type v = V.t * L.t and type r = R.t = 
struct
  type v = V.t * L.t
  type r = R.t

  type d
    = Leaf of r
    | Branch of v * int * int

  type t = int
  module T = Frenetic_Hashcons.Make(struct
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

  let rec to_string t = match T.unget t with
    | Leaf r -> 
       Printf.sprintf "(%s)" (R.to_string r)
    | Branch((v, l), t, f) -> 
       Printf.sprintf "(%s = %s ? %s : %s)"
	 (V.to_string v) (L.to_string l) (to_string t) (to_string f)
		      
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
  let atom (v,l) t f = mk_branch (v,l) (const t) (const f)

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
