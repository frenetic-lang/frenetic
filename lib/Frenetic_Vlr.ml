open Core.Std

module type HashCmp = sig
  include Hashtbl.Key
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
  type t with sexp
  type v
  type r
  type d
    = Leaf of r
    | Branch of v * t * t
  module Tbl : Hashtbl.S with type key = t
  module BinTbl : Hashtbl.S with type key = (t * t)
  val get : d -> t
  val unget : t -> d
  val get_uid : t -> int
  val mk_branch : v -> t -> t -> t
  val mk_leaf : r -> t
  val drop : t
  val id : t
  val const : r -> t
  val atom : v -> r -> r -> t
  val restrict : v list -> t -> t
  val peek : t -> r option
  (* val apply : (r -> r -> r) -> bool -> bool -> r -> t -> t -> t *)
  val sum : t -> t -> t
  val prod : t -> t -> t
  val dp_map : (r -> t) -> (v -> t -> t -> t) -> t -> t
  val map_r : (r -> r) -> t -> t
  val fold : (r -> 'a) -> (v -> 'a -> 'a -> 'a) -> t -> 'a
  val equal : t -> t -> bool
  val to_string : t -> string
  val clear_cache : preserve:Int.Set.t -> unit
  val compressed_size : t -> int
  val uncompressed_size : t -> int
  val to_dot : t -> string
  val refs : t -> Int.Set.t
end


module Make(V:HashCmp)(L:Lattice)(R:Result) : 
  S with type v = V.t * L.t and type r = R.t = 
struct
  type v = V.t * L.t with sexp
  type r = R.t with sexp

  type d
    = Leaf of r
    | Branch of v * int * int
  with sexp
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

  type t = int with sexp
  module T = Frenetic_Hashcons.Make(struct
      type t = d with sexp

      let hash t = match t with
        | Leaf r ->
          (R.hash r) lsl 1
        | Branch((v, l), t, f) ->
          (1021 * (V.hash v) + 1031 * (L.hash l) + 1033 * t + 1039 * f) lor 0x1

      let compare a b = match a, b with
        | Leaf _, Branch _ -> -1
        | Branch _, Leaf _ -> 1
        | Leaf r1, Leaf r2 -> R.compare r1 r2
        | Branch((vx, lx), tx, fx), Branch((vy, ly), ty, fy) ->
          begin match V.compare vx vy with
          | 0 -> begin match L.compare lx ly with
            | 0 -> begin match Int.compare tx ty with
              | 0 -> Int.compare fx fy
              | x -> x
              end
            | x -> x
            end
          | x -> x
          end
    end)

  let get = T.get
  let unget = T.unget
  let get_uid (t:t) : int = t

  module Tbl = Int.Table

  module BinTbl = Frenetic_Util.IntPairTbl

  let equal x y = x = y (* comparing ints *)

  let rec to_string t = match T.unget t with
    | Leaf r -> 
       Printf.sprintf "(%s)" (R.to_string r)
    | Branch((v, l), t, f) -> 
       Printf.sprintf "(%s = %s ? %s : %s)"
	 (V.to_string v) (L.to_string l) (to_string t) (to_string f)
		      
  let mk_leaf r = T.get (Leaf r)

  let mk_branch (v,l) t f =
    (* When the ids of the diagrams are equal, then the diagram will take on the
       same value regardless of variable assignment. The node that's being
       constructed can therefore be eliminated and replaced with one of the
       sub-diagrams, which are identical.

       If the ids are distinct, then the node has to be constructed and assigned
       a new id. *)
    if equal t f then
      t
    else
      T.get (Branch((v, l), t, f))

  let drop = mk_leaf (R.zero)
  let id = mk_leaf (R.one)

  let clear_cache ~(preserve : Int.Set.t) =
    (* SJS: the interface exposes `id` and `drop` as constants,
       so they must NEVER be cleared from the cache *)
    let preserve = Int.Set.(add (add preserve drop) id) in
    T.clear preserve

  let rec fold g h t = match T.unget t with
    | Leaf r -> g r
    | Branch((v, l), t, f) ->
      h (v, l) (fold g h t) (fold g h f)

  let const r = mk_leaf r
  let atom (v,l) t f = mk_branch (v,l) (const t) (const f)

  let peek t = match T.unget t with
    | Leaf r   -> Some r
    | Branch _ -> None

  let rec map_r g = fold
    (fun r          -> const (g r))
    (fun (v, l) t f -> mk_branch (v,l) t f)

  let restrict lst u =
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
    loop (List.sort (fun (u, _) (v, _) -> V.compare u v) lst) u

  let apply f commutative idempotent zero x y =
    let tbl : (t * t, t) Hashtbl.t = BinTbl.create () in
    let rec sum x y =
      if idempotent && x=y then x else
      if commutative && y < x
        (* for commutative operations, we have f(x,y) = f(y,x),
           so to maximize memoization, we always look up f(min(x,y), max(x,y)) *)
        then Hashtbl.find_or_add tbl (y, x) ~default:(fun () -> sum' y x)
        else Hashtbl.find_or_add tbl (x, y) ~default:(fun () -> sum' x y)
    and sum' x y =
      match T.unget x, T.unget y with
      | Leaf r, _      ->
        if R.compare r zero = 0 then y
        else map_r (fun y -> f r y) y
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

  let sum = apply R.sum true true R.zero

  let prod = apply R.prod false false R.one

  let dp_map (g : R.t -> t)
             (h : V.t * L.t -> t -> t -> t)
             (t : t) : t =
    let tbl = Tbl.create () in
    let rec f t =
      Tbl.find_or_add tbl t ~default:(fun () -> f' t)
    and f' t = match unget t with
      | Leaf r -> g r
      | Branch ((v, l), tru, fls) -> h (v,l) (f tru) (f fls) in
    f t

  let compressed_size (node : t) : int =
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

  let to_dot t =
    let open Format in
    let buf = Buffer.create 200 in
    let fmt = formatter_of_buffer buf in
    let seen : Int.Hash_set.t = Int.Hash_set.create ~size:10 () in
    let rank : ((V.t*L.t), Int.Hash_set.t) Hashtbl.t = Hashtbl.Poly.create ~size:20 () in
    pp_set_margin fmt (1 lsl 29);
    fprintf fmt "digraph tdk {@\n";
    let rec loop t =
      if not (Hash_set.mem seen t) then begin
        Hash_set.add seen t;
        match T.unget t with
        | Leaf r ->
          fprintf fmt "%d [shape=box label=\"%s\"];@\n" t (R.to_string r)
        | Branch((v, l), a, b) ->
          begin
            try Hash_set.add (Hashtbl.find_exn rank (v, l)) t
            with Not_found ->
              let s = Int.Hash_set.create ~size:10 () in
              Hash_set.add s t;
              Hashtbl.set rank (v, l) s
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
    Hashtbl.iter rank ~f:(fun ~key:_ ~data:s ->
      fprintf fmt "{rank=same; ";
      Hash_set.iter s ~f:(fun x -> fprintf fmt "%d " x);
      fprintf fmt ";}@\n");
    fprintf fmt "}@.";
    Buffer.contents buf

  let refs (t : t) : Int.Set.t =
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
