exception Quit
exception Undo
open Sexplib.Std

let debug_mode = false
let profile_mode = false
let failed_Count = ref 0
let success_count = ref 1

type stats = {
  compact_percent : int list ref
}

let stats = {compact_percent = ref []}

let print_debugging_info _  =
  Printf.printf "%s"
    (List.fold_right
       (Printf.sprintf "BaseSet natural compaction rate: %u\n%s")
       !(stats.compact_percent) "")

let string_fold f s a =
  let acc = ref a in
  String.iter (fun e -> acc := (f e !acc)) s; !acc


module Field = struct
  type t = int [@@deriving sexp]
  let compare = Pervasives.compare
  let as_int x = x
  let hash x = Hashtbl.hash x
  let equal a b = 0 = (compare a b)
  let of_string,to_string,reset =
    let stringtoint = Hashtbl.create 11 in
    let inttostring = Hashtbl.create 11 in
    let counter = ref 0 in
    let of_string (x : string) : t =
      try Hashtbl.find stringtoint x
      with Not_found ->
	let id = !counter in
	counter := !counter + 1 ;
	Hashtbl.replace stringtoint x id;
	Hashtbl.replace inttostring id x;
	id in
    let to_string (x : t) : string =
      Hashtbl.find inttostring x in
    let reset () = counter := 0;
      Hashtbl.clear stringtoint;
      Hashtbl.clear inttostring in
    of_string,to_string,reset
end
module FieldSet = struct
  include Set.Make(Field)
  let of_list (ts:elt list) : t =
    List.fold_left (fun acc t -> add t acc) empty ts
end


module Value = struct
  type t = int [@@deriving sexp]
  let compare = Pervasives.compare
  let as_int x = x
  let hash x = Hashtbl.hash x
  let equal a b = 0 = (compare a b)
  let of_string,to_string,max_elem,reset =
    let stringtoint = Hashtbl.create 11 in
    let inttostring = Hashtbl.create 11 in
    let snowman =  "☃" in
    Hashtbl.replace stringtoint snowman (-1);
    Hashtbl.replace inttostring (-1) snowman;
    let counter = ref 0 in
    let of_string (x : string) : t =
      try Hashtbl.find stringtoint x
      with Not_found ->
	let id = !counter in
	counter := !counter + 1 ;
	Hashtbl.replace stringtoint x id;
	Hashtbl.replace inttostring id x;
	id in
    let to_string (x : t) : string =
      Hashtbl.find inttostring x in
    let reset () = counter := 0;
      Hashtbl.clear stringtoint;
      Hashtbl.clear inttostring in
    of_string,to_string,(fun _ -> !counter),reset
  let extra_val = -1
end
module ValueSet = struct
  include Set.Make(Value)
  let of_list (ts:elt list) : t =
    List.fold_left (fun acc t -> add t acc) empty ts
  let elt_of_sexp = Value.t_of_sexp
  let sexp_of_elt = Value.sexp_of_t
  let t_of_sexp (s : Sexplib.Sexp.t) : t = of_list (list_of_sexp elt_of_sexp s)
  let sexp_of_t (s : t) : Sexplib.Sexp.t = sexp_of_list sexp_of_elt (elements s)
end

let all_fields_fail = (fun _ -> failwith
  "Please set all_fields in Decide_Util.ml before trying to run any calculations!")
let all_fields = ref all_fields_fail
let all_values_fail = (fun _ -> failwith
  "Please set all_values in Decide_Util.ml before trying to run any calculations!")
let all_values = ref all_values_fail


module FieldArray = struct
  type 'a t = 'a array [@@deriving sexp]

  let size = 1

  let make (a : 'a) : 'a t =
    let seen = FieldSet.cardinal (!all_fields ()) in
    Array.make (size + seen) a
  let init f =
    let seen = FieldSet.cardinal (!all_fields ()) in
    Array.init (size + seen) f
  let set this k =
    Array.set this (Field.as_int k)
  let get this k =
    try
      Array.get this (Field.as_int k)
    with (Invalid_argument _) ->
      invalid_arg
	(Printf.sprintf
	   "Error! This field is not tracked: \"%s\" (has int %u)\n all_fields: %s"
	   (Field.to_string k) (Field.as_int k)
	   (FieldSet.fold (fun s -> Printf.sprintf "(%s : %u) %s " (Field.to_string s) (Field.as_int s)) (!all_fields ()) ""))
  let fold f arr acc =
    let accr = ref acc in
    Array.iteri (fun indx elem ->
      let acc = !accr in
      accr := (f indx elem acc)) arr;
    !accr
  let copy = Array.copy
  let size a = Array.length a

end
module ValueArray = struct
  type 'a t = 'a array
  let make (a : 'a) : 'a t =
    let _ = !all_fields () in
    ignore (Array.make (Value.as_int (Value.max_elem ())) a);
    failwith "don't use this, I haven't tested it."
  let set this k =
    Array.set this (Value.as_int k)
  let get this k =
    Array.get this (Value.as_int k)
end


let output_endline (out : out_channel) (s : string) : unit =
  output_string out s;
  output_char out '\n'

let copy_lines in_channel out_channel : unit =
  try
    while true do
      output_endline out_channel (input_line in_channel)
    done
  with End_of_file -> ()

let rec range (min : int) (max : int) : int list =
  if max <= min then [] else min :: range (min + 1) max

let rec remove_duplicates list =
  match list with
  | [] -> []
  | x :: t -> x :: remove_duplicates (List.filter ((<>) x) t)

(* perform f on all pairs *)
let cross (f : 'a -> 'b -> 'c) (s : 'a list) (t : 'b list) : 'c list =
  List.concat (List.map (fun x -> List.map (f x) t) s)


let thunkify f =
  let ret = ref None in
  (fun _ ->
    match !ret with
      | None -> let v = f() in ret := Some v; v
      | Some v -> v)


(*****************************************************
 * A functional version
 *****************************************************)
module type SetMapF =
  functor (K : Map.OrderedType) ->
  functor (V : Set.OrderedType) -> sig
    type t
    type elt = V.t
    module Values : Set.S with type elt = elt
    type eltSet = Values.t
    type key = K.t
    val empty : t
    val add : key -> elt -> t -> t
    val add_all : key -> eltSet -> t -> t
    val remove : key -> elt -> t -> t
    val remove_all : key -> t -> t
    val find_all : key -> t -> eltSet
    val contains_key : key -> t -> bool
    val contains_value : key -> elt -> t -> bool
    val size : key -> t -> int
    val keys : t -> key list
    val bindings : t -> (key * elt list) list
    (* val iter : (elt -> unit) -> key -> t -> unit     *)
    val iter : (key -> elt -> unit) -> t -> unit
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val fold : (key -> elt -> 'b -> 'b) -> t -> 'b -> 'b
    val fold_key : (elt -> 'b -> 'b) -> key -> t -> 'b -> 'b
    val filter : (key -> elt -> bool) -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val consis : key -> elt -> t -> bool
    val single_mapping : key -> t -> bool
    val for_all : (key -> elt -> bool) -> t -> bool
    val is_empty : t -> bool
    val val_inter : eltSet -> eltSet -> eltSet
    val val_equal : eltSet -> eltSet -> bool
    val val_is_empty : eltSet -> bool
    val val_empty : eltSet
    val val_mem : elt -> eltSet -> bool
    val val_size : eltSet -> int
    val val_singleton : elt -> eltSet
    val maps_to_empty : key -> t -> bool
    val to_string : t -> (key -> string -> string, unit, string) format ->
      (elt list -> string list) -> string
  end

module SetMapF : SetMapF =
  functor (K : Map.OrderedType) ->
  functor (V : Set.OrderedType) -> struct
    module Values = Set.Make(V)
    module Keys = Map.Make(K)
    type t = Values.t Keys.t
    type elt = Values.elt
    type eltSet = Values.t
    type key = Keys.key
    let empty = Keys.empty
    let contains_key = Keys.mem
    let contains_value x v h =
      contains_key x h && Values.mem v (Keys.find x h)
    let add x v h =
      let s = if contains_key x h then Keys.find x h else Values.empty in
      let t = Values.add v s in
      Keys.add x t h
    let add_all k es mp =
      if Values.is_empty es
      then mp
      else Keys.add k es mp
    let remove_all = Keys.remove
    let find_all k t = try Keys.find k t with Not_found -> Values.empty
    let remove x v h =
      if contains_key x h then
        let s = Keys.find x h in
        let t = Values.remove v s in
        if Values.is_empty t then Keys.remove x h else
        Keys.add x t h
      else h
    let size x h =
      if contains_key x h then Values.cardinal (Keys.find x h) else 0
    let keys h = List.map fst (Keys.bindings h)
    let bindings h =
      let s = Keys.bindings h in
      List.map (fun (x,a) -> (x, Values.elements a)) s
    (* let iter f x h =                                         *)
    (*   if contains_key x h then Values.iter f (Keys.find x h) *)
    let iter f = Keys.iter (fun x -> Values.iter (f x))
    let equal = Keys.equal Values.equal
    let compare = Keys.compare Values.compare
    let fold f = Keys.fold (fun x -> Values.fold (f x))
    let fold_key f x h b =
      if contains_key x h then Values.fold f (Keys.find x h) b
      else b
    let filter f h =
      let g x v h = if f x v then add x v h else h in
      fold g h empty
    let union = fold add
    let inter m1 m2 =
      fold (fun key elt acc ->
	if contains_value key elt m2
	then add key elt acc
	else acc
      ) m1 empty

    let consis x v h =
      not (contains_key x h) || contains_value x v h
    let for_all f =
      Keys.for_all (fun k a -> Values.for_all (f k) a)

    let single_mapping k t =
      try
	(Values.cardinal (Keys.find k t)) = 1
      with Not_found -> true

    exception Matthew_wants_call_cc

    let is_empty m =
      try
	fold (fun _ _ _ -> raise Matthew_wants_call_cc) m true
      with Matthew_wants_call_cc -> false

    let val_inter = Values.inter
    let val_equal = Values.equal
    let val_is_empty = Values.is_empty
    let val_empty = Values.empty
    let val_mem = Values.mem
    let val_size = Values.cardinal
    let val_singleton = Values.singleton
    let maps_to_empty k t = Values.is_empty (find_all k t)
    let to_string (ssm : t) op elt_to_string =
      let s = bindings ssm in
      let f (x,a) = Printf.sprintf op x (String.concat "," (elt_to_string a)) in
      String.concat ";" (List.map f s)

  end

module Int = struct
  type t = int
  let compare = compare
end

module StringSetMap = SetMapF (String) (String)

(*****************************************************
 * Stream of strings in length-lexicographic order --
 * use to create new variable names
 *****************************************************)
module type LexStream = sig
  type t
  val make : unit -> t
  val next : t -> string
end

module LexStream : LexStream = struct
  type t = int list ref

  let rec inc (s : int list) : int list =
    match s with
      | [] -> [Char.code 'a']
      | x :: t ->
          if x < Char.code 'z' then (x + 1) :: t
          else Char.code 'a' :: inc t

  let make() : t = ref [Char.code 'a']

  let next (h : t) : string =
    let l = !h in
    h := inc l;
    String.concat "" (List.map (String.make 1) (List.map Char.chr (List.rev l)))
end

module WorkList = functor (K : Set.OrderedType) ->
struct
  module S = Set.Make(K)
  type t = S.t * (K.t list)

  let add (e : K.t) (wl : t)  : t =
    let set,worklist = wl in
    if S.mem e set
    then (
      wl)
    else S.add e set,e::worklist

  let singleton (e : K.t ) : t =
    S.singleton e, [e]

  let is_empty wl : bool =
    let set,wl = wl in
    match wl with
      | [] -> true
      | _ -> false

  let hd (set,wl) : K.t =
    List.hd wl

  let tl (set,wl) : t = set, List.tl wl

  let all_seen_items (set,_) =
    S.elements set

end

module UnionFind = functor(Ord : Core.Std.Map.Key) -> struct
  open Core.Std
  module FindMap = Map.Make(Ord)
  type union_find_ds =
    | Root_node of Ord.t * int ref (* maxdepth *)
    | Leaf_node of Ord.t * union_find_ds ref [@@deriving sexp]

  type t = { mutable node_map : union_find_ds FindMap.t;
             mutable root_ref_map : (union_find_ds ref) FindMap.t}

  let create () =
    {node_map = FindMap.empty; root_ref_map = FindMap.empty}

  (* Returns a reference to the root node of the equivalence class *)
  let rec get_parent t = function
    | Leaf_node (_,p) ->
      (match !p with
       | Root_node _ -> p
       | Leaf_node _ -> get_parent t !p)
    | Root_node (e,_)  -> FindMap.find_exn t.root_ref_map e

  let find_ref t e =
    match FindMap.find t.node_map e with
    | None ->
      let root = (Root_node (e, ref 0)) in
      t.node_map <- FindMap.add t.node_map ~key:e ~data:root;
      let r = ref root in
      t.root_ref_map <- FindMap.add t.root_ref_map ~key:e ~data:r;
      r
    | Some v -> get_parent t v

  let find t e = match !(find_ref t e) with
    | Root_node (v, _) -> v
    | _ -> failwith "get_parent didn't return a Root node!"

  let eq t a b =
    let l1,l2 = (find t a, find t b) in
    Ord.compare l1 l2 = 0

  let union t c1 c2 =
    let c1_root = find_ref t c1 in
    let c2_root = find_ref t c2 in
    match (!c1_root,!c2_root) with
    | (Root_node (l1,d1), Root_node (l2,d2)) ->
      if Ord.compare l1 l2 = 0 then ()
      else if !d2 < !d1 then (*c1 is new root*)
        let leaf = Leaf_node (l2,c1_root) in
	c2_root := leaf;
        t.node_map <- FindMap.add t.node_map ~key:l2 ~data:leaf
      else if !d1 > !d2 then
        let leaf = Leaf_node (l1,c2_root) in
	c1_root := leaf;
        t.node_map <- FindMap.add t.node_map ~key:l1 ~data:leaf
      else
        let leaf = Leaf_node(l2,c1_root) in
	d1 := !d1 + 1;
        c2_root := leaf;
        t.node_map <- FindMap.add t.node_map ~key:l2 ~data:leaf
    | _ -> failwith "get_parent didn't return a Root node!"

  let sexp_of_t t =
    failwith "TODO"
    (* let canonical_map = *)
      (* FindMap.fold t.node_map ~init:FindMap.empty ~f:(fun ~key:x ~data:node acc -> *)
          (* let root = find t x in *)
          (* if Ord.compare root x = 0 *)
          (* then *)
            (* acc *)
          (* else *)
            (* FindMap.add_multi acc ~key:root ~data:x) in *)
    (* <:sexp_of<(Ord.t * (Ord.t list)) list>> (FindMap.to_alist canonical_map) *)

  let t_of_sexp sexp =
    failwith "TODO"
    (* let alist = <:of_sexp<(Ord.t * (Ord.t list)) list>> sexp in *)
    (* let node_map, root_ref_map = (List.fold alist ~init:(FindMap.empty, FindMap.empty) ~f:(fun acc x -> *)
        (* let root, nodes = x in *)
        (* let root_node = Root_node (root, ref (if List.length nodes > 0 then 1 else 0)) in *)
        (* let root_ref = ref root_node in *)
        (* (List.fold nodes ~init:(FindMap.add (fst acc) ~key:root ~data:root_node) ~f:(fun acc v -> *)
             (* FindMap.add acc ~key:v ~data:(Leaf_node (v, root_ref)))), *)
        (* FindMap.add (snd acc) ~key:root ~data:root_ref)) in *)
    (* { node_map; root_ref_map } *)

  exception Invalid_root_reference
  exception Duplicate_node

  (* 1) Every node with a reference to a root node uses the same reference *)
  let check_root_refs t =
    let module NodeMap = Map.Make(struct
        type t = union_find_ds [@@deriving sexp]
        let compare n1 n2 =
          match n1,n2 with
          | Root_node(l1, d1), Root_node(l2, d2) -> Pervasives.compare (l1,d1) (l2,d2)
          (* Not sure this will work (not symmetric) *)
          | _ -> failwith "Should only be called on Root_node"
      end) in
    let _ = FindMap.fold t.node_map ~init:NodeMap.empty
        ~f:(fun ~key:key ~data:data node_map -> match data with
            | Leaf_node(l1,d1) -> let root_ref = get_parent t data in
              begin match NodeMap.mem node_map !root_ref with
                | true -> if not (phys_equal (NodeMap.find_exn node_map !root_ref) root_ref)
                  then raise Invalid_root_reference
                  else node_map
                | false -> NodeMap.add node_map ~key:!root_ref ~data:root_ref
              end
            | _ -> node_map) in
    ()

  (* 2) There is only one node with a given Ord.t value *)
  (* 3) FindMap.find e points to the unique node with value e *)
  let check_node_uniqueness t =
    let rec crawl_up_tree f node = match node with
      | Leaf_node (l, r) -> f node; crawl_up_tree f !r
      | Root_node (l, d) -> f node in
    FindMap.iter t.node_map ~f:(fun ~key:key ~data:data ->
        crawl_up_tree (fun node -> match node with
            | Root_node(v,_)
            | Leaf_node(v,_) -> if FindMap.find_exn t.node_map v = node then () else raise Duplicate_node) data)
   (* Invariants:
     1) Every node with a reference to a root node uses the same reference
     2) There is only one node with a given Ord.t value
     3) FindMap.find e points to the unique node with value e
     4) if n = Root_node(e, d), then d = depth of the tree under n
   *)
  let validate t = check_root_refs t; check_node_uniqueness t

  module Class = struct
    type t = { identifier : Ord.t;
               members : Ord.t list } [@@deriving sexp]
    let members t = t.members
    let canonical_element t = t.identifier
  end

  let equivalence_classes t =
    FindMap.fold (FindMap.fold t.node_map ~init:FindMap.empty ~f:(fun ~key:k ~data:v acc -> match !(get_parent t v) with
        | Root_node(v,_) -> FindMap.add_multi acc ~key:v ~data:k
        | _ -> failwith "Decide_Util.UnionFind.get_parent returned a non-root node!"))
      ~init:[] ~f:(fun ~key:k ~data:v acc -> {Class.identifier = k; Class.members = v} :: acc)
end

module UnivMap = SetMapF(Field)(Value)

(* returns true if universe is non-empty *)
let set_univ (tvallist : UnivMap.t list) : bool =
  let module UnivMap = SetMapF (Field) (Value) in
  let univ = List.fold_right UnivMap.union tvallist UnivMap.empty in
  let univ = List.fold_left (fun u x -> UnivMap.add x Value.extra_val u) univ (UnivMap.keys univ) in
  let module UnivDescr = struct
	let all_fields : FieldSet.t =
	  (* TODO: fix me when SSM is eliminated *)
	  List.fold_right
	    (fun f ->
	      FieldSet.add f) (UnivMap.keys univ) FieldSet.empty
	let all_values f : ValueSet.t =
	  try
	    UnivMap.Values.fold (fun v acc -> ValueSet.add v acc ) (UnivMap.find_all f univ)
	      ValueSet.empty
	  with Not_found ->
	    ValueSet.empty
      end in
  all_fields := (fun _ -> UnivDescr.all_fields);
  all_values := (fun _ -> UnivDescr.all_values);
  List.exists (fun e -> not (UnivMap.is_empty e)) tvallist
