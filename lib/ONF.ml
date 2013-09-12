(* metavariable conventions
  - a, b, c, actions
  - s, t, u, action sets
  - x, y, z, patterns
  - p, q, r, local
*)

module K = NetKAT_Types

type pat = K.header_val_map (* f1 = n1 & ... & fk = nk *)

let tru : pat = K.HeaderMap.empty

let is_tru (x:pat) : bool = K.HeaderMap.is_empty x

let rec pat_matches (h : K.header) (v : K.header_val) (pt : pat) : bool =
  not (K.HeaderMap.mem h pt) || K.HeaderMap.find h pt = v

let pat_shadows (x:pat) (y:pat) = 
  K.HeaderMap.fold 
    (fun h v b -> b && (not (K.HeaderMap.mem h x) || K.HeaderMap.find h x = v))
    y true

let rec pat_wildcard (h : K.header) (pt : pat) : pat =
  K.HeaderMap.remove h pt

type act = K.header_val_map (* f1 := n1; ... ; fk := nk *)

module ActSet = Set.Make (struct
    type t = act
    let compare = K.HeaderMap.compare Pervasives.compare
end)

type acts = ActSet.t

type local = (pat * acts) list 

(* constants *)
let id : act = K.HeaderMap.empty

let is_id (s : acts) : bool =
  not (ActSet.is_empty s) && K.HeaderMap.is_empty (ActSet.choose s)

let drop : acts = ActSet.empty

let is_drop (s : acts) : bool = ActSet.is_empty s

(* normalizing constructor *)    
let mk (x:pat) (s:acts) (p:local) : local = 
  if is_tru x && is_drop s then 
    []
  else
    (x,s)::List.filter (fun (y,_) -> pat_shadows x y) p  

(* ugly printing *)
let header_val_map_to_string eq sep m = 
  K.HeaderMap.fold
    (fun h v acc -> 
      Printf.sprintf "%s%s%s%s" 
	(if acc = "" then "" else acc ^ sep)
	(K.string_of_header h) 
	eq 
	(K.string_of_vint v))
    m ""

let pat_to_string pt = 
  if K.HeaderMap.is_empty pt then "true" 
  else Printf.sprintf "%s" (header_val_map_to_string "=" ", " pt)

let act_to_string a = 
  if K.HeaderMap.is_empty a then "id"
  else Printf.sprintf "%s" (header_val_map_to_string ":=" "; " a)

let acts_to_string s = 
  Printf.sprintf "{%s}"
    (ActSet.fold
       (fun a acc -> act_to_string a ^ if acc = "" then "" else ", " ^ acc)
       s "")
    
let to_string (p:local) = 
  let rec loop (b:bool) = function
    | [] -> 
      Printf.sprintf "{}"
    | [(x,s)] when b && is_tru x -> 
      acts_to_string s
    | ((x,s)::q) -> 
      Printf.sprintf "if %s then %s \nelse %s"
	(pat_to_string x)
	(acts_to_string s)
	(loop true q) in 
  loop false p

(* Only used by and_pred *)
exception Empty_pat

(* map_acts f (act_1 + ... + act_n) = f act_1 + ... + f act_n *)
let map_acts (f : act -> act) (s : acts) =
  let g a s' = ActSet.add (f a) s' in
  ActSet.fold g s drop

(* Some (x ; y) unless the conjunct is empty, in which case, return None. *)
let rec and_pat (x : pat) (y : pat) : pat option =
  let f h vo1 vo2 = match (vo1, vo2) with
    | (Some v1, Some v2) -> 
      if v1 <> v2 then raise Empty_pat else Some v1
    | (Some v1, None) -> 
      Some v1
    | (None, Some v2) -> 
      Some v2
    | (None, None) -> 
      raise Empty_pat in 
  try 
    Some (K.HeaderMap.merge f x y)
  with
    Empty_pat -> None

(* s1 + s2 *)
let rec par_acts_acts (s1 : acts) (s2 : acts) : acts = 
  ActSet.union s1 s2

(* simpl_par_acts_local S P = S + P *)
let rec simpl_par_acts_local (s : acts) (p : local) : local = 
  match p with 
    | [] -> 
      [tru, s]
    | ((x,s')::q) -> 
      mk x (par_acts_acts s s') (simpl_par_acts_local s q)

(* par_sum_local X S P Q = if X then S + P else Q *)
let rec par_acts_local (x : pat) (s : acts) (p : local) (q : local) : local =
  match p with
  | [] -> 
    q
  | (y, s')::p' ->
    let q' = par_acts_local x s p' q in
    begin match and_pat x y with
      | None ->
	q'
      | Some x_and_y ->
	mk x_and_y (par_acts_acts s s') q'
    end

(* par_local_local P Q = P + Q *)
let rec par_local_local (p : local) (q : local) : local =
  match (p, q) with
    | [],_ -> 
      q
    | _,[] -> 
      p
    | (x1,s1)::p1,(x2,s2)::q2 -> 
      begin match and_pat x1 x2 with 
	| Some x1_and_x2 -> 
	  mk x1_and_x2 (par_acts_acts s1 s2) (par_local_local p1 q2)
  	| None -> 
	  mk x1 s1 (mk x2 s2 (par_local_local p1 q2))
      end

(* seq_act H V A = H<-V; A *)
let seq_mod_act (h : K.header) (v : K.header_val) (a : act) : act =
  if K.HeaderMap.mem h a then
    a
  else
    K.HeaderMap.add h v a

(* seq_mod_acts H V S = H <- V; S *)
let seq_mod_acts (h : K.header) (v : K.header_val) (s : acts) : acts =
  map_acts (seq_mod_act h v) s

(* seq_mod_local H V P = H<-V; P *)
let rec seq_mod_local (h : K.header) (v : K.header_val) (p : local) : local =
  match p with
    | [] -> 
      []
    | (x, s)::p' ->
      let hvp' = seq_mod_local h v p' in
      let hvs = seq_mod_acts h v s in
      if K.HeaderMap.mem h x then
	if K.HeaderMap.find h x = v then
	  mk (pat_wildcard h x) hvs hvp'
	else
	  hvp'
      else
	mk x hvs hvp'
	  
(* returns a term equivalent to [if pr then pol1 else pol2] *)
let rec norm_ite (x : pat) (p1 : local) (p2 : local) : local =
  match p1 with
    | [] ->
      mk x drop p2
    | (x1', s1')::p1' ->
      begin match and_pat x x1' with
	| None ->
	  norm_ite x p1' p2
	| Some x_and_x1' ->
	  mk x_and_x1' s1' (norm_ite x p1' p2)
	end

let seq_act_local (a : act) (p : local) : local =
  K.HeaderMap.fold seq_mod_local a p

let seq_acts_local (s1 : acts) (p2 : local) : local =
  ActSet.fold (fun a p -> par_local_local (seq_act_local a p2) p) s1 []
 
let rec seq_local_local (p1 : local) (p2 : local) : local =
  match p1,p2 with
    | [],_ | _,[] -> 
      [] 
    | (x1, s1)::p1',_ ->
      norm_ite x1 (seq_acts_local s1 p2) (seq_local_local p1' p2) 
      
let rec negate (p : local) : local = match p with
  | [] -> 
    [(tru, ActSet.singleton id)]
  | (x,s)::p' ->
    if is_id s then
      seq_local_local
        (mk x drop (mk tru s []))
        (negate p')
    else if is_drop s then
      par_local_local
        [(x, ActSet.singleton id)]
        (negate p')
    else 
      failwith "not a predicate"

let rec pred_local (pr : K.pred) : local = match pr with
  | K.True ->
    [(tru,ActSet.singleton id)]
  | K.False ->
    []
  | K.Neg p ->
    negate (pred_local p)
  | K.Test (h, v) ->
    [(K.HeaderMap.singleton h v, ActSet.singleton id)]
  | K.And (pr1, pr2) ->
    seq_local_local (pred_local pr1) (pred_local pr2)
  | K.Or (pr1, pr2) ->
    par_local_local (pred_local pr1) (pred_local pr2)

let rec eq_local (p:local) (q:local) : bool = match p,q with
  | [],[] -> 
    true
  | (x1,s1)::p1,(x2,s2)::q2 -> 
    K.HeaderMap.compare Pervasives.compare x1 x2 = 0 &&
    ActSet.equal s1 s2 &&
    eq_local p1 q2
  | _ -> false

let star_local (p:local) : local =
  Format.printf "STAR\n%s\n%!" (to_string p);
  let rec loop (acc:local) : local =
    Format.printf "--- ACC ---\n%s\n%!" (to_string acc);
    let seq' = seq_local_local acc p in
    let acc' = par_local_local acc seq' in
    if eq_local acc acc' then acc
    else loop acc' in
  loop [(tru,ActSet.singleton id)]
 
let rec local_normalize (pol : K.policy) : local = match pol with
  | K.Filter pr ->
    pred_local pr
  | K.Mod (K.Switch, _) ->
    failwith "unexpected Switch in local_normalize"
  | K.Mod (h, v) ->
    [(tru, ActSet.singleton (K.HeaderMap.singleton h v))]
  | K.Par (pol1, pol2) ->
    par_local_local (local_normalize pol1) (local_normalize pol2)
  | K.Seq (pol1, pol2) ->
    seq_local_local (local_normalize pol1) (local_normalize pol2)
  | K.Star pol ->
    star_local (local_normalize pol)
      
let compile = local_normalize

let pat_to_netkat pt =
  (* avoid printing trailing K.Id if it is unnecessary *)
  if K.HeaderMap.is_empty pt then
    K.True
  else
    let (h, v) = K.HeaderMap.min_binding pt in
    let pt' = K.HeaderMap.remove h pt in
    let f h v pol = K.And (K.Test (h, v), pol) in
    (K.HeaderMap.fold f pt' (K.Test (h, v)))

let act_to_netkat (pol : act) : K.policy =
  (* avoid printing trailing K.Id if it is unnecessary *)
  if K.HeaderMap.is_empty pol then
    K.Filter K.True
  else
    let (h, v) = K.HeaderMap.max_binding pol in
    let pol' = K.HeaderMap.remove h pol in
    let f h v pol' = K.Seq (K.Mod (h, v), pol') in
    K.HeaderMap.fold f pol' (K.Mod  (h, v))

let acts_to_netkat (pol : acts) : K.policy =
  (* avoid printing trailing K.Drop if it is unnecessary *)
  if ActSet.is_empty pol then
    K.Filter K.False
  else
    let f seq pol' = K.Par (act_to_netkat seq, pol') in
    let seq = ActSet.max_elt pol in
    let pol' = ActSet.remove seq pol in
    ActSet.fold f pol' (act_to_netkat seq)

let rec to_netkat (p : local) : K.policy = match p with
  | [] ->
    K.Filter K.False
  | [(x,s)] -> 
    if is_id s then K.Filter (pat_to_netkat x)
    else if is_tru x then acts_to_netkat s
    else K.Seq (K.Filter (pat_to_netkat x), acts_to_netkat s)
  | (x,s)::p' -> 
    let pr = pat_to_netkat x in
    K.Par (K.Seq (K.Filter pr, acts_to_netkat s),
           K.Seq (K.Filter (K.Neg pr), to_netkat p'))
