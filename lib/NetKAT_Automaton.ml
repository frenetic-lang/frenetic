type policy = NetKAT_Types.policy
type pred = NetKAT_Types.pred
type header = NetKAT_Types.header
type header_val = NetKAT_Types.header_val

(* BEGIN GENERIC HELPER FUNCTIONS ------------------------------------------- *)

let is_none m =
  match m with
    | None   -> true
    | Some _ -> false

let from_option a m =
  match m with
    | None   -> a
    | Some b -> b

let optional a f m =
  match m with
    | None   -> a
    | Some e -> f e

let flip f a b = f b a

(* END GENERIC HELPER FUNCTIONS --------------------------------------------- *)


(* BEGIN DATA TYPES --------------------------------------------------------- *)

(* A link-free policy is—as the name suggests—a NetKAT policy that does not
 * contain links. 
 *)
type lf_policy =
  | Filter of pred
  | Mod of header * header_val
  | Par of lf_policy * lf_policy
  | Choice of lf_policy * lf_policy
  | Seq of lf_policy * lf_policy
  | Star of lf_policy

(* A separate datatype to represent links.
 *
 *   link = (sw1, pt1, sw2, pt2)
 *)
type link = header_val * header_val * header_val * header_val

type 'a aregex =
  | Char of 'a
  | Alt of 'a aregex * 'a aregex
  | Cat of 'a aregex * 'a aregex
  | Kleene of 'a aregex
  | Empty

let rec fmap_aregex (f : 'a -> 'b) (r : 'a aregex) : 'b aregex =
  match r with
    | Char(c) -> Char(f c)
    | Alt(r1, r2) -> Alt(fmap_aregex f r1, fmap_aregex f r2)
    | Cat(r1, r2) -> Cat(fmap_aregex f r1, fmap_aregex f r2)
    | Kleene(s) -> Kleene(fmap_aregex f s)
    | Empty -> Empty

type pchar = lf_policy * link

(* A regular expression over link-free policy, link pairs. *)
type regex = pchar aregex

(* END DATA TYPES ----------------------------------------------------------- *)


(* BEGIN TO POLICY ---------------------------------------------------------- *)

(*
 * Helper function for `regex_of_policy`. Straight up replace constructors from
 * this module with the identically-named constructors from `NetKAT_Types`.
 *)
let rec lf_policy_to_policy (lfp : lf_policy) : policy =
  match lfp with
    | Filter(p) -> NetKAT_Types.Filter(p)
    | Mod(h, v) -> NetKAT_Types.Mod(h, v)
    | Par(p1, p2) ->
      NetKAT_Types.Par(lf_policy_to_policy p1, lf_policy_to_policy p2)
    | Choice(p1, p2) ->
      NetKAT_Types.Choice(lf_policy_to_policy p1, lf_policy_to_policy p2)
    | Seq(p1, p2) ->
      NetKAT_Types.Seq(lf_policy_to_policy p1, lf_policy_to_policy p2)
    | Star(p) ->
      NetKAT_Types.Star(lf_policy_to_policy p)

(* Helper function for `regex_of_policy`. Take care of link translation *)
let link_to_policy ((sw1, pt1, sw2, pt2) : link) : policy =
  NetKAT_Types.Link(sw1, pt1, sw2, pt2)

let rec regex_to_policy (r : regex) : policy =
  match r with
    | Char(lfp, l) ->
      NetKAT_Types.Seq(lf_policy_to_policy lfp, link_to_policy l)
    | Alt(r1, r2) ->
      NetKAT_Types.Choice(regex_to_policy r1, regex_to_policy r2)
    | Cat(r1, r2) ->
      NetKAT_Types.Seq(regex_to_policy r1, regex_to_policy r2)
    | Kleene(r) ->
      NetKAT_Types.Star(regex_to_policy r)
    | Empty -> NetKAT_Types.Filter(NetKAT_Types.True)

let regex_to_string (r : regex) : string =
  NetKAT_Types.string_of_policy (regex_to_policy r)

let lf_policy_to_string (lf_p : lf_policy) : string =
  NetKAT_Types.string_of_policy (lf_policy_to_policy lf_p)

let mlf_policy_to_string (mp : lf_policy option) : string =
  optional "<no lf_p>" lf_policy_to_string mp

(* END TO POLICY ------------------------------------------------------------ *)


(* BEGIN OF POLICY ---------------------------------------------------------- *)

(* An intermediate representation for the conversion from a NetKAT policy to a
 * regex. inter is a continuation with three cases. 
 *
 *   - The TP case can accept link-free policies and create a new continuation
 *   from that.
 *
 *   - The NL policy needs a link_provider to transition to another type of
 *   continuation, but can optionally accumulate link-free policies in the mean
 *   time. Accumulator is combined using the cstr function.
 *
 *   - An S is a fully-formed regular expression. At this point, the computation
 *   is done, as the continuation cannot accept new link-free policies or links.
 *
 * In the TP and NL case, the cstr parameter is used to combine link-free
 * policies, if necessary.
 *)
type inter =
    | TP of link_provider
    | NL of link_consumer
    | S  of regex

and cstr = lf_policy -> lf_policy -> lf_policy 

and link_provider = cstr -> lf_policy option -> inter 
and link_consumer = cstr -> lf_policy option -> link_provider option -> inter

let seq (p : lf_policy) (q : lf_policy) : lf_policy = Seq(p, q)
let par (p : lf_policy) (q : lf_policy) : lf_policy = Par(p, q)

let mk_mcstr c mp q =
  optional q (fun p -> c p q) mp

(* Constructor for a link_consumer. Requires a link-free policy as the basis for
 * its accumulator so that if it passes its link-free policy to a link_provider,
 * it will not force the link_provider to transition to a regex.
 *)
let rec mk_nl (lf_p : lf_policy) : link_consumer =
  fun cstr mlf_p mlp ->
    let lfp' = mk_mcstr cstr mlf_p lf_p in
    (* print_string ("mk_nl with lfp': " ^ (lf_policy_to_string lfp') ^ "\n"); *)
    match mlp with
      | None    -> NL(mk_nl lfp')
      | Some lp -> lp cstr (Some(lfp'))

(* Constructor for link_provider. Requires a link and an optional link-free
 * policy that acts as an accumulator. If the link provider receives a `None`,
 * it will transition to a regex. If it receives `Some lfp` then it will
 * continue taking policies.
 *)
let rec mk_tp (l : link) (macc : lf_policy option) : link_provider =
  fun cstr mlf_p ->
    match mlf_p with
      | None -> 
        S(Char(from_option (Filter(NetKAT_Types.True)) macc, l))
      | Some lf_p ->
        (* print_string ("mk_tp with lf_p: " ^ (lf_policy_to_string lf_p) ^ "\n"); *)
        TP(mk_tp l (Some(mk_mcstr (flip cstr) macc lf_p)))


(* END DATA TYPES ----------------------------------------------------------- *)


let regex_of_policy (p : policy) : regex =

  let rec of_mlink (c : cstr) (mlp : link_provider option) : inter =
    match mlp with
      | None    -> NL(fun cstr mp mlp -> assert (is_none mp); of_mlink cstr mlp)
      | Some lp -> lp c None in

  let rec rpc (p : policy) : inter =
    (* print_string ((NetKAT_Types.string_of_policy p) ^ "\n"); *)
    begin match p with
      | NetKAT_Types.Filter(q) ->
        NL(mk_nl (Filter q))
      | NetKAT_Types.Mod(h, v) ->
        NL(mk_nl (Mod(h, v)))
      | NetKAT_Types.Link(sw1, pt1, sw2, pt2) ->
        TP(mk_tp (sw1, pt1, sw2, pt2) None)
      | NetKAT_Types.Seq(p1, p2) ->
        rpc_seq (rpc p1) (rpc p2)
      | NetKAT_Types.Par(p1, p2) ->
        rpc_par (rpc p1) (rpc p2)
      | NetKAT_Types.Choice(_, _) -> failwith "nyi"
      | NetKAT_Types.Star(q) ->
        S(Kleene(run (rpc q)))
    end

  and rpc_seq i j =
    begin match i, j with
      | TP f1, NL f2 -> NL(fun c mlf_p mlp -> rpc_seq (f1 c mlf_p) (f2 c None mlp))
      | TP f1, _     -> let r = run j in TP(fun c mp -> rpc_seq (f1 c mp) (S(r)))
      | NL f1, TP f2 -> f1 seq None (Some f2)
      | NL f1, NL f2 -> f1 seq None (Some(fun c mlf_q -> f2 c mlf_q None))
      | NL f1, S  r  -> failwith "Cat(NL, Star) can't be represented"
      | S   r, _     -> s_trans r j (fun x y -> Cat(x, y))
    end

  and rpc_par i j =
    begin match i, j with
      | TP f1, TP f2 -> TP(fun c mlp -> rpc_par (f1 c mlp) (f2 c mlp))
      | TP f1, NL f2 -> NL(fun c mlf_p mlp ->
                            rpc_par (rpc_seq (f1 c mlf_p) (of_mlink c mlp))
                                    (f2 c mlf_p mlp))
      | TP f1, S  r  -> S(Alt(run i, r))
      | NL f1, TP f2 -> NL(fun c mlf_p mlp ->
                            rpc_par (f1 c mlf_p mlp)
                                    (rpc_seq (f2 c mlf_p) (of_mlink c mlp)))
      | NL f1, NL f2 -> f1 par None (Some(fun c mlf_q -> f2 c mlf_q None))
      | NL f1, S  r  -> s_trans r i (fun x y -> Alt(y, x))
      | S   r, _     -> s_trans r j (fun x y -> Alt(x, y))
    end

  and s_trans (r : regex) (i : inter) c =
    begin match i with
      | NL f ->
        NL(fun cstr mp ml -> assert (is_none mp); S(c r (run (f cstr None ml))))
      | _ ->
        S(c r (run i))
    end

  and run_with (i : inter) (mp : lf_policy option) (mlp : link_provider option) : inter =
    begin match i with
      | TP f -> f seq mp
      | NL f -> f seq mp mlp
      | S  r -> S(r)
    end

  and run (i : inter) : regex =
    begin match run_with i None None with
      | TP _ -> failwith "should not happen"
      | NL _ -> failwith "need a link in there"
      | S  r -> r
    end in

  run (rpc p)

let regex_to_aregex (r : regex) : (int aregex) * ((int,  pchar) Hashtbl.t) =
  (* like an ST monad lol *)
  let htbl = Hashtbl.create 20 in
  let intg = ref 0 in
  let next () = let n = !intg in intg := n + 1; n in
  let add c   = let i = next () in Hashtbl.add htbl i c; i in

  (fmap_aregex add r, htbl)

let rec regex_of_aregex (r : int aregex) (htbl : (int, pchar) Hashtbl.t) : regex =
  fmap_aregex (Hashtbl.find htbl) r

module NFA = struct
  open Nfa

  type t = nfa

  type state = Nfa.state

  type edge = state * charset * state 

  module EdgeSet = Set.Make(struct
    type t = edge
    let compare = Pervasives.compare
  end)

  module StateSet = Nfa.StateSet

  module CharSet = struct
    type t = Charset.set
    let mem = Charset.mem 
    let iter = Charset.iter 
    let fold = Charset.fold 
  end

  let to_dot m = nfa_to_dot m

  let state_equal q1 q2 = 
    q1 = q2

  let state_accept m q = 
    StateSet.mem m.f (eps_closure m q) 

  let state_init m q = 
    m.s = q

    (*
  let to_string m = 
    forward_fold_nfa
      (fun q acc -> 
	Hashtbl.fold (fun q' ns acc -> 
	  CharSet.fold (fun n acc -> 
	    try 
	      let string_of_state q = 
            if state_init m q then "<q" ^ string_of_int q ^ ">"
            else if state_accept m q then "[q" ^ string_of_int q ^ "]"
            else " q" ^ string_of_int q ^ " " in 		
	      let (s,fo) = SymbolHash.find symbol_to_code n in 
	      acc ^ (Printf.sprintf "%s ==(%s,%s)==> %s\n" 
		       (string_of_state q)
		       s 
		       (match fo with None -> "_" | Some f -> f) 
		       (string_of_state q'))
	    with Not_found -> 
	      acc ^ (Printf.sprintf "q%d -???-> q%d\n" q q'))
	    ns acc)
	  (all_delta m.delta q) acc)
      m m.s "\n" 
      *)

  let state_name s = Printf.sprintf "%d" s

  let edge_symbols (_,ns,_) = 
    ns

  let edge_name (q1,_,q2) = 
    Printf.sprintf "%d_%d" q1 q2

  let edge_src (q,_,_) = 
    q

  let edge_dst (_,_,q) = 
    q

  let states m = 
    Hashset.fold StateSet.add m.q StateSet.empty

  let inits m = 
    StateSet.singleton m.s
  
  let accepts m = 
    StateSet.filter (state_accept m) (states m)
  
  let edges m = 
    forward_fold_nfa 
      (fun q acc -> 
	Hashtbl.fold 
	  (fun q' ns acc -> EdgeSet.add (q,ns,q') acc)
	  (all_delta m.delta q) acc)
      m m.s EdgeSet.empty  
  
  let outgoing m q = 
    Hashtbl.fold 
      (fun q' ns acc -> EdgeSet.add (q,ns,q') acc)
      (all_delta m.delta q)
      EdgeSet.empty  
      
  let eps_eliminate m =
    let qi,qf = 0,1 in 
    let m' = new_nfa_states qi qf in 
    let h_eps = Hashtbl.create 17 in 
    let h_r = Hashtbl.create 17 in 
    let () = 
      Hashset.iter 
	(fun q -> 
	  let qs = eps_closure m q in 
	  Hashtbl.add h_eps q qs;
	  if q = m.s then 
	    Hashtbl.add h_r qs qi 
	  else if StateSet.mem m.f qs then 
	    let r = new_state m' in 
	    Hashtbl.add h_r qs r;
	    add_trans m' r Epsilon qf
	  else if not (Hashtbl.mem h_r qs) then 
	    let r = new_state m' in 
	    Hashtbl.add h_r qs r)
	m.q in 
    let () = 
      forward_fold_nfa 
	(fun q () -> 
	  let qs = Hashtbl.find h_eps q in 
	  let r = Hashtbl.find h_r qs in 
	  StateSet.iter
	    (fun qi -> 
	      Hashtbl.iter
		(fun q' ns -> 	      
		  let qs' = Hashtbl.find h_eps q' in 
		  let r' = Hashtbl.find h_r qs' in 
		  add_set_trans m' r ns r')
		(all_delta m.delta qi))
	    qs)
	m m.s () in 
    m'

  let subseteq = nfa_subseteq

  let regex_to_t r = 
    let create () = new_nfa_states 0 1 in 
    let rec loop r = 
      match r with 
      | Char n -> 
        let m = create () in 
        add_trans m m.s (Character n) m.f;
        m
      | Alt(r1,r2) ->
        union (loop r1) (loop r2)
      | Cat(r1,r2) ->
        simple_concat (loop r1) (loop r2)
      | Kleene(r) -> 
        let m = loop r in 
        add_trans m m.s Epsilon m.f;
        add_trans m m.f Epsilon m.s;
        m
      | Empty ->
        create () in 
        let m = eps_eliminate (loop r) in 
        elim_dead_states m; 
        m
end

module SwitchMap = Map.Make(struct
  type t = VInt.t
  let compare = Pervasives.compare
end)

module EdgeSet = Set.Make(struct
  type t = NFA.state * pchar * NFA.state
  let compare = Pervasives.compare
end)

let switch_policies_to_policy (sm : policy SwitchMap.t) : policy =
  SwitchMap.fold (fun sw p acc -> NetKAT_Types.(Par(acc, p)))
    sm NetKAT_Types.drop

let regex_to_switch_policies (r : regex) : policy SwitchMap.t =
  let (aregex, chash) = regex_to_aregex r in
  let auto = NFA.regex_to_t aregex in

  let switch_of_pchar (_, (sw, _, _, _)) = sw in

  let add (sw : VInt.t) e (m : EdgeSet.t SwitchMap.t) =
    try
       SwitchMap.add sw (EdgeSet.add e (SwitchMap.find sw m)) m
    with Not_found ->
      SwitchMap.add sw (EdgeSet.singleton e) m in

  let add_all ((q, ns, q') : NFA.edge) (m : EdgeSet.t SwitchMap.t) =
    Hashtbl.fold (fun i () acc ->
      let pchar = Hashtbl.find chash i in
      add (switch_of_pchar pchar) (q, pchar, q') acc)
    ns m in

  let to_edge_map (m : NFA.t) : EdgeSet.t SwitchMap.t =
    let open Nfa in 
    forward_fold_nfa (fun q acc ->
      Hashtbl.fold (fun q' ns acc -> 
        add_all (q,ns,q') acc)
      (all_delta m.delta q) acc)
    m m.s SwitchMap.empty in

  let to_lf_policy (q, (lf_p, l), q') : lf_policy = 
    let ingress =
        Seq(
          Filter(NetKAT_Types.(Test(SDN_Headers.Header SDN_Types.Vlan, VInt.Int16 q))),
          Filter(NetKAT_Types.(Test(SDN_Headers.Switch, switch_of_pchar (lf_p, l))))) in
    let egress = 
        Mod(SDN_Headers.Header(SDN_Types.Vlan), VInt.Int16 q') in
    Seq(ingress, Seq(lf_p, egress)) in

  let edges_to_lf_policy (es : EdgeSet.t) : lf_policy =
    let start = EdgeSet.choose es in
    EdgeSet.fold (fun e acc -> 
      Par(to_lf_policy e, acc))
    (EdgeSet.remove start es) (to_lf_policy start) in

  let edge_map = to_edge_map auto in

  SwitchMap.map 
    (fun es -> lf_policy_to_policy (edges_to_lf_policy es)) 
    edge_map

let compile (p : policy) : policy SwitchMap.t =
  regex_to_switch_policies (regex_of_policy p)
