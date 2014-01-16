open NetKAT_Types

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

let lift_maybe2 f ma mb =
  match ma, mb with
    | None  , None   -> None
    | None  , Some b -> Some b
    | Some a, None   -> Some a
    | Some a, Some b -> Some (f a b)

let flip f a b = f b a

let foldl1_map (f : 'b -> 'b -> 'b) (g : 'a -> 'b) (xs : 'a list) : 'b =
  List.(fold_left (fun acc e -> f acc (g e))
    (g (hd xs)) (tl xs))

(* END GENERIC HELPER FUNCTIONS --------------------------------------------- *)


(* BEGIN TOPOLOGY ----------------------------------------------------------- *)

(* The different sorts of things a port on a switch can be connected to *)
type port_dst =
  | SwitchPort of SDN_Types.switchId * VInt.t
  | Outside

module SwitchMap = Map.Make(struct
  type t = SDN_Types.switchId
  let compare = Pervasives.compare
end)

module PortMap = Map.Make(struct
  type t = VInt.t
  let compare = Pervasives.compare
end)

(* XXX: Fix this name clash *)
type portmap = port_dst PortMap.t

(* A topology maps switches to a portmap *)
type topology = portmap SwitchMap.t

(* For when a port's connected too to many things *)
exception Inconsistent_topology

let merge_pt_dst _ =
  lift_maybe2
    (fun a b -> if a = b then a else raise Inconsistent_topology)

let merge_topologies _ =
  lift_maybe2
    (fun pt_m1 pt_m2 -> PortMap.merge merge_pt_dst pt_m1 pt_m2)

(* END   TOPOLOGY ----------------------------------------------------------- *)


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


type 'a aregex =
  | Char of 'a
  | Pick of 'a aregex * 'a aregex
  | Alt of 'a aregex * 'a aregex
  | Cat of 'a aregex * 'a aregex
  | Kleene of 'a aregex
  | Empty

let rec fmap_aregex (f : 'a -> 'b) (r : 'a aregex) : 'b aregex =
  match r with
    | Char(c) -> Char(f c)
    | Pick(r1, r2) -> Pick(fmap_aregex f r1, fmap_aregex f r2)
    | Alt(r1, r2) -> Alt(fmap_aregex f r1, fmap_aregex f r2)
    | Cat(r1, r2) -> Cat(fmap_aregex f r1, fmap_aregex f r2)
    | Kleene(s) -> Kleene(fmap_aregex f s)
    | Empty -> Empty

(* This is a "policy" character for use in the regular expression above. *)
type pchar = lf_policy * topology

(* A regular expression over link-free policy, link pairs. *)
type regex = pchar aregex

(* END DATA TYPES ----------------------------------------------------------- *)


(* BEGIN TO POLICY ---------------------------------------------------------- *)

let lf_policy_to_policy (lfp : lf_policy) : policy =
  let rec lf_policy_to_policy_k lfp k =
    match lfp with
      | Filter(p) -> k (NetKAT_Types.Filter(p))
      | Mod(h, v) -> k (NetKAT_Types.Mod(h, v))
      | Par(p1, p2) ->
        lf_policy_to_policy_k p1 (fun p1' ->
        lf_policy_to_policy_k p2 (fun p2' ->
          k (NetKAT_Types.Par(p1', p2'))))
      | Choice(p1, p2) ->
        lf_policy_to_policy_k p1 (fun p1' ->
        lf_policy_to_policy_k p2 (fun p2' ->
          k (NetKAT_Types.Choice(p1', p2'))))
      | Seq(p1, p2) ->
        lf_policy_to_policy_k p1 (fun p1' ->
        lf_policy_to_policy_k p2 (fun p2' ->
          k (NetKAT_Types.Seq(p1', p2'))))
      | Star(p) ->
        lf_policy_to_policy_k p (fun p' ->
          k (NetKAT_Types.Star(p')))
  in lf_policy_to_policy_k lfp (fun x -> x)

let topology_to_policy (topo : topology) : policy =
  SwitchMap.fold (fun sw1 pt_m acc ->
    PortMap.fold (fun pt1 dst acc ->
      let link = match dst with
        | SwitchPort (sw2, pt2) -> NetKAT_Types.Link(sw1, pt1, sw2, pt2)
        | Outside -> failwith "No policy representation for Outside entities" in
      if acc = NetKAT_Types.drop
        then link
        else NetKAT_Types.Par(link, acc))
    pt_m acc)
  topo NetKAT_Types.drop

let rec regex_to_policy (r : regex) : policy =
  match r with
    | Char(lfp, topo) ->
      NetKAT_Types.Seq(lf_policy_to_policy lfp, topology_to_policy topo)
    | Pick(r1, r2) ->
      NetKAT_Types.Choice(regex_to_policy r1, regex_to_policy r2)
    | Alt(r1, r2) ->
      NetKAT_Types.Par(regex_to_policy r1, regex_to_policy r2)
    | Cat(r1, r2) ->
      NetKAT_Types.Seq(regex_to_policy r1, regex_to_policy r2)
    | Kleene(r) ->
      NetKAT_Types.Star(regex_to_policy r)
    | Empty -> NetKAT_Types.Filter(NetKAT_Types.False)

let regex_to_string (r : regex) : string =
  NetKAT_Pretty.string_of_policy (regex_to_policy r)

let lf_policy_to_string (lf_p : lf_policy) : string =
  NetKAT_Pretty.string_of_policy (lf_policy_to_policy lf_p)

(* END TO POLICY ------------------------------------------------------------ *)


(* BEGIN OF POLICY ---------------------------------------------------------- *)

module TRegex = struct
  (* This is a "policy-link" character for regex. *)
  type pl_char =
    | PChar of lf_policy
    | TChar of lf_policy option * topology

  type tregex = pl_char aregex

  let of_policy (p : policy) : tregex =
    let rec of_policy_k p k =
      begin match p with
        | NetKAT_Types.Filter(q) ->
          k (Char(PChar(Filter q)))
        | NetKAT_Types.Mod(h, v) ->
          k (Char(PChar(Mod(h, v))))
        | NetKAT_Types.Link(sw1, pt1, sw2, pt2) ->
          let dst = SwitchPort(sw2, pt2) in
          let topo = SwitchMap.singleton sw1 (PortMap.singleton pt1 dst) in
          k (Char(TChar(None, topo)))
        | NetKAT_Types.Seq(p1, p2) ->
          of_policy_k p1 (fun p1' ->
          of_policy_k p2 (fun p2' ->
            k (begin match p1', p2' with
              | Char(PChar(lfp1)), Char(PChar(lfp2)) ->
                Char(PChar(Seq(lfp1, lfp2)))
              | Char(PChar(lfp1)), Char(TChar(mlfp2, sw)) ->
                let mlfp = optional lfp1 (fun x -> Seq(lfp1,x)) mlfp2 in
                Char(TChar(Some(mlfp), sw))
              | _, _ -> Cat(p1', p2')
            end)))
        | NetKAT_Types.Par(p1, p2) ->
          of_policy_k p1 (fun p1' ->
          of_policy_k p2 (fun p2' ->
            k (begin match p1', p2' with
              | Char(PChar(lfp1)), Char(PChar(lfp2)) ->
                Char(PChar(Par(lfp1, lfp2)))
              | Char(TChar(None, topo1)), Char(TChar(None, topo2)) ->
                let topo3 = SwitchMap.merge merge_topologies topo1 topo2 in
                Char(TChar(None, topo3))
              | _ , _ -> Alt(p1', p2')
            end)))
        | NetKAT_Types.Choice(p1, p2) ->
          of_policy_k p1 (fun p1' ->
          of_policy_k p2 (fun p2' ->
            k (begin match p1', p2' with
              | Char(PChar(lfp1)), Char(PChar(lfp2)) ->
                Char(PChar(Choice(lfp1, lfp2)))
              | _ , _ -> Pick(p1', p2')
            end)))
        | NetKAT_Types.Star(q) ->
          of_policy_k q (fun q' ->
            k (begin match q' with
              | Char(PChar(lfq)) -> Char(PChar(Star(lfq)))
              | _ -> Kleene(q')
            end))
      end
    in of_policy_k p (fun x -> x)

  let rec to_policy (r : tregex) : policy =
    begin match r with
      | Char(PChar(lfp)) ->
        lf_policy_to_policy lfp
      | Char(TChar(mlfp, topo)) ->
        let links = topology_to_policy topo in
        optional links (fun x -> NetKAT_Types.Seq(lf_policy_to_policy x, links)) mlfp
      | Pick(r1, r2) ->
        NetKAT_Types.Choice(to_policy r1, to_policy r2)
      | Alt(r1, r2) ->
        NetKAT_Types.Par(to_policy r1, to_policy r2)
      | Cat(r1, r2) ->
        NetKAT_Types.Seq(to_policy r1, to_policy r2)
      | Kleene(r) ->
        NetKAT_Types.Star(to_policy r)
      | Empty -> NetKAT_Types.Filter(NetKAT_Types.False)
    end
end

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

and link_provider = lf_policy option -> inter
and link_consumer = cstr -> lf_policy option -> link_provider option -> inter

let seq (p : lf_policy) (q : lf_policy) : lf_policy = Seq(p, q)
let par (p : lf_policy) (q : lf_policy) : lf_policy = Par(p, q)
(* let pick (p : lf_policy) (q : lf_policy) : lf_policy = Choice(p, q) *)

let mk_mcstr (c : cstr) mp q =
  optional q (fun p -> c p q) mp

let mk_mmcstr (c : cstr) mp mq =
  match mq with
    | None -> mp
    | Some q -> Some(mk_mcstr c mp q)

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
      | Some lp -> lp (Some(lfp'))

(* Constructor for link_provider. Requires a topology and an optional link-free
 * policy that acts as an accumulator. If the link provider receives a `None`,
 * it will transition to a regex. If it receives `Some lfp` then it will
 * continue taking policies.
 *)
let rec mk_tp (macc : lf_policy option) (topo : topology) : link_provider =
  fun mlf_p ->
    match mlf_p with
      | None -> 
        S(Char(from_option (Filter(NetKAT_Types.True)) macc, topo))
      | Some lf_p ->
        (* print_string ("mk_tp with lf_p: " ^ (lf_policy_to_string lf_p) ^ "\n"); *)
        TP(mk_tp (Some(mk_mcstr (flip seq) macc lf_p)) topo)

(* Turn an optional link_provider into an inter. Call in the case where
 * a link is needed, and link-free policies can no longer be accepted. The inter
 * this function produces will ensure that it never receives further link-free
 * policies, and will continue to produce an NL continuation until some context
 * provides it with a valid (non-None) link-provider.
 *)
let rec mk_inter_of_mlp (mlp : link_provider option) : inter =
  match mlp with
    | None    -> NL(fun _ mp mlp -> assert (is_none mp); mk_inter_of_mlp mlp)
    | Some lp -> lp None

let regex_of_policy (p: policy) : regex =
  let open TRegex in

  let rec rpc (tregex : tregex) : inter =
    (* print_string ((Types.string_of_policy p) ^ "\n"); *)
    begin match tregex with
      | Char(PChar(lfp)) ->
        NL(mk_nl lfp)
      | Char(TChar(mlfp, sw)) ->
        TP(mk_tp mlfp sw)
      | Cat(p1, p2) ->
        rpc_seq (rpc p1) (rpc p2)
      | Alt(p1, p2) ->
        rpc_branchy (fun p q -> Alt(p, q)) (rpc p1) (rpc p2)
      | Pick(p1, p2) ->
        rpc_branchy (fun p q -> Pick(p, q)) (rpc p1) (rpc p2)
      | Kleene(q) ->
        S(Kleene(run (rpc q)))
      | Empty -> S(Empty)
    end

  and rpc_seq i j =
    (* The following match impelements this "truth table" for the inter type:
     *
     *    a  | b  | a ; b
     *   ----+----+-------
     *    TP | TP | TP
     *    TP | NL | NL
     *    TP | S  | TP
     *    NL | TP | TP
     *    NL | NL | NL
     *    NL | S  | <<error>>
     *    S  | TP | S
     *    S  | NL | NL
     *    S  | S  | S
     *
     *  Note that in the NL, TP case below, calling f1 with None as the second
     *  argument will force a transition to TP, thus satisfying the truth table
     *  above.
     * *)
    match i, j with
      | TP f1, NL f2 ->
        NL(fun c mlf_p mlp -> rpc_seq (f1 mlf_p) (f2 c None mlp))
      | TP f1, _     ->
        let r = run j in TP(fun mp -> rpc_seq (f1 mp) (S(r)))
      | NL f1, TP f2 -> f1 seq None (Some(f2))
      | NL f1, NL f2 ->
        NL(fun c mlf_p mlp ->
           let c' x y = mk_mcstr c mlf_p (seq x y) in
           f1 seq None (Some(fun mlf_q -> f2 c' mlf_q mlp)))
      | NL _ , S _   -> failwith "Cat(NL, S) can't be represented"
      | S  r , _     -> s_trans r j (fun x y -> Cat(x, y))

  and rpc_branchy cr i j =
    (* The following match impelements this "truth table" for the inter type:
     *
     *    a  | b  | a U/+ b
     *   ----+----+---------
     *    TP | TP | TP
     *    TP | NL | NL
     *    TP | S  | S
     *    NL | TP | NL
     *    NL | NL | NL
     *    NL | S  | NL
     *    S  | TP | S
     *    S  | NL | NL
     *    S  | S  | S
     * *)
    begin match i, j with
      | TP f1, TP f2 -> TP(fun mlp -> rpc_branchy cr (f1 mlp) (f2 mlp))
      | TP f1, NL f2 -> NL(fun c mlf_p mlp ->
                            rpc_branchy cr
                                (rpc_seq (f1 mlf_p) (mk_inter_of_mlp mlp))
                                (f2 c mlf_p mlp))
      | TP f1, S  r  -> S(cr (run i) r)
      | NL f1, TP f2 -> NL(fun c mlf_p mlp ->
                            rpc_branchy cr
                                (f1 c mlf_p mlp)
                                (rpc_seq (f2 mlf_p) (mk_inter_of_mlp mlp)))
      | NL f1, NL f2 ->
        NL(fun c mlf_p mlp ->
           rpc_branchy cr
             (f1 c mlf_p mlp)
             (f2 c mlf_p mlp))
      | NL f1, S  r  -> s_trans r i (flip cr)
      | S   r, _     -> s_trans r j cr
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
      | TP f -> f mp
      | NL f -> f seq mp mlp
      | S  r -> S(r)
    end

  and run (i : inter) : regex =
    begin match run_with i None None with
      | TP f -> failwith "shouldn't happen"
      | NL _ -> failwith "need a link in there"
      | S  r -> r
    end in

  run (rpc (TRegex.of_policy p))

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

  (* copypasta'd from dprle, with modifications *)
  let eps_closure_upto (pred : state -> bool) (n : nfa) (q : state) : stateset =
    let visited = ref StateSet.empty in
    let rec walk (queue : state list) : unit = match queue with
      | x::xs when (not (StateSet.mem x !visited)) ->
        let to_enqueue = if pred x
            then Hashset.to_list (which_states ~create:false n.epsilon x)
            else [] in
	      visited := StateSet.add x !visited;
	      walk (List.rev_append xs to_enqueue)
      | x::xs -> walk xs
      | _ -> ()
    in
      walk [q];
      !visited
      
  let eps_eliminate m is_pick_state =
    (* Printf.printf "--- EPS_ELIMINATE ---\n%s\n" (Nfa.nfa_to_dot m); *)
    (* NOTE(seliopou): The initial state should not be 0 or 1! Code below does not
     * directly depend on this, but it does make debugging it much easier when
     * you can assume that the start state will never be 0 or 1. That way, you can
     * assume packets outside of the network are on vlan 1 and you don't have to
     * renumber states.
     * *)
    let qi,qf = 2,3 in
    let m' = new_nfa_states qi qf in 
    (* epsilon closure cache *)
    let h_eps = Hashtbl.create 17 in 
    (* maps sets of epsilon-closed states to new automaton states *)
    let h_r = Hashtbl.create 17 in 
    (* new automaton pick states *)
    let h_pick = Hashtbl.create 5 in 
    (* helper function: convert old state to epsilon closure and new state *)
    let lookup_state qi = 
      let qsi = Hashtbl.find h_eps qi in 
      (qsi, Hashtbl.find h_r qsi) in 
    (* Phase I: initialize new automaton states *)
    Hashset.iter 
      (fun q ->
	let qs = 
	  if is_pick_state q then 
	    eps_closure_upto is_pick_state m q
	  else 
	    eps_closure_upto (fun q' -> not (is_pick_state q')) m q in 
	Hashtbl.add h_eps q qs;
	if q = m.s then 
	  Hashtbl.add h_r qs qi
	else if not (Hashtbl.mem h_r qs) then
          Hashtbl.add h_r qs (new_state m')
	else ())
      m.q;
    (* Phase II: initialize new automaton transitions *)
    forward_fold_nfa (fun q () ->
      let qs,r = lookup_state q in 
      if StateSet.mem m.f qs then 
	add_trans m' r Epsilon qf;
      if is_pick_state q then 
	(* Case: q is a pick node *)
	begin 
	  Hashtbl.add h_pick r ();
	  StateSet.iter 
	    (fun q' -> 
	      let _,r' = lookup_state q' in 
	      add_trans m' r Epsilon r')
	    (StateSet.remove q qs)
	end
      else 
        (* Case: q is not a pick node *)
        StateSet.iter
          (fun qi ->
            (if is_pick_state qi then
              let _,ri' = lookup_state qi in
              add_trans m' r Epsilon ri'
            else ());

          Hashtbl.iter
	      (fun q' ns ->
		let qs',r' = lookup_state q' in
		add_set_trans m' r ns r';
		StateSet.iter
		  (fun qi' ->
		    if is_pick_state qi' then
		      let _,ri' = lookup_state qi' in
		      add_set_trans m' r ns ri')
		  qs')
	      (all_delta m.delta qi))
	  qs)
      m m.s ();
    (* Final result *)
    (m', h_pick)
      
  let subseteq = nfa_subseteq

  let regex_to_t r = 
    let create () = new_nfa_states 0 1 in 

    let update s m = Hashtbl.iter (fun q r ->
      Hashtbl.remove  s q;
      Hashtbl.replace s r ()) m; s in

    let combine op (m, lhs) (n, rhs) =
      let (ml, mr, m') = op m n lhs rhs in
      let lhs' = update lhs ml in
      let rhs' = update rhs mr in
        Hashtbl.iter (Hashtbl.replace lhs') rhs';
        (m', lhs') in

    let rec loop r =
      match r with
      | Char n ->
        let m = create () in 
        add_trans m m.s (Character n) m.f;
        (m, Hashtbl.create 7)
      | Pick(r1, r2) ->
        let (m, pick_states) = combine union (loop r1) (loop r2) in
          Hashtbl.add pick_states m.s ();
          (m, pick_states)
      | Alt(r1,r2) ->
         combine union (loop r1) (loop r2)
      | Cat(r1,r2) ->
         combine concat (loop r1) (loop r2)
      | Kleene(r) -> 
        let (m, pick_states) = loop r in
        add_trans m m.s Epsilon m.f;
        add_trans m m.f Epsilon m.s;
        (m, pick_states)
      | Empty ->
        (create (), Hashtbl.create 7) in
    let (m, pick_states) = loop r in
    let m', pick_states' = eps_eliminate m (Hashtbl.mem pick_states) in
    elim_dead_states m';
    (m', pick_states')
end

module EdgeSet = Set.Make(struct
  type t = NFA.state * (NetKAT_Types.policy * topology) * NFA.state
  let compare = Pervasives.compare
end)


type 'a dehopified = 'a * ('a SwitchMap.t) * topology * 'a

let regex_to_switch_policies (r : regex) : policy dehopified =
  let (aregex, chash) = regex_to_aregex r in
  let (auto, pick_states) = NFA.regex_to_t aregex in

  (* Used to compress state space to sequential integers. Note that the state 0
   * is never used (unless there's an overflow ;) Note that for debugging
   * purposes you should change convert to be the identity function. *)
  let curq = ref 2 in
  let qmap = Hashtbl.create (Hashset.size Nfa.(auto.q)) in
  let convert q =
    try Hashtbl.find qmap q with Not_found ->
      Hashtbl.replace qmap q !curq;
      incr curq;
      (!curq - 1) in

  let add_all ((q, ns, q') : NFA.edge) (m : EdgeSet.t SwitchMap.t) =
    Hashtbl.fold (fun i () acc ->
      let lfp, topo = Hashtbl.find chash i in
      let p = lf_policy_to_policy lfp in
      let edge = (q, (p, topo), q') in
      SwitchMap.merge (fun _ -> lift_maybe2 EdgeSet.union)
        (SwitchMap.map (fun _ -> EdgeSet.singleton edge) topo) acc)
    ns m in

  let to_edge_map (m : NFA.t) : EdgeSet.t SwitchMap.t =
    let open Nfa in 
    forward_fold_nfa (fun q acc ->
      Hashtbl.fold (fun q' ns acc -> 
        add_all (q,ns,q') acc)
      (all_delta m.delta q) acc)
    m m.s SwitchMap.empty in

  let mk_test q = NetKAT_Types.Test(Header SDN_Types.Vlan, VInt.Int16 (convert q)) in
  let mk_filter q = NetKAT_Types.Filter(mk_test q) in
  let mk_mod q = NetKAT_Types.Mod(Header SDN_Types.Vlan, VInt.Int16 (convert q)) in
  let mk_choice qs = foldl1_map (fun p q -> NetKAT_Types.Choice(p, q)) mk_mod qs in

  let topology = ref SwitchMap.empty in

  let to_policy sw (q, (p, topo), q') : policy =
    (* Printf.printf "Working on q%d -> q%d\n" q q'; *)
    topology := SwitchMap.merge merge_topologies topo !topology;
    let pt_m = SwitchMap.find sw topo in

    let ports_f = foldl1_map (fun p q -> NetKAT_Types.Par(p, q))
        (fun (pt, _) -> NetKAT_Types.Filter(Test(Header SDN_Types.InPort, pt)))
      (PortMap.bindings pt_m) in
    let switch_f = NetKAT_Types.Filter(Test(Switch, sw)) in

    let next_states = if Hashtbl.mem pick_states q'
      then Nfa.neighbors auto q'
      else [q'] in

    let ingress = NetKAT_Types.Seq(switch_f, mk_filter q) in
    let egress = NetKAT_Types.Seq(ports_f, mk_choice next_states) in

    NetKAT_Types.Seq(ingress, NetKAT_Types.Seq(p, egress)) in

  let edges_to_policy sw (es : EdgeSet.t) : policy =
    let start = EdgeSet.choose es in
    EdgeSet.fold (fun e acc ->
      NetKAT_Types.Par(acc, to_policy sw e))
    (EdgeSet.remove start es) (to_policy sw start) in

  (* All packets entering the network are on vlan 1. Detect these packets and
   * set their vlan to the initial state of the automaton. If the initial state
   * is not a choice state, then is straightforward (the second case). If the
   * initial state is a choice state, then the packet will immediately
   * transition to another non-choice state via an epsilon transition, by the
   * construction of the automaton. In this case, skip the choice state and
   * chose between the epsilon transition reachable states as the initial state.
   *
   * Packets that are not on vlan 1 are considered in the network and may pass.
   * *)
  let check_outside = NetKAT_Types.Test(Header SDN_Types.Vlan, VInt.Int16 1) in
  let ingress_mod =
    let open NFA.StateSet in
    let choice_closure = NFA.eps_closure_upto (Hashtbl.mem pick_states) auto in
    if Hashtbl.mem pick_states Nfa.(auto.s) then
      (* A choice node may be an initial state. In this case, all states that
       * are reachable by epsilon transitions from that initial states are
       * potential start states and the ingress policy must choose between them.
       * *)
      mk_choice (elements (choice_closure Nfa.(auto.s)))
    else
      (* The initial state may not be a choice node, but a choice node may be
       * reachable from the initial state by epsilon transitions. In that case,
       * find all the states that are reachable from those choice nodes via
       * epsilon transition. The ingress policy must choose between these, but
       * may also in parallel perform other character transitions from the
       * initial state.
       * *)
      let open List in
      let neighbor_qs = Nfa.(neighbors auto auto.s) in
      let choice_qs0 = filter (Hashtbl.mem pick_states) neighbor_qs in
      let choice_qs1 = concat (map (fun x -> elements (choice_closure x)) choice_qs0) in
      match choice_qs1 with
        | [] -> mk_mod Nfa.(auto.s)
        | _  -> if length neighbor_qs = length choice_qs0
                  then assert false (* If all the neighbors are choice nodes,
                                     * then the start node should just be a
                                     * choice node.
                                     * *)
                  else NetKAT_Types.Par(mk_choice choice_qs1, mk_mod Nfa.(auto.s)) in


  let ingress =
    NetKAT_Types.(Par(Seq(Filter(check_outside), ingress_mod),
               Seq(Filter(Neg(check_outside)), Filter(True)))) in

  (* Once a packet has reached a state that is backwards reachable from the
   * final state, it will immediately transition to the final state via an
   * epsilon transition, by the construction of the automaton. At that point,
   * the packet is exiting the network and is no longer subject to the policy,
   * so its vlan header should be set back to 1.
   *
   * Packets that are not on an egress vlan are considered still inthe network
   * and may pass.
   * *)
  let final_qs = Hashset.to_list
    (Hashtbl.find (Nfa.backward_mapping auto) auto.Nfa.f) in
  let go_outside = NetKAT_Types.Mod(Header SDN_Types.Vlan, VInt.Int16 1) in
  let egress_test = foldl1_map (fun x y -> NetKAT_Types.Or(x, y)) mk_test final_qs in
  let egress =
    NetKAT_Types.(Par(Seq(Filter(egress_test), go_outside),
               Seq(Filter(NetKAT_Types.Neg(egress_test)), Filter(NetKAT_Types.True)))) in

  let swpm = SwitchMap.mapi edges_to_policy (to_edge_map auto) in

  (* Printf.printf "%s\n" (regex_to_string r); *)
  (* Printf.printf "AUTO: %s\n" (Nfa.nfa_to_dot auto); *)
  (* Hashtbl.iter (fun i (lf_p, sw) -> *)
  (*   Printf.printf "%d: %s; %s\n" *)
  (*    i (lf_policy_to_string lf_p) (NetKAT_Pretty.string_of_policy (switch_to_policy
   *    sw))) *)
  (* chash; *)
  (ingress, swpm, !topology, egress)

let dehopify (p : policy) : policy dehopified =
  regex_to_switch_policies (regex_of_policy p)
