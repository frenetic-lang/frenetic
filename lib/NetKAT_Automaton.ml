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


(* A regular expression over link-free policy, link pairs. *)
type regex =
  | Char of lf_policy * link
  | Alt of regex * regex
  | Cat of regex * regex
  | Kleene of regex
  | Empty

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
      | TP f1, _     -> let r = run i in TP(fun c mp -> rpc_seq (f1 c mp) (S(r)))
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

(* END OF POLICY ------------------------------------------------------------ *)
