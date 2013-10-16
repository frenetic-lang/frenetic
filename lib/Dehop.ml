open NetKAT_Types
open SDN_Headers

type explicit_topo_pol =
  | Filter of pred
  | Mod of SDN_Types.field * header_val
    (* switch, port -> switch, port *)
  | Link of header_val*header_val*header_val*header_val
  | Par of explicit_topo_pol * explicit_topo_pol
  | Seq of explicit_topo_pol * explicit_topo_pol
  | Star of explicit_topo_pol

  (* i;(p;t)^*;p;e 
     where 
     i = t = v | t <- v | i + i | i ; i
     p = t = v | h = v | t <- v | h <- v | p + p | p ; p | p*
     t = (sw,pt) -> (sw',pt') | t + t
     e = t = v | i + i | i ; i
  *)

type vtag = int*int
type vheader = 
  | Field of SDN_Types.field
  | Tag of vtag

type virtual_pol =
  | VFilter of pred
  | VTest of vtag*header_val
  | VMod of vheader * header_val
    (* switch, port -> switch, port *)
  | VLink of header_val*header_val*header_val*header_val
  | VPar of virtual_pol * virtual_pol
  | VSeq of virtual_pol * virtual_pol
  | VStar of virtual_pol
      
type restricted_pol = virtual_pol * virtual_pol * virtual_pol * virtual_pol

module Formatting = struct

  open Format

  let header (fmt : formatter) (h : header) : unit = match h with
    | Header h' -> SDN_Types.format_field fmt h'
    | Switch -> pp_print_string fmt "switch"

  let vheader (fmt : formatter) (h : vheader) : unit = match h with
    | Field h' -> SDN_Types.format_field fmt h'
    | Tag (h,_) -> pp_print_string fmt (string_of_int h)

    (* The type of the immediately surrounding context, which guides parenthesis-
       intersion. *)
    (* JNF: YES. This is the Right Way to pretty print. *)
  type context = SEQ | PAR | STAR | NEG | PAREN

  let rec pred (cxt : context) (fmt : formatter) (pr : pred) : unit = 
    match pr with
      | True -> 
        fprintf fmt "@[true@]"
      | False -> 
        fprintf fmt "@[false@]"
      | (Test (h, v)) -> 
        fprintf fmt "@[%a = %a@]" header h VInt.format v
      | Neg p' -> 
        begin match cxt with
          | PAREN -> fprintf fmt "@[!%a@]" (pred NEG) p'
          | _ -> fprintf fmt "@[!@[(%a)@]@]" (pred PAREN) p'
        end
      | Or (p1, p2) -> 
        begin match cxt with
          | PAREN
          | PAR -> fprintf fmt "@[%a + %a@]" (pred PAR) p1 (pred PAR) p2
          | _ -> fprintf fmt "@[(@[%a + %a@])@]" (pred PAR) p1 (pred PAR) p2
        end
      | And (p1, p2) -> 
        begin match cxt with
          | PAREN
          | SEQ
          | PAR -> fprintf fmt "@[%a ; %a@]" (pred SEQ) p1 (pred SEQ) p2
          | _ -> fprintf fmt "@[(@[%a ; %a@])@]" (pred SEQ) p1 (pred SEQ) p2
        end

  let rec vpol (cxt : context) (fmt : formatter) (p : virtual_pol) : unit =
    match p with
      | VFilter pr -> 
        pred cxt fmt pr
      | VTest (h,v) -> fprintf fmt "@[%a = %a@]" vheader (Tag h) VInt.format v
      | VMod (h, v) -> 
        fprintf fmt "@[%a <- %a@]" vheader h VInt.format v
      | VStar p' -> 
        begin match cxt with
          | PAREN 
	  | STAR ->  fprintf fmt "@[%a*@]" (vpol STAR) p' 
          | _ -> fprintf fmt "@[@[(%a)*@]@]" (vpol PAREN) p'
        end
      | VLink (sw,pt,sw',pt') ->
        let fmter = VInt.format in
        fprintf fmt "@[(%a@@%a) -> (%a@%a)@]" fmter sw fmter pt fmter sw' fmter pt'
      | VPar (p1, p2) -> 
        begin match cxt with
          | PAREN
          | PAR -> fprintf fmt "@[%a + %a@]" (vpol PAR) p1 (vpol PAR) p2
          | _ -> fprintf fmt "@[(@[%a + %a@])@]" (vpol PAR) p1 (vpol PAR) p2
        end
      | VSeq (p1, p2) -> 
        begin match cxt with
          | PAREN
          | SEQ
          | PAR -> fprintf fmt "@[%a ; %a@]" (vpol SEQ) p1 (vpol SEQ) p2
          | _ -> fprintf fmt "@[(@[%a ; %a@])@]" (vpol SEQ) p1 (vpol SEQ) p2
        end

  let rec epol (cxt : context) (fmt : formatter) (p : explicit_topo_pol) : unit =
    match p with
      | Filter pr -> 
        pred cxt fmt pr
      | Mod (h, v) -> 
        fprintf fmt "@[%a <- %a@]" SDN_Types.format_field h VInt.format v
      | Link (sw,pt,sw',pt') -> 
        let fmter = VInt.format in
        fprintf fmt "@[(%a@@%a) -> (%a@@%a)@]" fmter sw fmter pt fmter sw' fmter pt'
      | Star p' -> 
        begin match cxt with
          | PAREN 
	  | STAR ->  fprintf fmt "@[%a*@]" (epol STAR) p' 
          | _ -> fprintf fmt "@[@[(%a)*@]@]" (epol PAREN) p'
        end
      | Par (p1, p2) -> 
        begin match cxt with
          | PAREN
          | PAR -> fprintf fmt "@[%a + %a@]" (epol PAR) p1 (epol PAR) p2
          | _ -> fprintf fmt "@[(@[%a + %a@])@]" (epol PAR) p1 (epol PAR) p2
        end
      | Seq (p1, p2) -> 
        begin match cxt with
          | PAREN
          | SEQ
          | PAR -> fprintf fmt "@[%a ; %a@]" (epol SEQ) p1 (epol SEQ) p2
          | _ -> fprintf fmt "@[(@[%a ; %a@])@]" (epol SEQ) p1 (epol SEQ) p2
        end
end

let make_string_of formatter x =
  let open Format in
      let buf = Buffer.create 100 in
      let fmt = formatter_of_buffer buf in
      pp_set_margin fmt 80;
      formatter fmt x;
      fprintf fmt "@?";
      Buffer.contents buf

let string_of_vint = make_string_of VInt.format 

let string_of_header = make_string_of Formatting.header

let format_vpolicy = Formatting.vpol Formatting.PAREN
let format_epolicy = Formatting.epol Formatting.PAREN

let string_of_vpolicy = make_string_of format_vpolicy
let string_of_epolicy = make_string_of format_epolicy

let vheader_count = ref 0

let gen_header size =
  incr vheader_count;
  (!vheader_count, size)

  (* Compilation story: we have an unlimited number of header fields we
     can allocate on demand. Each header field has a specific number of
     entries. At a later stage, we inject the multiple header fields into a
     single header that allows arbitrary bitmasking. There should be some
     analysis/optimizations we can do (ala the earlier slices compiler) to
     reduce the number of unique entries required.

     Alternatively, lacking a bitmaskable field, we could simply expand
     all possible combinations of values and match/set them
     appropriately. We'd need some pretty good analysis to keep this
     from exploding.
  *)
let rec seqList ls =
  match ls with
    | [] -> VFilter True
    | l :: ls -> VSeq(l, seqList ls)

let rec parList ls =
  match ls with
    | [] -> VFilter False
    | l :: ls -> VPar(l, parList ls)


let rec dehopify (p : explicit_topo_pol) : restricted_pol =
  match p with
    | Filter pr -> 
      let h = gen_header 2 in
      let h0 = VInt.Int16 0 in
      let h1 = VInt.Int16 1 in
      VMod(Tag h,h0), 
      seqList [VTest(h,h0);
               VFilter pr;
               VMod(Tag h,h1)],
      VFilter False, 
      VTest(h,h1)
    | Mod (h', v) -> 
      let h = gen_header 2 in
      let h0 = VInt.Int16 0 in
      let h1 = VInt.Int16 1 in
      VMod(Tag h,h0), 
      seqList [VTest(h,h0);
               VMod(Field h',v);
               VMod(Tag h,h1)],
      VFilter False, 
      VTest(h,h1)
    | Link (sw1,p1,sw2,p2) -> 
      let h = gen_header 3 in
      let h0 = VInt.Int16 0 in
      let h1 = VInt.Int16 1 in
      let h2 = VInt.Int16 2 in
      VMod(Tag h,h0),
      VPar(seqList [VTest(h,h0);
                    VFilter(Test(Switch, sw1));
                    VFilter(Test(Header SDN_Types.InPort, p1));
                    VMod(Tag h,h1)],
           seqList [VTest(h,h1);
                    VFilter(Test(Switch, sw2));
                    VFilter(Test(Header SDN_Types.InPort, p2));
                    VMod(Tag h,h2)]),
      VLink(sw1,p1,sw2,p2),
      VTest(h,h1)
    | Par (p,q) -> 
      let i_p,s_p,t_p,e_p = dehopify p in
      let i_q,s_q,t_q,e_q = dehopify q in
      let h = gen_header 2 in
      let h0 = VInt.Int16 0 in
      let h1 = VInt.Int16 1 in
      VPar(VSeq(VMod(Tag h,h0), i_p),
           VSeq(VMod(Tag h,h1), i_q)),
      VPar(VSeq(VTest(h,h0),s_p),
           VSeq(VTest(h,h1),s_q)),
      VPar(t_p,t_q),
      VPar(VSeq(VTest(h,h0), e_p),
           VSeq(VTest(h,h1), e_q))
    | Seq (p,q) -> 
      let i_p,s_p,t_p,e_p = dehopify p in
      let i_q,s_q,t_q,e_q = dehopify q in
      let h = gen_header 8 in
      let h0 = VInt.Int16 0 in
      let h1 = VInt.Int16 1 in
      let h1' = VInt.Int16 2 in
      let h2 = VInt.Int16 3 in
      let h2' = VInt.Int16 4 in
      let h3 = VInt.Int16 5 in
      let h3' = VInt.Int16 6 in
      let h3'' = VInt.Int16 7 in
      let h4 = VInt.Int16 8 in
      VSeq(parList [VMod(Tag h,h0);
                    VMod(Tag h,h1);
                    VMod(Tag h,h2);
                    VMod(Tag h,h3)],
           i_p),
      parList [seqList [VTest(h,h0);
                        s_p;
                        e_p;
                        i_q;
                        s_q;
                        VMod(Tag h,h4)];
               seqList [VTest(h,h1);
                        s_p;
                        VPar(VFilter True,
                             VMod(Tag h,h1'))];
               seqList [VTest(h,h1');
                        s_p;
                        e_p;
                        i_q;
                        s_q;
                        VMod(Tag h,h4)];
               seqList [VTest(h,h2);
                        s_p;
                        e_p;
                        i_q;
                        s_q;
                        VPar(VMod(Tag h,h2'),
                             VMod(Tag h, h4))];
               VSeq (VTest(h,h2'), s_q);
               seqList [VTest(h,h3);
                        s_p;
                        VMod(Tag h,h3')];
               seqList [VTest(h,h3');
                        s_p;
                        VPar(VFilter True,
                             seqList [e_p;
                                      i_q;
                                      s_q;
                                      VPar(VMod(Tag h, h3''),
                                           VMod(Tag h, h4))])];
               seqList [VTest(h,h3'');
                        s_q;
                        VPar(VFilter True,
                             VMod(Tag h, h4))]],
      VPar(t_p,t_q),
      VSeq(VTest(h, h4), e_q)
    | Star p -> 
      let i_p,s_p,t_p,e_p = dehopify p in
      let h = gen_header 7 in
      let h0 = VInt.Int16 0 in
      let h1 = VInt.Int16 1 in
      let h2 = VInt.Int16 2 in
      let h2' = VInt.Int16 3 in
      let h2'' = VInt.Int16 4 in
      let h3 = VInt.Int16 5 in
      let h4 = VInt.Int16 6 in
      parList [VMod(Tag h,h0);
               VMod(Tag h,h1);
               VMod(Tag h,h2)],
      parList [ VSeq(VTest(h,h0),
                     VMod(Tag h,h3));
                seqList [VTest(h,h1);
                         i_p;
                         s_p;
                         VStar(seqList [e_p;i_p;s_p]);
                         VMod(Tag h,h4)];
                seqList [VTest(h,h2);
                         i_p;
                         s_p;
                         VStar(seqList [e_p;i_p;s_p]);
                         VPar(VMod(Tag h,h2'),
                              VMod(Tag h,h2''))];
                seqList [VTest(h,h2');
                         s_p;
                         VPar(VMod(Tag h,h2'),
                              VMod(Tag h,h2''))];
                seqList [VTest(h,h2'');
                         s_p;
                         VStar(seqList [e_p;i_p;s_p]);                                     
                         VPar(VMod(Tag h,h2'),
                              VMod(Tag h,h2''))]],
      t_p,
      VPar(VSeq(VPar(VTest(h,h2'),
                     VPar(VTest(h,h2''),
                          VTest(h,h4))),
                e_p),
           VTest(h,h3))

  (* Optimizations Observation: Virtual headers are semantically
     meaningless if they aren't matched in the NetKAT code itself. Thus,
     if we optimize away as many matches as possible, we can then drop
     any headers that are not matched on.
  *)

  (* reduces policies by using identities (drop;p = drop, etc) *)
let rec simplify_vpol p =
  match p with
    | VSeq(VFilter False, _) -> VFilter False
    | VSeq(_, VFilter False) -> VFilter False
    | VSeq(VFilter True, p) -> simplify_vpol p
    | VSeq(p, VFilter True) -> simplify_vpol p
    | VSeq(p, q) -> let p' = simplify_vpol p in
                    let q' = simplify_vpol q in
                    begin
                      match p',q' with
                        | VFilter False, _ -> VFilter False
                        | _, VFilter False -> VFilter False
                        | VFilter True, q' -> q'
                        | p', VFilter True -> p'
                        | _ -> VSeq(p',q')
                    end
    | VPar(VFilter False, p) -> simplify_vpol p
    | VPar(p, VFilter False) -> simplify_vpol p
    | VPar(p, VPar(q,r)) ->
      if p = q then simplify_vpol (VPar(q,r))
      else let p' = simplify_vpol p in
           if p' = p then
             VPar(p, simplify_vpol (VPar(q,r)))
           else
             simplify_vpol (VPar(p', VPar(q,r)))
    | VPar(p,q) -> 
      if p = q then p
      else
        let p' = simplify_vpol p in
        let q' = simplify_vpol q in
        begin
          match p',q' with
            | VFilter False, q' -> q'
            | p', VFilter False -> p'
            | p',q' -> VPar(p',q')
        end
    | VStar(VFilter False) -> VFilter True
    | VStar(VFilter True) -> VFilter True
    | VStar(p) -> 
      begin
        match simplify_vpol p with
          | VFilter False -> VFilter True
          | VFilter True -> VFilter True
          | p -> VStar p
      end
    | _ -> p

  (* Linearizes seqs *)
let rec vpol_to_linear_vpol p = 
  match p with
      (* (p | q) | r = p (q | r) *)
    | VPar (VPar (p,q), r) ->
      vpol_to_linear_vpol (VPar (p, VPar(q,r)))
    | VPar (p, q) ->
      let p' = vpol_to_linear_vpol p in
      let q' = vpol_to_linear_vpol q in
      if p' = p && q' = q then
        VPar(p,q)
      else vpol_to_linear_vpol (VPar(p',q'))
      (* (p;q);r = p;(q;r) *)
    | VSeq (VSeq (p,q), r) ->
      vpol_to_linear_vpol (VSeq (p, VSeq (q,r)))
    | VSeq (p,q) ->
      let p' = vpol_to_linear_vpol p in
      let q' = vpol_to_linear_vpol q in
      if p' = p && q' = q then
        VSeq(p,q)
      else vpol_to_linear_vpol (VSeq(p',q'))
    | VStar p -> VStar (vpol_to_linear_vpol p)
    | _ -> p

  (* distributes seq over par *)
let rec distribute_seq p =
  match p with
      (* (p|q);r = p;r | q;r *)
    | VSeq(VPar(p,q), r) -> 
      VPar(distribute_seq (VSeq(p,r)), distribute_seq(VSeq(q,r)))
      (* p;(q|r) = p;q | p;r *)
    | VSeq(p, VPar(q,r)) -> 
      VPar(distribute_seq (VSeq(p,q)), distribute_seq(VSeq(p,r)))
    | VSeq(p,q) -> let p' = distribute_seq p in
                   let q' = distribute_seq q in
                   if p = p' & q = q' then
                     VSeq(p',q')
                   else
                     distribute_seq(VSeq(p',q'))
    | VPar(p,q) -> VPar(distribute_seq p,distribute_seq q)
    | VStar(p) -> VStar(distribute_seq p)
    | _ -> p

let reorder_seq p q =
  match p,q with
    | VMod(Tag h,v), VMod(Tag h',v') ->
      if h' < h then q,p else p,q
    | VMod(h,v), VMod(Tag h',v') ->
      q,p
    | VMod(Tag h,v), VMod(h',v') ->
      p,q
    | VMod(h,v), VMod(h',v') ->
      if h' < h then q,p else p,q
    | VMod(Tag h,v), VTest(h',v') ->
      if h' < h then q,p else p,q
    | VTest(h,v),VMod(Tag h',v') ->
      if h' < h then q,p else p,q
    | VMod(h',v'),VTest(h,v) ->
      q,p
    | VTest(h,v), VTest(h',v') ->
      if h' < h then q,p else p,q
      (* Because preds can't match on tags, they always commute with a tag mod *)
    | VFilter a, VMod (Tag h, v) ->
      q,p
      (* Because preds can't match on tags, they always commute with a tag test *)
    | VFilter a, VTest (h, v) ->
      q,p
    | _ -> p,q

  (* Normalizes sequences by reordering mods/matches by the headers:
     h <- v; h' <- v' = h' <- v'; h <- v if h <> h'
     Assumes linearized seq
     Equivalent to bubble sort. No, I don't care.
  *)
let rec normalize_seq p =
  match p with
    | VSeq(p, VSeq(q,r)) ->
      let p', q' = reorder_seq p q in
      if p' = p then
        let s = VSeq(q, r) in
        let s' = normalize_seq s in
        if s = s' then VSeq(p, s)
        else normalize_seq (VSeq(p, s'))
      else
        normalize_seq (VSeq(p',VSeq(q',r)))
    | VSeq(p,q) ->
      let p',q' = reorder_seq p q in
      VSeq(p',q')
    | VPar(p,q) -> let p' = normalize_seq p in
                   let q' = normalize_seq q in
                   if q' < p' then
                     normalize_seq (VPar(q',p'))
                   else
                     VPar(p',q')
    | VStar(p) -> VStar(normalize_seq p)
    | _ -> p

let reduce_seq p q =
  match p,q with
    | VMod(Tag h,v), VTest (h',v') ->
      if h = h' then
        if v = v' then
          p,VFilter True
        else
          VFilter False,VFilter False
      else p,q
    | VTest (h,v), VMod(Tag h',v') ->
      if h = h' && v = v' then p, VFilter True
      else p,q
    | VTest (h,v), VTest(h',v') ->
      if h = h' then
        if v = v' then
          p, VFilter True
        else
          VFilter False, VFilter False
      else p,q
    | VMod(h,v), VMod(h',v') ->
      if h = h' then
        VFilter True, q
      else p,q
    | _ -> p,q
      
  (* Assumes linearized seqs *)        
let rec remove_matches p = 
  match p with
    | VSeq(p, VSeq(q, r)) ->
      let p', q' = reduce_seq p q in
      let p'' = remove_matches p' in
      if p'' = p && q' = q then
        VSeq(p, remove_matches (VSeq(q,r)))
      else
        remove_matches (VSeq(p'', VSeq(q',r)))
    | VSeq(p,q) -> let p', q' = reduce_seq p q in
                   let p'' = remove_matches p' in
                   let q'' = remove_matches q' in
                   if p = p'' & q = q'' then
                     VSeq(p,q)
                   else
                     remove_matches (VSeq(p'',q''))
    | VPar(p,q) -> VPar(remove_matches p, remove_matches q)
    | VStar(p) -> VStar(remove_matches p)
    | p -> p

module TagSet = Set.Make (struct
  type t = vtag
  let compare = Pervasives.compare
end)

  (* Because we are using virtual headers, and no one else gets to use
     them, we know that the headers have no values until we initialize
     them. Thus, any match on a virtual header that is not preceded by a
     mod on the same header is "dead code", and can be eliminated
     (replaced by VFilter False) *)
  
let elim_vtest_helper p live_headers =
  match p with
    | VMod(Tag h,v) -> p, TagSet.add h live_headers
    | VTest(h,v) -> 
      if TagSet.mem h live_headers then p,live_headers
      else VFilter False, live_headers
    | _ -> p, live_headers

let rec elim_vtest' p live_headers = 
  match p with
    | VSeq(p,q) ->
      let p', live_headers' = elim_vtest_helper p live_headers in
      VSeq(elim_vtest' p' live_headers, elim_vtest' q live_headers')
    | VPar(p,q) ->
      let p', _  = elim_vtest_helper p live_headers in        
      let q', _ = elim_vtest_helper q live_headers in
      VPar(elim_vtest' p' live_headers, elim_vtest' q' live_headers)
    | _ -> p

let elim_vtest p = elim_vtest' p TagSet.empty

  (* Similarly, if we know that a virtual header is never read, we can eliminate the mod *)
let elim_vmod_helper p live_headers =
  match p with
    | VTest(h,v) -> p, TagSet.add h live_headers
    | VMod(Tag h,v) -> 
      if TagSet.mem h live_headers then p,live_headers
      else VFilter True, live_headers
    | _ -> p, live_headers

let rec elim_vmod' p live_headers = 
  match p with
    | VSeq(p,q) ->
      let q', live_headers' = elim_vmod' q live_headers in
      let p', live_headers'' = elim_vmod_helper p live_headers' in
      VSeq(p',q'), live_headers''
    | VPar(p,q) ->
      let p', live_headers' = elim_vmod' p live_headers in
      let q', live_headers'' = elim_vmod' q live_headers in
      VPar(p', q'), TagSet.union live_headers' live_headers''
    | _ -> elim_vmod_helper p live_headers

let elim_vmod p = fst (elim_vmod' p TagSet.empty)

  (* Observation: we don't have to reorder the mods/tests to be next
     to each other to eliminate them. Instead, we can do a dataflow
     analysis, tracking the current values of headers/tags. This should
     be way more efficient than (quadratically) sorting the terms
     repeatedly. The only problem is that reducing p + p' (when p = p')
     still requires normalizing *)
let rec optimize' p = 
  let renorm = vpol_to_linear_vpol in
  let simpl = simplify_vpol in
  let p' = elim_vmod (elim_vtest (renorm (remove_matches (renorm (distribute_seq (simpl p)))))) in
  if p' = p then p'
  else optimize' p'
    
let optimize p = 
  let renorm = vpol_to_linear_vpol in
  let simpl = simplify_vpol in
  optimize' (normalize_seq (renorm (distribute_seq (simpl p))))
    
  (* First pass: collect all the tags used in the program
     Construct the cross product. I.e. if we had tag (h,v) in the original program, then that corresponds to the set of tags {(t,(v,v',v'',\ldots)} where v',v'',... are possible values for the other tags appearing in the program.
     Second pass: *)
let rec collect_tags' p tags =
  match p with
    | VMod(Tag h,_) -> TagSet.add h tags
    | VTest(h, _) -> TagSet.add h tags
    | VPar(p,q) -> TagSet.union (collect_tags' p tags) (collect_tags' q tags)
    | VSeq(p,q) -> TagSet.union (collect_tags' p tags) (collect_tags' q tags)
    | VStar(p) -> collect_tags' p tags
    | _ -> tags

let rec range n = 
  if n <= 0 then
    [VInt.Int16 0]
  else (VInt.Int16 n) :: (range (n - 1))

let collect_tags p = TagSet.fold (fun (h,rng) acc -> ((h,rng), range rng) :: acc) (collect_tags' p TagSet.empty) []

  (* Assume we have a list of lists H, where the elements of H[i] are the values of h_i. Now, for each H[i][j], we want to compute a list representing each possible tuple containing value j in position i. We'll do this in several steps. First, represent this as [[[int]]] *)

let singletons h = List.fold_left (fun acc x -> [(h,x)]::acc) []

let rec prod h elems lst = 
  match elems with
    | []  -> []
    | elem :: elems -> (List.map (fun x -> (h, elem) :: x) lst) @ prod h elems lst

let rec make_tuples lst =
  match lst with
    | [] -> []
    | [(h,elems)] -> singletons h elems
    | (h,elems) :: lst -> prod h elems (make_tuples lst)

  (* Need a function that takes an old tag/value, and returns every
     matching new tag. *)
  (* Store the tag array as an alist, indexed by headers *)
  (* Can't keep old members around, key must have pre-existing value *)
let update_alist alist k v =
  List.map (fun (k',v') -> if k' = k then (k,v) else (k',v')) alist

  (* returns a list of alists, where each alist represents a tuple *)
let rec compute_matching_tuples tag_alist h v =
  make_tuples (update_alist tag_alist h [v])

  (* Takes an old value (h <- v), and returns a list of tuples (a,b)
     where b is a new value equivalent to (h=v)*)
let rec get_new_values tag_alist h v = 
  let old_vals = List.fold_left 
    (fun acc v' -> if v = v' 
      then acc else compute_matching_tuples tag_alist h v' @ acc) []
    (List.assoc h tag_alist) in
  List.map (fun old_val -> (old_val, update_alist old_val h v)) old_vals

let tuple_hashTbl = Hashtbl.create 100 
let tuple_cnter = ref 0
let tuple_to_int tpl =
  if Hashtbl.mem tuple_hashTbl tpl
  then VInt.Int16 (Hashtbl.find tuple_hashTbl tpl)
  else 
    let _ = incr tuple_cnter in
    let _ = Hashtbl.add tuple_hashTbl tpl !tuple_cnter in
    VInt.Int16 !tuple_cnter

open NetKAT_Types

(* We have to be careful here. The very first time we write the tag,
   we won't have any pre-existing values, so we can't match upon them *)
let rec convert_tag_to_hdr tag_alist h p written =
  match p with
    | VFilter pr -> Filter pr, written
    | VMod (Tag h', v') -> 
      let tuples = get_new_values tag_alist h' v' in
      if written then
        (List.fold_left (fun acc (old_v, new_v) ->
          Par(Seq(Filter(Test(Header h, tuple_to_int old_v)), Mod(Header h, tuple_to_int new_v)), acc)) (Filter False) tuples,
         written)
      else
        (* Just pick one legal tuple and use that. We always
           initialize other tags before matching on them, so the other
           values don't matter *)
        Mod(Header h, tuple_to_int (update_alist (fst (List.hd tuples)) h' v')), true
    | VTest(h',v') -> let tuples = compute_matching_tuples tag_alist h' v' in
                      List.fold_left (fun acc new_v -> Par(Filter(Test (Header h, tuple_to_int new_v)), acc)) (Filter False) tuples, written
    | VMod (Field h', v') -> Mod (Header h', v'), written
    | VSeq(p,q) -> let p', written = convert_tag_to_hdr tag_alist h p written in
                   let q', written = convert_tag_to_hdr tag_alist h q written in
                   Seq(p',q'), written
    | VPar(p,q) -> let p', written' = convert_tag_to_hdr tag_alist h p written in
                   let q', written'' = convert_tag_to_hdr tag_alist h q written in
                   Par(p',q'), written' || written''
    | VStar(p) -> let p',written = convert_tag_to_hdr tag_alist h p written in
                  Star(p'), written
    | VLink(sw,pt,sw',pt') -> Seq(Seq(Filter(Test(Switch, sw)), Filter(Test(Header SDN_Types.InPort, pt))),
                                  Seq(Mod(Switch, sw'), Mod(Header SDN_Types.InPort, pt'))), written

      
let dehop_policy p =
  let (i,s,t,e) = dehopify p in
  let p' = (VSeq(i, VSeq(VStar(VSeq(s,t)), VSeq(s,e)))) in
  fst (convert_tag_to_hdr (collect_tags p') SDN_Types.Vlan p' false)
