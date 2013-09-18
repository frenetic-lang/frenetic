open NetKAT_Types

type explicit_topo_policy =
  | Filter of pred
  | Mod of SDN_Types.field * header_val
      (* switch, port -> switch, port *)
  | Link of header_val*header_val*header_val*header_val
  | Par of explicit_topo_policy * explicit_topo_policy
  | Seq of explicit_topo_policy * explicit_topo_policy
  | Star of explicit_topo_policy

(* i;(p;t)^*;e 
   where 
   i = t = v | h = v | t <- v | i + i | i ; i
   p = t = v | h = v | t <- v | h <- v | p + p | p ; p | p*
   t = sw = v | p = v | sw <- v | p <- v | t + t
   e = i
*)

type restricted_header = 
  | PHeader of SDN_Types.field
  | VHeader of int * int

type header_pred = 
  | HTrue
  | HFalse
  | HTest of restricted_header * header_val
  | HTTest of header_val
  | HNeg of header_pred
  | HAnd of header_pred * header_pred
  | HOr of header_pred * header_pred

type ingress_pol =
  | IFilter of header_pred
  | IMod of restricted_header * header_val
  | IPar of ingress_pol * ingress_pol
  | ISeq of ingress_pol * ingress_pol
  | IPass
  | IDrop

type switch_pol =
  | SFilter of header_pred
  | SMod of restricted_header * header_val
  | SPar of switch_pol * switch_pol
  | SSeq of switch_pol * switch_pol
  | SStar of switch_pol
  | SPass
  | SDrop

type topo_header =
  | TSwitch
  | TPort

type topo_pol = 
  | TTest of topo_header * header_val
  | TMod of topo_header * header_val
  | TPar of topo_pol * topo_pol
  | TSeq of topo_pol * topo_pol
  | TDrop

type restricted_pol = ingress_pol * switch_pol * topo_pol * ingress_pol

let vheader_count = ref 0

let gen_header size =
  incr vheader_count;
  VHeader(!vheader_count, size)

let rec pred_to_ipred pr = 
  match pr with
  | True -> HTrue
  | False -> HFalse
  | Test (Switch, v) -> HTTest(v)
  | Test (Header h, v) -> HTest (PHeader h, v)
  | Neg p -> HNeg (pred_to_ipred p)
  | And(a,b) -> HAnd (pred_to_ipred a, pred_to_ipred b)
  | Or(a,b) -> HOr (pred_to_ipred a, pred_to_ipred b)

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
let rec dehopify (p : explicit_topo_policy) : restricted_pol =
  match p with
    | Filter pr -> IFilter (pred_to_ipred pr), SDrop, TDrop, IPass
    | Mod (h, v) -> IMod (PHeader h, v), SDrop, TDrop, IPass
    | Link (sw1,p1,sw2,p2) -> 
      IPass, 
      SPass, 
      TSeq(TTest (TSwitch, sw1), 
	   TSeq( TTest (TPort, p1), 
		 TSeq (TMod (TSwitch, sw2),  
		       TMod (TPort, p2)))), 
      IPass
    (* Todo: add topo filter terms *)
    | Par (p,q) -> let i_p,s_p,t_p,e_p = dehopify p in
                   let i_q,s_q,t_q,e_q = dehopify q in
                   let h = gen_header 6 in
                   let h0 = VInt.Int16 0 in
                   let h1 = VInt.Int16 1 in
                   let h2 = VInt.Int16 2 in
                   let h3 = VInt.Int16 3 in
                   let h4 = VInt.Int16 4 in
                   let h5 = VInt.Int16 5 in
                   (IPar(
                     IPar(
                       IPar(ISeq(i_p, IMod (h,h1)), ISeq(i_p, IMod(h,h2))), 
                       ISeq(i_q, IMod(h, h4))), 
                     ISeq(i_q, IMod(h,h5))),
		    SPar(SSeq(SFilter(HTest(h,h1)), s_p), 
                         SPar(SSeq(SFilter(HTest(h, h1)), 
                                   SSeq(s_p, SMod(h,h2))), 
                              SPar(SSeq(SFilter(HTest(h,h4)), s_q), 
                                   SSeq(SFilter(HTest(h,h4)), SSeq(s_q, SMod(h,h5)))))),
                    TPar(t_p,t_q),
                    IPar(ISeq(IFilter(HTest(h,h2)), e_p),
                         ISeq(IFilter(HTest(h, h5)), e_q)))
