open NetKAT_Types
    
(** {2 Semantics}
 
  [eval pkt pol] raises [Not_found] if it tests or updates a header that  [pkt]
  does not have. This behavior is different from OpenFlow, which fails  silently
  in both cases. *)

let size_pred (pr:pred) : int = 
  let rec size_pred (pr:pred) f : int = 
    match pr with
      | True -> f 1
      | False -> f 1
      | Test(_,_) -> f 3
      | And(pr1,pr2)
      | Or(pr1,pr2) -> size_pred pr1 (fun spr1 -> size_pred pr2 (fun spr2 -> f (1 + spr1 + spr2)))
      | Neg(pr) -> size_pred pr (fun spr -> f (1 + spr)) in
  size_pred pr (fun spr -> spr)

let size (pol:policy) : int = 
  let rec size (pol:policy) f : int = 
    match pol with
      | Filter pr -> f (size_pred pr + 1)
      | Mod(_,_) -> f 3
      | Par(pol1, pol2)
      | Seq(pol1, pol2)
      | Choice(pol1, pol2) -> size pol1 (fun spol1 -> size pol2 (fun spol2 -> f (1 + spol1 + spol2)))
      | Star(pol) -> size pol (fun spol -> f (1 + spol))
      | Link(_,_,_,_) -> f 5 in
  size pol (fun spol -> spol)
  
let rec eval_pred (pkt : packet) (pr : pred) : bool = match pr with
  | True -> true
  | False -> false
  | Test (h, v) -> HeaderMap.find h pkt.headers = v
  | And (pr1, pr2) -> eval_pred pkt pr1 && eval_pred pkt pr2
  | Or (pr1, pr2) -> eval_pred pkt pr1 || eval_pred pkt pr2
  | Neg pr1 -> not (eval_pred pkt pr1)
    
let rec eval (pkt : packet) (pol : policy) : PacketSetSet.t = match pol with
  | Filter pr -> 
    if eval_pred pkt pr then 
      PacketSetSet.singleton (PacketSet.singleton pkt)
    else 
      PacketSetSet.empty
  | Mod (h, v) ->
    if HeaderMap.mem h pkt.headers then
      PacketSetSet.singleton 
	(PacketSet.singleton 
	   { pkt with headers = HeaderMap.add h v pkt.headers })
    else
      raise Not_found (* for consistency with Test *)
    | Par (pol1, pol2) ->
      let cartesian_product s1 s2 =
        let f x y setset = PacketSetSet.add (PacketSet.union x y) setset in
        let g x setset = PacketSetSet.fold (f x) s2 setset in
        PacketSetSet.fold g s1 PacketSetSet.empty in
      cartesian_product (eval pkt pol1) (eval pkt pol2)
    | Seq (pol1, pol2) ->
      let f pkt' setset = PacketSetSet.union (eval pkt' pol2) setset in
      let g pktset = PacketSet.fold f pktset PacketSetSet.empty in
      let h pktset setset = PacketSetSet.union (g pktset) setset in
      PacketSetSet.fold h (eval pkt pol1) PacketSetSet.empty
    | Star pol -> raise Not_found (* MARCO: Can't figure this out :( *)
    (*let rec loop acc = 
      let f pkt' set = PacketSet.union (eval pkt' pol) set in 
      let acc' = PacketSet.fold f acc PacketSet.empty in 
      if PacketSet.equal acc acc' then acc else loop acc' in 
      loop (PacketSet.singleton pkt)*)
    | Choice (pol1, pol2) ->
      PacketSetSet.union (eval pkt pol1) (eval pkt pol2)

    | Link(sw,pt,sw',pt') -> 
      begin 
        try 
          if HeaderMap.find Switch pkt.headers = sw && 
            HeaderMap.find (Header SDN_Types.InPort) pkt.headers = pt then
            let pkt' = 
	      { pkt with 
		headers = 
		  HeaderMap.add Switch sw' 
		    (HeaderMap.add (Header SDN_Types.InPort) pt' 
		       pkt.headers) } in 	
            PacketSetSet.singleton (PacketSet.singleton pkt')
          else
            PacketSetSet.empty
        with Not_found -> 
          raise Not_found
      end
