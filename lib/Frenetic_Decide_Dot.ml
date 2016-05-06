
type t = bool * bool * int * int

let init term_to_string base_set_non_empty =
  
  
  let f1 = open_out "/tmp/fsm1.dot" in 
  let f2 = open_out "/tmp/fsm2.dot" in 
  let cell1,cell2 = ref 0, ref 0 in
  let states1, states2 = ref [], ref [] in 
  let edges1, edges2 = ref [], ref [] in 
  
  let get_state1 t f = 
    let s = term_to_string t in 
    try fst (List.assoc s !states1)
    with Not_found -> 
      incr cell1;
      states1 := (s,(!cell1,f))::!states1;
      !cell1 in
  let get_state2 t f = 
    let s = term_to_string t in 
    try fst (List.assoc s !states2)
    with Not_found -> 
      incr cell2;
      states2 := (s,(!cell2,f))::!states2;
      !cell2 in
  let add_edge1 q1 q2 = 
    edges1 := (q1,q2)::!edges1 in
  let add_edge2 q1 q2 = 
    edges2 := (q1,q2)::!edges2 in

  
  let print_states _ =
  (* no primes *)
    Printf.fprintf f1 "digraph fsm {\noverlap=false;\n";
    Printf.fprintf f1 "rankdir=LR;\n";
    Printf.fprintf f1 "node [shape=circle]; ";
    List.iter (fun (_,(x,_)) -> Printf.fprintf f1 " q%d" x) 
      (List.filter (fun (_,(_,f)) -> not f) !states1);
    Printf.fprintf f1 "\n";
    Printf.fprintf f1 "node [shape=doublecircle]; ";
    List.iter (fun (_,(x,_)) -> Printf.fprintf f1 " q%d" x) 
      (List.filter (fun (_,(_,f)) -> f) !states1);
    Printf.fprintf f1 "\n";
    List.iter (fun (x,y) -> Printf.fprintf f1 "q%d -> q%d;\n" x y) !edges1;
    Printf.fprintf f1 "}\n";
    (* primes *)
    Printf.fprintf f2 "digraph fsm {\n";
    Printf.fprintf f2 "rankdir=LR;\n";
    Printf.fprintf f2 "node [shape=circle]; ";
    List.iter (fun (_,(x,_)) -> Printf.fprintf f2 " q%d" x) 
      (List.filter (fun (_,(_,f)) -> not f) !states2);
    Printf.fprintf f2 "\n";
    Printf.fprintf f2 "node [shape=doublecircle]; ";
    List.iter (fun (_,(x,_)) -> Printf.fprintf f2 " q%d" x) 
      (List.filter (fun (_,(_,f)) -> f) !states2);
    Printf.fprintf f2 "\n";
    List.iter (fun (x,y) -> Printf.fprintf f2 "q%d -> q%d;\n" x y) !edges2;
    Printf.fprintf f2 "}\n";
    close_out f1;
    close_out f2;
    ignore 
      (Sys.command 
	 "neato -Tpdf -o /home/research/research/fsm1.pdf /tmp/fsm1.dot");
    ignore 
      (Sys.command 
	 "dot -Tpdf -o /home/research/research/fsm2.pdf /tmp/fsm2.dot"); in
  
  let get_state q1 q2 q1_E q2_E = 
    let f1 = base_set_non_empty (q1_E) in 
    let f2 = base_set_non_empty (q2_E) in 
    let z1 = get_state1 q1 f1 in
    let z2 = get_state2 q2 f2 in
    f1,f2,z1,z2 in  

  let update_state (f1,f2,z1,z2) q1' q2' q1'_E q2'_E = 
    let f1' = base_set_non_empty (q1'_E) in 
    let f2' = base_set_non_empty (q2'_E) in 
    let z1' = get_state1 q1' f1' in 
    let z2' = get_state2 q2' f2' in 
    let _= add_edge1 z1 z1' in 
    let _ = add_edge2 z2 z2' in 
    () in
  get_state,update_state,print_states  

