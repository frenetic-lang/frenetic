open NetKAT_Verify_Reachability
open NetKAT_Verify_Equivalence
open Types

type parseType = 
  | NetKAT
  | GML
      
type mode = 
  | Equiv
  | Reach

let parseType = ref NetKAT
let mode = ref Reach
let parse_things = ref []
let run_name = ref []
  
let usage = "usage: verify [-f NetKAT|GML] [-m equiv|reach] run-name [inp pol outp] [prog1 prog2]"
  
  
(* from http://rosettacode.org/wiki/Command-line_arguments#OCaml *)
let speclist = [
  ("-f", Arg.String (fun s -> 
    parseType := match s with 
      | "NetKAT" -> NetKAT 
      | "GML" -> GML 
      | _ -> failwith (Printf.sprintf "not supported: %s" s)
   ),      ": format to expect for parsing ");
  ("-m", Arg.String    (fun s -> 
    mode := match s with
      | "equiv" -> Equiv
      | "equivalence" -> Equiv
      | "reach" -> Reach
      | "reachability" -> Reach
      | _ -> failwith (Printf.sprintf "not supported: %s" s)
   ),      ": algorithm to run");
]

let parse_program _ = Filter (True)
let parse_predicate _ = True
let parse_reach_args argl = 
  match argl with
    | [inp;pol;outp] -> parse_predicate inp, parse_program pol, parse_predicate outp
    | _ -> failwith "incorrect arguments to reachability"

let _ =
  Arg.parse 
    speclist
    (fun x -> 
      match (!run_name) with
	| [] -> run_name := [x]
	| _ -> parse_things := x::(!parse_things))
    usage; match (!mode),(!parse_things) with 
      | Equiv,[hd;tl] -> ()
      | Reach, [hd;mid;tl] -> ()
      | _ -> failwith "incorrect number of arguments supplied for selected run type"

let _ = match (!mode) with
  | Equiv -> 
    let prog1, prog2 = match List.map parse_program (!parse_things) with
      | [prog1;prog2] -> prog1,prog2
      | _ -> failwith "incorrect arguments supplied to equiv" in
    if check_equivalence prog1 prog2 (List.hd (!run_name))
    then Printf.printf "Sat: programs equivalent\n"
    else Printf.printf "Unsat: programs differ\n"
  | Reach -> 
    let (inp,prog,outp) = parse_reach_args (!parse_things) in
    if check_reachability (List.hd (!run_name)) inp prog outp (Some true)
    then Printf.printf "Sat: path found.\n"
    else Printf.eprintf "Unsat: path impossible.\n"
