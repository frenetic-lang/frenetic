open NetKAT_Verify_Reachability
open NetKAT_Verify_Equivalence

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
  
let usage = "usage: verify [f NetKAT|GML] [-m equiv|reach] run-name [inp pol outp] [prog1 prog2]"
  
  
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
   ),      ": some int parameter");
]


let _ =
  Arg.parse 
    speclist
    (fun x -> 
      match (!run_name) with
	| [] -> run_name := [x]
	| _ -> parse_things := (parse x)::(!parsed_things))
    usage; match mode,(!parse_things) with 
      | Equiv,(hd::tl::[]) -> ()
      | Reach, (hd::mid::tl::[]) -> ()
      | _ -> failwith "incorrect number of arguments supplied for selected run type"

let _ = match mode with
  | Equiv -> 
    let [prog1; prog2]::_ = List.map parse_program (!parse_things) in
    if check_equivalence prog1 prog2 (!run_name)
    then Printf.printf "Sat: programs equivalent"
    else Printf.printf "Unsat: programs differ"
  | Reach -> 
    let (inp,prog,outp) = parse_reach_args (!parse_things) in
    if check_reachability (!run_name) inp prog outp (Some true)
    then Printf.printf "Sat: path found."
    else Printf.eprintf "Unsat: path impossible."
