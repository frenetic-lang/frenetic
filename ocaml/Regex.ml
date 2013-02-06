open NetCore
open MessagesDef

type regex =
  | Hop of switchId
  | Star
  | Option of regex * regex
  | Sequence of regex * regex

type regex_policy = 
  | RegPol of predicate * regex


let get_hop topo s1 s2 = 0

let get_path s1 s2 topo = []

let rec flatten_reg pol = match pol with
  | Hop sw -> [Hop sw]
  | Star -> [Star]
  (* | Option reg1 reg2 -> [Option reg1 reg2] *)
  | Sequence (reg1, reg2) -> (flatten_reg reg1) @ (flatten_reg reg2)

let rec collapse_star pol = match pol with
  | Star :: Star :: pol -> collapse_star (Star :: pol)
  | a :: pol -> a :: collapse_star pol

  (* Naive compilation: does not guarantee loop-free semantics
     Possible issues:
     1) reg contains an explicit loop
     2) We compile star paths to contain the same node

     Possible solutions:
     1) Second compilation phase that detects repeated nodes and tags packets inbetween such repeats
*)
let rec compile1 pred reg topo = match reg with
  | Hop s1 :: Hop s2 :: reg -> 
    Par ((Pol ((And (pred, (Switch s1))), [To (get_hop topo s1 s2)])), ((compile1 pred ((Hop s2) :: reg) topo)))
  | Hop s1 :: Star :: Hop s2 :: reg -> compile1 pred (get_path s1 s2 topo) topo
  | _ -> Pol (pred, [])

let compile pol topo = match pol with
  | RegPol (pred, reg) -> compile1 pred (collapse_star (flatten_reg reg)) topo
