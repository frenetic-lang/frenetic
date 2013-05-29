open NetCore_Types
open NetCore_Verify
open NetCore_Verify.Sat

module Log = Frenetic_Log

type example = { 
  name:string;
  topology:topology;
  policy:pol;
  formula:topology -> pol -> zVar -> zVar -> zFormula;
  expected:bool }

let check (ex:example) : unit = 
  let pkt1 = fresh SPacket in 
  let pkt2 = fresh SPacket in 
  let f = ex.formula ex.topology ex.policy pkt1 pkt2 in 
  let p = ZProgram [ZAssertDeclare f] in 
  if solve p = ex.expected then 
    Log.printf "Verify" "%s [[32mpassed[0m]\n" ex.name
  else 
    Log.printf "Verify" "%s [[31mfailed[0m]\n" ex.name

let ex1 : example = 
  let topology = 
    let s1 = Int64.of_int 1 in
    let s2 = Int64.of_int 2 in
    let s3 = Int64.of_int 3 in
    bidirectionalize 
      (Topology [ (Link (s1, 2), Link (s2, 1)) 
                ; (Link (s2, 2), Link (s3, 1)) ]) in 
  let policy = PoAction [SwitchAction { id with outPort = All }] in
  let formula = forwards 2 in 
  { name="Linear";
    topology=topology;
    policy=policy;
    formula=formula;
    expected=true }

let examples = [ex1]
    
let () = List.iter check examples
