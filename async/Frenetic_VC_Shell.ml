(* This implements an interactive shell for the virtual compiler *)

open Core.Std
open Or_error
open Async.Std
open Frenetic_NetKAT

type command =
  | VAdd              of int
  | VRemove           of int
  | VPolicy           of int * policy
  | VRelation         of int * pred
  | VTopology         of int * policy
  | VIngressPolicy    of int * policy
  | VIngressPredicate of int * pred
  | VEgressPredicate  of int * pred
  | PTopology         of policy
  | PIngressPredicate of pred
  | PEgressPredicate  of pred
  | Compile
  | CompileLocal      of switchId * policy
  | CompileSelective  of int list
  | FlowTable         of switchId
  | RemoveDuplicates  of bool

module Parser : sig
  val parse : string -> command Or_error.t
end = struct
  open MParser
  module T = MParser_RE.Tokens
  
  (* Some custom parsers for policies, predicates, 64-bit integers, and booleans *)
  (*let parse_pol  = T.string_literal  |>> Frenetic_NetKAT_Json.policy_from_json_string*)
  (*let parse_pol  = T.string_literal  >>= (fun input -> return (Printf.printf "%s%!\n" input; Frenetic_NetKAT_Parser.policy_from_string input))*)
  let parse_pol  = T.string_literal  |>> Frenetic_NetKAT_Parser.policy_from_string
  let parse_pred = T.string_literal  |>> Frenetic_NetKAT_Parser.pred_from_string
  let integer64  = many1_chars digit |>> Int64.of_string
  let boolean    = choice [T.symbol "true"  >>$ true;
                           T.symbol "false" >>$ false]
  
  (* Convenient parsers for the different types of commands *)
  let nullary str = (>>$) (T.symbol str)
  
  let unary str ~f v = T.symbol str >> f |>> v
  let int_cmd   = unary ~f:T.integer
  let int64_cmd = unary ~f:integer64
  let unpol     = unary ~f:parse_pol
  let unpred    = unary ~f:parse_pred
  
  let binary str ~f ~g v = T.symbol str >> pipe2 f g v
  let bipol  = binary ~f:T.integer ~g:parse_pol
  let bipred = binary ~f:T.integer ~g:parse_pred
  
  (* Main command parser *)
  let cmd = choice
    [
      int_cmd   "add-vno"                    (fun i   -> VAdd               i);
      int_cmd   "remove-vno"                 (fun i   -> VRemove            i);
      bipol     "virtual-policy"             (fun i p -> VPolicy           (i, p));
      bipred    "virtual-relation"           (fun i p -> VRelation         (i, p));
      bipol     "virtual-topology"           (fun i p -> VTopology         (i, p));
      bipol     "virtual-ingress-policy"     (fun i p -> VIngressPolicy    (i, p));
      bipred    "virtual-ingress-predicate"  (fun i p -> VIngressPredicate (i, p));
      bipred    "virtual-egress-predicate"   (fun i p -> VEgressPredicate  (i, p));
      unpol     "physical-topology"          (fun p   -> PTopology          p);
      unpred    "physical-ingress-predicate" (fun p   -> PIngressPredicate  p);
      unpred    "physical-egress-predicate"  (fun p   -> PEgressPredicate   p);
      nullary   "compile"                                Compile;
      binary    "compile-local"     ~f:integer64 ~g:parse_pol   (fun sw p -> CompileLocal (sw, p));
      unary     "compile-selective" ~f:(T.comma_sep1 T.integer) (fun ids  -> CompileSelective ids);
      int64_cmd "get-flowtable"                 (fun sw -> FlowTable       sw);
      unary     "remove-duplicates" ~f:boolean  (fun b  -> RemoveDuplicates b);
   ]
  
  let parse input =
    match parse_string cmd input [] with
    | Success command -> Ok command
    | Failed (msg, e) -> error_string msg
end

module Interpreter : sig
  val execute : command -> unit
end = struct
  open Option
  open Printf
  
  module VNO = struct
    type t =
      { id                : int;
        policy            : policy;
        relation          : pred;
        topology          : policy;
        ingress_policy    : policy;
        ingress_predicate : pred;
        egress_predicate  : pred;
      }
    with fields
    
    (* Represents a sensible default VNO *)
    let default =
      { id                = 0;
        policy            = Filter True;
        relation          = True;
        topology          = Filter True;
        ingress_policy    = Filter True;
        ingress_predicate = True;
        egress_predicate  = True;
      }
  end
  
  (* Mutable state for execute function *)
  let vnos              = Hashtbl.create ~hashable:Int.hashable ()
  let topology          = ref (Filter True)
  let ingress_predicate = ref True
  let egress_predicate  = ref True
  let compiled          = ref None
  let compiler_options  = ref Frenetic_NetKAT_Local_Compiler.default_compiler_options
  
  (* Virtualization functions for predicates and policies *)
  let rec virtualize_pred = function
    | True                          -> True
    | False                         -> False
    | Test (Switch sw)              -> Test (VSwitch sw)
    | Test (Location (Physical pt)) -> Test (VPort (Int64.of_int32 pt))
    | Test hv                       -> Test hv
    | And (a, b)                    -> And (virtualize_pred a, virtualize_pred b)
    | Or  (a, b)                    -> Or  (virtualize_pred a, virtualize_pred b)
    | Neg  a                        -> Neg (virtualize_pred a)
  
  let rec virtualize_pol = function
    | Filter pred                  -> Filter (virtualize_pred pred)
    | Mod (Location (Physical pt)) -> Mod    (VPort (Int64.of_int32 pt))
    | Union (p, q)                 -> Union  (virtualize_pol p, virtualize_pol q)
    | Seq   (p, q)                 -> Seq    (virtualize_pol p, virtualize_pol q)
    | Star   p                     -> Star   (virtualize_pol p)
    | _                            -> invalid_arg "Unable to virtualize this kind of policy"
  
  (* Updates the ith VNO's field with v *)
  let update i field v =
    value ~default:VNO.default (Hashtbl.find vnos i) |> (* Get either the ith VNO or defer to default *)
    fun vno -> Field.fset field vno v                |> (* Update the corresponding field *)
    fun vno -> Hashtbl.replace vnos ~key:i ~data:vno    (* Replace it in the VNO table *)
  
  (* Compiles a VNO's policy into a local NetKAT program  *)
  let compile (vno : VNO.t) = Frenetic_NetKAT_Virtual_Compiler.compile vno.policy
     vno.relation vno.topology vno.ingress_policy
     vno.ingress_predicate vno.egress_predicate
     !topology !ingress_predicate
     !egress_predicate
  
  (* Compiles the global policy for the entire network *)
  let global_compile policy =
    compiled := Some (Frenetic_NetKAT_Local_Compiler.compile_global policy)
  
  (* Compiles the policies for all VNOs *)
  let compile_vnos vnos =
    match List.hd vnos, List.tl vnos with
    | Some hd, Some tl ->
        List.fold      (List.tl_exn vnos)
        ~init:(compile (List.hd_exn vnos))
        ~f:(fun acc vno -> Frenetic_NetKAT_Optimize.mk_union acc (compile vno)) |> global_compile
    | Some hd, None -> global_compile (compile hd)
    | _ -> compiled := None
  
  (* Returns the flow table of a switch's policy as a JSON string *)
  let pol_to_string sw pol =
    Frenetic_NetKAT_Local_Compiler.to_table sw pol |>
    Frenetic_NetKAT_SDN_Json.flowTable_to_json     |>
    Yojson.Basic.to_string ~std:true
  
  (* Executes the given command *)
  let execute = function
    | VAdd               i       ->
        begin
          match Hashtbl.add vnos i {VNO.default with id = i} with
          | `Duplicate -> print_endline "Replaced VNO"
          | `Ok        -> print_endline "Ok"
        end
    | VRemove            i       -> Hashtbl.remove vnos i
    | VPolicy           (i, pol) -> update i VNO.Fields.policy           (virtualize_pol pol)
    | VRelation         (i, pre) -> update i VNO.Fields.relation          pre
    | VTopology         (i, pol) -> update i VNO.Fields.topology          pol
    | VIngressPolicy    (i, pol) -> update i VNO.Fields.ingress_policy    pol
    | VIngressPredicate (i, pre) -> update i VNO.Fields.ingress_predicate pre
    | VEgressPredicate  (i, pre) -> update i VNO.Fields.egress_predicate  pre
    | PTopology          pol     -> topology          := pol
    | PIngressPredicate  pre     -> ingress_predicate := pre
    | PEgressPredicate   pre     -> egress_predicate  := pre
    | Compile                    -> Hashtbl.fold vnos ~init:[]
                                    ~f:(fun ~key:id ~data:vno acc -> vno::acc) |> compile_vnos
    | CompileLocal     (sw, pol) ->
        Frenetic_NetKAT_Local_Compiler.compile pol |>
        pol_to_string sw |> print_endline
    | CompileSelective  ids      -> 
        let (vnos, unknowns) =
          List.fold_left ids ~init:([], [])
                             ~f:(fun (acc, unknowns) id ->
                                   value_map ~default:(acc, id::unknowns)
                                             ~f:(fun vno -> (vno::acc, unknowns))
                                             (Hashtbl.find vnos id)) in
        
          if (List.is_empty unknowns |> not) then
            begin
              print_endline "Unknown VNOs:";
              List.iter ~f:(printf "%d, ") unknowns;
              print_endline ""
            end
          else
            compile_vnos vnos
    | FlowTable          sw      ->
        value_map !compiled ~default:()
                            ~f:(fun repr -> pol_to_string sw repr |> print_endline)
    | RemoveDuplicates   b       -> compiler_options := {!compiler_options with dedup_flows = b}
end



let rec repl () =
  printf "vc> %!";
  Lazy.force Reader.stdin |>
  Reader.read_line       >>=
  fun input ->
    let perform = function
      | `Eof
      | `Ok "exit" -> Shutdown.shutdown 0
      | `Ok "help" -> print_endline "TODO help"
      | `Ok inp    ->
          (*try*)
            Parser.parse inp |> ok_exn |> Interpreter.execute  
          (*with exn -> Exn.to_string exn |> print_endline*)
    in
      perform input;
      repl ()
  
    
let main () =
  Frenetic_Log.set_output [Async.Std.Log.Output.file `Text "frenetic_vc_shell.log"];
  Frenetic_Fdd.Field.set_order
   [Switch; Location; VSwitch; VPort; IP4Dst; Vlan; TCPSrcPort; TCPDstPort; IP4Src;
      EthType; EthDst; EthSrc; VlanPcp; IPProto; VFabric; Wavelength];
  printf "Frenetic Virtual Compiler Shell v 1.0\n%!";
  printf "Type `help` for a list of commands\n%!";
  repl ();
  ()
