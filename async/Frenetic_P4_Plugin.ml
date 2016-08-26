open Core.Std
open Async.Std

module Log = Frenetic_Log

open Frenetic_NetKAT_Compiler
open Frenetic_NetKAT_Compiler.Multitable
open Frenetic_OpenFlow
open Frenetic_Packet



let string_of_vlan (x : int) : string =
  Format.sprintf "Vlan = %d" x

let string_of_vlanpcp (x : dlVlanPcp) : string =
  Format.sprintf "VlanPcp = %d" x

let string_of_ethType (x : dlTyp) : string =
  let extra = if x = 0x800 then " (ip)"
	      else if x = 0x806 then " (arp)"
	      else ""
  in
  Format.sprintf "EthType = 0x%x%s" x extra

let string_of_ipProto (x : nwProto) : string =
  let extra = match x with
    | 0x01 -> " (icmp)"
    | 0x02 -> " (igmp)"
    | 0x06 -> " (tcp)"
    | 0x11 -> " (udp)"
    | _ -> ""
  in
  Format.sprintf "ipProto = 0x%x%s" x extra

let string_of_ethSrc (x : dlAddr) : string =
  Format.sprintf "EthSrc = %s" (Frenetic_Packet.string_of_mac x)

let string_of_ethDst (x : dlAddr) : string =
  Format.sprintf "EthDst = %s" (Frenetic_Packet.string_of_mac x)

let string_of_ip4src (x : Pattern.Ip.t) : string =
  Format.sprintf "IP4Src = %s" (Pattern.Ip.string_of x)

let string_of_ip4dst (x : Pattern.Ip.t) : string =
  Format.sprintf "IP4Dst = %s" (Pattern.Ip.string_of x)

let string_of_tcpSrcPort (x : tpPort) : string =
  Format.sprintf "TCPSrcPort = %d" x

let string_of_tcpDstPort (x : tpPort) : string =
  Format.sprintf "TCPDstPort = %d" x

let string_of_inPort (x : portId) : string =
  Format.sprintf "InPort = %lu" x

let check (string_of : 'a -> string)
	  (x : 'a option)
	  (acc : string list) : string list =
  match x with
  | None -> acc
  | Some x' -> (string_of x') :: acc

(* Builds up a list of strings one for each pattern *)
let pattern_list (p : Pattern.t) : string list =
  check string_of_ethSrc p.dlSrc [] |>
    check string_of_ethDst p.dlDst |>
    check string_of_ethType p.dlTyp |>
    check string_of_vlan p.dlVlan |>
    check string_of_vlanpcp p.dlVlanPcp |>
    check string_of_ip4src p.nwSrc |>
    check string_of_ip4dst p.nwDst |>
    check string_of_ipProto p.nwProto |>
    check string_of_tcpSrcPort p.tpSrc |>
    check string_of_tcpDstPort p.tpDst |>
    check string_of_inPort p.inPort

let string_of_instruction (instr:instruction) : string list = 
  match instr with 
  | `Action actions -> List.map actions ~f:string_of_par 
  | `GotoTable (table,meta) -> [Format.sprintf "GotoTable(%d)" table; Format.sprintf "SetMeta(%d)" meta]

(* Given a flow, return a pair of list of strings where the first list
 * contains the strings of the pattern and the second list contains
 * the strings of the actions associated with the pattern. *)
let to_entry (f : multitable_flow) : (string list) * (string list) =
  let open Core.Std in
  let tableId,metaId = f.flowId in 
  let pattern_list = Format.sprintf "Meta = %d" metaId :: pattern_list f.pattern in
  let pars : string list list = [string_of_instruction f.instruction] in 
  let add_sep = function
    | [] -> assert false
    | h::tl -> ("+ " ^ h)::tl
  in
  let action_list = List.concat_mapi pars ~f:(function 0 -> ident | _ -> add_sep) in
  (pattern_list, action_list)

(* Pads a string with spaces so that it is atleast `len` characters. *)
let pad (len : int) (e : string) : string =
  let open Core.Std in
  let padding_size = max 0 (len - (String.length e)) in
  let padding = String.make padding_size ' ' in
  String.concat [e; padding]

(* Helper function *)
let unwrap x =
  match x with
  | None -> 0
  | Some x -> x

(* Given a list of entries to be displayed in the table, calculate a pair
 * containing the max characters in a pattern string and action string *)
let table_size (label : string) (entries : ((string list) * (string list)) list) : int * int =
  let open Core.Std in
  let open List in
  let patterns = map entries fst |> concat in
  let actions = map entries snd |> concat in
  let max_p =  max_elt (map patterns String.length) (-) |> unwrap in
  let max_a = max_elt (map actions String.length) (-) |> unwrap in
  (max max_p ((String.length label) + 3 + (String.length "Match")), max max_a (String.length "Action"))

(* Create the top edge of the table *)
let top max_p max_a : string =
  let open Core.Std in
  let open Char in
  let fill = String.make (max_p + max_a + 5) '-' in
  Format.sprintf "+%s+\n" fill

(* Create the bottom edge of the table *)
let bottom max_p max_a : string=
  let open Core.Std in
  let fill = String.make (max_p + max_a + 5) '-' in
  Format.sprintf "+%s+\n" fill

(* Create a divider between entries *)
let div max_p max_a : string =
  let open Core.Std in
  let fill = String.make (max_p + max_a + 5) '-' in
  Format.sprintf "|%s|\n" fill

(* Create the columns of the table *)
let title label max_p max_a : string =
  let open Core.Std in
  let pattern = pad max_p (Format.sprintf "%s | Match" label) in
  let action = pad max_a "Action" in
  Format.sprintf "| %s | %s |\n" pattern action

(* Create a row in the table *)
let string_of_entry (max_p : int) (max_a : int) (e : (string list) * (string list)) : string =
  let open Core.Std in
  let open List in
  let padded_patterns = map (fst e) (pad max_p) in
  let padded_actions = map (snd e) (pad max_a) in
  let blank_action = String.make max_a ' ' in
  let blank_pattern = String.make max_p ' ' in
  let rec helper pats acts acc =
    match pats, acts with
    | [], [] -> if (length acc) = 1
		then (Format.sprintf "| %s | %s |\n" blank_pattern blank_action) :: acc
		else acc
    | (p::ps), [] ->
       let acc' = (Format.sprintf "| %s | %s |\n" p blank_action) :: acc in
       helper ps [] acc'
    | [], (a::rest) ->
       let acc' = (Format.sprintf "| %s | %s |\n" blank_pattern a) :: acc in
       helper [] rest acc'
    | (p::ps), (a::rest) ->
       let acc' = (Format.sprintf "| %s | %s |\n" p a) :: acc in
       helper ps rest acc'
  in
  helper padded_patterns padded_actions [(div max_p max_a)]
  |> rev |> String.concat

let parse_layout (layout_string : string) : flow_layout = 
  let open Frenetic_Fdd.Field in
  let opts = [ 
      ("location", Location);
      ("switch", Switch); 
      ("vswitch", VSwitch); 
      ("vport", VPort);
      ("vfabric", VFabric);
      ("ethsrc", EthSrc);
      ("ethdst", EthDst); 
      ("ethtype", EthType); 
      ("vlan", Vlan); 
      ("vlanpcp", VlanPcp);
      ("ip4src", IP4Src); 
      ("ip4dst", IP4Dst);
      ("ipproto", IPProto); 
      ("tcpsrc", TCPSrcPort); 
      ("tcpdst", TCPDstPort);
      ("meta0", Meta0);
      ("meta1", Meta1);
      ("meta2", Meta2);
      ("meta3", Meta3);
      ("meta4", Meta4) ] in 
  (* Break each table def into a string of fields ["ethsrc,ethdst", "ipsrc,ipdst"] *)
  let table_list = Str.split (Str.regexp "[;]" ) layout_string in
  (* Break each string of fields into a list of fields: [["ethsrc","ethdst"],["ipsrc","ipdst"]] *)
  let field_list_list = List.map ~f:(fun t_str -> Str.split (Str.regexp "[,]") t_str) table_list in
  (* This takes a field list [ ethsrc,ethdst ] and converts to Field.t definition *)
  let table_to_fields = 
    List.map ~f:(fun f_str -> 
                  try List.Assoc.find_exn opts f_str
                  with Not_found -> 
                    failwith ("Error parsing table: " ^ f_str ^ " is not a valid field name")) in 
  (* Applies the above to each table definition *)
  let layout = List.map ~f:table_to_fields field_list_list in 
  (* Add any missing fields *)
  let missing = Set.Poly.(to_list (diff (of_list Field.all) (of_list (List.concat layout)))) in 
  layout @ [missing]

(* Given a label and a flowTable, returns an ascii flowtable *)
let string_of_flowTable tableId (flows:multitable_flow list) : string =
  let open Core.Std in
  let label = Format.sprintf "Table(%d)" tableId in 
  let entries = List.map flows to_entry in
  let (max_p, max_a) = table_size label entries in
  let t = (top max_p max_a) in
  let l = (title label max_p max_a) in
  let entry_strings = List.map entries (string_of_entry max_p max_a) in
  let b = bottom max_p max_a in
  String.concat (t :: l :: (List.append entry_strings [b]))

let implement_fdd sw_id layout fdd = 
  Multitable.to_multitable sw_id layout fdd 
  |> Map.Poly.iter ~f:(fun ~key:tableId ~data:flows -> Format.printf "%s\n" (string_of_flowTable tableId flows))

let main pol_file tbl_file () = 
  Printexc.record_backtrace true;
  Format.printf "Starting P4 controller\n%!";
  Format.printf "Policy File  : %s\n%!" pol_file;
  Format.printf "Table File : %s\n%!" tbl_file; 
  let pol_str = In_channel.read_all pol_file in 
  let tbl_str = In_channel.read_all tbl_file in 
  let pol = Frenetic_NetKAT_Parser.policy_of_string pol_str in 
  let layout = parse_layout tbl_str in 
  let compiler_opts = {default_compiler_options with field_order = `Static (List.concat layout)} in
  let fdd = compile_local compiler_opts pol in 
  Format.printf "Order        : ";
  List.iter (Frenetic_Fdd.Field.get_order ()) ~f:(fun f -> Format.printf " %s" (Field.to_string f));
  Format.printf "\n";
  Format.printf "FDD          : %s\n%!" (Frenetic_NetKAT_Compiler.to_string fdd);
  implement_fdd 1L layout fdd;
  Log.info "Done!";
  Pervasives.exit 0
