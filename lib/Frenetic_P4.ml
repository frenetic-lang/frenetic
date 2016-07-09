open Core.Std
open Frenetic_Fdd
open Frenetic_NetKAT

module Compiler = Frenetic_NetKAT_Compiler
module FieldTable : Hashtbl.S with type key = Field.t = Hashtbl.Make(Field)

type fdd_path = Field.t list * Action.t
type parser =
  | Eth
  | Ip
  | Tcp
  | Udp
  | Icmp
  | Igmp1and2
  | Igmp3
  | Igmp
  | Arp
  | VLAN
      [@@deriving show]

type table = int * Field.t list * action list
and action =
      | Actions of Action.t
      | Jump of table

type p4 = parser list * table list

let compile_local =
  let open Compiler in
  compile_local ~options:{ default_compiler_options with cache_prepare = `Keep }

let tables = FieldTable.create ~size:15 ()
let table_id = ref 0

let rec traverse node fields : fdd_path list =
  match FDD.unget node with
  | FDD.Branch ((v,l), t, f) ->
    let fields' = begin match List.hd fields with
      | Some field -> if field = v then fields else v::fields
      | None -> [v] end in
    let true_paths  = traverse t fields' in
    let false_paths = traverse f fields' in
    List.unordered_append true_paths false_paths
  | FDD.Leaf a ->
    [ (fields,a) ]

let update_table field action tbl_opt = match tbl_opt with
  | None ->
    table_id := !table_id + 1;
    (!table_id, [ field ], [action])
  | Some (id,_,actions) ->
    (id, [field], action::actions)

let process_path (p:fdd_path) =
  let rec aux (fields,action) = match fields with
  | [] -> failwith "Empty Fdd path"
  | [f] ->
    FieldTable.update tables f ~f:(update_table f (Actions action ));
    f
  | first::rest ->
    let next = aux (rest, action) in
    let jump = Jump (FieldTable.find_exn tables next) in
    FieldTable.update tables first ~f:(update_table first jump);
    first in
  ignore(aux p)


let p4_of_policy (pol:policy) : p4 =
  let fdd = compile_local pol in
  let paths = traverse fdd [] in
  List.iter paths ~f:process_path;
  let parsers,tbls = FieldTable.fold tables ~init:([],[])
      ~f:(fun  ~key ~data (parsers,tables) -> match key with
      | Vlan
      | VlanPcp -> ( VLAN::parsers, data::tables )
      | EthType
      | EthSrc
      | EthDst -> ( Eth::parsers, data::tables )
      | IP4Src
      | IP4Dst
      | IPProto -> ( Eth::Ip::parsers, data::tables)
      | TCPSrcPort
      | TCPDstPort -> ( Eth::Ip::Tcp::parsers, data::tables )
      | Location
      | Switch
      | VSwitch
      | VPort
      | VFabric
      | Channel ->
        let id,_,_ = data in
        printf "Unsupported match field:%s for table %d\n"
          (Field.to_string key) id;
        (parsers,tables)) in
  (List.dedup parsers, tbls)

let string_of_action a : string = match a with
  | Actions a -> Action.to_string a
  | Jump (id,_,_) -> sprintf "Jump:%d" id
let string_of_table (id,fs,acts) : string =
  let fields = String.concat ~sep:";" (List.map fs Field.to_string) in
  let actions = String.concat ~sep:";" (List.map acts string_of_action) in
  sprintf "Id:%d\tFields:[%s]\tActions:[%s]\n" id fields actions

let string_of_p4 (parsers, tables) =
  let open Buffer in
  let buffer = create 1000 in
  add_string buffer "\nParsers :";
  List.iter parsers ~f:(fun p ->
      add_string buffer "|" ;
      add_bytes buffer (show_parser p) );
  add_string buffer "\nTables:\n";
  List.iter tables ~f:(fun t ->
      add_string buffer (string_of_table t);
      add_string buffer "\n" );
  contents buffer
