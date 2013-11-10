open PolicyGenerator

let help args =
  match args with 
    | [ "run" ] -> 
      Format.printf "usage: katnetic run [local|classic|automaton] [filename] \n"
    | [ "dump" ] -> 
      Format.printf "usage: katnetic dump [filename] \n"
    | _ -> 
      Format.printf "%s" ("usage: katnetic <command> \n" ^
			  "  run    Compile and start the controller\n" ^ 
			  "  dump   Compile and dump flow table\n")


let run_with_channel f chan =
  let exp = Parser.program Lexer.token (Lexing.from_channel chan) in
  Lwt_main.run (Controller.start f 6633 (NetKAT_Stream.constant exp))

let run_with_file f filename =
  run_with_channel f (open_in filename)

let run args =
  let open LocalCompiler.RunTime in

  let local p =
    let i = compile p in
    (fun sw -> to_table sw i) in

  let classic p =
    let p' = Dehop.dehop_policy p in
    let i = compile p' in
    (fun sw -> to_table sw i) in

  let automaton p =
    let open NetKAT_Automaton in
    let open Types in
    let i,m,_,e = dehopify p in
    let switch_policies = switch_port_policies_to_switch_policies m in
    (fun sw ->
      let sw_p  = SwitchMap.find sw switch_policies in
      let sw_p' = Types.(Seq(Seq(Par(i,id),sw_p),Par(e,id))) in
      to_table sw (compile sw_p')) in

  match args with
    | [filename]
    | ("local"     :: [filename]) -> run_with_file local filename
    | ("classic"   :: [filename]) -> run_with_file classic filename
    | ("automaton" :: [filename]) -> run_with_file automaton filename
    | _ -> help [ "run" ]


let dump_with_channel f chan =
  f (Parser.program Lexer.token (Lexing.from_channel chan))

let dump_with_file f filename =
  dump_with_channel f (open_in filename)

let dump args =
  let open LocalCompiler.RunTime in

  let local p =
    Format.printf "@[%a\n\n@]%!" Pretty.format_policy p;
    let i = compile p in
    (* NOTE(seliopou): This may not catch all ports, but it'll catch some of
     * 'em! Also, lol for loop.
     * *)
    for sw = 0 to 40 do
      let table = to_table (VInt.Int64 (Int64.of_int sw)) i in
      if List.length table > 0 then
        Format.printf "@[flowtable for switch %d:\n%a@\n\n@]%!" sw
          SDN_Types.format_flowTable table;
    done in

  let automaton p =
    let open NetKAT_Automaton in
    let i,m,_,e = dehopify p in
    let switch_policies = switch_port_policies_to_switch_policies m in
    SwitchMap.iter (fun sw p ->
      let open Types in
      let pol0  = SwitchMap.find sw switch_policies in
      let pol0' = Seq(Seq(Par(i,id),pol0),Par(e,id)) in
      let tbl0 = to_table sw (compile pol0') in
      let open VInt in
      Format.printf "@[%a\nflowtable for switch %ld:\n%a@\n\n@]%!"
        Pretty.format_policy pol0'
        (get_int32 sw)
        SDN_Types.format_flowTable tbl0)
    switch_policies in

  match args with
    | [filename]
    | ("local"    :: [filename]) -> dump_with_file local filename
    | ("automaton" :: [filename]) -> dump_with_file automaton filename
    | _ -> help [ "dump" ]

let () = 
  match Array.to_list Sys.argv with
    | (_ :: "run" :: args) ->  run args
    | (_ :: "dump" :: args) -> dump args
    | _ -> help []
