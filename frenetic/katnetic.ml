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
    (fun sw -> to_table (compile sw p)) in  

  let classic p =
    let p' = Dehop.dehop_policy p in
    local p' in 

  let automaton p =
    let open NetKAT_Automaton in
    let open Types in
    let i,m,_,e = dehopify p in
    let switch_policies = switch_port_policies_to_switch_policies m in
    let cache = SwitchMap.mapi (fun sw sw_p ->
      let sw_f  = Types.Filter(Test(Switch, sw)) in
      let sw_p' = Types.(Seq(Seq(i,Seq(sw_f, sw_p)),e)) in
      to_table (compile sw sw_p'))
    switch_policies in
    (fun sw -> SwitchMap.find sw cache) in

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
    (* NOTE(seliopou): This may not catch all ports, but it'll catch some of
     * 'em! Also, lol for loop.
     * *)
    for sw = 0 to 40 do
      let vs = VInt.Int64 (Int64.of_int sw) in 
      let sw_p = Types.(Seq(Filter(Test(Switch,vs)), p)) in 
      let table = to_table (compile vs sw_p) in 
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
      let sw_f  = Types.Filter(Test(Switch, sw)) in
      let pol0' = Seq(Seq(i,Seq(sw_f,pol0)),e) in
      let tbl0 = to_table (compile sw pol0') in
      let open VInt in
      Format.printf "@[policy for switch %ld:\n%!%a\n\nflowtable for switch %ld:\n%!%a@\n\n@]%!"
        (get_int32 sw)
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
