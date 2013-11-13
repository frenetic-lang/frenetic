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

module Run = struct
  open LocalCompiler.RunTime

  let with_channel f chan =
    let exp = Parser.program Lexer.token (Lexing.from_channel chan) in
    Lwt_main.run (Controller.start f 6633 (NetKAT_Stream.constant exp))
  
  let with_file f filename =
    with_channel f (open_in filename)

  let local p =
    (fun sw -> to_table (compile sw p))

  let classic p =
    let p' = Dehop.dehop_policy p in
    local p'

  let automaton p =
    let open NetKAT_Automaton in
    let open Types in
    let i,m,_,e = dehopify p in
    let cache = SwitchMap.mapi (fun sw sw_p ->
      let sw_f  = Types.Filter(Test(Switch, sw)) in
      let sw_p' = Types.(Seq(Seq(i,Seq(sw_f, sw_p)),e)) in
      to_table (compile sw sw_p'))
    m in
    (fun sw -> SwitchMap.find sw cache)

  let main args = 
    match args with
      | [filename]
      | ("local"     :: [filename]) -> with_file local filename
      | ("classic"   :: [filename]) -> with_file classic filename
      | ("automaton" :: [filename]) -> with_file automaton filename
      | _ -> help [ "run" ]
end

module Dump = struct
  open LocalCompiler.RunTime

  let with_channel f chan =
    f (Parser.program Lexer.token (Lexing.from_channel chan))
  
  let with_file f filename =
    with_channel f (open_in filename)

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
    done

  let automaton p =
    let open NetKAT_Automaton in
    let i,m,_,e = dehopify p in
    SwitchMap.iter (fun sw p ->
      let open Types in
      let pol0  = SwitchMap.find sw m in
      let sw_f  = Types.Filter(Test(Switch, sw)) in
      let pol0' = Seq(Seq(i,Seq(sw_f,pol0)),e) in
      let tbl0 = to_table (compile sw pol0') in
      let open VInt in
      Format.printf "@[policy for switch %ld:\n%!%a\n\nflowtable for switch %ld:\n%!%a@\n\n@]%!"
        (get_int32 sw)
        Pretty.format_policy pol0'
        (get_int32 sw)
        SDN_Types.format_flowTable tbl0)
    m

  let main args  =
    match args with
      | [filename]
      | ("local"    :: [filename]) -> with_file local filename
      | ("automaton" :: [filename]) -> with_file automaton filename
      | _ -> help [ "dump" ]

end

let () = 
  match Array.to_list Sys.argv with
    | (_ :: "run"  :: args) -> Run.main args
    | (_ :: "dump" :: args) -> Dump.main args
    | _ -> help []
