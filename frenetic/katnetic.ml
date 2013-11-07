open PolicyGenerator

let help args =
  match args with 
    | [ "run" ] -> 
      Format.printf "usage: katnetic run [filename] \n"
    | [ "dump" ] -> 
      Format.printf "usage: katnetic dump [filename] \n"
    | _ -> 
      Format.printf "%s" ("usage: katnetic <command> \n" ^
			  "  run    Compile and start the controller\n" ^ 
			  "  dump   Compile and dump flow table\n")


let run args = match args with
  | [ filename ] ->
    let cin = open_in filename in
    let exp = Parser.program Lexer.token (Lexing.from_channel cin) in
    Lwt_main.run (Controller.start 6633 (NetKAT_Stream.constant exp))
  | _ -> help [ "run" ]

let dump args = match args with 
  | [ filename ] ->
    let cin = open_in filename in
    let pol = Parser.program Lexer.token (Lexing.from_channel cin) in
    let i,m,_,e = NetKAT_Automaton.dehopify pol in
    let switch_policies =
        NetKAT_Automaton.switch_port_policies_to_switch_policies m in
    NetKAT_Automaton.SwitchMap.iter (fun sw p ->
      let pol0 =
        let open Types in
        let open SDN_Types in
        Seq(Filter(Test(Switch, sw)),
            NetKAT_Automaton.SwitchMap.find sw switch_policies) in
      let pol0' = Types.(Seq(Seq(Par(i, id),pol0),Par(e, id))) in
      let ifm0 = LocalCompiler.RunTime.compile pol0' in
      let tbl0 = LocalCompiler.RunTime.to_table sw ifm0 in
      Format.printf "@[%a\n%a@\n\n@]%!"
        Pretty.format_policy pol0'
        SDN_Types.format_flowTable tbl0)
    switch_policies
  | _ -> help [ "dump" ]

let () = 
  match Array.to_list Sys.argv with
    | (_ :: "run" :: args) ->  run args
    | (_ :: "dump" :: args) -> dump args
    | _ -> help []
