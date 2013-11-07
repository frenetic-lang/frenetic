open PolicyGenerator

let help args =
  match args with 
    | [ "run" ] -> 
      Format.printf "usage: katnetic run [local|classic] [filename] \n"
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

  match args with
    | [filename]
    | ("local"   :: [filename]) -> run_with_file local filename
    | ("classic" :: [filename]) -> run_with_file classic filename
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
