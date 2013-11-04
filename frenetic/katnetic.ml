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
    Lwt_main.run (Controller.start 6633 (Stream.constant exp))
  | _ -> help [ "run" ]

let dump args = match args with 
  | [ filename ] ->
    let sw = VInt.Int64 1L in 
    let cin = open_in filename in
    let pol = Parser.program Lexer.token (Lexing.from_channel cin) in
    let i,m,_ = NetKAT_Automaton.dehopify pol in     
    let pol0 = 
      let open Types in 
      let open SDN_Types in 
      NetKAT_Automaton.SwitchMap.fold 
        (fun (swi,pti) poli acc -> 
          if sw = swi then 
            Par(Seq(Filter (And(Test(Switch, swi), 
                                Test(Header InPort, pti))),
                    poli),
                acc)
          else 
            acc) 
        m Types.drop in 
    let pol0' = Types.Seq(i,pol0) in 
    let ifm0 = LocalCompiler.RunTime.compile pol0' in 
    let tbl0 = LocalCompiler.RunTime.to_table sw ifm0 in 
    Format.printf "@[%a\n%a@\n@]%!"
      Pretty.format_policy pol0
      SDN_Types.format_flowTable tbl0
  | _ -> help [ "dump" ]

let () = 
  match Array.to_list Sys.argv with
    | (_ :: "run" :: args) ->  run args
    | (_ :: "dump" :: args) -> dump args
    | _ -> help []
