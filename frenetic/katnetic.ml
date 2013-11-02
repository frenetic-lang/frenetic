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
    Lwt_main.run (Controller.start 6633 (NetCore_Stream.constant exp))
  | _ -> help [ "run" ]

let dump args = match args with 
  | [ filename ] ->
    let cin = open_in filename in
    let pol = Parser.program Lexer.token (Lexing.from_channel cin) in
    let ifm = LocalCompiler.RunTime.compile pol in  
    let tbl = LocalCompiler.RunTime.to_table (VInt.Int64 0L) ifm in 
    Format.printf "%a\n%!" SDN_Types.format_flowTable tbl
  | _ -> help [ "dump" ]

let () = 
  match Array.to_list Sys.argv with
    | (_ :: "run" :: args) ->  run args
    | (_ :: "dump" :: args) -> dump args
    | _ -> help []
