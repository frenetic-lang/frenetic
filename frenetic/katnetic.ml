open PolicyGenerator

let help args =
	  Format.printf "Unknown command\n"

let run args = match args with
  | [ filename ] ->
    let cin = open_in filename in
    let exp = Parser.program Lexer.token (Lexing.from_channel cin) in
    Lwt_main.run (Controller.start 6633 (NetCore_Stream.constant exp))
  | _ -> help [ "run" ]

let () = 
  match Array.to_list Sys.argv with
    | (_ :: "run" :: args) ->  run args
    | _ -> help []
