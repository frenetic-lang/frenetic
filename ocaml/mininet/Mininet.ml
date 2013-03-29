open Printf

let string_of_position p =
  let open Lexing in
  Format.sprintf "%s:%d:%d" p.pos_fname p.pos_lnum (p.pos_cnum - p.pos_bol)

let parse_from_lexbuf lexbuf name =
  let open Lexing in
    try
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = name };
      MininetParser.program MininetLexer.token lexbuf
    with
      |  Failure "lexing: empty token" ->
           failwith (sprintf "lexical error at %s"
                       (string_of_position lexbuf.lex_curr_p))
      | MininetParser.Error ->
           failwith (sprintf "parse error at %s; unexpected token %s"
                       (string_of_position lexbuf.lex_curr_p)
                       (lexeme lexbuf))

let parse_from_chan cin name =
  parse_from_lexbuf (Lexing.from_channel cin) name

let parse_from_string str =
  parse_from_lexbuf (Lexing.from_string str) "<string>"


type mininet = {
  mn_pid : int;
  mn_stdin : out_channel;
  mn_stdout : in_channel;
  mn_stderr : in_channel
}

let rec input_upto_prompt (prompt : string) (chan : in_channel) : string =
  let buf = Buffer.create 100 in
  let rec loop n =
    let ch = input_char chan in
    match ch = String.get prompt n with
      | true -> 
        if n = String.length prompt - 1 then
          ()
        else
          loop (n + 1)
      | false ->
        Buffer.add_substring buf prompt 0 n; (* prefix of prompt parsed *)
        Buffer.add_char buf ch; (* this char, which diverges from the prompt *)
        let line = input_line chan in
        Buffer.add_string buf line;
        Buffer.add_char buf '\n';
        eprintf "mn> %s%c%s\n%!" (String.sub prompt 0 n) ch line;
        loop 0 (* search for prompt again *) in
  loop 0;
  Buffer.contents buf

let interact (mn : mininet) (cmd : string) : string = 
  fprintf mn.mn_stdin "%s\nsh echo Done.1>&2\n%!" cmd;
  input_upto_prompt "Done." mn.mn_stderr

let net (mn : mininet) =
  parse_from_string (interact mn "net")

let ping_all (mn : mininet) : bool = 
  let str = interact mn "pingall" in
  true

let create_mininet_process ?custom:custom (topo:string) : mininet =
  let (stdin_r, stdin_w) = Unix.pipe () in
  let (stdout_r, stdout_w) = Unix.pipe () in
  let (stderr_r, stderr_w) = Unix.pipe () in
  let argv = 
    ["sudo"; "mn"; "--switch=user"; "--controller=remote"] @
      (match custom with
        | None -> []
        | Some py_file -> ["--custom"; py_file]) @
      ["--topo"; topo; "--arp"; "--mac"] in
  let mn_pid = Unix.create_process "sudo" (Array.of_list argv)
    stdin_r Unix.stdout stderr_w in
  let stdin_chan = Unix.out_channel_of_descr stdin_w in
  let stdout_chan = Unix.in_channel_of_descr stdout_r in
  let stderr_chan = Unix.in_channel_of_descr stderr_r in
  set_binary_mode_out stdin_chan false;
  set_binary_mode_in stdout_chan false;
  set_binary_mode_in stderr_chan false; 
  let mn = { mn_pid = mn_pid;
             mn_stdin = stdin_chan;
             mn_stdout = stdout_chan;
             mn_stderr = stderr_chan } in
  let _ = input_upto_prompt "*** Starting CLI:" mn.mn_stderr in
  mn
