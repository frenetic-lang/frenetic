open Printf
open Lwt
open Lwt_io
open Misc

module Types = Mininet_Types
open Types

let string_of_position p =
  let open Lexing in
  Format.sprintf "%s:%d:%d" p.pos_fname p.pos_lnum (p.pos_cnum - p.pos_bol)

let parse_from_lexbuf lexbuf name =
  let open Lexing in
    try
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = name };
      Mininet_Parser.program Mininet_Lexer.token lexbuf
    with
      |  Failure "lexing: empty token" ->
           failwith (sprintf "lexical error at %s"
                       (string_of_position lexbuf.lex_curr_p))
      | Mininet_Parser.Error ->
           failwith (sprintf "parse error at %s; unexpected token %s"
                       (string_of_position lexbuf.lex_curr_p)
                       (lexeme lexbuf))

let parse_from_chan cin name =
  parse_from_lexbuf (Lexing.from_channel cin) name

let parse_from_string str =
  parse_from_lexbuf (Lexing.from_string str) "<string>"


type mininet = {
  mn_pid : int;
  mn_stdin : output channel;
  mn_stdout : input channel;
  mn_stderr : input channel
}

let rec input_upto_prompt (prompt : string) (chan : input channel) 
  : string list Lwt.t =
  let buf = Buffer.create 100 in
  let rec loop n =
    lwt ch = read_char chan in
    match ch = String.get prompt n with
      | true -> 
        if n = String.length prompt - 1 then
          return []
        else
          loop (n + 1)
      | false ->
        if ch = '\n' then
          loop 0
        else
          begin
            Buffer.add_substring buf prompt 0 n; (* prefix of prompt parsed *)
            Buffer.add_char buf ch; (* this char, which diverges from prompt *)
            lwt line = read_line chan in
            Buffer.add_string buf line;
            let line = Buffer.contents buf in
            Log.printf "mn> %s\n%!" line;
            Buffer.clear buf;
            lwt rest = loop 0 in  (* search for prompt again *) 
            return (line :: rest)
           end in
  loop 0

let interact (mn : mininet) (cmd : string) : string list Lwt.t = 
  Log.printf "mininet> %s\n%!" cmd;
  Lwt_io.fprintf mn.mn_stdin "%s\nsh echo Done. 1>&2\n%!" cmd >>
  input_upto_prompt "Done." mn.mn_stderr

let net (mn : mininet) =
  lwt lines = interact mn "net" in
  return (parse_from_string (String.concat "\n" lines))

let ping (mn : mininet) (count : int) (src : hostAddr) (dst : hostAddr) 
  : bool Lwt.t =
  lwt lines = interact mn (sprintf "h%Ld ping -q -c %d h%Ld" src count dst) in
  let num_lines = List.length lines in
  let b = 
    num_lines >= 2 && 
    Str.string_match (Str.regexp ".*0% packet loss")
      (List.nth lines (num_lines - 2)) 0 in
  if b then
    return true
  else
    begin
      Log.printf "[Mininet.ml] ERROR packets lost during ping.\n%!";
      return false
    end

let ping_all (mn : mininet) (hosts : hostAddr list) : bool Lwt.t = 
  let result = ref true in
  Lwt_list.iter_s
    (fun src ->
      Lwt_list.iter_s
        (fun dst ->
          if src <> dst then
            lwt r = ping mn 1 src dst in
            result := !result && r;
            return ()            
          else
            return ())
        hosts)
    hosts >>
    return !result

let create_mininet_process ?custom:custom (topo:string) : mininet Lwt.t = 
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
    stdin_r stdin_w stderr_w in
  Lwt_main.at_exit
    (fun () ->
     (* TODO(arjun): I'm bad at unix. How do kill gracefully?? *)
      Unix.kill mn_pid Sys.sighup; 
      let _ = Unix.system "sudo mn -c 2> /dev/null" in
      return ());
  let stdin_chan = of_unix_fd output stdin_w in
  let stdout_chan = of_unix_fd input stdout_r in
  let stderr_chan = of_unix_fd input stderr_r in
  let mn = { mn_pid = mn_pid;
             mn_stdin = stdin_chan;
             mn_stdout = stdout_chan;
             mn_stderr = stderr_chan } in
  lwt _ = input_upto_prompt "*** Starting CLI:" mn.mn_stderr in
  return mn

let dump_tables (mn : mininet) (sw : switchId) : unit Lwt.t =
  lwt _ = interact mn (sprintf "s%Ld dpctl dump-flows unix:/tmp/s%Ld" sw sw) in
  return ()

let broadcast_ping (mn : mininet) (count : int) (src : hostAddr) : unit Lwt.t =
  lwt _ = interact mn (sprintf "h%Ld ping -c %d -b 10.255.255.255" src count) in
  return ()

let enable_broadcast_ping (mn : mininet) (host : hostAddr) : unit Lwt.t =
  interact mn 
    (sprintf "h%Ld echo 0 >  /proc/sys/net/ipv4/icmp_echo_ignore_broadcasts" 
       host) >>
  interact mn 
    (sprintf "h%Ld arp -s 10.255.255.255 ff:ff:ff:ff:ff:ff" host) >>
    return ()
