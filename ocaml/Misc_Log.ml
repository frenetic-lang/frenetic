let log = ref stderr

let get_log_chan () : out_channel = !log

let set_log_file (replace : bool) (filename : string) : unit =
  let chan = 
    match replace with
      | true -> open_out filename 
      | false -> 
        open_out_gen [Open_wronly; Open_text; Open_creat; Open_excl] 
          0o600 filename in
  log := chan;
  at_exit (fun () -> close_out chan)

let printf (fmt : ('a, out_channel, unit) format) : 'a =
  Printf.printf ".%!";
  Printf.fprintf !log fmt
