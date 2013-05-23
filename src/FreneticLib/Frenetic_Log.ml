module Log = struct

  let logger = ref stderr

  let get_log_chan () : out_channel = !logger

  let set_log_file (replace : bool) (filename : string) : unit =
    let chan = 
      if replace then open_out filename
      else 
        open_out_gen 
          [Open_wronly; Open_text; Open_creat; Open_excl]
          0o600 filename in
    logger := chan;
    at_exit (fun () -> close_out chan)

  let printf (log:string) (fmt : ('a, out_channel, unit) format) : 'a =
    Printf.fprintf !logger "[%s] " log;
    Printf.fprintf !logger fmt
end
