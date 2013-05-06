let rec pull_stream n stream =
  lwt v = Lwt_stream.get stream in
  match v with
    | Some x -> Lwt_io.printf "%s%d" n x >> pull_stream n stream
    | None -> Lwt_io.printf "%s: stream closed.\n" n

let main = 
  Lwt_io.printf "in main\n" >>
  let (stream1, push) = Lwt_stream.create () in
  let stream2 = Lwt_stream.clone stream1 in
  let rec pusher n = 
    match n with
      | 0 -> push None; Lwt.return ()
      | n -> (push (Some n); Lwt_main.yield ()) >>
             pusher (n - 1) in
  Lwt.join [pull_stream "*" stream1; pull_stream " " stream2; pusher 200]
      
let _ = Lwt_main.run main
