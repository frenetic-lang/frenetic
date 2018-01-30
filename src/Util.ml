open! Core

(** {2} timing function calls *)
let time f x =
  let t1 = Unix.gettimeofday () in
  let r = f x in
  let t2 = Unix.gettimeofday () in
  (t2 -. t1, r)

let time' f = time f ()

let timed descr f =
  printf "start %s...\n%!" descr;
  let t, y = time f () in
  printf "--> %s done! %f seconds\n%!" descr t;
  y

let print_time time =
  printf "time: %.4f\n" time


(** {2} mapping pairs  *)
let map_fst xs ~f =
  List.map xs ~f:(fun (x,y) -> (f x, y))

let map_snd xs ~f =
  List.map xs ~f:(fun (x,y) -> (x, f y))

let map_both xs ~f =
  List.map xs ~f:(fun (x,y) -> (f x, f y))



(** {2} useful higher order functions  *)
let tap x ~f =
  f x; x

let iter n ~f ~init =
  let rec loop n acc = if n <= 0 then acc else loop (n-1) (f acc) in
  loop n init



(*===========================================================================*)
(* open files with default application                                       *)
(*===========================================================================*)

type os =
  | MacOS
  | Linux

let detect_os () : os =
  let open Caml in
  let ic = Unix.open_process_in "uname" in
  let uname = input_line ic in
  close_in ic;
  match uname with
  | "Darwin" -> MacOS
  | "Linux" -> Linux
  | _ -> failwith "unknown operating system"

let open_cmd =
  match detect_os () with
  | MacOS -> "open"
  | Linux -> "xdg-open"

let open_file f =
  let silence = "&> /dev/null" in
  Format.sprintf "%s %s % s" open_cmd f silence
  |> Caml.Unix.system
  |> ignore

(*===========================================================================*)
(* Graphviz                                                                  *)
(*===========================================================================*)

let compile_dot ?(format="pdf") ?(engine="dot") ?(title=engine) data : string =
  let output_file = Filename.temp_file (title ^ "_") ("." ^ format) in
  let to_dot = Unix.open_process_out (sprintf "dot -T%s -o %s" format output_file) in
  Out_channel.output_string to_dot data;
  Out_channel.close to_dot;
  ignore (Unix.close_process_out to_dot);
  output_file

let show_dot ?format ?title ?engine data : unit =
  compile_dot ?format ?title ?engine data
  |> open_file

let show_dot_file ?format ?title ?engine file : unit =
  In_channel.read_all file
  |> show_dot ?format ?title ?engine



(*===========================================================================*)
(* Logging                                                                   *)
(*===========================================================================*)

let sandboxed ?timeout ~logfile ~f : [`Ok | `Tout | `Err | `Int ] =
  let read_done_fd, write_done_fd = Unix.pipe () in
  let read_done = Unix.in_channel_of_descr read_done_fd in
  let write_done = Unix.out_channel_of_descr write_done_fd in
  match Unix.fork () with
  | `In_the_child ->
    Out_channel.with_file logfile ~append:true ~f:(fun log ->
      (* redirect stderr and stdout to log  *)
      let log = Unix.descr_of_out_channel log in
      Unix.dup2 log Unix.stdout;
      Unix.dup2 log Unix.stderr;

      (* then run closure *)
      match f () with
      | exception e ->
        Format.printf "%a\n%!" Exn.pp e;
        Out_channel.fprintf write_done "Err\n%!";
        exit 1
      | () ->
        Out_channel.fprintf write_done "Ok\n%!";
        exit 0
    )
  | `In_the_parent pid ->
    protect ~finally:(fun () ->
      Signal.(send_i kill (`Pid pid));
      Caml.Sys.catch_break false;
      In_channel.close read_done;
      Out_channel.close write_done;
    ) ~f:(fun () ->
      match
        (* throw exception upon interrupt *)
        Caml.Sys.catch_break true;
        let timeout = match timeout with
          | None -> `Never
          | Some t -> `After (Time_ns.Span.of_sec (Float.of_int t))
        in
        Unix.select ~restart:true ~read:[read_done_fd] ~write:[]
          ~except:[] ~timeout ()
        |> ignore;
        In_channel.input_line read_done
      with
        | None -> `Tout
        | Some "Ok" -> `Ok
        | Some "Err" -> `Err
        | Some _ -> failwith "unexpected message"
        | exception Caml.Sys.Break -> `Int
    )


let green = "\027[32m"
let red = "\027[31m"
let no_color = "\027[0m"
let ok = sprintf "%s[OK]%s" green no_color
let err = sprintf "%s[ERR]%s" red no_color
let intr = sprintf "%s[INTR]%s" red no_color
let tout = sprintf "%s[TOUT]%s" red no_color

let log_and_sandbox ?timeout ~logfile descr ~f : unit =
  printf "%s%!\t" descr;
  let t, status = time (fun () -> sandboxed ?timeout ~logfile ~f) () in
  begin match status with
  | `Ok -> printf "%s" ok
  | `Err -> printf "%s" err
  | `Int -> printf "%s" intr
  | `Tout -> printf "%s" tout
  end;
  printf "\t(%.3f seconds)\n%!" t
