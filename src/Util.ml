open! Core

let time f x =
  let t1 = Unix.gettimeofday () in
  let r = f x in
  let t2 = Unix.gettimeofday () in
  (t2 -. t1, r)

let timed descr f =
  printf "start %s...\n%!" descr;
  let t, y = time f () in
  printf "--> %s done! %f seconds\n%!" descr t;
  y

let print_time time =
  printf "time: %.4f\n" time

let map_fst xs ~f =
  List.map xs ~f:(fun (x,y) -> (f x, y))

let map_snd xs ~f =
  List.map xs ~f:(fun (x,y) -> (x, f y))

let map_both xs ~f =
  List.map xs ~f:(fun (x,y) -> (f x, f y))

let tap x ~f =
  f x; x


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

let compile_dot ?(format="pdf") ?(title="dot") data : string =
  let output_file = Filename.temp_file (title ^ "_") ("." ^ format) in
  let to_dot = Unix.open_process_out (sprintf "dot -T%s -o %s" format output_file) in
  Out_channel.output_string to_dot data;
  Out_channel.close to_dot;
  ignore (Unix.close_process_out to_dot);
  output_file

let show_dot ?format ?title data : unit =
  compile_dot ?format ?title data
  |> open_file



(*===========================================================================*)
(* modified Unix module                                                      *)
(*===========================================================================*)

(* adapted from https://github.com/ocaml/ocaml/blob/
   c5fe6932b2151d0e4426072b4df3510318bc4edc/otherlibs/unix/unix.ml
*)
module Unix = struct
  open Caml.Unix

  external sys_exit : int -> 'a = "caml_sys_exit"

  let rec file_descr_not_standard fd =
    if Unix.File_descr.to_int fd >= 3 then fd else file_descr_not_standard (dup fd)

  let safe_close fd =
    try close fd with Unix_error(_,_,_) -> ()

  let perform_redirections new_stdin new_stdout new_stderr =
    let new_stdin = file_descr_not_standard new_stdin in
    let new_stdout = file_descr_not_standard new_stdout in
    let new_stderr = file_descr_not_standard new_stderr in
    (*  The three dup2 close the original stdin, stdout, stderr,
        which are the descriptors possibly left open
        by file_descr_not_standard *)
    dup2 ~cloexec:false new_stdin stdin;
    dup2 ~cloexec:false new_stdout stdout;
    dup2 ~cloexec:false new_stderr stderr;
    safe_close new_stdin;
    safe_close new_stdout;


  type popen_process =
      Process of In_channel.t * Out_channel.t
    | Process_in of In_channel.t
    | Process_out of Out_channel.t
    | Process_full of In_channel.t * Out_channel.t * In_channel.t

  let open_proc cmd envopt proc input output error =
    match fork() with
    | 0 -> perform_redirections input output error;
            let shell = "/bin/sh" in
            let argv = [| shell; "-c"; cmd |] in
            begin try
              match envopt with
              | Some env -> execve shell argv env
              | None     -> execv shell argv
            with _ ->
              sys_exit 127
            end
     | id -> id

  let open_process cmd =
    let (in_read, in_write) = pipe ~cloexec:true () in
    let (out_read, out_write) =
      try pipe ~cloexec:true ()
      with e -> close in_read; close in_write; raise e in
    let inchan = in_channel_of_descr in_read in
    let outchan = out_channel_of_descr out_write in
    let pid = begin
      try
        open_proc cmd None (Process(inchan, outchan)) out_read in_write stderr
      with e ->
        close out_read; close out_write;
        close in_read; close in_write;
        raise e
    end in
    close out_read;
    close in_write;
    (Core.Pid.of_int pid, inchan, outchan)

end
