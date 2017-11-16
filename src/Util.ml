open Core

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

let tap x ~f =
  f x; x

(* Given p_1, ..., p_n s.t. sum p_i = 1, we are looking for
         b_1, ..., b_n s.t.
      b1 = p1
  /\  (1-b1)b2 = p2
  /\  (1-b1)(1-b2)b3 = p3 ... etc.
  Solving for b_i, we get b_i = p_i/(\prod_{j<i}(1-b_i))
*)
let n_ary_to_binary_right_associative_probs ps =
  let init = (Prob.one, []) in
  List.fold_left ps ~init ~f:(fun (prod, acc) p_i ->
    let b_i = Prob.(p_i / prod) in
    let prod = Prob.(prod * (one - p_i)) in
    (prod, b_i::acc)
  )
  |> fun (_, acc) -> List.rev acc


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
