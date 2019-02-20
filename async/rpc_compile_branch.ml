open Probnetkat
open Core
open Async
open Stdio

module T = struct

  module Input = struct
    type t = Symbolic.Field.t Syntax.policy [@@deriving bin_io]
  end

  module Accum = struct
    type t = Symbolic.Fdd.t [@@deriving bin_io]
  end

  module Param = struct
    type t = {
      bound : int option;
      order : Symbolic.Field.t list;
      cps : bool;
      parallelize : bool;
    } [@@deriving bin_io]
  end

  type state_type = Param.t

  let init (Param.{ order } as params) =
    Symbolic.Field.set_order order;
    return params

  let map Param.{ bound; cps; parallelize } p =
    (if cps then
      Symbolic.Fdd.(of_pol_cps ~parallelize ?bound id p)
    else
      Symbolic.Fdd.of_pol_k ~parallelize ?bound p)
    |> return

  let combine _ (p : Accum.t) (q : Accum.t) =
    (* SJS: this line is key for keeping memory consumption under control. *)
    Symbolic.Fdd.clear_cache ~preserve:(Int.Set.of_list [(p:>int); (q:>int)]);
    Symbolic.Fdd.(sum p q)
    |> return

end

module Compile = Rpc_parallel.Map_reduce.Make_map_reduce_function_with_init (T)


(* let write_fdd (fdd : Symbolic.Fdd.t) =
  Bin_prot.Util.bin_dump ~header:true Symbolic.Fdd.bin_writer_t fdd *)

let cmd_spec = Command.Spec.(
  empty
  +> flag "order" (required sexp) ~doc:" Fdd variable ordering, as s-expression."
  +> flag "cps" (required bool) ~doc:" use CPS-style left-to-right compilation."
  +> flag "parallelize" (optional_with_default false bool)
    ~doc:" parallelize subcomputations further."
  +> flag "bound" (optional int) ~doc:" bounded iteration"
  +> flag "j" (optional int) ~doc:" number of processes to run locally (default: # logical cores)"
  +> flag "rj" (optional int) ~doc:" number of processes to run remotely per remote (default: j)"
  +> flag "remote" (listed string) ~doc: " remote host on which to spawn workers"
)

let cmd =
  Command.async_spec
    ~summary:"Compiler given Probnetkat policy to FDD in parallel"
    cmd_spec
    (fun order cps parallelize bound j rj remote () ->
      eprintf "[compile server] %s\n%!"
        (Sys.argv |> Array.to_list |> String.concat ~sep:" ");
      let wdir = "/tmp/" in
      let exe =
        [%here].Lexing.pos_fname
        |> Filename.basename
        |> Filename.chop_extension
        |> (^) "probnetkat."
        |> Filename.concat wdir
      in
      let order = [%of_sexp: Symbolic.Field.t list] order in
      let param = T.Param.{ bound; order; cps; parallelize } in
      let local = match j with
        | Some j -> j
        | None -> Or_error.ok_exn Linux_ext.cores ()
      in
      let rj = Option.value rj ~default:local in
      let%bind remote =
        List.map remote ~f:(fun h ->
          Rpc_parallel.Remote_executable.copy_to_host h
            ~executable_dir:wdir
          >>| function
          | Ok host -> (host, rj)
          | Error _ -> failwith ("unable to connect to host: " ^ h)
        )
        |> Deferred.all
      in
(*       let remote =
        List.map remote ~f:(fun h ->
          Rpc_parallel.Remote_executable.existing_on_host h
            ~executable_path:exe,
          rj
        )
      in *)
      let rpc_config =
        Rpc_parallel.Map_reduce.Config.create
          ~local
          ~remote
          ~cd:wdir
          ~redirect_stderr:(`File_append "rpc_compile_branch.worker.err")
          ~redirect_stdout:(`File_append "rpc_compile_branch.worker.out")
          ()
      in
      let stdin = Lazy.force (Async_unix.Reader.stdin) in
      let stdout = Lazy.force (Async_unix.Writer.stdout) in
      let inp = ref 1 in
      let pols = Pipe.unfold ~init:() ~f:(fun () ->
        eprintf "[compile server] read: input %d ..." (!inp);
        incr inp;
        Async_unix.Reader.read_bin_prot stdin ~max_len:1_000_000_000
          (Syntax.bin_reader_policy Symbolic.Field.bin_reader_t)
        >>| function
          | `Ok p ->
            eprintf " received.\n%!";
            (* Format.eprintf "%a\n" (Syntax.pp_policy Symbolic.Field.pp) p; *)
            Some (p, ())
          | `Eof ->
            eprintf " eof.\n";
            eprintf "[compile server] -> all inputs received!\n%!";
            None
      )
      in
(*       let pols =
        Async_unix.Reader.lines stdin
        |> Pipe.map ~f:Sexp.of_string
        |> Pipe.map ~f:[%of_sexp: Symbolic.Field.t Syntax.policy]
      in *)
      let%bind result =
        Rpc_parallel.Map_reduce.map_reduce_commutative
          rpc_config
          pols
          ~m:(module Compile)
          ~param
      in
      match result with
      | None ->
        eprintf "Error - no result";
        Deferred.unit
      | Some fdd ->
        eprintf "[compile server] sending back result...\n%!";
        (* Format.eprintf "%a\n" (Symbolic.Fdd.pp ~show:true) fdd; *)
        Async_unix.Writer.write_bin_prot stdout Symbolic.Fdd.bin_writer_t fdd;
(*         Symbolic.Fdd.serialize fdd
        |> Async_unix.Writer.write_line stdout; *)
        eprintf "[compile server] -> done.\n%!";
        let%bind () = Async_unix.Writer.close stdout in
        eprintf "[compile server] Shutting down. Bye.\n%!";
        Deferred.unit
    )

let rpc_heartbeat_config = Rpc.Connection.Heartbeat_config.create
  ~timeout:Time_ns.Span.hour ~send_every:Time_ns.Span.hour

let () = Rpc_parallel.start_app cmd ~rpc_heartbeat_config
