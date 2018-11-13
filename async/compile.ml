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

  let combine _ p q =
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
  +> flag "j" (optional int) ~doc:" number of processes to run in parallel"
)

let cmd =
  Command.async_spec
    ~summary:"Compiler given Probnetkat policy to FDD in parallel"
    cmd_spec
    (fun order cps parallelize bound local () ->
      let order = [%of_sexp: Symbolic.Field.t list] order in
      let param = T.Param.{ bound; order; cps; parallelize } in
      let rpc_config =
        Rpc_parallel.Map_reduce.Config.create
          ?local
          ~redirect_stderr:(`Dev_null)
          ~redirect_stdout:(`Dev_null)
          ()
      in
      let stdin = Lazy.force (Async_unix.Reader.stdin) in
      let stdout = Lazy.force (Async_unix.Writer.stdout) in
      let pols = Pipe.unfold ~init:() ~f:(fun () ->
        eprintf "[compile server] attempt read...\n%!";
        Async_unix.Reader.read_bin_prot stdin ~max_len:1_000_000
          (Syntax.bin_reader_policy Symbolic.Field.bin_reader_t)
        >>| function
          | `Ok p -> Some (p, ())
          | `Eof ->
            eprintf "[compile server] -> inputs received!\n%!";
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
        Async_unix.Writer.write_bin_prot stdout Symbolic.Fdd.bin_writer_t fdd;
(*         Symbolic.Fdd.serialize fdd
        |> Async_unix.Writer.write_line stdout; *)
        eprintf "[compile server] -> done.\n%!";
        let%bind () = Async_unix.Writer.close stdout in
        eprintf "[compile server] Shuttind down. Bye.\n%!";
        Deferred.unit
    )

let rpc_heartbeat_config = Rpc.Connection.Heartbeat_config.create
  ~timeout:Time_ns.Span.hour ~send_every:Time_ns.Span.hour

let () = Rpc_parallel.start_app cmd ~rpc_heartbeat_config
