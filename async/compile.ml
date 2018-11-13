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
    } [@@deriving bin_io]
  end

  type state_type = Param.t

  let init (Param.{ bound; order } as params) =
    Symbolic.Field.set_order order;
    return params

  let map Param.{ bound; } p = 
    Symbolic.Fdd.of_pol_k bound p ident
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
  +> flag "bound" (optional int) ~doc:" bounded iteration"
  +> flag "j" (optional int) ~doc:" number of processes to run in parallel"
)

let cmd =
  Command.async_spec
    ~summary:"Compiler given Probnetkat policy to FDD in parallel"
    cmd_spec
    (fun order bound local () ->
      let order = [%of_sexp: Symbolic.Field.t list] order in
      let param = T.Param.{ bound; order } in
      let rpc_config =
        Rpc_parallel.Map_reduce.Config.create
          ?local
          ~redirect_stderr:(`Dev_null)
          ~redirect_stdout:(`Dev_null)
          ()
      in
      let stdin = Lazy.force (Async_unix.Reader.stdin) in
      let stdout = Lazy.force (Async_unix.Writer.stdout) in
    (*   let pols = Pipe.unfold ~init:() ~f:(fun () -> 
        Async_unix.Reader.read_bin_prot stdin
          (Syntax.bin_reader_policy Symbolic.Field.bin_reader_t)
        >>| function `Ok p -> Some (p, ()) | `Eof -> None
      )
      in *)
      let pols = 
        Async_unix.Reader.lines stdin
        |> Pipe.map ~f:Sexp.of_string
        |> Pipe.map ~f:[%of_sexp: Symbolic.Field.t Syntax.policy]
      in
      let%bind result =
        Rpc_parallel.Map_reduce.map_reduce_commutative
          rpc_config
          pols
          ~m:(module Compile)
          ~param
      in
      match result with
      | None -> Deferred.unit
      | Some fdd ->
        eprintf "about to write...\n\n";
        Async_unix.Writer.write_bin_prot stdout Symbolic.Fdd.bin_writer_t fdd
        |> return
    )


let () = Rpc_parallel.start_app cmd
