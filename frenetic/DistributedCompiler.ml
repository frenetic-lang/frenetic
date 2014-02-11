open Core.Std
open Async.Std
open Async_parallel.Std

let rec range (min : int) (max : int) : int list =
  if min = max then [max] else min :: range (min + 1) max

(* 

 An example configuration:

{
  source: "/home/arjun/abfattree-tables-fail-local-16-3.kat",
  dump: "/home/arjun/abfattree-tables-fail-local-16-3.bin",
  max_switch: 2,
  workers: [ "10.152.136.136" ]

}

*)
type config = {
  source: string;         (* we parse this file *)
  dump: string;           (* we write to this file to parse faster *)
  max_switch: int;        (* switches range from 1 .. max_switch inclusive *)
  workers: string list;   (* hostnames or IP addresses of worker machines  *)
}

let p s = Printf.printf "%s:%s: %s\n%!" (Unix.gethostname ())
  (Pid.to_string (Unix.getpid ())) s

let parse_config (filename : string) : config = 
  let json = Yojson.Basic.from_file filename in
  let open Yojson.Basic.Util in
  let source = json |> member "source" |> to_string in
  let dump = json |> member "dump" |> to_string in
  let max_switch = json |> member "max_switch" |> to_int in
  let workers = json |> member "workers" |> to_list |> filter_string in
  { source; dump; max_switch; workers }

(* Parses src and caches it to dump. It is up to you to "clear the cache"
   by deleting dump if you change src. *)
let parse_caching (src : string) (dump : string) : Types.policy Deferred.t =
  Sys.is_file dump
  >>= function
  | `Yes ->
    In_thread.run (fun () -> Marshal.from_channel (Pervasives.open_in dump))
  | _ -> 
    p "Parsing textual source...";
    In_thread.run 
      (fun () ->
         let out = Pervasives.open_out dump in
         let chan = Pervasives.open_in src in
         let pol = Parser.program Lexer.token (Lexing.from_channel chan) in
         Marshal.to_channel out pol [];
         Pervasives.close_out out;
         pol)

let compile ~pol ~sw () = 
  p (sprintf "starting compile ~sw:%d ~pol:_" sw);
  In_thread.run (fun () -> LocalCompiler.to_table (LocalCompiler.compile (Int64.of_int sw) pol))
  >>= fun tbl ->
  p (sprintf "finished compile ~sw:%d ~pol:_ (flow table has length %d)" sw (List.length tbl));
  return tbl

let compile_par (pol : Types.policy) (sw : int) : unit Deferred.t = 
  p (sprintf "invoking parallel compile ~sw:%d ~pol:_" sw);
  Parallel.run ~where:Parallel.random (compile ~pol ~sw)
  >>= function
  | Ok r -> p (sprintf "compile ~sw:%d ~pol:_ succeeded" sw); return ()
  | Error e -> p (sprintf "compile ~sw:%d ~pol:_ died with %s" sw e); return ()

let distributed_compilation config = 
  (* Lots of blocking I/O and computation while parsing *)
  parse_caching config.source config.dump
  >>= fun pol ->
  let switches = range 1 config.max_switch in
  Deferred.List.map ~how:`Parallel ~f:(compile_par pol) switches 
  >>= fun local_tables ->
  p "All jobs completed.";
  Shutdown.shutdown 0;
  return ()

let main () = 
  match Array.to_list Sys.argv with
    | [_; "master"; filename ] ->
      let config = parse_config filename in
      Parallel.init ~cluster: { Cluster.master_machine = Unix.gethostname();
                                worker_machines = config.workers } ();
      p "Master started";
      let _ = distributed_compilation config in
      never_returns (Scheduler.go ())
    | [_] -> Parallel.init ()
    | _ -> printf "Invalid argument. See source code for help.\n%!"

let () = Exn.handle_uncaught ~exit:true main
