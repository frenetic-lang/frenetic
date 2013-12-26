open Core.Std
open Async.Std

let filter_by_tags (tags : (string * string) list) 
  (messages : Log.Message.t Queue.t) : Log.Message.t Queue.t =
  Queue.filter_map messages (fun msg ->
    match Log.Message.tags msg with
    | [] -> Some msg (* untagged messages are printed indiscriminately *)
    | tags ->
      if List.exists ~f:(List.mem tags) tags then
        Some msg
      else
        None)

let make_colored_filtered_output (tags : (string * string) list) 
  : Log.Output.t =
  Log.Output.create
    (fun queue ->
      Textutils.Console.Log.Output.create 
         ~debug:[`Dim] ~info:[`Blue] ~error:[`Red]
         (Lazy.force Writer.stderr)
         (filter_by_tags tags queue))

let colorized_stderr : Log.Output.t =
  make_colored_filtered_output [("openflow", "")]

let log = lazy (Log.create ~level:`Info ~output:[colorized_stderr])

let set_level = Log.set_level (Lazy.force log)

let set_output = Log.set_output (Lazy.force log)

let raw ?(tags=[]) fmt = Log.raw (Lazy.force log) ~tags fmt

let info ?(tags=[]) fmt = Log.info (Lazy.force log) ~tags fmt

let error ?(tags=[]) fmt = Log.error (Lazy.force log) ~tags fmt

let debug ?(tags=[]) fmt = Log.debug (Lazy.force log) ~tags fmt

let flushed () =
  Log.flushed (Lazy.force log)

let printf ?(tags=[]) ?(level=`Debug) fmt =
  Log.printf (Lazy.force log) ~tags ~level fmt

let of_lazy ?(tags=[]) ?(level=`Debug) lazy_str =
  Log.of_lazy (Lazy.force log) ~tags ~level lazy_str

let sexp ?(tags=[]) ?(level=`Debug) msg =
    Log.sexp (Lazy.force log) ~tags ~level msg

let message = Log.message (Lazy.force log)