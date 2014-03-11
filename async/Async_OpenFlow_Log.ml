open Core.Std
open Async.Std

module TagSet = Set.Make(struct
  type t = string * string with sexp
  let compare = Pervasives.compare
end)

let filter_by_tags (enabled_tags : (string * string) list) =
  let enabled_tags = TagSet.of_list enabled_tags in
  (fun msgs ->
    Queue.filter_map msgs (fun msg ->
      match Log.Message.tags msg with
      | [] -> Some msg (* untagged messages are printed indiscriminately *)
      | msg_tags ->
        if List.exists ~f:(TagSet.mem enabled_tags) msg_tags then
          Some msg
        else
          None))

let label_severity msg =
  let debug, info, error = [`Dim], [`Blue], [`Red] in
  let style, prefix = match Log.Message.level msg with
    | None -> info, ""
    | Some `Debug -> debug, "[DEBUG]"
    | Some `Info  -> info,  " [INFO]"
    | Some `Error -> error, "[ERROR]" in
  String.concat ~sep:" "
    [ prefix
    ; Log.Message.message msg ]

let make_colored_filtered_output (tags : (string * string) list) 
  : Log.Output.t =
  let filter = filter_by_tags tags in
  Log.Output.create
    (fun msgs ->
      let writer = Lazy.force (Writer.stderr) in
       return (Queue.iter (filter msgs) ~f:(fun msg ->
         Writer.write writer (label_severity msg);
         Writer.newline writer)))

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
