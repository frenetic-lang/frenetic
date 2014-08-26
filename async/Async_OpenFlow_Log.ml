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

let make_filtered_output (tags : (string * string) list) 
  : Log.Output.t =
  let filter = filter_by_tags tags in
  Log.Output.create
    (fun msgs ->
      let writer = Lazy.force (Writer.stderr) in
       return (Queue.iter (filter msgs) ~f:(fun msg ->
         Writer.write writer (label_severity msg);
         Writer.newline writer)))

let current_outputs = ref []

let stderr : Log.Output.t =
  make_filtered_output [("openflow", "")]

let log = lazy (Log.create ~level:`Info ~output:[stderr])

let level () = Log.level (Lazy.force log)
let set_level = Log.set_level (Lazy.force log)

let set_output outputs = current_outputs := outputs;
  Log.set_output (Lazy.force log) outputs

let add_output outputs =
  let outputs = outputs @ !current_outputs in
  current_outputs := outputs;
  set_output outputs

let raw ?time ?(tags=[]) fmt = Log.raw (Lazy.force log) ?time ~tags fmt

let info ?time ?(tags=[]) fmt = Log.info (Lazy.force log) ?time ~tags fmt

let error ?time ?(tags=[]) fmt = Log.error (Lazy.force log) ?time ~tags fmt

let debug ?time ?(tags=[]) fmt = Log.debug (Lazy.force log) ?time ~tags fmt

let flushed () =
  Log.flushed (Lazy.force log)

let printf ?(level=`Debug) ?time ?(tags=[]) fmt =
  Log.printf (Lazy.force log) ~tags ~level fmt

let of_lazy ?(level=`Debug) ?time ?(tags=[]) lazy_str =
  (* As of core/async.111.25.00, `Log.of_lazy` is no longer part of that
   * package's public API. In 111.28.00, the `Log.level` call was added,
   * allowing users of the package to implement `of_lazy` without having to
   * manage the log level manually.
   * *)
  if level = Log.level (Lazy.force log) then
    Log.printf (Lazy.force log) ~tags ~level "%s" (Lazy.force lazy_str)

let sexp ?(level=`Debug) ?time ?(tags=[]) msg =
  Log.sexp (Lazy.force log) ~tags ~level msg

let string ?(level=`Debug) ?time ?(tags=[]) str =
  Log.string (Lazy.force log) ~tags ~level str

let message = Log.message (Lazy.force log)
