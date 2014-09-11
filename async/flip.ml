open Async.Std


type 'a t =
  { (* [enabled] is a boolean recording the state of the [Flip] application.
       When true the application should take on the value of [policy].
       When false, ti should take on the value [Filter False]. *)
    mutable enabled : bool;
    (* [e_value] is the value that the application should use when the app is
       in an enabled state. *)
    e_value: 'a;
    (* [d_value] is the value that the application should use when the app is
       in an enabled state. *)
    d_value: 'a;
    (* [updates] is the write-side of the pipe for pushing value updates for
       the application. *)
    updates : 'a Pipe.Writer.t;
  }

let create (e_value : 'a) (d_value : 'a) : ('a t * ('r, 'a) Raw_app.t) =
  let open Raw_app in
  let r_updates, w_updates = Pipe.create () in
  let app = create_primitive d_value (fun _ send () ->
    Pipe.close send.pkt_out;
    Deferred.don't_wait_for
      (Pipe.transfer ~f:(fun a -> Async a) r_updates send.update);
    fun e -> Pipe.write send.update EventNoop)
  in
  { enabled = false; e_value; d_value; updates = w_updates }, app

let enable t =
  if t.enabled then
    return ()
  else begin
    t.enabled <- true;
    Pipe.write t.updates t.e_value
    >>= fun () -> Deferred.ignore (Pipe.downstream_flushed t.updates)
  end

let disable t =
  if not t.enabled then
    return ()
  else begin
    t.enabled <- false;
    Pipe.write t.updates t.d_value
    >>= fun () -> Deferred.ignore (Pipe.downstream_flushed t.updates)
  end
