open Core.Std
open Async.Std

open NetKAT_Types

(* A collection of pipes that apps use to communicate with the controller.
 * Currently this allows apps to send packet out messages and push value
 * updates.
 * *)
type 'a send = {
  pkt_out : (switchId * SDN_Types.pktOut) Pipe.Writer.t;
  update  : 'a Pipe.Writer.t
}

(* The other end of the communication pipes for an application. Running an app
 * produces this structure, which the controller can consume and process
 * accordingly.
 * *)
type 'a recv = {
  pkt_out : (switchId * SDN_Types.pktOut) Pipe.Reader.t;
  update  : 'a Pipe.Reader.t
}

module PipeSet = Set.Make(struct
  type t = string with sexp
  let compare = Pervasives.compare
end)

(* A handler is a callback constructor that takes an environment of type ['a]
 * and that can write to a ['b send].
 * *)
type ('a, 'b) handler = 'a -> 'b send -> unit -> event -> unit Deferred.t

(* INTERNAL
 *
 * apps can trigger value updates in two ways now:
 *
 *   - in response to a network event; and
 *   - asynchronously, according to its own requirements.
 *
 * When a network event occurs, an app must always push an [update]. That
 * [update] must either be an [Event(value)] when the network event necessitates
 * a value update, or an [EventNoop] when the app does not need to update its
 * value.
 *
 * When a app wants to update its value asynchronously, it can push an
 * [Async(value)] instead. However, NOTE that policies pushed asynchronously in
 * this way are not guaranteed to ever be realized in the network. The runtime
 * may buffer asynchronous updates while awaiting a synchronous update from a
 * network event. If the network event produces an updated value, that
 * will be installed rather than the buffered asynchronous value update.
 *
 * *)
type 'a update
  = Async of 'a
  | Event of 'a
  | EventNoop

(* INTERNAL
 *
 * This bit of state is used by primitive apps to indicate whether it is
 * processing an event or creating value updates asynchronously.
 *
 * When responding to an event, the application will buffer the most recent
 * [Async(value_a)] update that the app produces. If the app ultimately produces
 * an [Event(value_e)] in response to an event, then [value_a] is discarded and
 * [value_e] is written to [send.update]. If the app produces an [EventNoop],
 * then [value_a], i.e., the most recent asynchronous update from above, is
 * written to [send.update].
 *
 * *)
type state
  = SAsync
  | SEvent

(* INTERNAL
 *
 * The app struct, which should remain abstract to the end user:
 *
 *   - pipes:   the set of pipes that the app listens on for packet_ins
 *   - handler: a function that can be partially applied up to the unit argument
 *              in order to create an event callback.
 *   - value:   the most recent value that this app has produced.
 *
 * There is an important synchronization invariant that's maintained by [run]
 * and [combine] below. This invariant relates to the [unit Deferred.t] returned
 * by the event callback produced by partially apply the handler. This [unit
 * Deferred.t] should not become determined until the update produced by the
 * callback has reached its downstream consumer, be that another pipe or the
 * controller itself.
 *
 * *)
type ('r, 'a) t = {
  pipes : PipeSet.t;
  handler : [ `Primitive of ('r, 'a update) handler
            | `Composite of ('r, 'a) handler ];
  mutable value : 'a;
}

(* INTERNAL
 *
 * Helper function create a send/recv pair.
 * *)
let create_send_recv (app : ('r, 'a) t) : ('b send * 'b recv) =
  let r_pkt_out, w_pkt_out = Pipe.create () in
  let r_update, w_update = Pipe.create () in
  { pkt_out = w_pkt_out; update = w_update },
  { pkt_out = r_pkt_out; update = r_update }

let run (app : ('r, 'a) t) (a : 'r) () : ('a recv * (event -> unit Deferred.t)) =
  match app.handler with
  | `Primitive(handler) ->
    (* Primitive handlers need to handle Event and Async updates carefully.
     * While processing an event, the last Async value update should be
     * buffered. If the callback results in an EventNoop, then the buffered
     * async value should be written to the update pipe. If the callback results
     * in an Event update, then that value should be written to the update pipe
     * and all previous async value updates should be discarded.
     *
     * NOTE: It's assumed that [callback e] will not become determined until
     * either an Event or EventNoop update is written to the update pipe.
     * *)
    let state = ref SAsync in
    let send, recv = create_send_recv app in
    let callback = handler a send () in
    let callback' e =
      begin match e with
      | PacketIn(p, _, _, _, _) when not (PipeSet.mem app.pipes p) ->
        return ()
      | _ ->
        state := SEvent;
        callback e
        >>| fun () ->
          state := SAsync
      end in
    let update' =
      let last_async = ref None in
      (* This pipe implements the async update buffering mentioned above. While
       * not processing events, it will simply forward Async updates along.
       * *)
      Pipe.filter_map recv.update ~f:(fun (u : 'a update) ->
        match u, !state with
        | Async v,   SAsync -> assert (!last_async = None);
                               app.value <- v;
                               Some v
        | Async v,   SEvent -> last_async := Some v;
                               None
        | Event v,   SEvent -> last_async := None;
                               app.value <- v;
                               Some v
        | EventNoop, SEvent -> let result = !last_async in
                               last_async := None;
                               begin match result with
                               | None    -> ()
                               | Some(v) -> app.value <- v
                               end;
                               result
        | Event _  , SAsync
        | EventNoop, SAsync -> assert false) in
    { recv with update = update' }, callback'
  | `Composite handler ->
    (* Composite handlers aren't complicated. Most of the complexity has been
     * pushed down to the Primitive case or the combine function below.
     * *)
    let send, recv = create_send_recv app in
    let callback = handler a send () in
    let callback' e =
      begin match e with
      | PacketIn(p, _, _, _, _) when not (PipeSet.mem app.pipes p) ->
        return ()
      | _ ->
        callback e
      end in
    let update' = Pipe.map recv.update ~f:(fun v -> app.value <- v; v) in
    { recv with update = update' }, callback'

(* INTERNAL
 *
 * Create an application given a list of pipes to listen on, a default value,
 * and a handler.
 *
 * NOTE: The event callback produces by partial application of the handler
 * _must_ write an Event or EventNoop update before its [unit Deferred.t] result
 * becomes determined. The one exception to this rule is if the update pipe has
 * been closed, as in the create_static function below.
 * *)
let create_primitive ?pipes (value : 'a) (handler : ('r, 'a update) handler) : ('r, 'a) t =
  let pipes = match pipes with
    | None -> PipeSet.empty
    | Some(pipes) -> pipes in
  { pipes; value; handler = `Primitive(handler) }

(* INTERNAL
 *
 * Create an application that will only ever take on one value. The send/recv
 * pipes of this app are never used and are therefore immediately closed.
 * Because of this, the app's handler doesn't need to write any value updates to
 * the send.update to avoid deadlocks, as described in the comment for the
 * `create_primitive` function.
 * *)
let create_static (value : 'a) : ('r, 'a) t =
  create_primitive value (fun a send () ->
    Pipe.close send.pkt_out;
    Pipe.close send.update;
    fun e -> return ())

(* INTERNAL
 *
 * Lift a pure function to the the [Raw_app.t] type that will transform the
 * app's value on each update.
 *)
let lift (f : 'a -> 'b) (t : ('r, 'a) t) : ('r, 'b) t =
  { pipes = t.pipes
  ; value = f t.value
  ; handler = `Composite(fun nib send () ->
      let recv, callback = run t nib () in
      Deferred.don't_wait_for (Pipe.transfer_id recv.pkt_out send.pkt_out);
      Deferred.don't_wait_for (Pipe.transfer ~f recv.update  send.update);
      fun e ->
        callback e
        >>= fun () -> Deferred.ignore (Pipe.downstream_flushed recv.update))
  }


(* INTERNAL
 *
 * Application of function-valued [app]s. The event handling can be done in
 * sequence or in parallel. See the [combine] function for an example of its use.
 *)
let ap ?(how=`Sequential) (t1 : ('r, 'a -> 'b) t) (t2 : ('r, 'a) t) : ('r, 'b) t =
  { pipes = PipeSet.union t1.pipes t2.pipes
  ; value = t1.value t2.value
  ; handler = `Composite(fun a send () ->
    let recv1, callback1 = run t1 a () in
    let recv2, callback2 = run t2 a () in

    (* Transfer packets form the recv.pkt_out pipes of the sub-applications
     * to the recv.pkt_out pipe of this application. There are no order
     * guarantees here. Whoever is ready first goes first.
     *)
    Deferred.don't_wait_for (
      Pipe.transfer_id
        (Pipe.interleave [recv1.pkt_out; recv2.pkt_out])
        send.pkt_out);

    (* Wait for a value update from either sub-application, do the appropriate
     * composition using the [op] provided, and push the updated value to the
     * update pipe.
     * *)
    Deferred.don't_wait_for (
      (* XXX(seliopou): There's a chance for reducing the number of updates
       * here. First, the update pipes should have a size set to something
       * larger than zero. Then, rather than reading a single update at a
       * time, this code can read all pending updates in the pipe and drop all
       * but the last one. The last one would wipe out the preceding updates
       * anyways, so just use that as the update to propagate up the
       * application tree.
       *
       * This approach will be very useful in networks with larger topologies
       * or for applications that require large flowtables. Implementing the
       * value on the network will take longer in these cases. Rather than
       * implementing the backed-up policies in sequence, just take the last
       * one generated and install that one.
       *
       * Note however that this scheme will only benefit applications that
       * generate asynchronous value updates. Since the code handles value
       * updates generated by events synchronously, there will only be at most
       * one such value in an application's update pipe at any given moment.
       * *)
      Deferred.repeat_until_finished
        (Some(Pipe.read recv1.update), Some(Pipe.read recv2.update))
        (fun (l_state, r_state) ->
          let choices = List.filter_opt
            [ Option.map l_state (fun d -> Deferred.choice d (fun x -> `L x))
            ; Option.map r_state (fun d -> Deferred.choice d (fun x -> `R x)) ]
          in
          Deferred.enabled choices >>= fun results ->
          begin match results () with
          | [ `L (`Ok f); `R (`Ok v) ] ->
            Pipe.write send.update (f v);
            >>= fun () -> return (Some(Pipe.read recv1.update), Some(Pipe.read recv2.update))
          | [ `L `Eof       ; `R `Eof ] ->
            return (None, None)
          | [ `L (`Ok f); `R `Eof ]
          | [ `L (`Ok f)          ] ->
            Pipe.write send.update (f t2.value);
            >>= fun ()  -> return (Some(Pipe.read recv1.update), r_state)
          | [ `L `Eof       ; `R (`Ok v) ]
          | [                 `R (`Ok v) ] ->
            Pipe.write send.update (t1.value v);
            >>= fun ()  -> return (l_state,  Some(Pipe.read recv2.update))
          | [ `L `Eof ] -> return (None, r_state)
          | [ `R `Eof ] -> return (l_state, None)
          | _           -> assert false
          end
          >>= function
            | None, None -> return (`Finished ())
            | l   , r    -> return (`Repeat (l, r))));

    (* The result is a callback that will delegate the network event to two
     * sub-applications.
     *
     * NOTE: The result that the callback returns is a [unit Deferred.t] that
     * will become determined when both sub-applications are done processing
     * the event, and the value updates they pushed have been successfully
     * received and processed by the loop above.
     *
     * *)
    fun e ->
      let open Deferred in
      List.iter ~how:how ~f:(fun c -> c e)
        [ (fun e -> callback1 e
           >>= fun () -> ignore (Pipe.downstream_flushed recv1.update))
        ; (fun e -> callback2 e
           >>= fun () -> ignore (Pipe.downstream_flushed recv2.update))
        ]
    )
  }

(* INTERNAL
 *
 * Combine two applications, specifying if their callbacks should be called
 * sequentially or in parallel, as well as how value updates should be composed.
 * *)
let combine
    ?(how=`Sequential)
    (op : 'a -> 'a -> 'a)
    (t1 : ('r, 'a) t)
    (t2 : ('r, 'a) t) : ('r, 'a) t =
  ap ~how (lift op t1) t2
