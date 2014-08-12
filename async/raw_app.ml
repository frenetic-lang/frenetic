open Core.Std
open Async.Std

open NetKAT_Types

(* A collection of pipes that apps use to communicate with the controller.
 * Currently this allows apps to send packet out messages and push policy
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
 * apps can trigger policy updates in two ways now:
 *
 *   - in response to a network event; and
 *   - asynchronously, according to its own requirements.
 *
 * When a network event occurs, an app must always push an [update]. That
 * [update] must either be an [Event(policy)] when the network event
 * necessitates a policy update, or an [EventNoop] when the app does not need to
 * update its policy.
 *
 * When a app wants to update its policy asynchronously, it can push an
 * [Async(policy)] instead. However, NOTE that policies pushed asynchronously
 * in this way are not guaranteed to ever be realized in the network. The
 * runtime may buffer asynchronous updates while awaiting a synchronous update
 * from a network event. If the network event produces an updated policy, that
 * will be installed rather than the buffered asynchronous policy update.
 * *)
type update
  = Async of policy
  | Event of policy
  | EventNoop

(* INTERNAL
 *
 * This bit of state is used by primitive apps to indicate whether it is
 * processing an event or creating policy updates asynchronously.
 *
 * When responding to an event, the application will buffer the most recent
 * [Async(policy_a)] update that the app produces. If the app ultimately
 * produces an [Event(policy_e)] in response to an event, then [policy_a] is
 * discarded and [policy_e] is written to [send.update]. If the app produces an
 * [EventNoop], then [policy_a], i.e., the most recent asynchronous update from
 * above, is written to [send.update].
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
 *   - policy:  the most recent policy that this app has produced.
 *
 * There is an important synchronization invariant that's maintained by [run]
 * and [combine] below. This invariant relates to the [unit Deferred.t]
 * returned by the event callback produced by partially apply the handler. This
 * [unit Deferred.t] should not become determined until the update produced by
 * the callback has reached its downstream consumer, be that another pipe or
 * the controller itself.
 * *)
type 'a t = {
  pipes : PipeSet.t;
  handler : [ `Primitive of ('a, update) handler
            | `Composite of ('a, policy) handler ];
  mutable policy : policy;
}

(* INTERNAL
 *
 * Helper function create a send/recv pair.
 * *)
let create_send_recv (app : 'a t) : ('b send * 'b recv) =
  let r_pkt_out, w_pkt_out = Pipe.create () in
  let r_update, w_update = Pipe.create () in
  { pkt_out = w_pkt_out; update = w_update },
  { pkt_out = r_pkt_out; update = r_update }

let run (app : 'a t) (a : 'a) () : (policy recv * (event -> unit Deferred.t)) =
  match app.handler with
  | `Primitive(handler) ->
    (* Primitive handlers need to handle Event and Async updates carefully.
     * While processing an event, the last Async policy update should be
     * buffered. If the callback results in an EventNoop, then the buffered
     * async policy should be written to the update pipe. If the callback
     * results in an Event update, then that policy should be written to the
     * update pipe and all previous async policy updates should be discarded.
     *
     * NOTE: It's assumed that [callback e] will not become determined until
     * either an Event or EventNoop update is written to the update pipe.
     *)
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
      Pipe.filter_map recv.update ~f:(fun (u : update) ->
        match u, !state with
        | Async p,   SAsync -> assert (!last_async = None);
                              app.policy <- p;
                              Some p
        | Async p,   SEvent -> last_async := Some p;
                              None
        | Event p,   SEvent -> last_async := None;
                              app.policy <- p;
                              Some p
        | EventNoop, SEvent -> let result = !last_async in
                              last_async := None;
                              begin match result with
                              | None    -> ()
                              | Some(p) -> app.policy <- p
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
    let update' = Pipe.map recv.update ~f:(fun p -> app.policy <- p; p) in
    { recv with update = update' }, callback'

(* INTERNAL
 *
 * Create an application given a list of pipes to listen on, a default policy,
 * and a handler.
 *
 * NOTE: The event callback produces by partial application of the handler
 * _must_ write an Event or EventNoop update before its [unit Deferred.t]
 * result becomes determined. The one exception to this rule is if the update
 * pipe has been closed, as in the create_static function below.
 * *)
let create_primitive ?pipes (policy : policy) (handler : ('a, update) handler) : 'a t =
  let pipes = match pipes with
    | None -> PipeSet.empty
    | Some(pipes) -> pipes in
  { pipes; policy; handler = `Primitive(handler) }

(* INTERNAL
 *
 * Create an application that will only ever take on one value. The send/recv
 * pipes of this app are never used and are therefore immediately closed.
 * Because of this, the app's handler doesn't need to write any policy updates
 * to the send.update to avoid deadlocks, as described in the comment for the
 * `create_primitive` function.
 * *)
let create_static (policy : policy) : 'a t =
  create_primitive policy (fun a send () ->
    Pipe.close send.pkt_out;
    Pipe.close send.update;
    fun e -> return ())

(* INTERNAL
 *
 * Combine two applications, specifying if their callbacks should be called
 * sequentially or in parallel, as well as how policy updates should be composed.
 * *)
let combine
    ?(how=`Sequential)
    (op : policy -> policy -> policy)
    (app1 : 'a t)
    (app2 : 'a t) : 'a t =
  { pipes = PipeSet.union app1.pipes app2.pipes
  ; policy = op app1.policy app2.policy
  ; handler = `Composite(fun a send () ->
      let recv1, callback1 = run app1 a () in
      let recv2, callback2 = run app2 a () in

      (* Transfer packets form the recv.pkt_out pipes of the sub-applications
       * to the recv.pkt_out pipe of this application. There are no order
       * guarantees here. Whoever is ready first goes first.
       *)
      Deferred.don't_wait_for (
        Pipe.transfer_id
          (Pipe.interleave [recv1.pkt_out; recv2.pkt_out])
          send.pkt_out);

      (* Wait for a policy update from either sub-application, do the
       * appropriate composition using the [op] provided, and push the updated
       * policy to the update pipe.
       * *)
      Deferred.don't_wait_for (
        (* XXX(seliopou): There's a chance for reducing the number of updates
         * here. First, the update pipes should have a size set to something
         * larger than zero. Then, rather than reading a single update at a
         * time, this code can read all pending updates in the pipe and drop
         * all but the last one. The last one would wipe out the preceding
         * updates anyways, so just use that as the update to propagate up the
         * application tree.
         *
         * This approach will be very useful in networks with larger topologies
         * or for applications that require large flowtables. Implementing the
         * policy on the network will take longer in these cases. Rather than
         * implementing the backed-up policies in sequence, just take the last
         * one generated and install that one.
         *
         * Note however that this scheme will only benefit applications that
         * generate asynchronous policy updates. Since the code handles policy
         * updates generated by events synchronously, there will only be at most
         * one such policy in an application's update pipe at any given moment.
         * *)
        Deferred.repeat_until_finished
          (Some(Pipe.read recv1.update), Some(Pipe.read recv2.update))
          (fun (l_state, r_state) ->
            let choices = List.filter_opt
              [ Option.map l_state (fun d -> Deferred.choice d (fun x -> `L x))
              ; Option.map r_state (fun d -> Deferred.choice d (fun x -> `R x)) ] in
            Deferred.enabled choices >>= fun results ->
            begin match results () with
            | [ `L (`Ok l_pol); `R (`Ok r_pol) ] ->
              Pipe.write send.update (op l_pol r_pol)
              >>= fun () -> return (Some(Pipe.read recv1.update), Some(Pipe.read recv2.update))
            | [ `L `Eof       ; `R `Eof ] ->
              return (None, None)
            | [ `L (`Ok l_pol); `R `Eof ]
            | [ `L (`Ok l_pol)          ] ->
              Pipe.write send.update (op l_pol app2.policy)
              >>= fun ()  -> return (Some(Pipe.read recv1.update), r_state)
            | [ `L `Eof       ; `R (`Ok r_pol) ]
            | [                 `R (`Ok r_pol) ] ->
              Pipe.write send.update (op app1.policy r_pol)
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
       * the event, and the policy updates they pushed have been successfully
       * received and processed by the loop above.
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
