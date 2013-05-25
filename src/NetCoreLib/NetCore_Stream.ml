open Printf

module Q = struct
    
  type t = Queue of (int * (t -> t)) list

  let singleton rank v = Queue [(rank,v)]

  let rec enqueue rank handler q = match q with
    | Queue [] -> Queue [(rank, handler)]
    | Queue ((rank', handler') :: lst) ->
      if rank <= rank' then
        Queue ((rank, handler) :: (rank', handler') :: lst)
      else
        let (Queue lst'') = enqueue rank handler (Queue lst) in
        Queue ((rank', handler') :: lst'')

  let dequeue q = match q with
    | Queue [] -> None
    | Queue ((rank, handler) :: lst) -> Some (rank, handler, Queue lst)

end

type 'a node = {
  mutable now : 'a; (* current value *)
  rank : int;
  attach_consumer : (Q.t -> Q.t) -> unit
}

type 'a t = 'a node

let now node = node.now

let map (f : 'a -> 'b) (src : 'a node) : 'b node =
  let sends_to = ref [] in
  let self = { 
    now = f src.now;
    rank = 1 + src.rank;
    attach_consumer = (fun f -> sends_to := f :: !sends_to) 
  } in
  let queued_now = ref false in
  let producer q =
    queued_now := false;
    self.now <- f src.now;
    List.fold_right (fun f q -> f q) !sends_to q in
  let consumer q = 
    if !queued_now = false then
      begin
        queued_now := true;
        Q.enqueue self.rank producer q
      end
    else 
      q in
  src.attach_consumer consumer;
  self

let map2 (f : 'a -> 'b -> 'c) (a_src : 'a node) (b_src : 'b node) : 'c node =
  let sends_to = ref [] in
  let self = { 
    now = f a_src.now b_src.now;
    rank = 1 + max a_src.rank b_src.rank;
    attach_consumer = (fun f -> sends_to := f :: !sends_to) 
  } in
  let queued_now = ref false in
  let producer q =
    queued_now := false;
    self.now <- f a_src.now b_src.now;
    List.fold_right (fun f q -> f q) !sends_to q in
  let consumer q = 
    if !queued_now = false then
      begin
        queued_now := true;
        Q.enqueue self.rank producer q
      end
    else 
      q in
  a_src.attach_consumer consumer;
  b_src.attach_consumer consumer;
  self


let constant (v : 'a) : 'a node =
  { 
    now = v;
    rank = 0;
    attach_consumer = (fun _ -> ());
  }

let from_stream (init : 'a) (stream : 'a Lwt_stream.t) : 'a node =
  let sends_to = ref [] in
  let self = { 
    now = init;
    rank = 0;
    attach_consumer = (fun f -> sends_to := f :: !sends_to) 
  } in
  let producer q =
    List.fold_right (fun f q -> f q) !sends_to q in
  let rec propagate q = 
    match Q.dequeue q with
    | None -> ()
    | Some (_, v, q') -> propagate (v q') in
  Lwt.async
    (fun () -> 
      Lwt_stream.iter
        (fun a ->
          self.now <- a;
          try
             propagate (Q.singleton 0 producer)
          with exn ->
            eprintf "EXN: %s\n" (Printexc.to_string exn))
        stream);
  self

let to_stream (x : 'a node) : 'a Lwt_stream.t =
  let (stream, push) = Lwt_stream.create () in
  x.attach_consumer (fun q -> push (Some x.now); q);
  push (Some x.now);
  stream
