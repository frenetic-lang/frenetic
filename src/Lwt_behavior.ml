type 'a t = {
  now : unit -> 'a;
  values : 'a Lwt_stream.t
}

let map (f : 'a -> 'b) (src : 'a t) : 'b t =
  let now = ref (f (src.now ())) in
  let g a =
    now := f a;
    !now in
  let values = Lwt_stream.map g src.values in
  { now = (fun () -> !now); values = values }

let map2 f a_src b_src =
  let now = ref (f (a_src.now ()) (b_src.now ())) in
  let (stream, push) = Lwt_stream.create () in
  let recv_a = 
    Lwt_stream.iter 
      (fun a -> now := f a (b_src.now ()); push (Some !now)) a_src.values in
  let recv_b =
    Lwt_stream.iter
      (fun b -> now := f (a_src.now ()) b; push (Some !now)) b_src.values in
  Lwt.async (fun () -> Lwt.join [recv_a; recv_b]);
  { now = (fun () -> !now); values = stream }

let return (x : 'a) = 
  let (stream, _) = Lwt_stream.create () in
  { now = (fun () -> x); values = stream }

let bind (f : 'a -> 'b t) (src : 'a t) : 'b t =
  let (stream, push) = Lwt_stream.create () in
  let now_src = ref (f (src.now ())) in
  let now  = ref (!now_src.now ()) in
  let get_next_b () = 
    lwt x = Lwt_stream.get !now_src.values in
    match x with
      | None -> failwith "should not close stream"
      | Some b ->
        begin 
          now := b;
          push (Some b);
          Lwt.return ()
        end in
  let get_next_stream () =
    lwt a = Lwt_stream.get src.values in
    match a with
      | None -> failwith "should not close stream"
      | Some a ->
        now_src := f a;
        now := !now_src.now ();
        push (Some !now);
        Lwt.return () in
  let rec loop () =
    Lwt.bind
      (Lwt.pick [get_next_b (); get_next_stream ()])
      (fun () -> loop ()) in
  Lwt.async loop;
  { now = (fun () -> !now); values = stream }

let from_stream (init : 'a) (stream : 'a Lwt_stream.t) : 'a t =
  let now = ref init in
  { now = (fun () -> !now); 
    values = Lwt_stream.map (fun a -> now := a; a) stream }

let to_stream (x : 'a t) : 'a Lwt_stream.t =
  let (stream, push) = Lwt_stream.create () in
  push (Some (x.now ()));
  Lwt.async (fun () -> Lwt_stream.iter (fun x -> push (Some x)) x.values);
  stream
