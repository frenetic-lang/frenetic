module H = Hashtbl
module Q = Queue

exception NoPath of string*string


let rec bfs' target graph queue =
  let (sw, path) = (Q.take queue) in
  match sw = target with
    | true -> path
    | false -> let () = List.iter (fun x -> Q.add (x, x :: path) queue) (try (H.find graph sw) with _ -> []); H.remove graph sw in
	       bfs' target graph queue

let bfs graph src dst = 
  let q = Queue.create () in
  let () = Q.add (src, [src]) q in
  try (bfs' dst (Hashtbl.copy graph) q) 
  with Queue.Empty -> raise (NoPath(Int64.to_string src, Int64.to_string dst))

let shortest_path = bfs

let get_hop topo s1 s2 = let () = Printf.printf "get_hop %Ld %Ld\n" s1 s2 in
			 try Some (Hashtbl.find (Hashtbl.find (snd topo) s1) s2) with _ -> None

let get_host_port topo host = try (match (Hashtbl.find (fst topo) host) with
  | [sw] ->   get_hop topo sw host) with _ -> None
