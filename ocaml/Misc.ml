type ('a,'b) sum = 
  | Inl of 'a 
  | Inr of 'b

module Log = struct

  let log = ref stderr
    
  let get_log_chan () : out_channel = !log
    
  let set_log_file (replace : bool) (filename : string) : unit =
    let chan = 
      match replace with
      | true -> open_out filename 
      | false -> 
        open_out_gen [Open_wronly; Open_text; Open_creat; Open_excl] 
          0o600 filename in
    log := chan;
    at_exit (fun () -> close_out chan)
      
  let printf (fmt : ('a, out_channel, unit) format) : 'a =
    Printf.printf ".%!";
    Printf.fprintf !log fmt
end

module Lwt_channel = struct
  type 'a t = {
    stream : 'a Lwt_stream.t;
    push : 'a option -> unit
  }
    
  let of_pushed_stream stream push = { stream; push }
    
  let create () = 
    let (stream, push) = Lwt_stream.create () in
    of_pushed_stream stream push
      
  let send (v : 'a) (chan : 'a t) = Lwt.return (chan.push (Some v))
    
  let recv (chan : 'a t) = Lwt_stream.next chan.stream
    
  let to_stream (chan : 'a t) = chan.stream
    
end

let test_bit n x = 
  Int32.logand (Int32.shift_right_logical x n) Int32.one = Int32.one
let clear_bit n x =
  Int32.logand x (Int32.lognot (Int32.shift_left Int32.one n))
let set_bit n x = 
  Int32.logor x (Int32.shift_left Int32.one n)
let bit (x : int32) (n : int) (v : bool) : int32 = 
  if v then set_bit n x else clear_bit n x

let mac_of_bytes (str:string) : int64 = 
  if String.length str != 6 then
    raise (Invalid_argument 
             (Format.sprintf "mac_of_bytes expected six-byte string, got %d
                              bytes" (String.length str)));
  let byte n = Int64.of_int (Char.code (String.get str n)) in
  let open Int64 in
  logor (shift_left (byte 0) (8 * 5))
    (logor (shift_left (byte 1) (8 * 4))
       (logor (shift_left (byte 2) (8 * 3))
          (logor (shift_left (byte 3) (8 * 2))
             (logor (shift_left (byte 4) (8 * 1))
                (byte 5)))))

let get_byte (n:int64) (i:int) : int = 
  if i < 0 or i > 5 then
    raise (Invalid_argument "Int64.get_byte index out of range");
  Int64.to_int (Int64.logand 0xFFL (Int64.shift_right_logical n (8 * i)))

let string_of_mac (x:int64) : string = 
  Format.sprintf "%02x:%02x:%02x:%02x:%02x:%02x" 
    (get_byte x 5) (get_byte x 4) (get_byte x 3)
    (get_byte x 2) (get_byte x 1) (get_byte x 0)


let bytes_of_mac (x:int64) : string = 
  let byte n = Char.chr (get_byte x n) in
  Format.sprintf "%c%c%c%c%c%c"
    (byte 5) (byte 4) (byte 3)
    (byte 2) (byte 1) (byte 0)

let rec filter_map f xs = match xs with
  | [] -> []
  | x :: xs' -> match f x with
      | Some y -> y :: (filter_map f xs')
      | None -> filter_map f xs'

let concat_map f lst =
  List.fold_right (fun a bs -> List.append (f a) bs) lst []

let intersperse v lst =
  List.fold_right (fun x xs -> x :: (v :: xs)) [] lst

module type SAFESOCKET = sig
  type t = Lwt_unix.file_descr
  val create : Lwt_unix.file_descr -> t
  val recv : t -> string -> int -> int -> bool Lwt.t
end

module SafeSocket : SAFESOCKET = struct
  open Lwt
  open Lwt_unix

  type t = Lwt_unix.file_descr

  let create fd = fd

  let rec recv fd buf off len = 
    if len = 0 then 
      return true
    else 
      lwt n = Lwt_unix.recv fd buf off len [] in  
      if n = 0 then 
	return false
      else if n = len then 
	return true
      else
	recv fd buf (off + n) (len - n)
end

let string_of_pair to_string (p1,p2) =
  Printf.sprintf "(%s, %s)" (to_string p1) (to_string p2)

let string_of_option to_string opt =
  match opt with
  | Some v -> "Some " ^ to_string v
  | None -> "None"

