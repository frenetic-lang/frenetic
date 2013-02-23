(* utilities *)

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
  logor (shift_left (byte 5) (8 * 5))
    (logor (shift_left (byte 4) (8 * 4))
       (logor (shift_left (byte 3) (8 * 3))
          (logor (shift_left (byte 2) (8 * 2))
             (logor (shift_left (byte 1) (8 * 1))
                (byte 0)))))

let get_byte (n:int64) (i:int) : char = 
  if i < 0 or i > 5 then
    raise (Invalid_argument "Int64.get_byte index out of range");
  let n = Int64.to_int (Int64.logand 0xFFL (Int64.shift_right_logical n (8 * i))) in 
  Char.chr n

let bytes_of_mac (x:int64) : string = 
  Format.sprintf "%c%c%c%c%c%c"
    (get_byte x 0) (get_byte x 1) (get_byte x 2)
    (get_byte x 3) (get_byte x 4) (get_byte x 5)

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
