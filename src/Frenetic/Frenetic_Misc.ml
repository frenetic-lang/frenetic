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

let get_byte32 (n : Int32.t) (i : int) : int = 
  let open Int32 in
  if i < 0 or i > 3 then
    raise (Invalid_argument "get_byte32 index out of range");
  to_int (logand 0xFFl (shift_right_logical n (8 * i)))

let string_of_mac (x:int64) : string =
  Format.sprintf "%02x:%02x:%02x:%02x:%02x:%02x"
    (get_byte x 5) (get_byte x 4) (get_byte x 3)
    (get_byte x 2) (get_byte x 1) (get_byte x 0)

let bytes_of_mac (x:int64) : string =
  let byte n = Char.chr (get_byte x n) in
  Format.sprintf "%c%c%c%c%c%c"
    (byte 5) (byte 4) (byte 3)
    (byte 2) (byte 1) (byte 0)


let intersperse v lst =
  List.fold_right (fun x xs -> x :: (v :: xs)) [] lst


let string_of_list to_string l =
  let strs = List.map to_string l in
  "[" ^ (String.concat ", " strs) ^ "]"

let string_of_option to_string opt =
  match opt with
  | Some v -> "Some " ^ to_string v
  | None -> "None"

let string_of_pair to_string1 to_string2 (p1,p2) =
  Printf.sprintf "(%s, %s)" (to_string1 p1) (to_string2 p2)

let is_some opt =
  match opt with
  | Some _ -> true
  | None -> false

let is_none opt = not (is_some opt)
