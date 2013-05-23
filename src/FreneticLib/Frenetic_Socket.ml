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

let string_of_sockaddr (sa:Lwt_unix.sockaddr) : string =
  match sa with
  | Lwt_unix.ADDR_UNIX str ->
    str
  | Lwt_unix.ADDR_INET (addr,port) ->
    Format.sprintf "%s:%d" (Unix.string_of_inet_addr addr) port
