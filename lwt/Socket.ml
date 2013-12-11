type t = Lwt_unix.file_descr

let create fd = fd

let rec recv fd buf off len =
  if len = 0 then
    Lwt.return true
  else
    lwt n = Lwt_unix.recv fd buf off len [] in
    if n = 0 then
	    Lwt.return false
    else if n = len then
	    Lwt.return true
    else
	    recv fd buf (off + n) (len - n)

let string_of_sockaddr (sa:Lwt_unix.sockaddr) : string =
  match sa with
  | Lwt_unix.ADDR_UNIX str ->
    str
  | Lwt_unix.ADDR_INET (addr,port) ->
    Unix.string_of_inet_addr addr ^ ":" ^ (string_of_int port)
