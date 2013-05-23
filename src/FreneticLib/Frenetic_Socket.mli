module type SAFESOCKET = sig
  type t = Lwt_unix.file_descr (* JNF: hide this, eventually *)

  val create : Lwt_unix.file_descr -> t

  val recv : t -> string -> int -> int -> bool Lwt.t

end

module SafeSocket : SAFESOCKET

val string_of_sockaddr : Lwt_unix.sockaddr -> string
