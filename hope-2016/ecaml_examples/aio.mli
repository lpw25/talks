(* Asynchronous IO scheduler.
 *
 * For each blocking action, if the action can be performed immediately, then it
 * is. Otherwise, the thread performing the blocking task is suspended and
 * automatically wakes up when the action completes. The suspend/resume is
 * transparent to the programmer.
 *)

type file_descr = Unix.file_descr
type sockaddr = Unix.sockaddr
type msg_flag = Unix.msg_flag

effect aio =
  | Fork  : (unit -[aio | io]-> unit) -> unit
  | Yield : unit
(*
effect aio =*)
  | Accept : file_descr -> (file_descr * sockaddr)
  | Recv : file_descr * bytes * int * int * msg_flag list -> int
  | Send : file_descr * bytes * int * int * msg_flag list -> int
  | Sleep : float -> unit
(*with function*)

val fork  : (unit -[aio|io]-> unit) -[aio]-> unit
val yield : unit -[aio]-> unit
val accept : file_descr -[aio]-> file_descr * sockaddr
val recv : file_descr ->> bytes ->> int ->> int ->> msg_flag list -[aio]-> int
val send : file_descr ->> bytes ->> int ->> int ->> msg_flag list -[aio]-> int
val sleep : float -[aio]-> unit

val run : (unit -[aio | io]-> unit) -> unit
