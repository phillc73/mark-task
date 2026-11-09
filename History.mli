(* History module - tracks changes for undo/redo *)

open Types

module type HISTORY = sig
  type t
  val create : int -> t
  val restore : max_size:int -> task list list -> task list list -> t
  val push : t -> task list -> t
  val undo : t -> (task list option * t)
  val redo : t -> (task list option * t)
  val can_undo : t -> bool
  val can_redo : t -> bool
  val size : t -> int
  val states : t -> task list list
  val redo_states : t -> task list list
end

module TaskHistory : HISTORY

val create_history : ?max_size:int -> unit -> TaskHistory.t
val record_change : TaskHistory.t -> task list -> TaskHistory.t
val undo : TaskHistory.t -> task list option * TaskHistory.t
val redo : TaskHistory.t -> task list option * TaskHistory.t
