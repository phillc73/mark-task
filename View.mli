(* View module - task display and filtering *)

open Types

module type VIEW = sig
  type t
  val create : config -> t
  val display : t -> task list -> string
  val display_task : t -> task -> string
  val display_by_category : t -> task list -> string
  val display_by_status : t -> task list -> string
  val display_by_priority : t -> task list -> string
  val display_overdue : t -> task list -> string
  val display_due_soon : t -> task list -> string
end

module TaskView : VIEW

val create_view : ?config:Types.config -> unit -> TaskView.t
val display_tasks : ?config:Types.config -> task list -> string
val display_task_details : ?config:Types.config -> task -> string
