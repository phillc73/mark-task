(* Desktop notifications module *)

open Types

val send_system_notification : string -> string -> bool
val send_terminal_notification : string -> string -> bool
val send_notification : notification_mode -> string -> string -> bool

val check_due_tasks : ?now:Ptime.t -> ?send:(notification_mode -> string -> string -> bool) -> path:string -> config -> task list -> (task list, string) result
val periodic_check : ?config:config -> ?tasks:task list -> path:string -> unit -> unit
