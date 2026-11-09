(* Task module *)

open Types

val format_timestamp : Ptime.t -> string
val now_str : ?clock:(unit -> float) -> unit -> string

val create : 
  description:string -> 
  ?category:string option -> 
  ?status:status -> 
  ?priority:priority -> 
  ?due_date:string option -> 
  ?complexity:complexity -> 
  ?effort:effort -> 
  ?recurrence:string option -> 
  unit -> task

val update : 
  ?description:string -> 
  ?category:string option -> 
  ?status:status -> 
  ?priority:priority -> 
  ?due_date:string option -> 
  ?complexity:complexity -> 
  ?effort:effort -> 
  ?recurrence:string option -> 
  task -> task

val mark_complete : ?clock:(unit -> float) -> task -> task
val mark_status : status -> task -> task

val status_to_string : status -> string
val priority_to_string : priority -> string
val complexity_to_string : complexity -> string
val effort_to_string : effort -> string

val is_complete : task -> bool
val is_overdue : task -> bool
val is_due_soon : task -> bool
val should_archive : ?clock:(unit -> float) -> ?days:int -> task -> bool
