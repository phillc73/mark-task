(* Archive module - handles auto-archiving of completed tasks *)

open Types

val archive_path : ?config_path:string -> unit -> string
val should_archive_task : ?clock:(unit -> float) -> ?days:int -> task -> bool
val archive_completed_tasks : ?clock:(unit -> float) -> ?_path:string -> ?days:int -> task list -> (task list, string) result
val get_archived_tasks : ?config_path:string -> unit -> task list
