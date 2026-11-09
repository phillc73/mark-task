(* Storage module - handles task persistence *)

open Types

module type STORAGE = sig
  val load : ?path:string -> unit -> task list * string
  val save : ?path:string -> task list -> (unit, string) result
  val check_conflict : ?path:string -> task list -> conflict_resolution option
  val resolve_conflict : conflict_resolution -> task list -> task list -> task list * task list
end

module FileStorage : STORAGE

val load_tasks : ?path:string -> unit -> task list
val save_tasks : ?path:string -> task list -> (unit, string) result
val with_storage : ?path:string -> (task list -> (task list * string, string) result) -> (string, string) result

val history_path : string -> string
val save_history : ?path:string -> task list list -> task list list -> (unit, string) result
val load_history : ?path:string -> unit -> (task list list * task list list) option
