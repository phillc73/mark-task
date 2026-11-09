(* Task manager - core CRUD operations *)

open Types

module type MANAGER = sig
  type t
  val create : ?path:string -> unit -> t
  val add : t -> task -> (t * string, string) result
  val delete : t -> string -> (t * string, string) result
  val update : t -> task -> (t * string, string) result
  val mark : ?clock:(unit -> float) -> t -> string -> status -> (t * string, string) result
  val get : t -> string -> task option
  val get_all : t -> task list
  val find_by_id : t -> string -> task option
  val get_config : t -> config
  val get_tasks : t -> task list
  val search : t -> string -> task list
  val filter_by_category : t -> string -> task list
  val filter_by_status : t -> status -> task list
  val filter_by_priority : t -> priority -> task list
  val sort_by_priority : t -> task list
  val sort_by_due_date : t -> task list
  val undo : t -> (t * string, string) result
  val redo : t -> (t * string, string) result
  val save : t -> (t * string, string) result
  val archive_completed : ?clock:(unit -> float) -> t -> (t * string, string) result
end

module FileTaskManager : MANAGER

val with_manager : ?path:string -> (FileTaskManager.t -> 'a) -> 'a
