(* REPL module - interactive mode *)

open Task_manager

type command = 
  | Add of string * string list
  | Delete of string
  | Edit of string * string list
  | Mark of string * string
  | List
  | ListCategory of string
  | ListStatus of string
  | ListPriority of string
  | Search of string
  | Undo
  | Redo
  | Help
  | Quit
  | Unknown of string

val parse_command : string -> command
val execute_command : FileTaskManager.t -> command -> (FileTaskManager.t * string, string) result
val help_text : string

val run_repl : ?path:string -> unit -> unit
