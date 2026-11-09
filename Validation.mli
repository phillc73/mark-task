(* Validation module - input validation and error handling *)

open Types

type validation_error = 
  | EmptyDescription
  | InvalidStatus of string
  | InvalidPriority of string
  | InvalidComplexity of string
  | InvalidEffort of string
  | InvalidDueDate of string
  | TaskNotFound of string
  | FileNotFound of string
  | InvalidMarkdown of string
  | ConflictDetected
  | OtherError of string

val string_of_error : validation_error -> string

val validate_task : 
  string -> 
  ?category:string -> 
  ?status:string -> 
  ?priority:string -> 
  ?due_date:string -> 
  ?complexity:string -> 
  ?effort:string -> 
  ?recurrence:string -> 
  unit -> (unit, validation_error) result

val validate_task_exists : string -> task list -> (unit, validation_error) result
val validate_file : string -> (unit, validation_error) result
val validate_markdown : string -> (unit, validation_error) result
val check_conflict : task list -> task list -> (unit, validation_error) result
