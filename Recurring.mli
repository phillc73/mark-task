(* Recurring tasks module *)

open Types

type recurrence_pattern = 
  | Daily
  | Weekly of int list
  | Monthly of int list
  | Yearly of int * int
  | EveryN of int * string
  | Custom of string

val parse_recurrence : string -> recurrence_pattern
val should_regenerate : ?today:Ptime.t -> task -> bool
val regenerate_if_needed : ?today:Ptime.t -> task -> task option
val process_recurring_tasks : ?today:Ptime.t -> task list -> task list
