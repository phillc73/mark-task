(* Performance module - task loading optimisation *)

open Types

val filter_by_time_range : performance_mode -> task list -> task list
val get_mode : config -> performance_mode
val load_filtered : config -> task list -> task list
