(* UI module - terminal coloring and formatting *)

open Types

val get_color_code : string -> string
val colorize : string -> string -> string

val format_status : color_theme -> status -> string
val format_priority : color_theme -> priority -> string
val format_effort : color_theme -> effort -> string
val format_complexity : color_theme -> complexity -> string
val format_due_date : color_theme -> task -> string
val format_category : color_theme -> string option -> string

val format_task : ?index:bool -> ?num:int -> color_theme -> task -> string
val format_task_list : color_theme -> task list -> string

val format_error : string -> string
val format_success : string -> string
val format_warning : string -> string
val format_info : string -> string
