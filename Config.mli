(* Configuration module *)

open Types

type int_setting = Max_history_size | Auto_archive_days

val all_int_settings : int_setting list
val int_setting_key : int_setting -> string

type int_bound = {
  min_value : int;
  max_value : int;
  default_value : int;
  rationale : string;
}

val int_bounds : int_setting -> int_bound
val bounds_for_key : string -> int_bound

val config_file_path : ?config_path:string -> unit -> string
val expand_tilde : string -> string

val load : ?config_path:string -> unit -> config
val save : config -> ?config_path:string -> unit -> unit

val notification_mode_of_string : string -> notification_mode
val notification_mode_to_string : notification_mode -> string

val performance_mode_of_string : string -> performance_mode
val performance_mode_to_string : performance_mode -> string
