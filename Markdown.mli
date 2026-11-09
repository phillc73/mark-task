(* Markdown parser/serializer for tasks *)

open Types

type field_spec = {
  label : string;
  key : string;
  marker : string;
  prefix : string;
  valid : string -> bool;
  required : bool;
}

val field_specs : field_spec list

type parse_result = {
  tasks : task list;
  rejections : string list;
  normalisations : string list;
}

val parse_file_with_rejections : string -> task list * string list
val parse_document_with_rejections : string -> task list * string list
val parse_file_result : string -> parse_result
val parse_document_result : string -> parse_result
val parse_file : string -> task list
val write_file : string -> task list -> unit

val parse_document : string -> task list
val format_document : task list -> string

val normalise_value : string -> string

val status_of_string : string -> status option
val priority_of_string : string -> priority option
val complexity_of_string : string -> complexity option
val effort_of_string : string -> effort option

val status_to_string : status -> string
val priority_to_string : priority -> string
val complexity_to_string : complexity -> string
val effort_to_string : effort -> string

val parse_datetime : string -> Ptime.t option
val format_datetime : Ptime.t -> string
