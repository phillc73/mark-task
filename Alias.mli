(* Alias module - command aliases *)

val expand_alias : (string * string) list -> string -> string
val get_alias : string -> (string * string) list -> string option
val add_alias : string -> string -> (string * string) list -> (string * string) list
val remove_alias : string -> (string * string) list -> (string * string) list
val list_aliases : (string * string) list -> (string * string) list
