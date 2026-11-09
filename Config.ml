(* Configuration module *)

open Types

(* Every integer-typed configuration setting and its accepted range.
   This is the one bounds table; the loader and the tests both read it.
   Adding a constructor forces the bounds match and the test mapping to
   be extended before the tree compiles. *)
type int_setting = Max_history_size | Auto_archive_days

let all_int_settings = [ Max_history_size; Auto_archive_days ]

let int_setting_key = function
  | Max_history_size -> "max_history_size"
  | Auto_archive_days -> "auto_archive_days"

type int_bound = {
  min_value : int;
  max_value : int;
  default_value : int;
  rationale : string;
}

let int_bounds = function
  | Max_history_size ->
      { min_value = 0;
        max_value = 1000;
        default_value = default_config.max_history_size;
        rationale =
          "0 disables undo; 1000 states is far beyond the product's five-step undo surface" }
  | Auto_archive_days ->
      { min_value = 0;
        max_value = 36500;
        default_value = default_config.auto_archive_days;
        rationale =
          "0 archives every complete task; 36500 days is 100 years and keeps days * 24 inside the int range" }

let bounds_for_key key =
  match List.find_opt (fun setting -> int_setting_key setting = key) all_int_settings with
  | Some setting -> int_bounds setting
  | None -> invalid_arg ("config: integer setting without bounds: " ^ key)

(* Expand a leading ~ to the user's home directory *)
let expand_tilde path = 
  if String.starts_with ~prefix:"~" path then
    let home = try Unix.getenv "HOME" with Not_found -> "." in
    home ^ String.sub path 1 (String.length path - 1)
  else path

let config_file_path ?config_path () = 
  match config_path with
  | Some p -> p
  | None -> 
      let home = Unix.getenv "HOME" in
      Filename.concat home ".config/mark-task/config.yaml"

let assoc_member key = function
  | `Assoc fields -> List.assoc_opt key fields
  | _ -> None

let color_theme_of_json ~warn json = 
  let get field default =
    match assoc_member field json with
    | None -> default
    | Some (`String value) -> value
    | Some _ -> warn field; default
  in
  {
    status_backlog = get "status_backlog" "gray";
    status_in_progress = get "status_in_progress" "yellow";
    status_on_hold = get "status_on_hold" "blue";
    status_complete = get "status_complete" "green";
    status_idea = get "status_idea" "cyan";
    priority_low = get "priority_low" "green";
    priority_medium = get "priority_medium" "yellow";
    priority_high = get "priority_high" "red";
    due_overdue = get "due_overdue" "red";
    due_soon = get "due_soon" "magenta";
    effort_1 = get "effort_1" "green";
    effort_2 = get "effort_2" "blue";
    effort_3 = get "effort_3" "yellow";
    effort_4 = get "effort_4" "magenta";
    effort_5 = get "effort_5" "red";
    complexity_1 = get "complexity_1" "green";
    complexity_2 = get "complexity_2" "blue";
    complexity_3 = get "complexity_3" "yellow";
    complexity_4 = get "complexity_4" "magenta";
    complexity_5 = get "complexity_5" "red";
  }

let notification_mode_option = function
  | "system" -> Some System
  | "terminal" -> Some Terminal
  | "both" -> Some Both
  | _ -> None

let notification_mode_of_string value =
  Option.value ~default:Terminal (notification_mode_option value)

let notification_mode_to_string = function
  | System -> "system"
  | Terminal -> "terminal"
  | Both -> "both"

let performance_mode_option = function
  | "all" -> Some All
  | "next24h" -> Some Next24h
  | "next48h" -> Some Next48h
  | "next96h" -> Some Next96h
  | _ -> None

let performance_mode_of_string value =
  Option.value ~default:All (performance_mode_option value)

let performance_mode_to_string = function
  | All -> "all"
  | Next24h -> "next24h"
  | Next48h -> "next48h"
  | Next96h -> "next96h"

let load_with_warnings ?config_path () =
  let path = config_file_path ?config_path () in
  if not (Sys.file_exists path) then default_config, []
  else
    let parsed =
      try Ok (Yojson.Basic.from_file path) with
      | Yojson.Json_error message -> Error ("invalid JSON (" ^ message ^ ")")
      | Sys_error message -> Error ("cannot read file (" ^ message ^ ")")
      | exn -> Error (Printexc.to_string exn)
    in
    match parsed with
    | Error message ->
        default_config,
        [Printf.sprintf "config %s: %s; using defaults" path message]
    | Ok (`Assoc _ as json) ->
        let warnings = ref [] in
        let warn key expected =
          warnings := Printf.sprintf
              "config %s: key \"%s\": expected %s; using default"
              path key expected :: !warnings
        in
        let string_setting key default =
          match assoc_member key json with
          | None -> default
          | Some (`String value) -> value
          | Some _ -> warn key "string"; default
        in
        let int_setting setting =
          let key = int_setting_key setting in
          let bounds = bounds_for_key key in
          match assoc_member key json with
          | None -> bounds.default_value
          | Some (`Int value) ->
              if value >= bounds.min_value && value <= bounds.max_value then value
              else (
                warnings := Printf.sprintf
                    "config %s: key \"%s\": value %d is out of range %d..%d; using default %d"
                    path key value bounds.min_value bounds.max_value bounds.default_value
                  :: !warnings;
                bounds.default_value)
          | Some wrong -> (
              warnings := Printf.sprintf
                  "config %s: key \"%s\": value %s is not an integer; accepted range %d..%d; using default %d"
                  path key (Yojson.Basic.to_string wrong)
                  bounds.min_value bounds.max_value bounds.default_value
                :: !warnings;
              bounds.default_value)
        in
        let enum_setting key default parse =
          match assoc_member key json with
          | None -> default
          | Some (`String value) ->
              (match parse value with
               | Some parsed -> parsed
               | None -> warn key "a documented value"; default)
          | Some _ -> warn key "string"; default
        in
        let aliases =
          match assoc_member "aliases" json with
          | None -> default_config.aliases
          | Some (`Assoc fields) ->
              List.filter_map (fun (name, value) ->
                match value with
                | `String command -> Some (name, command)
                | _ -> warn ("aliases." ^ name) "string"; None) fields
          | Some _ -> warn "aliases" "object"; default_config.aliases
        in
        let color_theme =
          match assoc_member "color_theme" json with
          | None -> default_config.color_theme
          | Some (`Assoc _ as theme) ->
              color_theme_of_json
                ~warn:(fun key -> warn ("color_theme." ^ key) "string") theme
          | Some _ -> warn "color_theme" "object"; default_config.color_theme
        in
        let config = {
          markdown_location =
            string_setting "markdown_location" default_config.markdown_location;
          color_theme;
          notification_mode =
            enum_setting "notification_mode" default_config.notification_mode
              (fun value ->
                notification_mode_option (String.lowercase_ascii value));
          performance_mode =
            enum_setting "performance_mode" default_config.performance_mode
              (fun value ->
                performance_mode_option (String.lowercase_ascii value));
          max_history_size =
            int_setting Max_history_size;
          auto_archive_days =
            int_setting Auto_archive_days;
          aliases;
        } in
        (config, List.rev !warnings)
    | Ok _ ->
        default_config,
        [Printf.sprintf "config %s: expected a JSON object; using defaults" path]

let load ?config_path () =
  let config, warnings = load_with_warnings ?config_path () in
  List.iter (fun warning -> prerr_endline ("Warning: " ^ warning)) warnings;
  config

let save config ?config_path () = 
  let path = config_file_path ?config_path () in
  let ensure_dir () = 
    let dir = Filename.dirname path in
    if not (Sys.file_exists dir) then (
      let rec mkdir_p path perm =
        try Unix.mkdir path perm with
        | Unix.Unix_error (Unix.ENOENT, _, _) ->
            let parent = Filename.dirname path in
            mkdir_p parent perm;
            Unix.mkdir path perm
        | e -> raise e
      in
      mkdir_p dir 0o755
    )
  in
  ensure_dir ();
  let json = 
    `Assoc [
      "markdown_location", `String config.markdown_location;
      "notification_mode", `String (notification_mode_to_string config.notification_mode);
      "performance_mode", `String (performance_mode_to_string config.performance_mode);
      "max_history_size", `Int config.max_history_size;
      "auto_archive_days", `Int config.auto_archive_days;
      "aliases", 
        `Assoc (List.map (fun (k, v) -> (k, `String v)) config.aliases);
      "color_theme",
        `Assoc [
          "status_backlog", `String config.color_theme.status_backlog;
          "status_in_progress", `String config.color_theme.status_in_progress;
          "status_on_hold", `String config.color_theme.status_on_hold;
          "status_complete", `String config.color_theme.status_complete;
          "status_idea", `String config.color_theme.status_idea;
          "priority_low", `String config.color_theme.priority_low;
          "priority_medium", `String config.color_theme.priority_medium;
          "priority_high", `String config.color_theme.priority_high;
          "due_overdue", `String config.color_theme.due_overdue;
          "due_soon", `String config.color_theme.due_soon;
          "effort_1", `String config.color_theme.effort_1;
          "effort_2", `String config.color_theme.effort_2;
          "effort_3", `String config.color_theme.effort_3;
          "effort_4", `String config.color_theme.effort_4;
          "effort_5", `String config.color_theme.effort_5;
          "complexity_1", `String config.color_theme.complexity_1;
          "complexity_2", `String config.color_theme.complexity_2;
          "complexity_3", `String config.color_theme.complexity_3;
          "complexity_4", `String config.color_theme.complexity_4;
          "complexity_5", `String config.color_theme.complexity_5;
        ]
    ]
  in
  Atomic_write.write path (fun oc -> Yojson.Basic.to_channel oc json)
