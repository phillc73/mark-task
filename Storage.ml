(* Storage module - handles task persistence *)

open Types

module type STORAGE = sig
  val load : ?path:string -> unit -> task list * string
  val save : ?path:string -> task list -> (unit, string) result
  val check_conflict : ?path:string -> task list -> conflict_resolution option
  val resolve_conflict : conflict_resolution -> task list -> task list -> task list * task list
end

module FileStorage : STORAGE = struct
  let load ?(path="") () = 
    if path = "" then
      let config = Config.load () in
      let location = Config.expand_tilde config.markdown_location in
      Markdown.parse_file location, location
    else
      Markdown.parse_file path, path

  let save ?(path="") tasks = 
    try
      if path = "" then
        let config = Config.load () in
        Markdown.write_file (Config.expand_tilde config.markdown_location) tasks
      else
        Markdown.write_file path tasks;
      Ok ()
    with exn -> Error (Printexc.to_string exn)

  let check_conflict ?(path="") current_tasks = 
    let loaded_tasks, _ = load ~path () in
    if loaded_tasks = current_tasks then Some NoConflict
    else Some KeepMemory

  let resolve_conflict resolution memory_tasks file_tasks = 
    match resolution with
    | KeepMemory -> memory_tasks, file_tasks
    | KeepFile -> file_tasks, file_tasks
    | NoConflict -> memory_tasks, file_tasks
end

(* Convenience functions *)
let load_tasks ?(path="") () = fst (FileStorage.load ~path ())

let save_tasks ?(path="") tasks = FileStorage.save ~path tasks

(* JSON serialization of tasks for history persistence *)
let task_to_json task =
  `Assoc [
    ("id", `String task.id);
    ("description", `String task.description);
    ("category", match task.category with Some c -> `String c | None -> `Null);
    ("status", `String (Markdown.status_to_string task.status));
    ("priority", `String (Markdown.priority_to_string task.priority));
    ("due_date", match task.due_date with Some d -> `String d | None -> `Null);
    ("complexity", `String (Markdown.complexity_to_string task.complexity));
    ("effort", `String (Markdown.effort_to_string task.effort));
    ("created_at", `String task.created_at);
    ("updated_at", `String task.updated_at);
    ("completed_at", match task.completed_at with Some c -> `String c | None -> `Null);
    ("recurrence", match task.recurrence with Some r -> `String r | None -> `Null);
  ]

let task_of_json (json : Yojson.Basic.t) =
  let open Yojson.Basic.Util in
  let get_s field = json |> member field |> to_string_option |> Option.value ~default:"" in
  let get_opt field = json |> member field |> to_string_option in
  {
    id = get_s "id";
    description = get_s "description";
    category = get_opt "category";
    status = get_s "status" |> Markdown.status_of_string |> Option.value ~default:Backlog;
    priority = get_s "priority" |> Markdown.priority_of_string |> Option.value ~default:Low;
    due_date = get_opt "due_date";
    complexity = get_s "complexity" |> Markdown.complexity_of_string |> Option.value ~default:C1;
    effort = get_s "effort" |> Markdown.effort_of_string |> Option.value ~default:E1;
    created_at = get_s "created_at";
    updated_at = get_s "updated_at";
    completed_at = get_opt "completed_at";
    recurrence = get_opt "recurrence";
  }

(* Sidecar file holding undo/redo history next to the task file *)
let history_path path = path ^ ".history"

let save_history ?(path="") history redo =
  let p = 
    if path = "" then
      let config = Config.load () in
      Config.expand_tilde config.markdown_location
    else path in
  let states_to_json states =
    `List (List.map (fun state -> `List (List.map task_to_json state)) states) in
  let json = `Assoc [
    ("history", states_to_json history);
    ("redo", states_to_json redo);
  ] in
  try Ok (Atomic_write.write (history_path p) (fun oc -> Yojson.Basic.to_channel oc json)) with
  | exn -> Error (Printexc.to_string exn)

let load_history ?(path="") () =
  let p = 
    if path = "" then
      let config = Config.load () in
      Config.expand_tilde config.markdown_location
    else path in
  let hp = history_path p in
  if Sys.file_exists hp then
    try
      let open Yojson.Basic.Util in
      let json = Yojson.Basic.from_file hp in
      let states field =
        json |> member field |> to_list
        |> List.map (fun state -> state |> to_list |> List.map task_of_json) in
      Some (states "history", states "redo")
    with _ -> None
  else None

let with_storage ?(path="") f = 
  let tasks = load_tasks ~path () in
  let result = f tasks in
  match result with
  | Ok (new_tasks, msg) ->
      let _ = save_tasks ~path new_tasks in
      Ok msg
  | Error msg -> Error msg
