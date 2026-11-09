(* Task manager - core CRUD operations *)

open Types
open Archive

module type MANAGER = sig
  type t
  val create : ?path:string -> unit -> t
  val add : t -> task -> (t * string, string) result
  val delete : t -> string -> (t * string, string) result
  val update : t -> task -> (t * string, string) result
  val mark : ?clock:(unit -> float) -> t -> string -> status -> (t * string, string) result
  val get : t -> string -> task option
  val get_all : t -> task list
  val find_by_id : t -> string -> task option
  val get_config : t -> config
  val get_tasks : t -> task list
  val search : t -> string -> task list
  val filter_by_category : t -> string -> task list
  val filter_by_status : t -> status -> task list
  val filter_by_priority : t -> priority -> task list
  val sort_by_priority : t -> task list
  val sort_by_due_date : t -> task list
  val undo : t -> (t * string, string) result
  val redo : t -> (t * string, string) result
  val save : t -> (t * string, string) result
  val archive_completed : ?clock:(unit -> float) -> t -> (t * string, string) result
end

module FileTaskManager : MANAGER = struct
  type t = {
    tasks : task list;
    history : History.TaskHistory.t;
    path : string;
    config : config;
    rejections : string list;
  }

  let create ?(path="") () = 
    let config = Config.load () in
    let p = 
      if path = "" then Config.expand_tilde config.markdown_location else path in
    let read = Markdown.parse_file_result p in
    List.iter (fun note -> prerr_endline ("Note: " ^ note)) read.normalisations;
    List.iter (fun warning -> prerr_endline ("Warning: " ^ warning)) read.rejections;
    let persisted_tasks = read.tasks in
    let tasks = Recurring.process_recurring_tasks persisted_tasks in
    let () = match Notifications.check_due_tasks ~path:p config tasks with
      | Ok _ -> ()
      | Error error -> prerr_endline error in
    let history =
      match Storage.load_history ~path:p () with
      | Some (current :: _ as history, redo) when current = persisted_tasks ->
          History.TaskHistory.restore ~max_size:config.max_history_size history redo
      | Some _ ->
          prerr_endline "Warning: history does not match task file; undo history ignored";
          History.TaskHistory.create config.max_history_size
          |> fun h -> History.TaskHistory.push h tasks
      | None ->
          (* Seed with the initial state so the first change is undoable *)
          History.TaskHistory.create config.max_history_size
          |> fun h -> History.TaskHistory.push h tasks
    in
    {
      tasks;
      history;
      path = p;
      config;
      rejections = read.rejections;
    }

  let writable m =
    match m.rejections with
    | [] -> Ok ()
    | warnings -> Error ("Read-only: " ^ String.concat "; " warnings)

  let save_state m =
    match writable m with
    | Error e -> Error e
    | Ok () -> match Storage.save_tasks ~path:m.path m.tasks with
    | Ok () ->
        (match Storage.save_history ~path:m.path
            (History.TaskHistory.states m.history)
            (History.TaskHistory.redo_states m.history) with
         | Ok () -> Ok (m, "Tasks saved")
         | Error e -> Error ("Task file saved, but history was not saved; " ^
                             "do not retry blindly: " ^ e))
    | Error e -> Error e

  let add m task =
    match writable m with
    | Error e -> Error e
    | Ok () ->
    let new_tasks = task :: m.tasks in
    let new_m = { m with tasks = new_tasks } in
    let new_history = History.record_change m.history new_tasks in
    let new_m = { new_m with history = new_history } in
    match save_state new_m with
    | Ok (saved_m, msg) -> Ok (saved_m, msg)
    | Error e -> Error e

  let delete m task_id =
    match writable m with
    | Error e -> Error e
    | Ok () ->
    let new_tasks = List.filter (fun t -> t.id <> task_id) m.tasks in
    if new_tasks = m.tasks then
      Error (Printf.sprintf "Task %s not found" task_id)
    else
      let new_m = { m with tasks = new_tasks } in
      let new_history = History.record_change m.history new_tasks in
      let new_m = { new_m with history = new_history } in
      match save_state new_m with
      | Ok (saved_m, msg) -> Ok (saved_m, msg)
      | Error e -> Error e

  let update m updated_task =
    match writable m with
    | Error e -> Error e
    | Ok () ->
    let new_tasks = List.map (fun t -> if t.id = updated_task.id then updated_task else t) m.tasks in
    if new_tasks = m.tasks then
      Error (Printf.sprintf "Task %s not found" updated_task.id)
    else
      let new_m = { m with tasks = new_tasks } in
      let new_history = History.record_change m.history new_tasks in
      let new_m = { new_m with history = new_history } in
      match save_state new_m with
      | Ok (saved_m, msg) -> Ok (saved_m, msg)
      | Error e -> Error e

  let mark ?clock m task_id new_status =
    match writable m with
    | Error e -> Error e
    | Ok () ->
    let new_tasks = List.map (fun t ->
      if t.id <> task_id then t
      else if new_status = Complete && (t.status <> Complete || t.completed_at = None)
      then Task.mark_complete ?clock t
      else Task.mark_status new_status t) m.tasks in
    if not (List.exists (fun t -> t.id = task_id) m.tasks) then
      Error (Printf.sprintf "Task %s not found" task_id)
    else if new_tasks = m.tasks then Ok (m, "Tasks saved")
    else
      let new_m = { m with tasks = new_tasks } in
      let new_history = History.record_change m.history new_tasks in
      let new_m = { new_m with history = new_history } in
      match save_state new_m with
      | Ok (saved_m, msg) -> Ok (saved_m, msg)
      | Error e -> Error e

  let get m index = 
    try Some (List.nth m.tasks (int_of_string index))
    with _ -> None

  let get_all m = m.tasks

  let find_by_id m task_id = List.find_opt (fun t -> t.id = task_id) m.tasks

  let string_contains haystack needle = 
    let len_h = String.length haystack in
    let len_n = String.length needle in
    if len_n = 0 then true else
    let rec check i = 
      if i > len_h - len_n then false
      else if String.sub haystack i len_n = needle then true
      else check (i + 1)
    in check 0

  let search m query = 
    let query_lower = String.lowercase_ascii query in
    List.filter (fun t -> 
      string_contains (String.lowercase_ascii t.description) query_lower
      || (match t.category with Some c -> string_contains (String.lowercase_ascii c) query_lower | None -> false)
    ) m.tasks

  let filter_by_category m category = 
    List.filter (fun t -> match t.category with Some c -> c = category | None -> false) m.tasks

  let filter_by_status m status = List.filter (fun t -> t.status = status) m.tasks

  let filter_by_priority m priority = List.filter (fun t -> t.priority = priority) m.tasks

  let sort_by_priority m = 
    let priority_order = function Low -> 1 | Medium -> 2 | High -> 3 in
    List.sort (fun a b -> compare (priority_order b.priority) (priority_order a.priority)) m.tasks

  let sort_by_due_date m = 
    List.sort (fun a b -> 
      match a.due_date, b.due_date with
      | Some da, Some db -> String.compare da db
      | Some _, None -> -1
      | None, Some _ -> 1
      | None, None -> 0
    ) m.tasks

  let undo m =
    match writable m with
    | Error e -> Error e
    | Ok () ->
    let (state_opt : task list option), new_history = History.TaskHistory.undo m.history in
    match state_opt with
    | Some tasks ->
        let new_m = { m with tasks; history = new_history } in
        save_state new_m
    | None -> Error "Nothing to undo"

  let redo m =
    match writable m with
    | Error e -> Error e
    | Ok () ->
    let (state_opt : task list option), new_history = History.TaskHistory.redo m.history in
    match state_opt with
    | Some tasks ->
        let new_m = { m with tasks; history = new_history } in
        save_state new_m
    | None -> Error "Nothing to redo"

  let save m = save_state m

  let archive_completed ?clock m =
    match writable m with
    | Error e -> Error e
    | Ok () -> match archive_completed_tasks ?clock ~_path:m.path ~days:m.config.auto_archive_days m.tasks with
    | Ok remaining_tasks ->
        let new_m = { m with tasks = remaining_tasks } in
        let new_history = History.record_change m.history remaining_tasks in
        let new_m = { new_m with history = new_history } in
        save_state new_m
    | Error e -> Error e

  let get_config m = m.config
  let get_tasks m = m.tasks
end

(* Convenience functions *)
let with_manager ?path f = 
  let m = FileTaskManager.create ?path () in
  f m
