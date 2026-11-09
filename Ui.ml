(* UI module - terminal coloring and formatting *)

open Types

(* ANSI color codes *)
let color_codes = [
  ("black", "\027[30m");
  ("red", "\027[31m");
  ("green", "\027[32m");
  ("yellow", "\027[33m");
  ("blue", "\027[34m");
  ("magenta", "\027[35m");
  ("cyan", "\027[36m");
  ("white", "\027[37m");
  ("gray", "\027[90m");
  ("bright_red", "\027[91m");
  ("bright_green", "\027[92m");
  ("bright_yellow", "\027[93m");
  ("bright_blue", "\027[94m");
  ("bright_magenta", "\027[95m");
  ("bright_cyan", "\027[96m");
  ("reset", "\027[0m");
]

let color_map = 
  let tbl = Hashtbl.create (List.length color_codes) in
  List.iter (fun (k, v) -> Hashtbl.add tbl k v) color_codes;
  tbl

let get_color_code color = 
  try Hashtbl.find color_map color
  with Not_found -> "\027[0m"

let colorize color text = 
  let code = get_color_code color in
  Printf.sprintf "%s%s%s" code text (get_color_code "reset")

(* Format status with color *)
let format_status theme status = 
  let color = match status with
    | Backlog -> theme.status_backlog
    | InProgress -> theme.status_in_progress
    | OnHold -> theme.status_on_hold
    | Complete -> theme.status_complete
    | Idea -> theme.status_idea
  in
  colorize color (Task.status_to_string status)

(* Format priority with color *)
let format_priority theme priority = 
  let color = match priority with
    | Low -> theme.priority_low
    | Medium -> theme.priority_medium
    | High -> theme.priority_high
  in
  colorize color (Task.priority_to_string priority)

(* Format effort with color *)
let format_effort theme effort = 
  let color = match effort with
    | E1 -> theme.effort_1
    | E2 -> theme.effort_2
    | E3 -> theme.effort_3
    | E4 -> theme.effort_4
    | E5 -> theme.effort_5
  in
  colorize color (Task.effort_to_string effort)

(* Format complexity with color *)
let format_complexity theme complexity = 
  let color = match complexity with
    | C1 -> theme.complexity_1
    | C2 -> theme.complexity_2
    | C3 -> theme.complexity_3
    | C4 -> theme.complexity_4
    | C5 -> theme.complexity_5
  in
  colorize color (Task.complexity_to_string complexity)

(* Format due date with color *)
let format_due_date theme task = 
  match task.due_date with
  | Some due ->
      if Task.is_overdue task then
        colorize theme.due_overdue due
      else if Task.is_due_soon task then
        colorize theme.due_soon due
      else due
  | None -> "N/A"

(* Format category with color *)
let format_category _theme category = 
  match category with
  | Some c -> colorize "cyan" c
  | None -> "N/A"

(* Format single task for display *)
let format_task ?(index=false) ?(num=0) theme task = 
  let prefix = if index then Printf.sprintf "[%d] " num else "" in
  let status_str = format_status theme task.status in
  let priority_str = format_priority theme task.priority in
  let effort_str = format_effort theme task.effort in
  let complexity_str = format_complexity theme task.complexity in
  let due_str = format_due_date theme task in
  let category_str = format_category theme task.category in
  
  Printf.sprintf "%s%s | %s | %s | %s | %s | %s | %s"
    prefix
    (if String.length task.id >= 8 then String.sub task.id 0 8 else task.id) (* Shortened UUID *)
    task.description
    category_str
    status_str
    priority_str
    due_str
    (Printf.sprintf "C:%s E:%s" complexity_str effort_str)

(* Strip ANSI color codes from a string for length calculation *)
let strip_ansi s = 
  let len = String.length s in
  let buf = Buffer.create len in
  let rec loop i = 
    if i >= len then Buffer.contents buf
    else if s.[i] = '\027' then
      (* Skip ANSI escape sequence starting with ESC *)
      let j = ref (i + 1) in
      (* Skip until we find 'm' *)
      while !j < len && s.[!j] <> 'm' do incr j done;
      if !j < len then loop (!j + 1) else Buffer.contents buf
    else (
      Buffer.add_char buf s.[i];
      loop (i + 1)
    )
  in
  loop 0

(* Pad a string to a given width (ANSI-aware) *)
let pad_to_width s width = 
  let display_len = String.length (strip_ansi s) in
  if display_len < width then s ^ String.make (width - display_len) ' ' else s

(* Calculate column widths based on content *)
let calculate_column_widths theme tasks = 
  (* Header columns *)
  let header_id = "ID" in
  let header_desc = "Description" in
  let header_cat = "Category" in
  let header_status = "Status" in
  let header_prio = "Priority" in
  let header_due = "Due Date" in
  let header_comp = "Complexity/Effort" in
  
  (* Calculate max width for each column, using display length (without ANSI codes) *)
  (* Account for [n] prefix in ID column *)
  let max_id_with_prefix = 
    if tasks <> [] then 
      let max_index = List.length tasks in
      let index_str = Printf.sprintf "[%d] " max_index in
      List.fold_left (fun acc t -> 
        let id_str = if String.length t.id >= 8 then String.sub t.id 0 8 else t.id in
        max acc (String.length index_str + String.length id_str)
      ) (String.length index_str + String.length header_id) tasks
    else 
      max (String.length "[1] ") (String.length header_id)
  in
  let max_desc = 
    List.fold_left (fun acc t -> 
      let dlen = String.length (strip_ansi t.description) in
      max acc dlen
    ) (String.length (strip_ansi header_desc)) tasks
  in
  let max_cat = 
    List.fold_left (fun acc t -> 
      let cat_str = format_category theme t.category in
      max acc (String.length (strip_ansi cat_str))
    ) (String.length (strip_ansi header_cat)) tasks
  in
  let max_status = 
    List.fold_left (fun acc t -> 
      let status_str = format_status theme t.status in
      max acc (String.length (strip_ansi status_str))
    ) (String.length (strip_ansi header_status)) tasks
  in
  let max_prio = 
    List.fold_left (fun acc t -> 
      let prio_str = format_priority theme t.priority in
      max acc (String.length (strip_ansi prio_str))
    ) (String.length (strip_ansi header_prio)) tasks
  in
  let max_due = 
    List.fold_left (fun acc t -> 
      let due_str = format_due_date theme t in
      max acc (String.length (strip_ansi due_str))
    ) (String.length (strip_ansi header_due)) tasks
  in
  let max_comp = 
    List.fold_left (fun acc t -> 
      let comp_str = Printf.sprintf "C:%s E:%s" 
        (format_complexity theme t.complexity) 
        (format_effort theme t.effort) in
      max acc (String.length (strip_ansi comp_str))
    ) (String.length (strip_ansi header_comp)) tasks
  in
  
  (max_id_with_prefix, max_desc, max_cat, max_status, max_prio, max_due, max_comp)

(* Format single task row with padding *)
let format_task_row ?(index=false) ?(num=0) theme max_widths task = 
  let (max_id_with_prefix, max_desc, max_cat, max_status, max_prio, max_due, max_comp) = max_widths in
  let prefix = if index then Printf.sprintf "[%d] " num else "" in
  let id_str = if String.length task.id >= 8 then String.sub task.id 0 8 else task.id in
  let status_str = format_status theme task.status in
  let priority_str = format_priority theme task.priority in
  let effort_str = format_effort theme task.effort in
  let complexity_str = format_complexity theme task.complexity in
  let due_str = format_due_date theme task in
  let category_str = format_category theme task.category in
  let comp_str = Printf.sprintf "C:%s E:%s" complexity_str effort_str in
  
  (* For ID column, combine prefix + id_str and pad the whole thing *)
  let id_column = prefix ^ id_str in
  Printf.sprintf "%s | %s | %s | %s | %s | %s | %s"
    (pad_to_width id_column max_id_with_prefix)
    (pad_to_width task.description max_desc)
    (pad_to_width category_str max_cat)
    (pad_to_width status_str max_status)
    (pad_to_width priority_str max_prio)
    (pad_to_width due_str max_due)
    (pad_to_width comp_str max_comp)

(* Format task list as table *)
let format_task_list theme tasks = 
  if tasks = [] then "No tasks found" else
  let max_widths = calculate_column_widths theme tasks in
  let (max_id_with_prefix, max_desc, max_cat, max_status, max_prio, max_due, max_comp) = max_widths in

  (* Build header with padding *)
  let header = 
    Printf.sprintf "%s | %s | %s | %s | %s | %s | %s"
      (pad_to_width "ID" max_id_with_prefix)
      (pad_to_width "Description" max_desc)
      (pad_to_width "Category" max_cat)
      (pad_to_width "Status" max_status)
      (pad_to_width "Priority" max_prio)
      (pad_to_width "Due Date" max_due)
      (pad_to_width "Complexity/Effort" max_comp)
  in
  let separator = String.make (String.length header) '-' in
  
  let lines = header :: separator :: 
    (List.mapi (fun i t -> format_task_row ~index:true ~num:(i+1) theme max_widths t) tasks)
  in
  String.concat "\n" lines

(* Format error message *)
let format_error msg = colorize "red" (Printf.sprintf "Error: %s" msg)

(* Format success message *)
let format_success msg = colorize "green" (Printf.sprintf "Success: %s" msg)

(* Format warning message *)
let format_warning msg = colorize "yellow" (Printf.sprintf "Warning: %s" msg)

(* Format info message *)
let format_info msg = colorize "cyan" msg
