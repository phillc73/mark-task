(* REPL module - interactive mode *)

open Types
open Task_manager

(* Command type *)
type command =
  | Add of string * string list
  | Delete of string
  | Edit of string * string list
  | Mark of string * string
  | List
  | ListCategory of string
  | ListStatus of string
  | ListPriority of string
  | Search of string
  | Undo
  | Redo
  | Help
  | Quit
  | Unknown of string

(* Parse command string *)
let parse_command s =
  let parts = String.split_on_char ' ' s |> List.filter (fun s -> s <> "") in
  match parts with
  | [] -> Unknown ""
  | "add" :: desc :: attrs ->
      (* Check if using comma-separated syntax *)
      let full_text = String.concat " " (desc :: attrs) in
      if String.contains full_text ',' then
        (* Comma-separated format: description, category, priority, due, complexity, effort, recurring *)
        let parts =
          String.split_on_char ',' full_text |> List.map String.trim
        in
        let desc_part = List.hd parts in
        let rest_parts = List.tl parts in
        let category_part =
          if List.length rest_parts > 0 then Some (List.hd rest_parts) else None
        in
        let priority_part =
          if List.length rest_parts > 1 then Some (List.nth rest_parts 1)
          else None
        in
        let due_part =
          if List.length rest_parts > 2 then Some (List.nth rest_parts 2)
          else None
        in
        let complexity_part =
          if List.length rest_parts > 3 then Some (List.nth rest_parts 3)
          else None
        in
        let effort_part =
          if List.length rest_parts > 4 then Some (List.nth rest_parts 4)
          else None
        in
        let recurring_part =
          if List.length rest_parts > 5 then Some (List.nth rest_parts 5)
          else None
        in
        let flags = [] in
        let flags =
          match category_part with
          | Some c -> "--category" :: c :: flags
          | None -> flags
        in
        let flags =
          match priority_part with
          | Some p -> "--priority" :: p :: flags
          | None -> flags
        in
        let flags =
          match due_part with Some d -> "--due" :: d :: flags | None -> flags
        in
        let flags =
          match complexity_part with
          | Some c -> "--complexity" :: c :: flags
          | None -> flags
        in
        let flags =
          match effort_part with
          | Some e -> "--effort" :: e :: flags
          | None -> flags
        in
        let flags =
          match recurring_part with
          | Some r -> "--recurring" :: r :: flags
          | None -> flags
        in
        Add (desc_part, flags)
      else Add (full_text, [])
  | "edit" :: id :: attrs -> Edit (id, attrs)
  | "delete" :: id :: _ -> Delete id
  | "mark" :: id :: status :: rest ->
      Mark (id, String.concat " " (status :: rest))
  | "list" :: [] -> List
  | "list" :: ("--category" | "-c") :: cat :: rest ->
      ListCategory (String.concat " " (cat :: rest))
  | "list" :: ("--status" | "-s") :: status :: rest ->
      ListStatus (String.concat " " (status :: rest))
  | "list" :: ("--priority" | "-p") :: prio :: rest ->
      ListPriority (String.concat " " (prio :: rest))
  | "search" :: query :: rest -> Search (String.concat " " (query :: rest))
  | "undo" :: [] -> Undo
  | "redo" :: [] -> Redo
  | "help" :: [] -> Help
  | "quit" :: [] | "exit" :: [] -> Quit
  | _ -> Unknown (String.concat " " parts)

(* Help text *)
let help_text =
  "Available commands:\n"
  ^ "  add <description> [--category <cat>|-c] [--priority <prio>|-p] [--due \
     <date>|-d]\n"
  ^ "       [--complexity <1-5>|-comp] [--effort <1-5>|-e] [--recurring \
     <pattern>|-r]\n" ^ "    Add a new task\n"
  ^ "    Or: add <description>, <category>, <priority>, <due>, <complexity>, \
     <effort>, <recurring>\n"
  ^ "    Example: add Stuff to do, Work, high, 2026-06-02, 1, 1, weekly\n"
  ^ "  edit <id|#> [--category <cat>|-c] [--priority <prio>|-p] [--due \
     <date>|-d]\n"
  ^ "       [--complexity <1-5>|-comp] [--effort <1-5>|-e] [--recurring \
     <pattern>|-r]\n"
  ^ "       [--clear-due] [--clear-recurring]\n"
  ^ "    Edit an existing task by ID or row number (e.g., edit 1 -c Work)\n"
  ^ "    Same flags as add, but updates the specified task\n"
  ^ "    Omitted flags keep their current values; clear flags remove them\n"
  ^ "  delete <id|#>\n" ^ "    Delete a task by ID or row number (e.g., 1)\n"
  ^ "  mark <id|#> <status>\n"
  ^ "    Mark a task as Backlog, InProgress, OnHold, Complete, or Idea\n"
  ^ "  list [--category <cat>|-c] [--status <status>|-s] [--priority <prio>|-p]\n"
  ^ "    List all tasks, optionally filtered\n" ^ "  search <query>\n"
  ^ "    Search tasks by description or category\n" ^ "  undo\n"
  ^ "    Undo the last operation\n" ^ "  redo\n"
  ^ "    Redo the last undone operation\n" ^ "  help\n"
  ^ "    Show this help text\n" ^ "  quit/exit\n" ^ "    Exit the program\n"
  ^ "\nNotes:\n" ^ "  - Category filters match the stored text exactly (Work and work differ)\n"
  ^ "  - Due dates are RFC 3339 timestamps (e.g. 2026-06-02T09:00:00Z)\n"

(* Execute command on task manager *)
let execute_command (m : FileTaskManager.t) command =
  match command with
  | Add (full_desc, attrs) ->
      (* Parse full description to extract flags and clean description *)
      (* If attrs is non-empty (from comma-separated or flag syntax), use it directly *)
      let words = String.split_on_char ' ' full_desc in
      (* Check if comma-separated syntax is used *)
      let has_comma = String.contains full_desc ',' in
      let rec parse_and_clean acc desc_parts = function
        | [] -> (List.rev acc, List.rev desc_parts)
        | "--category" :: v :: rest | "-c" :: v :: rest ->
            parse_and_clean (("category", Some v) :: acc) desc_parts rest
        | "--priority" :: v :: rest | "-p" :: v :: rest ->
            parse_and_clean (("priority", Some v) :: acc) desc_parts rest
        | "--due" :: v :: rest | "-d" :: v :: rest ->
            parse_and_clean (("due_date", Some v) :: acc) desc_parts rest
        | "--complexity" :: v :: rest | "-comp" :: v :: rest ->
            parse_and_clean (("complexity", Some v) :: acc) desc_parts rest
        | "--effort" :: v :: rest | "-e" :: v :: rest ->
            parse_and_clean (("effort", Some v) :: acc) desc_parts rest
        | "--recurring" :: v :: rest | "-r" :: v :: rest ->
            parse_and_clean (("recurrence", Some v) :: acc) desc_parts rest
        | part :: rest -> parse_and_clean acc (part :: desc_parts) rest
      in
      (* Handle comma-separated syntax: "desc, category, priority, due, complexity, effort, recurring" *)
      let attrs_parsed, desc_parts =
        (* If attrs is non-empty from parse_command (comma-separated syntax), use it *)
        if attrs <> [] then
          (* attrs is already in flag format: ["--category"; "Work"; "--priority"; "high"; ...] *)
          (* For comma-separated syntax, full_desc is the description, attrs are the flags *)
          let rec parse_flags acc = function
            | [] -> List.rev acc
            | "--category" :: v :: rest | "-c" :: v :: rest ->
                parse_flags (("category", Some v) :: acc) rest
            | "--priority" :: v :: rest | "-p" :: v :: rest ->
                parse_flags (("priority", Some v) :: acc) rest
            | "--due" :: v :: rest | "-d" :: v :: rest ->
                parse_flags (("due_date", Some v) :: acc) rest
            | "--complexity" :: v :: rest | "-comp" :: v :: rest ->
                parse_flags (("complexity", Some v) :: acc) rest
            | "--effort" :: v :: rest | "-e" :: v :: rest ->
                parse_flags (("effort", Some v) :: acc) rest
            | "--recurring" :: v :: rest | "-r" :: v :: rest ->
                parse_flags (("recurrence", Some v) :: acc) rest
            | _ :: rest -> parse_flags acc rest (* Skip unknown flags *)
          in
          let attrs_parsed = parse_flags [] attrs in
          (attrs_parsed, [ full_desc ]) (* Use full_desc as the description *)
        else if has_comma then
          let parts =
            String.split_on_char ',' full_desc |> List.map String.trim
          in
          match parts with
          | desc_part :: [] -> ([], [ desc_part ])
          | [ desc_part; cat ] -> ([ ("category", Some cat) ], [ desc_part ])
          | [ desc_part; cat; prio ] ->
              ( [ ("category", Some cat); ("priority", Some prio) ],
                [ desc_part ] )
          | [ desc_part; cat; prio; due ] ->
              ( [
                  ("category", Some cat);
                  ("priority", Some prio);
                  ("due_date", Some due);
                ],
                [ desc_part ] )
          | [ desc_part; cat; prio; due; comp_val ] ->
              ( [
                  ("category", Some cat);
                  ("priority", Some prio);
                  ("due_date", Some due);
                  ("complexity", Some comp_val);
                ],
                [ desc_part ] )
          | [ desc_part; cat; prio; due; comp_val; eff_val ] ->
              ( [
                  ("category", Some cat);
                  ("priority", Some prio);
                  ("due_date", Some due);
                  ("complexity", Some comp_val);
                  ("effort", Some eff_val);
                ],
                [ desc_part ] )
          | [ desc_part; cat; prio; due; comp_val; eff_val; rec_val ] ->
              ( [
                  ("category", Some cat);
                  ("priority", Some prio);
                  ("due_date", Some due);
                  ("complexity", Some comp_val);
                  ("effort", Some eff_val);
                  ("recurrence", Some rec_val);
                ],
                [ desc_part ] )
          | _ -> parse_and_clean [] [] (full_desc |> String.split_on_char ' ')
        else parse_and_clean [] [] words
      in
      let desc = Markdown.normalise_value (String.concat " " desc_parts) in
      let get_opt_value key =
        match List.assoc_opt key attrs_parsed with
        | Some (Some v) -> Some v
        | _ -> None
      in
      let category =
        match get_opt_value "category" with
        | Some c -> Some (Markdown.normalise_value c)
        | None -> None
      in
      let priority = get_opt_value "priority" in
      let due_date = get_opt_value "due_date" in
      let complexity = get_opt_value "complexity" in
      let effort = get_opt_value "effort" in
      let recurrence = get_opt_value "recurrence" in

      let priority_enum =
        match priority with
        | Some (v : string) -> (
            match String.lowercase_ascii v with
            | "low" -> Some Low
            | "medium" -> Some Medium
            | "high" -> Some High
            | _ -> None)
        | _ -> None
      in
      let complexity_enum =
        match complexity with
        | Some v -> (
            match v with
            | "1" -> Some C1
            | "2" -> Some C2
            | "3" -> Some C3
            | "4" -> Some C4
            | "5" -> Some C5
            | _ -> None)
        | _ -> None
      in
      let effort_enum =
        match effort with
        | Some v -> (
            match v with
            | "1" -> Some E1
            | "2" -> Some E2
            | "3" -> Some E3
            | "4" -> Some E4
            | "5" -> Some E5
            | _ -> None)
        | _ -> None
      in

      (match Validation.validate_task desc ?category ?priority ?due_date
               ?complexity ?effort ?recurrence () with
      | Error e -> Error (Validation.string_of_error e)
      | Ok () ->
          let task =
            Task.create ~description:desc ~category ?priority:priority_enum
              ~due_date ?complexity:complexity_enum ?effort:effort_enum
              ~recurrence ()
          in
          FileTaskManager.add m task)
  | Edit (id, attrs) ->
      (* Support both task ID and row number *)
      let task_id =
        try
          let idx = int_of_string id - 1 in
          let tasks = FileTaskManager.get_tasks m in
          if idx >= 0 && idx < List.length tasks then (List.nth tasks idx).id
          else id
        with _ -> id
      in
      (* Get the existing task *)
      begin match FileTaskManager.find_by_id m task_id with
      | Some existing_task ->
          (* Parse the new attributes and update the task *)
          let words = String.split_on_char ' ' (String.concat " " attrs) in
          let rec parse_attrs acc = function
            | [] -> List.rev acc
            | "--category" :: v :: rest | "-c" :: v :: rest ->
                parse_attrs (("category", Some v) :: acc) rest
            | "--priority" :: v :: rest | "-p" :: v :: rest ->
                parse_attrs (("priority", Some v) :: acc) rest
            | "--due" :: v :: rest | "-d" :: v :: rest ->
                parse_attrs (("due_date", Some v) :: acc) rest
            | "--complexity" :: v :: rest | "-comp" :: v :: rest ->
                parse_attrs (("complexity", Some v) :: acc) rest
            | "--effort" :: v :: rest | "-e" :: v :: rest ->
                parse_attrs (("effort", Some v) :: acc) rest
            | "--recurring" :: v :: rest | "-r" :: v :: rest ->
                parse_attrs (("recurrence", Some v) :: acc) rest
            | "--clear-due" :: rest ->
                parse_attrs (("clear_due", Some "") :: acc) rest
            | "--clear-recurring" :: rest | "--clear-recurrence" :: rest ->
                parse_attrs (("clear_recurring", Some "") :: acc) rest
            | _ :: rest -> parse_attrs acc rest (* Skip unknown flags *)
          in
          let attrs_parsed = parse_attrs [] words in
          let get_opt_value key =
            match List.assoc_opt key attrs_parsed with
            | Some (Some v) -> Some v
            | _ -> None
          in
          let category =
            match get_opt_value "category" with
            | Some c -> Some (Markdown.normalise_value c)
            | None -> existing_task.category
          in
          let priority = get_opt_value "priority" in
          let provided_due = get_opt_value "due_date" in
          let provided_recurrence = get_opt_value "recurrence" in
          let complexity = get_opt_value "complexity" in
          let effort = get_opt_value "effort" in
          let due_date =
            if List.mem_assoc "clear_due" attrs_parsed then None
            else match provided_due with
              | Some value -> Some value
              | None -> existing_task.due_date
          in
          let recurrence =
            if List.mem_assoc "clear_recurring" attrs_parsed then None
            else match provided_recurrence with
              | Some value -> Some value
              | None -> existing_task.recurrence
          in

          let priority_enum =
            match priority with
            | Some v -> (
                match String.lowercase_ascii v with
                | "low" -> Some Low
                | "medium" -> Some Medium
                | "high" -> Some High
                | _ -> None)
            | _ -> Some existing_task.priority
          in
          let complexity_enum =
            match complexity with
            | Some v -> (
                match v with
                | "1" -> Some C1
                | "2" -> Some C2
                | "3" -> Some C3
                | "4" -> Some C4
                | "5" -> Some C5
                | _ -> None)
            | _ -> Some existing_task.complexity
          in
          let effort_enum =
            match effort with
            | Some v -> (
                match v with
                | "1" -> Some E1
                | "2" -> Some E2
                | "3" -> Some E3
                | "4" -> Some E4
                | "5" -> Some E5
                | _ -> None)
            | _ -> Some existing_task.effort
          in

          (match Validation.validate_task existing_task.description ?category
                   ?priority ?due_date:provided_due ?complexity ?effort
                   ?recurrence:provided_recurrence () with
           | Error e -> Error (Validation.string_of_error e)
           | Ok () ->
               (* Create updated task by copying existing and updating fields *)
               let updated_task =
                 {
                   existing_task with
                   category;
                   priority =
                     (match priority_enum with
                     | Some p -> p
                     | None -> existing_task.priority);
                   due_date;
                   complexity =
                     (match complexity_enum with
                     | Some c -> c
                     | None -> existing_task.complexity);
                   effort =
                     (match effort_enum with
                     | Some e -> e
                     | None -> existing_task.effort);
                   recurrence;
                 }
               in
               FileTaskManager.update m updated_task)
      | None -> Error (Printf.sprintf "Task %s not found" task_id)
      end
  | Delete id ->
      (* Support both task ID and row number *)
      let task_id =
        try
          let idx = int_of_string id - 1 in
          let tasks = FileTaskManager.get_tasks m in
          if idx >= 0 && idx < List.length tasks then (List.nth tasks idx).id
          else id
        with _ -> id
      in
      FileTaskManager.delete m task_id
  | Mark (id, status_str) ->
      (* Support both task ID and row number *)
      let task_id =
        try
          let idx = int_of_string id - 1 in
          let tasks = FileTaskManager.get_tasks m in
          if idx >= 0 && idx < List.length tasks then (List.nth tasks idx).id
          else id
        with _ -> id
      in
      let status =
        match String.lowercase_ascii status_str with
        | "backlog" -> Some Backlog
        | "inprogress" | "in progress" -> Some InProgress
        | "onhold" | "on hold" -> Some OnHold
        | "complete" -> Some Complete
        | "idea" -> Some Idea
        | _ -> None
      in
      (match status with
       | Some status -> FileTaskManager.mark m task_id status
       | None ->
           Error (Validation.string_of_error (InvalidStatus status_str)))
  | List ->
      let config = FileTaskManager.get_config m in
      Ok
        ( m,
          View.display_tasks ~config
            (Performance.load_filtered config (FileTaskManager.get_tasks m)) )
  | ListCategory cat ->
      Ok
        ( m,
          View.display_tasks
            ~config:(FileTaskManager.get_config m)
            (FileTaskManager.filter_by_category m cat) )
  | ListStatus status_str ->
      let status =
        match String.lowercase_ascii status_str with
        | "backlog" -> Some Backlog
        | "inprogress" | "in progress" -> Some InProgress
        | "onhold" | "on hold" -> Some OnHold
        | "complete" -> Some Complete
        | "idea" -> Some Idea
        | _ -> None
      in
      (match status with
       | Some status ->
           Ok
             ( m,
               View.display_tasks
                 ~config:(FileTaskManager.get_config m)
                 (FileTaskManager.filter_by_status m status) )
       | None -> Error (Validation.string_of_error (InvalidStatus status_str)))
  | ListPriority prio_str ->
      let priority =
        match String.lowercase_ascii prio_str with
        | "low" -> Some Low
        | "medium" -> Some Medium
        | "high" -> Some High
        | _ -> None
      in
      (match priority with
       | Some priority ->
           Ok
             ( m,
               View.display_tasks
                 ~config:(FileTaskManager.get_config m)
                 (FileTaskManager.filter_by_priority m priority) )
       | None -> Error (Validation.string_of_error (InvalidPriority prio_str)))
  | Search query ->
      Ok
        ( m,
          View.display_tasks
            ~config:(FileTaskManager.get_config m)
            (FileTaskManager.search m query) )
  | Undo -> FileTaskManager.undo m
  | Redo -> FileTaskManager.redo m
  | Help -> Ok (m, help_text)
  | Quit -> Ok (m, "Goodbye!")
  | Unknown cmd ->
      Ok
        ( m,
          Printf.sprintf
            "Unknown command: %s. Type 'help' for available commands." cmd )

(* Run REPL loop *)
let run_repl ?path () =
  let m = FileTaskManager.create ?path () in
  let config = FileTaskManager.get_config m in
  Printf.printf "Mark Task v0.1.0 - Interactive Mode\n";
  Printf.printf "Type 'help' for available commands.\n\n%!";

  let rec loop m =
    Printf.printf "> %!%s" (Ui.get_color_code "cyan");
    try
      let line = read_line () in
      Printf.printf "%s\n" (Ui.get_color_code "reset");
      let line_expanded = Alias.expand_alias config.aliases line in
      let command = parse_command line_expanded in
      match execute_command m command with
      | Ok (new_m, output) ->
          Printf.printf "%s\n%!" output;
          if command = Quit then () else loop new_m
      | Error err ->
          Printf.printf "%s\n%!" (Ui.format_error err);
          loop m
    with End_of_file -> Printf.printf "\nGoodbye!\n%!"
  in
  loop m
