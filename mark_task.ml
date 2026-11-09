(* Mark Task - CLI Task Manager *)
(* Main entry point *)

open Types
open Task_manager
open View
open Ui
open Cmdliner

(* CLI commands *)
let handle_add ?path description attrs =
  (* The stored form is the reader's form: surrounding whitespace and a
     backtick wrapper are decoration. *)
  let description = Markdown.normalise_value description in
  (* Parse attributes *)
  let category = ref None in
  let priority = ref None in
  let due_date = ref None in
  let complexity = ref None in
  let effort = ref None in
  let recurrence = ref None in

  let rec parse_attrs = function
    | [] -> ()
    | "--category" :: v :: rest ->
        category := Some (Markdown.normalise_value v);
        parse_attrs rest
    | "--priority" :: v :: rest ->
        priority := Some v;
        parse_attrs rest
    | "--due" :: v :: rest ->
        due_date := Some v;
        parse_attrs rest
    | "--complexity" :: v :: rest ->
        complexity := Some v;
        parse_attrs rest
    | "--effort" :: v :: rest ->
        effort := Some v;
        parse_attrs rest
    | "--recurring" :: v :: rest ->
        recurrence := Some v;
        parse_attrs rest
    | _ :: rest -> parse_attrs rest
  in
  parse_attrs attrs;

  match Validation.validate_task description ?category:!category ?priority:!priority
      ?due_date:!due_date ?complexity:!complexity ?effort:!effort
      ?recurrence:!recurrence () with
  | Error e -> Printf.printf "%s\n" (format_error (Validation.string_of_error e))
  | Ok () ->
      let priority_enum =
        match Option.map String.lowercase_ascii !priority with
        | Some "low" -> Low
        | Some "medium" -> Medium
        | Some "high" -> High
        | _ -> Low
      in
      let complexity_enum =
        match !complexity with
        | Some "1" -> C1
        | Some "2" -> C2
        | Some "3" -> C3
        | Some "4" -> C4
        | Some "5" -> C5
        | _ -> C1
      in
      let effort_enum =
        match !effort with
        | Some "1" -> E1
        | Some "2" -> E2
        | Some "3" -> E3
        | Some "4" -> E4
        | Some "5" -> E5
        | _ -> E1
      in

      let task =
        Task.create ~description ~category:!category ~priority:priority_enum
          ~due_date:!due_date ~complexity:complexity_enum ~effort:effort_enum
          ~recurrence:!recurrence ()
      in

      with_manager ?path (fun m ->
          match FileTaskManager.add m task with
          | Ok (_, msg) -> Printf.printf "%s\n" (format_success msg)
          | Error err -> Printf.printf "%s\n" (format_error err))

let handle_list ?path attrs =
  with_manager ?path (fun m ->
      let config = FileTaskManager.get_config m in
      let tasks = Performance.load_filtered config (FileTaskManager.get_tasks m) in
      let status_of_string s =
        match String.lowercase_ascii s with
        | "backlog" -> Some Backlog
        | "inprogress" | "in progress" -> Some InProgress
        | "onhold" | "on hold" -> Some OnHold
        | "complete" -> Some Complete
        | "idea" -> Some Idea
        | _ -> None
      in
      let priority_of_string s =
        match String.lowercase_ascii s with
        | "low" -> Some Low
        | "medium" -> Some Medium
        | "high" -> Some High
        | _ -> None
      in
      let tasks =
        match attrs with
        | "--category" :: cat :: rest ->
            FileTaskManager.filter_by_category m
              (String.concat " " (cat :: rest))
        | "--status" :: status :: rest ->
            (match status_of_string (String.concat " " (status :: rest)) with
             | Some s -> FileTaskManager.filter_by_status m s
             | None ->
                 Printf.printf "%s\n"
                   (format_error
                      (Validation.string_of_error
                         (InvalidStatus (String.concat " " (status :: rest)))));
                 [])
        | "--priority" :: prio :: rest ->
            (match priority_of_string (String.concat " " (prio :: rest)) with
             | Some p -> FileTaskManager.filter_by_priority m p
             | None ->
                 Printf.printf "%s\n"
                   (format_error
                      (Validation.string_of_error
                         (InvalidPriority (String.concat " " (prio :: rest)))));
                 [])
        | _ -> tasks
      in
      Printf.printf "%s\n"
        (display_tasks ~config tasks))

let handle_edit ?path id attrs =
  with_manager ?path (fun m ->
      (* Parse the command as if it came from REPL *)
      let command = Repl.(Edit (id, attrs)) in
      match Repl.execute_command m command with
      | Ok (_, msg) ->
          Printf.printf "%s\n" msg;
          Ok ()
      | Error err ->
          Printf.printf "%s\n" err;
          Error ())

let handle_delete ?path id =
  with_manager ?path (fun m ->
      match Repl.execute_command m (Repl.Delete id) with
      | Ok (_, msg) ->
          Printf.printf "%s\n" (format_success msg);
          Ok ()
      | Error err ->
          Printf.printf "%s\n" (format_error err);
          Error ())

let handle_mark ?path id status_str =
  with_manager ?path (fun m ->
      let status =
        match String.lowercase_ascii status_str with
        | "backlog" -> Some Backlog
        | "inprogress" | "in progress" -> Some InProgress
        | "onhold" | "on hold" -> Some OnHold
        | "complete" -> Some Complete
        | "idea" -> Some Idea
        | _ -> None
      in
      match status with
      | None ->
          Printf.printf "%s\n"
            (format_error (Validation.string_of_error (InvalidStatus status_str)));
          Error ()
      | Some _ ->
          (match Repl.execute_command m (Repl.Mark (id, status_str)) with
           | Ok (_, msg) ->
               Printf.printf "%s\n" (format_success msg);
               Ok ()
           | Error err ->
               Printf.printf "%s\n" (format_error err);
               Error ()))

let handle_search ?path query =
  with_manager ?path (fun m ->
      let results = FileTaskManager.search m query in
      Printf.printf "%s\n"
        (display_tasks ~config:(FileTaskManager.get_config m) results);
      Ok ())

let handle_undo ?path () =
  with_manager ?path (fun m ->
      match FileTaskManager.undo m with
      | Ok (_, msg) ->
          Printf.printf "%s\n" (format_success msg);
          Ok ()
      | Error err ->
          Printf.printf "%s\n" (format_error err);
          Error ())

let handle_redo ?path () =
  with_manager ?path (fun m ->
      match FileTaskManager.redo m with
      | Ok (_, msg) -> Printf.printf "%s\n" (format_success msg)
      | Error err -> Printf.printf "%s\n" (format_error err))

let handle_archive ?path () =
  with_manager ?path (fun m ->
      match FileTaskManager.archive_completed m with
      | Ok (_, msg) -> Printf.printf "%s\n" (format_success msg)
      | Error err -> Printf.printf "%s\n" (format_error err))

let handle_interactive ?path () = Repl.run_repl ?path ()
let handle_help () = Printf.printf "%s" Repl.help_text

(* CLI setup with cmdliner *)

let cmd_add =
  let doc = "Add a new task" in
  let description =
    Arg.(required & pos 0 (some string) None & info ~doc ~docv:"DESCRIPTION" [])
  in
  let category =
    Arg.(
      value
      & opt (some string) None
      & info ~doc:"Task category" [ "category"; "c" ])
  in
  let priority =
    Arg.(
      value
      & opt (some string) None
      & info ~doc:"Priority (Low, Medium, High)" [ "priority"; "p" ])
  in
  let due_date =
    Arg.(
      value
      & opt (some string) None
      & info ~doc:"Due date (RFC 3339 timestamp)" [ "due"; "d" ])
  in
  let complexity =
    Arg.(
      value
      & opt (some string) None
      & info ~doc:"Complexity (1-5)" [ "complexity"; "comp" ])
  in
  let effort =
    Arg.(
      value
      & opt (some string) None
      & info ~doc:"Effort (1-5)" [ "effort"; "e" ])
  in
  let recurring_val =
    Arg.(
      value
      & opt (some string) None
      & info ~doc:"Recurrence pattern" [ "recurring"; "r" ])
  in
  let file_path =
    Arg.(
      value
      & opt (some string) None
      & info ~doc:"Markdown file path" [ "file"; "f" ])
  in

  ( Term.(
      const (fun desc cat prio due comp eff recurring_val file_path ->
          let attrs = [] in
          let attrs =
            match cat with
            | Some c -> "--category" :: c :: attrs
            | None -> attrs
          in
          let attrs =
            match prio with
            | Some p -> "--priority" :: p :: attrs
            | None -> attrs
          in
          let attrs =
            match due with Some d -> "--due" :: d :: attrs | None -> attrs
          in
          let attrs =
            match comp with
            | Some c -> "--complexity" :: c :: attrs
            | None -> attrs
          in
          let attrs =
            match eff with Some e -> "--effort" :: e :: attrs | None -> attrs
          in
          let attrs =
            match recurring_val with
            | Some r -> "--recurring" :: r :: attrs
            | None -> attrs
          in
          handle_add ?path:file_path desc attrs)
      $ description $ category $ priority $ due_date $ complexity $ effort
      $ recurring_val $ file_path),
    Cmdliner.Cmd.info "add" ~doc )

let cmd_list =
  let doc = "List tasks" in
  let category =
    Arg.(
      value
      & opt (some string) None
      & info ~doc:"Filter by category" [ "category"; "c" ])
  in
  let status =
    Arg.(
      value
      & opt (some string) None
      & info ~doc:"Filter by status" [ "status"; "s" ])
  in
  let priority =
    Arg.(
      value
      & opt (some string) None
      & info ~doc:"Filter by priority" [ "priority"; "p" ])
  in
  let file_path =
    Arg.(
      value
      & opt (some string) None
      & info ~doc:"Markdown file path" [ "file"; "f" ])
  in

  ( Term.(
      const (fun cat status prio file_path ->
          let attrs = [] in
          let attrs =
            match cat with
            | Some c -> "--category" :: c :: attrs
            | None -> attrs
          in
          let attrs =
            match status with
            | Some s -> "--status" :: s :: attrs
            | None -> attrs
          in
          let attrs =
            match prio with
            | Some p -> "--priority" :: p :: attrs
            | None -> attrs
          in
          handle_list ?path:file_path attrs)
      $ category $ status $ priority $ file_path),
    Cmdliner.Cmd.info "list" ~doc )

let cmd_delete =
  let doc = "Delete a task" in
  let id =
    Arg.(required & pos 0 (some string) None & info ~doc ~docv:"TASK_ID" [])
  in
  let file_path =
    Arg.(
      value
      & opt (some string) None
      & info ~doc:"Markdown file path" [ "file"; "f" ])
  in

  ( Term.(
      const (fun id file_path ->
          ignore (handle_delete ?path:file_path id))
      $ id $ file_path),
    Cmdliner.Cmd.info "delete" ~doc )

let cmd_edit =
  let doc = "Edit a task" in
  let id =
    Arg.(
      required
      & pos 0 (some string) None
      & info ~doc:"Task ID or row number" ~docv:"TASK_ID" [])
  in
  let category =
    Arg.(
      value & opt (some string) None & info ~doc:"Category" [ "category"; "c" ])
  in
  let priority =
    Arg.(
      value
      & opt (some string) None
      & info ~doc:"Priority (low, medium, high)" [ "priority"; "p" ])
  in
  let due_date =
    Arg.(
      value
      & opt (some string) None
      & info ~doc:"Due date (RFC 3339 timestamp)" [ "due"; "d" ])
  in
  let complexity =
    Arg.(
      value
      & opt (some string) None
      & info ~doc:"Complexity (1-5)" [ "complexity"; "comp" ])
  in
  let effort =
    Arg.(
      value
      & opt (some string) None
      & info ~doc:"Effort (1-5)" [ "effort"; "e" ])
  in
  let recurring_val =
    Arg.(
      value
      & opt (some string) None
      & info ~doc:"Recurring pattern" [ "recurring"; "r" ])
  in
  let clear_due =
    Arg.(value & flag & info ~doc:"Clear the due date" [ "clear-due" ])
  in
  let clear_recurring =
    Arg.(value & flag & info ~doc:"Clear the recurrence" [ "clear-recurring" ])
  in
  let file_path =
    Arg.(
      value
      & opt (some string) None
      & info ~doc:"Markdown file path" [ "file"; "f" ])
  in

  ( Term.(
      const
        (fun
          id
          category
          priority
          due_date
          complexity
          effort
          recurring_val
          clear_due
          clear_recurring
          file_path
        ->
          let attrs = [] in
          let attrs =
            match category with
            | Some c -> "--category" :: c :: attrs
            | None -> attrs
          in
          let attrs =
            match priority with
            | Some p -> "--priority" :: p :: attrs
            | None -> attrs
          in
          let attrs =
            match due_date with
            | Some d -> "--due" :: d :: attrs
            | None -> attrs
          in
          let attrs =
            match complexity with
            | Some c -> "--complexity" :: c :: attrs
            | None -> attrs
          in
          let attrs =
            match effort with
            | Some e -> "--effort" :: e :: attrs
            | None -> attrs
          in
          let attrs =
            match recurring_val with
            | Some r -> "--recurring" :: r :: attrs
            | None -> attrs
          in
          let attrs = if clear_due then "--clear-due" :: attrs else attrs in
          let attrs =
            if clear_recurring then "--clear-recurring" :: attrs else attrs in
          ignore (handle_edit ?path:file_path id attrs))
      $ id $ category $ priority $ due_date $ complexity $ effort
      $ recurring_val $ clear_due $ clear_recurring $ file_path),
    Cmdliner.Cmd.info "edit" ~doc )

let cmd_mark =
  let doc = "Mark task status" in
  let id =
    Arg.(required & pos 0 (some string) None & info ~doc ~docv:"TASK_ID" [])
  in
  let status =
    Arg.(required & pos 1 (some string) None & info ~doc ~docv:"STATUS" [])
  in
  let file_path =
    Arg.(
      value
      & opt (some string) None
      & info ~doc:"Markdown file path" [ "file"; "f" ])
  in

  ( Term.(
      const (fun id status file_path ->
          ignore (handle_mark ?path:file_path id status))
      $ id $ status $ file_path),
    Cmdliner.Cmd.info "mark" ~doc )

let cmd_search =
  let doc = "Search tasks" in
  let query =
    Arg.(required & pos 0 (some string) None & info ~doc ~docv:"QUERY" [])
  in
  let file_path =
    Arg.(
      value
      & opt (some string) None
      & info ~doc:"Markdown file path" [ "file"; "f" ])
  in

  ( Term.(
      const (fun query file_path ->
          ignore (handle_search ?path:file_path query))
      $ query $ file_path),
    Cmdliner.Cmd.info "search" ~doc )

let cmd_undo =
  let doc = "Undo last operation" in
  let file_path =
    Arg.(
      value
      & opt (some string) None
      & info ~doc:"Markdown file path" [ "file"; "f" ])
  in

  ( Term.(
      const (fun file_path ->
          ignore (handle_undo ?path:file_path ()))
      $ file_path),
    Cmdliner.Cmd.info "undo" ~doc )

let cmd_redo =
  let doc = "Redo last undone operation" in
  let file_path =
    Arg.(
      value
      & opt (some string) None
      & info ~doc:"Markdown file path" [ "file"; "f" ])
  in

  ( Term.(
      const (fun file_path ->
          handle_redo ?path:file_path ())
      $ file_path),
    Cmdliner.Cmd.info "redo" ~doc )

let cmd_archive =
  let doc = "Archive completed tasks" in
  let file_path =
    Arg.(
      value
      & opt (some string) None
      & info ~doc:"Markdown file path" [ "file"; "f" ])
  in

  ( Term.(
      const (fun file_path ->
          handle_archive ?path:file_path ())
      $ file_path),
    Cmdliner.Cmd.info "archive" ~doc )

let cmd_interactive =
  let doc = "Run in interactive mode" in
  let file_path =
    Arg.(
      value
      & opt (some string) None
      & info ~doc:"Markdown file path" [ "file"; "f" ])
  in

  ( Term.(
      const (fun file_path ->
          handle_interactive ?path:file_path ())
      $ file_path),
    Cmdliner.Cmd.info "interactive" ~doc )

let cmd_help =
  let doc = "Show help" in
  (Term.(const handle_help $ const ()), Cmdliner.Cmd.info "help" ~doc)

let default_cmd =
  Cmdliner.Cmd.group
    (Cmdliner.Cmd.info "mark-task" ~version:"0.1.0" ~doc:"CLI Task Manager"
       ~man:
         [
           `S "DESCRIPTION";
           `P
             "Mark Task is a command-line task manager that stores tasks in \
              markdown files.";
           `P "Use $(b,mark-task help) to see available commands.";
         ])
    [
      Cmdliner.Cmd.v (snd cmd_add) (fst cmd_add);
      Cmdliner.Cmd.v (snd cmd_list) (fst cmd_list);
      Cmdliner.Cmd.v (snd cmd_edit) (fst cmd_edit);
      Cmdliner.Cmd.v (snd cmd_delete) (fst cmd_delete);
      Cmdliner.Cmd.v (snd cmd_mark) (fst cmd_mark);
      Cmdliner.Cmd.v (snd cmd_search) (fst cmd_search);
      Cmdliner.Cmd.v (snd cmd_undo) (fst cmd_undo);
      Cmdliner.Cmd.v (snd cmd_redo) (fst cmd_redo);
      Cmdliner.Cmd.v (snd cmd_archive) (fst cmd_archive);
      Cmdliner.Cmd.v (snd cmd_interactive) (fst cmd_interactive);
      Cmdliner.Cmd.v (snd cmd_help) (fst cmd_help);
    ]

let () = exit (Cmdliner.Cmd.eval default_cmd)
