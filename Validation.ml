(* Validation module - input validation and error handling *)

open Types

(* Validation error type *)
type validation_error = 
  | EmptyDescription
  | InvalidStatus of string
  | InvalidPriority of string
  | InvalidComplexity of string
  | InvalidEffort of string
  | InvalidDueDate of string
  | TaskNotFound of string
  | FileNotFound of string
  | InvalidMarkdown of string
  | ConflictDetected
  | OtherError of string

(* Format validation error as string *)
let string_of_error = function
  | EmptyDescription -> "Description cannot be empty"
  | InvalidStatus s -> Printf.sprintf "Invalid status: %s" s
  | InvalidPriority s -> Printf.sprintf "Invalid priority: %s" s
  | InvalidComplexity s -> Printf.sprintf "Invalid complexity: %s" s
  | InvalidEffort s -> Printf.sprintf "Invalid effort: %s" s
  | InvalidDueDate s -> Printf.sprintf "Invalid due date: %s" s
  | TaskNotFound id -> Printf.sprintf "Task not found: %s" id
  | FileNotFound path -> Printf.sprintf "File not found: %s" path
  | InvalidMarkdown msg -> Printf.sprintf "Invalid markdown: %s" msg
  | ConflictDetected -> "File conflict detected - please resolve"
  | OtherError msg -> msg

(* Validate task creation *)
let validate_task description ?category ?status ?priority ?due_date ?complexity ?effort ?recurrence () = 
  let errors = ref [] in
  (match recurrence with
   | None -> ()
   | Some text ->
     (try match Recurring.parse_recurrence text with
      | Recurring.Custom _ -> errors := OtherError ("Invalid recurrence: " ^ text) :: !errors
      | _ -> ()
      with Invalid_argument message -> errors := OtherError message :: !errors));
  
  if description = "" then
    errors := EmptyDescription :: !errors;
  if String.contains description '\n' || String.contains description '\r' then
    errors := OtherError "Description cannot contain a newline" :: !errors;
  (* An empty optional attribute would be written as an empty field line,
     which the reader refuses; refuse it at the input boundary instead. *)
  (match category with
   | Some value when String.trim value = "" ->
       errors := OtherError "Category cannot be empty" :: !errors
   | _ -> ());
  
  let _ = match status with
    | Some s -> 
        let valid = 
          ["backlog"; "inprogress"; "in progress"; "onhold"; "on hold"; "complete"; "idea"] in
        if not (List.mem (String.lowercase_ascii s) valid) then
          errors := InvalidStatus s :: !errors
    | None -> ()
  in
  
  let _ = match priority with
    | Some p -> 
        if not (List.mem (String.lowercase_ascii p) ["low"; "medium"; "high"]) then
          errors := InvalidPriority p :: !errors
    | None -> ()
  in
  
  let _ = match complexity with
    | Some c -> 
        if not (List.mem c ["1"; "2"; "3"; "4"; "5"]) then
          errors := InvalidComplexity c :: !errors
    | None -> ()
  in
  
  let _ = match effort with
    | Some e -> 
        if not (List.mem e ["1"; "2"; "3"; "4"; "5"]) then
          errors := InvalidEffort e :: !errors
    | None -> ()
  in
  
  let _ = match due_date with
    | Some d -> 
        (match Ptime.of_rfc3339 d with
         | Ok _ -> ()
         | Error _ -> errors := InvalidDueDate d :: !errors)
    | None -> ()
  in
  
  match !errors with
  | [] -> Ok ()
  | hd::_ -> Error hd

(* Validate task exists in list *)
let validate_task_exists task_id tasks = 
  if List.exists (fun t -> t.id = task_id) tasks then
    Ok ()
  else
    Error (TaskNotFound task_id)

(* Validate file exists *)
let validate_file path = 
  if Sys.file_exists path then
    Ok ()
  else
    Error (FileNotFound path)

(* Validate markdown syntax *)
let validate_markdown path = 
  try
    let _ = Markdown.parse_file path in
    Ok ()
  with exn ->
    Error (InvalidMarkdown (Printexc.to_string exn))

(* Check for conflicts between memory and file *)
let check_conflict memory_tasks file_tasks = 
  if memory_tasks = file_tasks then
    Ok ()
  else
    Error ConflictDetected
