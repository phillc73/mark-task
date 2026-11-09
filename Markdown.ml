(* Markdown parser/serializer for tasks *)

open Types

(* Parse ISO 8601 datetime string *)
let parse_datetime s =
  try
    match Ptime.of_rfc3339 s with
    | Ok (t, _, _) -> Some t
    | Error _ -> None
  with _ -> None

(* Format ISO 8601 datetime *)
let format_datetime t = Ptime.to_rfc3339 t

(* Parse status from string *)
let status_of_string s = match String.lowercase_ascii s with
  | "backlog" -> Some Backlog
  | "in progress" | "inprogress" -> Some InProgress
  | "on hold" | "onhold" -> Some OnHold
  | "complete" -> Some Complete
  | "idea" -> Some Idea
  | _ -> None

(* Parse priority from string *)
let priority_of_string s = match String.lowercase_ascii s with
  | "low" -> Some Low
  | "medium" -> Some Medium
  | "high" -> Some High
  | _ -> None

(* Parse complexity from string *)
let complexity_of_string = function
  | "1" -> Some C1
  | "2" -> Some C2
  | "3" -> Some C3
  | "4" -> Some C4
  | "5" -> Some C5
  | _ -> None

(* Parse effort from string *)
let effort_of_string = function
  | "1" -> Some E1
  | "2" -> Some E2
  | "3" -> Some E3
  | "4" -> Some E4
  | "5" -> Some E5
  | _ -> None

(* Status to string *)
let status_to_string = function
  | Backlog -> "Backlog"
  | InProgress -> "In Progress"
  | OnHold -> "On Hold"
  | Complete -> "Complete"
  | Idea -> "Idea"

(* Priority to string *)
let priority_to_string = function
  | Low -> "Low"
  | Medium -> "Medium"
  | High -> "High"

(* Complexity to string *)
let complexity_to_string = function
  | C1 -> "1"
  | C2 -> "2"
  | C3 -> "3"
  | C4 -> "4"
  | C5 -> "5"

(* Effort to string *)
let effort_to_string = function
  | E1 -> "1"
  | E2 -> "2"
  | E3 -> "3"
  | E4 -> "4"
  | E5 -> "5"

(* Attribute table shared by the reader and the rejection scanner, so the
   accepted prefixes and the validated fields cannot drift apart. *)
type field_spec = {
  label : string;
  key : string;
  marker : string;
  prefix : string;
  valid : string -> bool;
  required : bool;
}

let content_of_bullet line =
  let line = String.trim line in
  if String.length line >= 2 &&
     (line.[0] = '-' || line.[0] = '*' || line.[0] = '+') &&
     (line.[1] = ' ' || line.[1] = '\t') then
    Some (String.trim (String.sub line 2 (String.length line - 2)))
  else None

let strip_list_marker line =
  match content_of_bullet line with
  | Some content -> content
  | None -> String.trim line

let backtick_wrapped value =
  String.length value >= 2 && String.get value 0 = '`' &&
  String.get value (String.length value - 1) = '`'

let strip_backticks value =
  if backtick_wrapped value then
    String.sub value 1 (String.length value - 2)
  else value

(* The reader's stored form: surrounding whitespace and a backtick wrapper are
   Markdown decoration, not data. The writer and the input paths apply the
   same rule, so a stored value is a fixpoint of save/load. *)
let normalise_value value = String.trim (strip_backticks (String.trim value))

let value_after_prefix prefix line =
  let rest = String.sub line (String.length prefix)
      (String.length line - String.length prefix) in
  normalise_value rest

let value_after_marker marker line =
  let rest = String.trim (String.sub line (String.length marker)
      (String.length line - String.length marker)) in
  let rest =
    if String.starts_with ~prefix:":" rest then
      String.trim (String.sub rest 1 (String.length rest - 1))
    else rest
  in
  normalise_value rest

let non_empty value = value <> ""

let valid_recurrence value =
  try ignore (Recurring.parse_recurrence value); true
  with Invalid_argument _ -> false

let field_specs = [
  { label = "ID"; key = "id"; marker = "**ID**"; prefix = "**ID**: ";
    valid = non_empty; required = true };
  { label = "Description"; key = "description"; marker = "**Description**";
    prefix = "**Description**: "; valid = non_empty; required = true };
  { label = "Category"; key = "category"; marker = "**Category**";
    prefix = "**Category**: "; valid = non_empty; required = false };
  { label = "Status"; key = "status"; marker = "**Status**";
    prefix = "**Status**: ";
    valid = (fun value -> Option.is_some (status_of_string value));
    required = false };
  { label = "Priority"; key = "priority"; marker = "**Priority**";
    prefix = "**Priority**: ";
    valid = (fun value -> Option.is_some (priority_of_string value));
    required = false };
  { label = "Due Date"; key = "due_date"; marker = "**Due Date**";
    prefix = "**Due Date**: ";
    valid = (fun value -> Option.is_some (parse_datetime value));
    required = false };
  { label = "Complexity"; key = "complexity"; marker = "**Complexity**";
    prefix = "**Complexity**: ";
    valid = (fun value -> Option.is_some (complexity_of_string value));
    required = false };
  { label = "Effort"; key = "effort"; marker = "**Effort**";
    prefix = "**Effort**: ";
    valid = (fun value -> Option.is_some (effort_of_string value));
    required = false };
  { label = "Created"; key = "created_at"; marker = "**Created**";
    prefix = "**Created**: ";
    valid = (fun value -> Option.is_some (parse_datetime value));
    required = true };
  { label = "Updated"; key = "updated_at"; marker = "**Updated**";
    prefix = "**Updated**: ";
    valid = (fun value -> Option.is_some (parse_datetime value));
    required = true };
  { label = "Completed"; key = "completed_at"; marker = "**Completed**";
    prefix = "**Completed**: ";
    valid = (fun value -> Option.is_some (parse_datetime value));
    required = false };
  { label = "Recurrence"; key = "recurrence"; marker = "**Recurrence**";
    prefix = "**Recurrence**: "; valid = valid_recurrence; required = false };
]

let parse_task _id lines =
  let attrs = List.fold_left (fun acc line ->
    let line = strip_list_marker line in
    match List.find_opt
            (fun spec -> String.starts_with ~prefix:spec.prefix line) field_specs with
    | Some spec -> (spec.key, value_after_prefix spec.prefix line) :: acc
    | None -> acc
  ) [] lines
  in
  let get key default = List.assoc_opt key attrs |> Option.value ~default in
  let get_opt key = List.assoc_opt key attrs in
  {
    id = get "id" "";
    description = get "description" "";
    category = get_opt "category";
    status = get "status" "Backlog" |> status_of_string |> Option.value ~default:Backlog;
    priority = get "priority" "Low" |> priority_of_string |> Option.value ~default:Low;
    due_date = get_opt "due_date";
    complexity = get "complexity" "1" |> complexity_of_string |> Option.value ~default:C1;
    effort = get "effort" "1" |> effort_of_string |> Option.value ~default:E1;
    created_at = get "created_at" "";
    updated_at = get "updated_at" "";
    completed_at = get_opt "completed_at";
    recurrence = get_opt "recurrence";
  }

let classify_record_line raw =
  let text = String.trim raw in
  if text = "" then `Blank
  else
    match content_of_bullet text with
    | None -> `Unrecognised text
    | Some content ->
        match List.find_opt
                (fun spec -> String.starts_with ~prefix:spec.prefix content)
                field_specs with
        | Some spec -> `Field (spec, true, value_after_prefix spec.prefix content)
        | None ->
            match List.find_opt
                    (fun spec -> String.starts_with ~prefix:spec.marker content)
                    field_specs with
            | Some spec -> `Field (spec, false, value_after_marker spec.marker content)
            | None -> `Unrecognised text

let block_defects task_header header_number lines =
  let line_defects = List.filter_map (fun (number, raw) ->
    match classify_record_line raw with
    | `Blank -> None
    | `Unrecognised text ->
        Some (Printf.sprintf "Task %s, line %d: unrecognised line: %s"
                task_header number text)
    | `Field (spec, canonical, value) ->
        if canonical && spec.valid value then None
        else Some (Printf.sprintf "Task %s, line %d: invalid %s: %s"
                     task_header number spec.label value)
  ) lines in
  let present = List.filter_map (fun (_, raw) ->
    match classify_record_line raw with
    | `Field (spec, _, _) -> Some spec.key
    | _ -> None) lines in
  (* The header label is not a display title: the ID is the record's only
     identity. A differing label would be replaced by the writer on the
     next save, so it is refused instead. *)
  let header_mismatch =
    if not (List.mem "id" present) then []
    else
      let id = (parse_task task_header (List.map snd lines)).id in
      if id = "" || id = task_header then []
      else [Printf.sprintf "Task %s, line %d: header label does not match ID: %s"
              task_header header_number id]
  in
  let missing = List.filter_map (fun spec ->
    if spec.required && not (List.mem spec.key present) then
      Some (Printf.sprintf "Task %s, line %d: missing required field %s"
              task_header header_number spec.label)
    else None) field_specs in
  line_defects @ missing @ header_mismatch

(* Normalisations are printed as notes, not rejections: the record loads and
   the file stays writable, but the stored value is not the raw text. The ID
   line is the writer's canonical backtick decoration, so it is not noted. *)
let block_normalisations task_header lines =
  List.filter_map (fun (number, raw) ->
    let text = String.trim raw in
    match content_of_bullet text with
    | None -> None
    | Some content ->
        match List.find_opt
                (fun spec -> String.starts_with ~prefix:spec.prefix content)
                field_specs with
        | None -> None
        | Some spec ->
            let value = String.sub content (String.length spec.prefix)
                (String.length content - String.length spec.prefix) in
            if spec.key <> "id" && backtick_wrapped (String.trim value) then
              Some (Printf.sprintf
                      "Task %s, line %d: stripped backticks from %s"
                      task_header number spec.label)
            else None
  ) lines

let utf8_bom = "\xEF\xBB\xBF"

let strip_leading_bom doc =
  if String.starts_with ~prefix:utf8_bom doc then
    Some (String.sub doc 3 (String.length doc - 3))
  else None

let document_headings = [ "# Mark Task"; "## Tasks" ]

let is_document_heading line =
  List.mem (String.trim line) document_headings

let record_header line =
  let line = String.trim line in
  if String.starts_with ~prefix:"### " line then
    let label = String.trim (String.sub line 4 (String.length line - 4)) in
    if label = "" then None else Some label
  else None

type parse_result = {
  tasks : task list;
  rejections : string list;
  normalisations : string list;
}

let parse_document_result doc =
  let doc, bom_note =
    match strip_leading_bom doc with
    | Some stripped -> stripped, Some "stripped UTF-8 BOM at file start"
    | None -> doc, None
  in
  let lines = String.split_on_char '\n' doc |> List.mapi (fun i s -> (i + 1, s)) in
  let rec collect_block acc = function
    | [] -> List.rev acc, []
    | (_, raw) as numbered :: rest ->
        if record_header raw <> None || is_document_heading raw then
          List.rev acc, numbered :: rest
        else collect_block (numbered :: acc) rest
  in
  let rec collect tasks rejections normalisations = function
    | [] -> List.rev tasks, List.rev rejections, List.rev normalisations
    | (number, raw) :: rest ->
        let text = String.trim raw in
        if text = "" || is_document_heading text then
          collect tasks rejections normalisations rest
        else match record_header text with
          | Some task_header ->
              let block, remaining = collect_block [] rest in
              let defects = block_defects task_header number block in
              if defects = [] then
                let task = parse_task task_header (List.map snd block) in
                let notes = block_normalisations task_header block in
                collect (task :: tasks) rejections
                  (List.rev_append notes normalisations) remaining
              else collect tasks (List.rev_append defects rejections)
                     normalisations remaining
          | None ->
              let warning = Printf.sprintf "line %d: unrecognised line: %s" number text in
              collect tasks (warning :: rejections) normalisations rest
  in
  let tasks, rejections, normalisations = collect [] [] [] lines in
  { tasks; rejections;
    normalisations =
      (match bom_note with
       | Some note -> note :: normalisations
       | None -> normalisations) }

let parse_document_with_rejections doc =
  let result = parse_document_result doc in
  result.tasks, result.rejections

let parse_document doc = (parse_document_result doc).tasks

let parse_file_result path =
  if Sys.file_exists path then
    In_channel.with_open_text path In_channel.input_all |> parse_document_result
  else { tasks = []; rejections = []; normalisations = [] }

let parse_file_with_rejections path =
  let result = parse_file_result path in
  result.tasks, result.rejections

let parse_file path = (parse_file_result path).tasks

(* Format task to markdown. The writer emits the reader's stored form and
   refuses a description the reader would refuse, so the tool never writes a
   file it then treats as invalid. *)
let format_task task =
  let description = normalise_value task.description in
  if description = "" then invalid_arg "Description cannot be empty";
  if String.contains description '\n' || String.contains description '\r' then
    invalid_arg "Description cannot contain a newline";
  let category =
    match Option.map normalise_value task.category with
    | Some "" -> None
    | other -> other
  in
  let lines = [
    Printf.sprintf "### %s" task.id;
    Printf.sprintf "- **ID**: `%s`" task.id;
    Printf.sprintf "- **Description**: %s" description;
    (match category with Some c -> Printf.sprintf "- **Category**: %s" c | None -> "");
    Printf.sprintf "- **Status**: %s" (status_to_string task.status);
    Printf.sprintf "- **Priority**: %s" (priority_to_string task.priority);
    (match task.due_date with Some d -> Printf.sprintf "- **Due Date**: %s" d | None -> "");
    Printf.sprintf "- **Complexity**: %s" (complexity_to_string task.complexity);
    Printf.sprintf "- **Effort**: %s" (effort_to_string task.effort);
    Printf.sprintf "- **Created**: %s" task.created_at;
    Printf.sprintf "- **Updated**: %s" task.updated_at;
    (match task.completed_at with Some c -> Printf.sprintf "- **Completed**: %s" c | None -> "");
    (match task.recurrence with Some r -> Printf.sprintf "- **Recurrence**: %s" r | None -> "");
  ] in
  let non_empty = List.filter (fun s -> String.length s > 0) lines in
  String.concat "\n" non_empty

(* Format all tasks to markdown document *)
let format_document tasks =
  let header = "# Mark Task\n\n## Tasks\n" in
  let task_strings = List.map format_task tasks in
  header ^ String.concat "\n\n" task_strings

(* Write markdown file *)
let write_file path tasks =
  let content = format_document tasks in
  let dir = Filename.dirname path in
  if not (Sys.file_exists dir) then (
    let rec mkdir_p p perm =
      try Unix.mkdir p perm with
      | Unix.Unix_error (Unix.ENOENT, _, _) ->
          mkdir_p (Filename.dirname p) perm;
          Unix.mkdir p perm
      | e -> raise e
    in
    mkdir_p dir 0o755
  );
  Atomic_write.write path (fun oc -> Out_channel.output_string oc content)
