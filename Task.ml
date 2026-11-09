(* Task module *)

open Types

let format_timestamp timestamp =
  let text = Ptime.to_rfc3339 ~tz_offset_s:0 ~frac_s:12 timestamp in
  let rec last_digit index =
    if text.[index] = '0' then last_digit (index - 1) else index in
  let last = last_digit (String.length text - 2) in
  let last = if text.[last] = '.' then last - 1 else last in
  String.sub text 0 (last + 1) ^ "Z"

let now_str ?(clock=Unix.gettimeofday) () =
  let t = clock () in
  let date = Unix.gmtime t in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
    (date.tm_year + 1900) (date.tm_mon + 1) date.tm_mday
    date.tm_hour date.tm_min date.tm_sec

let create ~description ?(category = None) ?(status=Backlog) ?(priority=Low) 
    ?(due_date = None) ?(complexity=C1) ?(effort=E1) ?(recurrence = None) () = 
  let now = now_str () in
  {
    id = Uuidm.v4_gen (Random.State.make_self_init ()) () |> Uuidm.to_string;
    description;
    category;
    status;
    priority;
    due_date;
    complexity;
    effort;
    created_at = now;
    updated_at = now;
    completed_at = None;
    recurrence;
  }

let update ?description ?(category = None) ?status ?priority ?(due_date = None)
    ?complexity ?effort ?(recurrence = None) task = 
  let updated_at = now_str () in
  {
    task with
    description = Option.value description ~default:task.description;
    category = (match category with Some c -> Some c | None -> task.category);
    status = Option.value status ~default:task.status;
    priority = Option.value priority ~default:task.priority;
    due_date = (match due_date with Some d -> Some d | None -> task.due_date);
    complexity = Option.value complexity ~default:task.complexity;
    effort = Option.value effort ~default:task.effort;
    updated_at;
    recurrence = (match recurrence with Some r -> Some r | None -> task.recurrence);
  }

let mark_complete ?clock task = 
  let now = now_str ?clock () in
  { task with status = Complete; completed_at = Some now; updated_at = now }

let mark_status status task = 
  let now = now_str () in
  { task with status; updated_at = now }

(* Convert status to string *)
let status_to_string = function
  | Backlog -> "Backlog"
  | InProgress -> "In Progress"
  | OnHold -> "On Hold"
  | Complete -> "Complete"
  | Idea -> "Idea"

(* Convert priority to string *)
let priority_to_string = function
  | Low -> "Low"
  | Medium -> "Medium"
  | High -> "High"

(* Convert complexity to string *)
let complexity_to_string = function
  | C1 -> "1"
  | C2 -> "2"
  | C3 -> "3"
  | C4 -> "4"
  | C5 -> "5"

(* Convert effort to string *)
let effort_to_string = function
  | E1 -> "1"
  | E2 -> "2"
  | E3 -> "3"
  | E4 -> "4"
  | E5 -> "5"

let is_complete task = task.status = Complete

let is_overdue task = 
  match task.due_date with
  | Some due -> 
      begin
        try
          match Ptime.of_rfc3339 due, Ptime.of_rfc3339 (now_str ()) with
          | Ok (due_time, _, _), Ok (now_time, _, _) ->
              Ptime.is_earlier due_time ~than:now_time
          | _ -> false
        with _ -> false
      end
  | None -> false

let is_due_soon task = 
  match task.due_date with
  | Some due -> 
      begin
        try
          match Ptime.of_rfc3339 due, Ptime.of_rfc3339 (now_str ()) with
          | Ok (due_time, _, _), Ok (now_time, _, _) ->
              let span = Ptime.diff due_time now_time in
              let seconds = Ptime.Span.to_int_s span in
              let hours_until_due = Option.value ~default:0 seconds / 3600 in
              hours_until_due <= 48 && hours_until_due >= 0
          | _ -> false
        with _ -> false
      end
  | None -> false

let should_archive ?clock ?(days=3) task = 
  if task.status <> Complete then false else
  match task.completed_at with
  | Some completed_at -> 
      begin
        let invalid () =
          prerr_endline
            (Printf.sprintf "Warning: task %s: invalid Completed: %s"
               task.id completed_at);
          false
        in
        try
          match Ptime.of_rfc3339 completed_at, Ptime.of_rfc3339 (now_str ?clock ()) with
          | Ok (completed_time, _, _), Ok (now_time, _, _) ->
              let span = Ptime.diff now_time completed_time in
              let seconds = Ptime.Span.to_int_s span in
              let hours_since = Option.value ~default:0 seconds / 3600 in
              hours_since >= (days * 24)
          | Error _, _ -> invalid ()
          | _ -> false
        with _ -> invalid ()
      end
  | None -> false
