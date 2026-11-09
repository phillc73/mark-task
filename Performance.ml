(* Performance module - task loading optimisation *)

open Types

(* Keep tasks due within [hours] of now, plus tasks without a due date *)
let filter_due_within hours tasks =
  let limit =
    match Ptime.add_span (Ptime_clock.now ()) (Ptime.Span.of_int_s (hours * 3600)) with
    | Some limit -> limit
    | None -> Ptime_clock.now ()
  in
  List.filter (fun task ->
    match task.due_date with
    | Some due_date ->
        (match Ptime.of_rfc3339 due_date with
         | Ok (due, _, _) -> Ptime.is_earlier due ~than:limit
         | Error _ -> true (* If date parsing fails, include it *))
    | None -> true (* No due date, include *)
  ) tasks

(* Filter tasks by time range *)
let filter_by_time_range mode tasks = 
  match mode with
  | All -> tasks
  | Next24h -> filter_due_within 24 tasks
  | Next48h -> filter_due_within 48 tasks
  | Next96h -> filter_due_within 96 tasks

(* Get performance mode from config *)
let get_mode config = config.performance_mode

(* Load tasks with performance filtering *)
let load_filtered config tasks = 
  filter_by_time_range config.performance_mode tasks
