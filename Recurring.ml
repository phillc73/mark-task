(* Recurring tasks module *)

open Types
open Task

(* Substring check: String.contains only accepts a char *)
let contains_substring haystack needle =
  let len_h = String.length haystack in
  let len_n = String.length needle in
  if len_n = 0 then true
  else begin
    let rec check i =
      if i > len_h - len_n then false
      else if String.sub haystack i len_n = needle then true
      else check (i + 1)
    in
    check 0
  end

(* Parse cron-like recurrence patterns *)
type recurrence_pattern = 
  | Daily
  | Weekly of int list  (* Days of week: 0=Sunday, 1=Monday, etc. *)
  | Monthly of int list (* Days of month: 1-31 *)
  | Yearly of int * int (* Month * Day *)
  | EveryN of int * string (* Every N units (days, weeks, months, years) *)
  | Custom of string

(* Parse recurrence string into pattern *)
let parse_recurrence s = 
  let s_lower = String.lowercase_ascii (String.trim s) in
  if String.contains s_lower ',' then
    invalid_arg ("Invalid recurrence '" ^ s ^ "': multi-day schedules are not supported")
  else if contains_substring s_lower "weekly" && s_lower <> "weekly" then
    invalid_arg ("Invalid recurrence '" ^ s ^ "': ambiguous weekly schedule")
  else match s_lower with
  | "daily" | "every day" -> Daily
  | "weekly" -> Weekly [0; 1; 2; 3; 4; 5; 6]
  | "monthly" -> Monthly [1]
  | "yearly" | "annual" -> Yearly (1, 1)
  | "sun" | "sunday" -> Weekly [0]
  | "mon" | "monday" -> Weekly [1]
  | "tue" | "tuesday" -> Weekly [2]
  | "wed" | "wednesday" -> Weekly [3]
  | "thu" | "thursday" -> Weekly [4]
  | "fri" | "friday" -> Weekly [5]
  | "sat" | "saturday" -> Weekly [6]
  | _ ->
    match String.split_on_char ' ' s_lower with
    | ["every"; n; unit] ->
      (match int_of_string_opt n with
       | Some n when n > 0 &&
           List.mem unit ["day"; "days"; "week"; "weeks"; "month"; "months"; "year"; "years"] ->
         EveryN (n, unit)
       | _ -> invalid_arg ("Invalid recurrence '" ^ s ^ "': unsupported interval"))
    | _ -> invalid_arg ("Invalid recurrence '" ^ s ^ "': ambiguous or unsupported schedule")

(* Check if task should be regenerated today *)
let should_regenerate ?(today=Ptime_clock.now ()) task = 
  match task.recurrence with
  | Some pattern ->
      begin
        match parse_recurrence pattern with
        | Daily -> true
        | Weekly days ->
            (* Day of week: 0=Sunday. 1970-01-01 was a Thursday. *)
            let days_since_epoch =
              Option.value ~default:0
                (Ptime.Span.to_int_s (Ptime.diff today Ptime.epoch)) / 86400
            in
            List.mem ((4 + days_since_epoch) mod 7) days
        | Monthly days -> List.mem (Ptime.to_date today |> fun (_, _, d) -> d) days
        | Yearly (m, d) ->
            let (_, mon, day) = Ptime.to_date today in
            mon = m && day = d
        | EveryN (n, ("days" | "day")) ->
            (match Ptime.of_rfc3339 task.created_at with
             | Ok (created, _, _) ->
                 let diff_days =
                   Option.value ~default:0
                     (Ptime.Span.to_int_s (Ptime.diff today created)) / 86400
                 in
                 diff_days mod n = 0
             | Error _ -> false)
        | EveryN (n, ("weeks" | "week")) ->
            (match Ptime.of_rfc3339 task.created_at with
             | Ok (created, _, _) ->
                 let diff_days =
                   Option.value ~default:0
                     (Ptime.Span.to_int_s (Ptime.diff today created)) / 86400
                 in
                 diff_days mod (n * 7) = 0
             | Error _ -> false)
        | EveryN (n, ("months" | "month")) ->
            (* Approximate: check if n months have passed *)
            (match Ptime.of_rfc3339 task.created_at with
             | Ok (created, _, _) ->
                 let (y1, month1, _) = Ptime.to_date created in
                 let (y2, month2, _) = Ptime.to_date today in
                 ((y2 * 12 + month2) - (y1 * 12 + month1)) mod n = 0
             | Error _ -> false)
        | EveryN (n, ("years" | "year")) ->
            (match Ptime.of_rfc3339 task.created_at with
             | Ok (created, _, _) ->
                 let (y1, _, _) = Ptime.to_date created in
                 let (y2, _, _) = Ptime.to_date today in
                 (y2 - y1) mod n = 0
             | Error _ -> false)
        | EveryN (_, _) -> false
        | Custom _ -> false (* Cannot determine from custom pattern *)
      end
  | None -> false

let regenerate_if_needed ?today:_ task =
  let timestamp name value =
    match value with
    | Some value -> (match Ptime.of_rfc3339 value with
        | Ok (time, _, _) -> Some time
        | Error _ -> prerr_endline ("Recurring task " ^ task.id ^ ": invalid " ^ name); None)
    | None -> prerr_endline ("Recurring task " ^ task.id ^ ": missing " ^ name); None
  in
  if not (Task.is_complete task) || task.recurrence = None then None
  else
    let due = timestamp "due date" task.due_date in
    let completed = timestamp "completion date" task.completed_at in
    match due, completed, Option.map parse_recurrence task.recurrence with
    | Some due, Some completed, Some pattern ->
      let period = match pattern with
        | Daily -> Some (`Days 1)
        | Weekly _ -> Some (`Days 7)
        | Monthly _ -> Some (`Months 1)
        | Yearly _ -> Some (`Months 12)
        | EveryN (n, ("day" | "days")) when n > 0 -> Some (`Days n)
        | EveryN (n, ("week" | "weeks")) when n > 0 && n <= max_int / 7 -> Some (`Days (n * 7))
        | EveryN (n, ("month" | "months")) when n > 0 -> Some (`Months n)
        | EveryN (n, ("year" | "years")) when n > 0 && n <= max_int / 12 -> Some (`Months (n * 12))
        | _ -> None in
      let next = match period with
        | None -> None
        | Some (`Days days) ->
          let elapsed = Ptime.Span.to_float_s (Ptime.diff completed due) in
          let periods = if elapsed < 0. then 1 else int_of_float (floor (elapsed /. (float days *. 86400.))) + 1 in
          if days > max_int / 86400 || periods > max_int / (days * 86400) then None else
          Ptime.add_span due (Ptime.Span.of_int_s (periods * days * 86400))
        | Some (`Months months) ->
          let (year, month, day), time = Ptime.to_date_time due in
          let fraction = Ptime.frac_s due in
          let anchor = year * 12 + month - 1 in
          let rec seek index =
            if index > 9999 * 12 + 11 then None else
            let date = (index / 12, index mod 12 + 1, day) in
            let candidate = Option.bind (Ptime.of_date_time (date, time))
                (fun t -> Ptime.add_span t fraction) in
            match candidate with
            | Some candidate when Ptime.compare candidate due > 0 && Ptime.compare candidate completed > 0 -> Some candidate
            | _ -> if months > 9999 * 12 + 11 - index then None else seek (index + months)
          in
          if months > 9999 * 12 + 11 - anchor then None else seek (anchor + months)
      in
      (match next with
       | None -> None
       | Some next ->
         Some (create ~description:task.description ~category:task.category
           ~status:Backlog ~priority:task.priority ~due_date:(Some (Task.format_timestamp next))
           ~complexity:task.complexity ~effort:task.effort ~recurrence:task.recurrence ()))
    | _ -> None

let process_recurring_tasks ?today tasks =
  List.map (fun task -> match regenerate_if_needed ?today task with
    | Some next -> next
    | None -> task) tasks
