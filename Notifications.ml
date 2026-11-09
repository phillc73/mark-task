(* Desktop notifications module *)

open Types

(* Send system notification using notify-send *)
let send_system_notification title message = 
  let escape s = String.concat "'\\''" (String.split_on_char '\'' s) in
  try
    Sys.command
      (Printf.sprintf "notify-send '%s' '%s'" (escape title) (escape message)) = 0
  with _ -> false

(* Send terminal notification *)
let send_terminal_notification title message = 
  Printf.printf "\027[35m[Notification] %s: %s\027[0m\n%!" title message;
  true

(* Send notification based on mode *)
let send_notification mode title message = 
  match mode with
  | System -> send_system_notification title message
  | Terminal -> send_terminal_notification title message
  | Both -> 
      let system_ok = send_system_notification title message in
      let terminal_ok = send_terminal_notification title message in
      system_ok && terminal_ok

let read_state path =
  if not (Sys.file_exists path) then []
  else
    match Yojson.Safe.from_file path with
    | `List entries -> entries
    | _ -> failwith "invalid notification state"

let write_state path entries =
  Atomic_write.write path (fun oc -> Yojson.Safe.to_channel oc (`List entries))

let check_due_tasks ?(now=Ptime_clock.now ()) ?(send=send_notification) ~path config tasks =
  let state_path = path ^ ".notifications" in
  try
    let entries = ref (read_state state_path) in
    let delivered = ref [] in
    List.iter (fun task ->
      if Task.is_complete task then begin
        let remaining = List.filter (function
          | `Assoc [("pending", `List (`String id :: _))] -> id <> task.id
          | _ -> true) !entries in
        if remaining <> !entries then begin
          write_state state_path remaining;
          entries := remaining
        end
      end else
        match task.due_date with
        | None -> ()
        | Some due ->
          match Ptime.of_rfc3339 due with
          | Error _ -> ()
          | Ok (timestamp, _, _) ->
            let seconds = Ptime.Span.to_float_s (Ptime.diff timestamp now) in
            let state = if seconds < 0. then Some "overdue"
              else if seconds <= 48. *. 3600. then Some "due-soon" else None in
            match state with
            | None -> ()
            | Some state ->
              let due_key = Task.format_timestamp timestamp in
              let key = `List [`String task.id; `String due_key; `String state] in
              let pending = `Assoc [("pending", key)] in
              let remaining = List.filter (function
                | `Assoc [("pending", `List [`String id; `String due; `String _])] ->
                  id <> task.id || due <> due_key
                | _ -> true) !entries in
              let channels = match config.notification_mode with
                | System -> [System, "system"]
                | Terminal -> [Terminal, "terminal"]
                | Both -> [System, "system"; Terminal, "terminal"] in
              let marker name = `Assoc [("delivered", key); ("channel", `String name)] in
              let is_delivered name = List.mem key !entries || List.mem (marker name) !entries in
              let updated = if List.mem key remaining ||
                  List.for_all (fun (_, name) -> List.mem (marker name) remaining) channels
                then List.filter ((<>) pending) remaining else pending :: List.filter ((<>) pending) remaining in
              if updated <> !entries then begin
                write_state state_path updated;
                entries := updated
              end;
              let sent = ref false in
              List.iter (fun (mode, name) ->
                if not (is_delivered name) then begin
                  let title = if state = "overdue" then "Mark Task: OVERDUE"
                    else "Mark Task: Due Soon" in
                  if send mode title (Printf.sprintf "Task: %s (Due: %s)" task.description due)
                  then begin
                    let updated = marker name :: !entries in
                    write_state state_path updated;
                    entries := updated;
                    sent := true
                  end
                end) channels;
              if List.for_all (fun (_, name) -> is_delivered name) channels &&
                  List.mem pending !entries then begin
                let updated = List.filter ((<>) pending) !entries in
                write_state state_path updated;
                entries := updated
              end;
              if !sent then delivered := task :: !delivered) tasks;
    Ok (List.rev !delivered)
  with exn -> Error ("Notification state: " ^ Printexc.to_string exn)

let periodic_check ?(config=default_config) ?(tasks=[]) ~path () =
  match check_due_tasks ~path config tasks with
  | Ok _ -> ()
  | Error error -> prerr_endline error
