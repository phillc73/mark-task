(* View module - task display and filtering *)

open Types
open Ui

module type VIEW = sig
  type t
  val create : config -> t
  val display : t -> task list -> string
  val display_task : t -> task -> string
  val display_by_category : t -> task list -> string
  val display_by_status : t -> task list -> string
  val display_by_priority : t -> task list -> string
  val display_overdue : t -> task list -> string
  val display_due_soon : t -> task list -> string
end

module TaskView : VIEW = struct
  type t = {
    config : config;
  }

  let create config = { config }

  let display t tasks = 
    format_task_list t.config.color_theme tasks

  let display_task t task = 
    format_task t.config.color_theme task

  let display_by_category t tasks = 
    let grouped = 
      List.fold_left (fun acc task ->
        match task.category with
        | Some cat -> 
            let existing = List.assoc_opt cat acc |> Option.value ~default:[] in
            (cat, task::existing)::(List.remove_assoc cat acc)
        | None -> acc
      ) [] tasks
    in
    let formatted = List.map (fun (cat, cat_tasks) ->
      Printf.sprintf "\n=== Category: %s ===\n%s"
        (colorize "cyan" cat)
        (format_task_list t.config.color_theme cat_tasks)
    ) grouped
    in
    String.concat "\n" formatted

  let display_by_status t tasks = 
    let all_statuses = [Backlog; InProgress; OnHold; Complete; Idea] in
    let formatted = List.map (fun status ->
      let status_tasks = List.filter (fun task -> task.status = status) tasks in
      if status_tasks <> [] then
        Printf.sprintf "\n=== %s ===\n%s"
          (format_status t.config.color_theme status)
          (format_task_list t.config.color_theme status_tasks)
      else ""
    ) all_statuses
    in
    String.concat "\n" formatted |> String.trim

  let display_by_priority t tasks = 
    let all_priorities = [High; Medium; Low] in
    let formatted = List.map (fun priority ->
      let priority_tasks = List.filter (fun task -> task.priority = priority) tasks in
      if priority_tasks <> [] then
        Printf.sprintf "\n=== %s ===\n%s"
          (format_priority t.config.color_theme priority)
          (format_task_list t.config.color_theme priority_tasks)
      else ""
    ) all_priorities
    in
    String.concat "\n" formatted |> String.trim

  let display_overdue t tasks = 
    let overdue = List.filter Task.is_overdue tasks in
    if overdue = [] then "No overdue tasks"
    else
      Printf.sprintf "=== %s ===\n%s"
        (colorize "red" "OVERDUE TASKS")
        (format_task_list t.config.color_theme overdue)

  let display_due_soon t tasks = 
    let due_soon = List.filter Task.is_due_soon tasks in
    if due_soon = [] then "No tasks due soon"
    else
      Printf.sprintf "=== %s ===\n%s"
        (colorize "magenta" "TASKS DUE SOON")
        (format_task_list t.config.color_theme due_soon)
end

(* Convenience functions *)
let create_view ?(config=default_config) () = 
  TaskView.create config

let display_tasks ?config tasks = 
  let view = create_view ?config () in
  TaskView.display view tasks

let display_task_details ?config task = 
  let view = create_view ?config () in
  TaskView.display_task view task
