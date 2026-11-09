(* Core types for Mark Task *)

(* Task Status *)
type status = 
  | Backlog
  | InProgress
  | OnHold
  | Complete
  | Idea

(* Priority levels *)
type priority = 
  | Low
  | Medium
  | High

(* Complexity ratings 1-5 *)
type complexity = 
  | C1
  | C2
  | C3
  | C4
  | C5

(* Effort ratings 1-5 *)
type effort = 
  | E1
  | E2
  | E3
  | E4
  | E5

(* Task record *)
type task = {
  id : string;  (* UUID *)
  description : string;
  category : string option;
  status : status;
  priority : priority;
  due_date : string option;  (* ISO 8601 datetime *)
  complexity : complexity;
  effort : effort;
  created_at : string;  (* ISO 8601 datetime *)
  updated_at : string;  (* ISO 8601 datetime *)
  completed_at : string option;  (* ISO 8601 datetime *)
  recurrence : string option;  (* cron-like syntax *)
}

(* Notification mode *)
type notification_mode = 
  | System
  | Terminal
  | Both

(* Performance mode *)
type performance_mode = 
  | All
  | Next24h
  | Next48h
  | Next96h

(* Conflict resolution *)
type conflict_resolution = 
  | KeepMemory
  | KeepFile
  | NoConflict

(* Color theme for UI *)
type color_theme = {
  status_backlog : string;
  status_in_progress : string;
  status_on_hold : string;
  status_complete : string;
  status_idea : string;
  priority_low : string;
  priority_medium : string;
  priority_high : string;
  due_overdue : string;
  due_soon : string;
  effort_1 : string;
  effort_2 : string;
  effort_3 : string;
  effort_4 : string;
  effort_5 : string;
  complexity_1 : string;
  complexity_2 : string;
  complexity_3 : string;
  complexity_4 : string;
  complexity_5 : string;
}

(* Configuration *)
type config = {
  markdown_location : string;
  color_theme : color_theme;
  notification_mode : notification_mode;
  performance_mode : performance_mode;
  max_history_size : int;
  auto_archive_days : int;
  aliases : (string * string) list;  (* alias name * command *)
}

(* Default configuration *)
let default_color_theme = {
  status_backlog = "gray";
  status_in_progress = "yellow";
  status_on_hold = "blue";
  status_complete = "green";
  status_idea = "cyan";
  priority_low = "green";
  priority_medium = "yellow";
  priority_high = "red";
  due_overdue = "red";
  due_soon = "magenta";
  effort_1 = "green";
  effort_2 = "blue";
  effort_3 = "yellow";
  effort_4 = "magenta";
  effort_5 = "red";
  complexity_1 = "green";
  complexity_2 = "blue";
  complexity_3 = "yellow";
  complexity_4 = "magenta";
  complexity_5 = "red";
}

let default_config = {
  markdown_location = "~/.local/share/mark-task/tasks.md";
  color_theme = default_color_theme;
  notification_mode = Terminal;
  performance_mode = All;
  max_history_size = 5;
  auto_archive_days = 3;
  aliases = [];
}
