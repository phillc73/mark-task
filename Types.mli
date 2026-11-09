(* Core types for Mark Task *)

type status = Backlog | InProgress | OnHold | Complete | Idea
type priority = Low | Medium | High
type complexity = C1 | C2 | C3 | C4 | C5
type effort = E1 | E2 | E3 | E4 | E5

type task = {
  id : string;
  description : string;
  category : string option;
  status : status;
  priority : priority;
  due_date : string option;
  complexity : complexity;
  effort : effort;
  created_at : string;
  updated_at : string;
  completed_at : string option;
  recurrence : string option;
}

type notification_mode = System | Terminal | Both
type performance_mode = All | Next24h | Next48h | Next96h
type conflict_resolution = KeepMemory | KeepFile | NoConflict

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

type config = {
  markdown_location : string;
  color_theme : color_theme;
  notification_mode : notification_mode;
  performance_mode : performance_mode;
  max_history_size : int;
  auto_archive_days : int;
  aliases : (string * string) list;
}

val default_color_theme : color_theme
val default_config : config
