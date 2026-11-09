(* History module - tracks changes for undo/redo *)

open Types

module type HISTORY = sig
  type t
  val create : int -> t
  val restore : max_size:int -> task list list -> task list list -> t
  val push : t -> task list -> t
  val undo : t -> (task list option * t)
  val redo : t -> (task list option * t)
  val can_undo : t -> bool
  val can_redo : t -> bool
  val size : t -> int
  val states : t -> task list list
  val redo_states : t -> task list list
end

module TaskHistory : HISTORY = struct
  type t = {
    mutable history : task list list;
    mutable redo_stack : task list list;
    max_size : int;
  }

  let validate max_size =
    if max_size < 0 || max_size = max_int then invalid_arg "invalid history size"

  let rec take n xs =
    if n <= 0 then [] else match xs with
    | [] -> []
    | x :: rest -> x :: take (n - 1) rest

  let create max_size =
    validate max_size;
    { history = []; redo_stack = []; max_size }

  let restore ~max_size history redo_stack =
    validate max_size;
    let history = take (max_size + 1) history in
    let redo_stack = take (max 0 (max_size + 1 - List.length history)) redo_stack in
    { history; redo_stack; max_size }

  let states t = t.history
  let redo_states t = t.redo_stack

  let push t state = 
    t.history <- take (t.max_size + 1) (state :: t.history);
    t.redo_stack <- [];
    t

  let undo t = 
    match t.history with
    | current::previous::rest ->
        t.redo_stack <- current :: t.redo_stack;
        t.history <- previous :: rest;
        Some previous, t
    | _ -> None, t

  let redo t = 
    match t.redo_stack with
    | next::rest ->
        t.history <- next :: t.history;
        t.redo_stack <- rest;
        Some next, t
    | _ -> None, t

  let can_undo t = List.length t.history > 1
  let can_redo t = t.redo_stack <> []
  let size t = List.length t.history
end

(* Convenience functions *)
let create_history ?(max_size=5) () = TaskHistory.create max_size

let record_change history tasks = TaskHistory.push history tasks

let undo last_history = 
  let state_opt, new_history = TaskHistory.undo last_history in
  state_opt, new_history

let redo last_history = 
  let state_opt, new_history = TaskHistory.redo last_history in
  state_opt, new_history
