(* Archive module - handles auto-archiving of completed tasks *)

open Types
open Markdown

let archive_path ?config_path () = 
  let config = Config.load ?config_path () in
  let base = Filename.dirname (Config.expand_tilde config.markdown_location) in
  Filename.concat base "archive.md"

let should_archive_task ?clock ?(days=3) task = 
  Task.should_archive ?clock ~days task

let archive_completed_tasks ?clock ?_path ?(days=3) tasks = 
  let archived, remaining = 
    List.partition (should_archive_task ?clock ~days) tasks
  in
  if archived <> [] then
    let archive_file = match _path with
      | Some path -> Filename.concat (Filename.dirname path) "archive.md"
      | None -> archive_path () in
    let existing_archive, rejections = parse_file_with_rejections archive_file in
    match rejections with
    | [] ->
        let all_archived = existing_archive @ archived in
        write_file archive_file all_archived;
        Ok remaining
    | warnings ->
        Error ("Archive not changed; correct rejected records in " ^ archive_file ^
               ": " ^ String.concat "; " warnings)
  else Ok tasks

let get_archived_tasks ?config_path () = 
  let path = archive_path ?config_path () in
  if Sys.file_exists path then
    parse_file path
  else []
