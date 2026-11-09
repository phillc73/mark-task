(* Tests for Mark Task *)

open Types
open Task
open Config
open Markdown
open Validation
open Task_manager

module Task_tests = struct
  open Alcotest

  let test_create () = 
    let task = create ~description:"Test task" () in
    check bool "id not empty" (String.length task.id > 0) true;
    check string "description" "Test task" task.description;
    check (option string) "category" None task.category;
    check bool "status is Backlog" (task.status = Backlog) true;
    check bool "priority is Low" (task.priority = Low) true;
    check bool "complexity is C1" (task.complexity = C1) true;
    check bool "effort is E1" (task.effort = E1) true

  let test_create_with_attrs () = 
    let task = create ~description:"Test" ~category:(Some "Work") ~status:InProgress 
        ~priority:High ~due_date:(Some "2023-12-31T00:00:00Z") ~complexity:C3 ~effort:E4 () in
    check (option string) "category" (Some "Work") task.category;
    check bool "status" (task.status = InProgress) true;
    check bool "priority" (task.priority = High) true;
    check (option string) "due_date" (Some "2023-12-31T00:00:00Z") task.due_date;
    check bool "complexity" (task.complexity = C3) true;
    check bool "effort" (task.effort = E4) true

  let test_mark_complete () = 
    let task = create ~description:"Test" () in
    let completed = mark_complete task in
    check bool "is complete" (is_complete completed) true;
    check bool "status" (completed.status = Complete) true;
    check bool "completed_at set" (completed.completed_at <> None) true

  let test_due_dates () =
    let past = create ~description:"Past" ~due_date:(Some "2020-01-01T00:00:00Z") () in
    let future = create ~description:"Future" ~due_date:(Some "2100-01-01T00:00:00Z") () in
    check bool "past due is overdue" (is_overdue past) true;
    check bool "future due not overdue" (is_overdue future) false;
    check bool "far future not due soon" (is_due_soon future) false

  let tests = [
    "create", `Quick, test_create;
    "create_with_attrs", `Quick, test_create_with_attrs;
    "mark_complete", `Quick, test_mark_complete;
    "due_dates", `Quick, test_due_dates;
  ]
end

module Config_tests = struct
  open Alcotest

  let test_default_config () = 
    let config = default_config in
    check string "markdown_location" "~/.local/share/mark-task/tasks.md" config.markdown_location;
    check int "max_history_size" 5 config.max_history_size;
    check int "auto_archive_days" 3 config.auto_archive_days

  let test_load_save () = 
    let config = 
      { default_config with max_history_size = 10; aliases = [("ls", "list")] } in
    let path = Filename.temp_file "mark-task-test" ".yaml" in
    save config ~config_path:path ();
    let loaded = load ~config_path:path () in
    check int "saved and loaded" 10 loaded.max_history_size;
    check bool "aliases roundtrip" (loaded.aliases = [("ls", "list")]) true;
    Sys.remove path

  let contains haystack needle =
    let len = String.length needle in
    let rec loop i =
      i + len <= String.length haystack &&
      (String.sub haystack i len = needle || loop (i + 1)) in
    loop 0

  let capture_stderr f =
    let path = Filename.temp_file "mark-task-stderr" ".txt" in
    let fd = Unix.openfile path [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600 in
    let saved = Unix.dup Unix.stderr in
    Unix.dup2 fd Unix.stderr;
    Unix.close fd;
    let result =
      Fun.protect
        ~finally:(fun () -> Unix.dup2 saved Unix.stderr; Unix.close saved)
        f in
    let output = In_channel.with_open_bin path In_channel.input_all in
    Sys.remove path;
    result, output

  let load_captured json =
    let path = Filename.temp_file "mark-task-config" ".json" in
    Out_channel.with_open_bin path (fun oc -> Out_channel.output_string oc json);
    let config, output = capture_stderr (fun () -> load ~config_path:path ()) in
    Sys.remove path;
    config, output

  let test_partial_config_applies_each_key () =
    let cases = [
      ("{\"performance_mode\":\"next24h\"}",
       (fun config -> check bool "performance_mode applies" true
           (config.performance_mode = Next24h)));
      ("{\"markdown_location\":\"custom.md\"}",
       (fun config -> check string "markdown_location applies" "custom.md"
           config.markdown_location));
      ("{\"notification_mode\":\"system\"}",
       (fun config -> check bool "notification_mode applies" true
           (config.notification_mode = System)));
      ("{\"max_history_size\":9}",
       (fun config -> check int "max_history_size applies" 9 config.max_history_size));
      ("{\"auto_archive_days\":7}",
       (fun config -> check int "auto_archive_days applies" 7 config.auto_archive_days));
      ("{\"aliases\":{\"ls\":\"list\"}}",
       (fun config -> check bool "aliases apply" true
           (config.aliases = [("ls", "list")])));
      ("{\"color_theme\":{\"status_backlog\":\"red\"}}",
       (fun config ->
         check string "theme value applies" "red" config.color_theme.status_backlog;
         check string "other theme keys default" "yellow"
           config.color_theme.status_in_progress));
    ] in
    List.iter (fun (json, check_case) ->
      let config, output = load_captured json in
      check_case config;
      check string "partial config warns nothing" "" output) cases

  let test_color_theme_variants () =
    let valid, valid_output = load_captured
        "{\"performance_mode\":\"next24h\",\"color_theme\":{}}" in
    check bool "valid empty theme keeps other keys" true
      (valid.performance_mode = Next24h);
    check string "valid empty theme warns nothing" "" valid_output;
    let null_theme, null_output = load_captured
        "{\"performance_mode\":\"next24h\",\"color_theme\":null}" in
    check bool "null theme keeps other keys" true
      (null_theme.performance_mode = Next24h);
    check bool "null theme falls back to defaults" true
      (null_theme.color_theme = default_config.color_theme);
    check bool "null theme warns" true (contains null_output "color_theme");
    let bad_theme, bad_output = load_captured
        "{\"performance_mode\":\"next24h\",\"color_theme\":\"red\"}" in
    check bool "non-object theme keeps other keys" true
      (bad_theme.performance_mode = Next24h);
    check bool "non-object theme warns" true (contains bad_output "color_theme");
    let bad_value, bad_value_output = load_captured
        "{\"performance_mode\":\"next24h\",\"color_theme\":{\"status_backlog\":5}}" in
    check bool "bad theme value keeps other keys" true
      (bad_value.performance_mode = Next24h);
    check string "bad theme value falls back for that key" "gray"
      bad_value.color_theme.status_backlog;
    check bool "bad theme value warns with the key" true
      (contains bad_value_output "status_backlog")

  let test_malformed_and_wrong_typed_config () =
    let malformed, malformed_output = load_captured "{not json" in
    check bool "malformed config falls back to defaults" true
      (malformed = default_config);
    check bool "malformed config warns" true (contains malformed_output "invalid JSON");
    let wrong, wrong_output = load_captured
        "{\"performance_mode\":\"next24h\",\"max_history_size\":\"ten\"}" in
    check bool "wrong-typed key keeps other keys" true
      (wrong.performance_mode = Next24h);
    check int "wrong-typed key falls back for that key" 5 wrong.max_history_size;
    check bool "wrong-typed key warns with the key" true
      (contains wrong_output "max_history_size")

  let int_value config key = match key with
    | "max_history_size" -> config.max_history_size
    | "auto_archive_days" -> config.auto_archive_days
    | _ -> fail ("unknown integer setting " ^ key)

  let test_int_bounds_accept_edges () =
    List.iter (fun (key, value, expected) ->
      let json = Printf.sprintf "{\"performance_mode\":\"next24h\",\"%s\":%d}" key value in
      let config, output = load_captured json in
      check int (Printf.sprintf "%s accepts %d" key value) expected (int_value config key);
      check bool "valid bound keeps other keys" true (config.performance_mode = Next24h);
      check string "valid bound warns nothing" "" output)
      [ "max_history_size", 0, 0;
        "max_history_size", 1000, 1000;
        "auto_archive_days", 0, 0;
        "auto_archive_days", 36500, 36500 ]

  let test_int_bounds_reject_out_of_range () =
    let cases = [
      "max_history_size", "-1", -1, 5, "0..1000";
      "max_history_size", "1001", 1001, 5, "0..1000";
      "max_history_size", "-4611686018427387904", min_int, 5, "0..1000";
      "max_history_size", "4611686018427387903", max_int, 5, "0..1000";
      "max_history_size", "1000000000000000000", 1000000000000000000, 5, "0..1000";
      "auto_archive_days", "-1", -1, 3, "0..36500";
      "auto_archive_days", "36501", 36501, 3, "0..36500";
      "auto_archive_days", "-4611686018427387904", min_int, 3, "0..36500";
      "auto_archive_days", "4611686018427387903", max_int, 3, "0..36500";
      "auto_archive_days", "1000000000000000000", 1000000000000000000, 3, "0..36500";
    ] in
    List.iter (fun (key, literal, value, default, range) ->
      let json = Printf.sprintf "{\"performance_mode\":\"next24h\",\"%s\":%s}" key literal in
      let config, output = load_captured json in
      check int (key ^ " out-of-range " ^ literal ^ " falls back") default (int_value config key);
      check bool (key ^ " warning names the key") true (contains output key);
      check bool (key ^ " warning names the value") true (contains output (string_of_int value));
      check bool (key ^ " warning names the range") true (contains output range);
      check bool (key ^ " keeps other keys") true (config.performance_mode = Next24h))
      cases

  let test_int_wrong_types_fall_back () =
    List.iter (fun (key, literal, default, value_text, range) ->
      let json = Printf.sprintf "{\"performance_mode\":\"next24h\",\"%s\":%s}" key literal in
      let config, output = load_captured json in
      check int (key ^ " wrong type " ^ literal ^ " falls back") default (int_value config key);
      check bool (key ^ " wrong type warns") true (contains output key);
      check bool (key ^ " wrong type names the value") true (contains output value_text);
      check bool (key ^ " wrong type names the range") true (contains output range);
      check bool (key ^ " wrong type keeps other keys") true (config.performance_mode = Next24h))
      [ "max_history_size", "\"ten\"", 5, "ten", "0..1000";
        "max_history_size", "1.5", 5, "1.5", "0..1000";
        "max_history_size", "-0.0", 5, "-0.0", "0..1000";
        "auto_archive_days", "\"many\"", 3, "many", "0..36500";
        "auto_archive_days", "2.5", 3, "2.5", "0..36500";
        "auto_archive_days", "-0.0", 3, "-0.0", "0..36500" ]

  let test_invalid_archive_days_uses_default_threshold () =
    let config, output = load_captured "{\"auto_archive_days\":1000000000000000000}" in
    check int "overflow value falls back to the default" 3 config.auto_archive_days;
    check bool "overflow value warns" true (contains output "auto_archive_days");
    let task = Task.create ~description:"today" () in
    let completed = { task with status = Complete; completed_at = Some task.created_at } in
    let now = match Ptime.of_rfc3339 task.created_at with
      | Ok (instant, _, _) -> Ptime.to_float_s instant
      | Error _ -> fail "invalid fixture timestamp" in
    check bool "same-day completion stays at the default threshold" false
      (Task.should_archive ~days:config.auto_archive_days ~clock:(fun () -> now) completed);
    check bool "zero threshold archives every complete task" true
      (Task.should_archive ~days:0 ~clock:(fun () -> now) completed)

  let test_int_bounds_table_is_single_source () =
    let configured = function
      | Max_history_size -> default_config.max_history_size
      | Auto_archive_days -> default_config.auto_archive_days in
    List.iter (fun setting ->
      let bounds = int_bounds setting in
      check bool "min not above default" true (bounds.min_value <= bounds.default_value);
      check bool "default not above max" true (bounds.default_value <= bounds.max_value);
      check int "default matches default_config" (configured setting) bounds.default_value;
      check bool "rationale recorded" true (String.length bounds.rationale > 0);
      let by_key = bounds_for_key (int_setting_key setting) in
      check bool "key resolves back to the setting" true (by_key = bounds))
      all_int_settings;
    check_raises "unbounded key is a development error"
      (Invalid_argument "config: integer setting without bounds: not_a_setting")
      (fun () -> ignore (bounds_for_key "not_a_setting"))

  let tests = [
    "int_bounds_table_is_single_source", `Quick, test_int_bounds_table_is_single_source;
    "int_bounds_accept_edges", `Quick, test_int_bounds_accept_edges;
    "int_bounds_reject_out_of_range", `Quick, test_int_bounds_reject_out_of_range;
    "int_wrong_types_fall_back", `Quick, test_int_wrong_types_fall_back;
    "invalid_archive_days_uses_default_threshold", `Quick, test_invalid_archive_days_uses_default_threshold;
    "default_config", `Quick, test_default_config;
    "load_save", `Quick, test_load_save;
    "partial_config_applies_each_key", `Quick, test_partial_config_applies_each_key;
    "color_theme_variants", `Quick, test_color_theme_variants;
    "malformed_and_wrong_typed_config", `Quick, test_malformed_and_wrong_typed_config;
  ]
end

module Markdown_tests = struct
  open Alcotest

  let test_parse_format () = 
    let task = Task.create ~description:"Test task" ~category:(Some "Work") ~status:InProgress () in
    let doc = format_document [task] in
    let parsed = parse_document doc in
    check int "parse format roundtrip" 1 (List.length parsed)

  let test_id_roundtrips () =
    List.iter (fun id ->
      let task = { (Task.create ~description:"identity" ()) with id } in
      let once = List.hd (parse_document (format_document [task])) in
      let twice = List.hd (parse_document (format_document [once])) in
      check string "first quoted load" id once.id;
      check string "second quoted load" id twice.id)
      ["11111111-2222-3333-4444-555555555555"; "legitimate`"];
    let plain = parse_document
        "### plain-id\n- **ID**: plain-id\n- **Description**: plain\n\
         - **Created**: 2026-01-01T00:00:00Z\n- **Updated**: 2026-01-01T00:00:00Z\n" in
    check string "unquoted id" "plain-id" (List.hd plain).id

  let test_empty_file () = 
    let tasks = parse_file "/nonexistent/file.md" in
    check int "empty file" 0 (List.length tasks)

  let test_format_rejects_newline () =
    let task = Task.create ~description:"first\n### phantom" () in
    check_raises "format guard" (Invalid_argument "Description cannot contain a newline")
      (fun () -> ignore (Markdown.format_document [task]))

  let test_invalid_file_attributes () =
    let attrs = [
      "Status", "in porogress";
      "Priority", "Ultra";
      "Complexity", "9";
      "Effort", "0";
      "Due Date", "notadate";
    ] in
    List.iter (fun (field, value) ->
      let required name = Printf.sprintf
          "- **ID**: %s\n- **Description**: %s\n\
           - **Created**: 2026-01-01T00:00:00Z\n\
           - **Updated**: 2026-01-01T00:00:00Z\n" name name in
      let doc = Printf.sprintf
          "### valid\n%s\n### rejected\n%s- **%s**: %s\n"
          (required "valid") (required "rejected") field value in
      let tasks, warnings = parse_document_with_rejections doc in
      check int (field ^ " rejects only invalid task") 1 (List.length tasks);
      check string "valid task remains" "valid" (List.hd tasks).description;
      check (list string) "rejection details"
        [Printf.sprintf "Task rejected, line 12: invalid %s: %s" field value] warnings
    ) attrs

  let test_all_invalid_and_valid () =
    let required name = Printf.sprintf
        "- **ID**: %s\n- **Description**: %s\n\
         - **Created**: 2026-01-01T00:00:00Z\n- **Updated**: 2026-01-01T00:00:00Z\n" name name in
    let doc = "### a\n" ^ required "a" ^ "- **Status**: nope\n\
               ### b\n" ^ required "b" ^ "- **Effort**: 0\n" in
    let tasks, warnings = parse_document_with_rejections doc in
    check int "all invalid yields empty" 0 (List.length tasks);
    check (list string) "all warnings" ["Task a, line 6: invalid Status: nope";
      "Task b, line 12: invalid Effort: 0"] warnings;
    let tasks, warnings = parse_document_with_rejections
        ("### good\n" ^ required "good" ^
         "- **Status**: in progress\n- **Priority**: HIGH\n- **Complexity**: 5\n\
          - **Effort**: 1\n- **Due Date**: 2026-12-31T23:59:59Z\n") in
    check int "valid task loaded" 1 (List.length tasks);
    check (list string) "no warnings" [] warnings;
    check bool "case-insensitive fields preserved" true
      ((List.hd tasks).status = InProgress && (List.hd tasks).priority = High)

  let test_tight_colon_forms_rejected () =
    let fields = [
      "ID", "tight-id";
      "Description", "tight description";
      "Category", "Work";
      "Status", "Complete";
      "Priority", "High";
      "Due Date", "2030-01-01T00:00:00Z";
      "Complexity", "3";
      "Effort", "2";
      "Created", "2026-01-01T00:00:00Z";
      "Updated", "2026-01-01T00:00:00Z";
      "Completed", "2026-01-01T00:00:00Z";
      "Recurrence", "daily";
    ] in
    List.iter (fun (field, value) ->
      let required name = Printf.sprintf
          "- **ID**: %s\n- **Description**: %s\n\
           - **Created**: 2026-01-01T00:00:00Z\n- **Updated**: 2026-01-01T00:00:00Z\n" name name in
      let doc = Printf.sprintf
          "### valid\n%s\n### tight\n%s- **%s**:%s\n"
          (required "valid") (required "tight") field value in
      let tasks, warnings = parse_document_with_rejections doc in
      check int (field ^ " rejects the tight record") 1 (List.length tasks);
      check string "valid record preserved" "valid" (List.hd tasks).description;
      check (list string) (field ^ " names the invalid line")
        [Printf.sprintf "Task tight, line 12: invalid %s: %s" field value] warnings
    ) fields

  let test_invalid_values_rejected_for_every_attribute () =
    let fields = [
      "ID", "";
      "Description", "";
      "Category", "";
      "Status", "in porogress";
      "Priority", "Ultra";
      "Due Date", "notadate";
      "Complexity", "9";
      "Effort", "0";
      "Created", "garbage";
      "Updated", "garbage";
      "Completed", "garbage";
      "Recurrence", "every blue moon";
    ] in
    List.iter (fun (field, value) ->
      let required name = Printf.sprintf
          "- **ID**: %s\n- **Description**: %s\n\
           - **Created**: 2026-01-01T00:00:00Z\n- **Updated**: 2026-01-01T00:00:00Z\n" name name in
      let doc = Printf.sprintf
          "### valid\n%s\n### rejected\n%s- **%s**: %s\n"
          (required "valid") (required "rejected") field value in
      let tasks, warnings = parse_document_with_rejections doc in
      check int (field ^ " rejects only the invalid record") 1 (List.length tasks);
      check string "valid record preserved" "valid" (List.hd tasks).description;
      check (list string) (field ^ " names the invalid value")
        [Printf.sprintf "Task rejected, line 12: invalid %s: %s" field value] warnings
    ) fields

  let test_all_invalid_reports_every_warning () =
    let block name extra = Printf.sprintf
        "### %s\n- **ID**: %s\n- **Description**: %s\n\
         - **Created**: 2026-01-01T00:00:00Z\n- **Updated**: 2026-01-01T00:00:00Z\n%s"
        name name name extra in
    let doc =
      block "a" "- **Status**: nope\n" ^
      block "b" "- **Completed**: garbage\n" ^
      block "c" "- **Created**: garbage\n" ^
      block "d" "- **Updated**: garbage\n" in
    let tasks, warnings = parse_document_with_rejections doc in
    check int "all invalid yields empty" 0 (List.length tasks);
    check (list string) "every warning reported" [
      "Task a, line 6: invalid Status: nope";
      "Task b, line 12: invalid Completed: garbage";
      "Task c, line 18: invalid Created: garbage";
      "Task d, line 24: invalid Updated: garbage";
    ] warnings

  let test_valid_completed_still_archives () =
    let doc = "### aged\n- **ID**: aged\n- **Description**: aged\n\
               - **Created**: 2026-01-01T00:00:00Z\n\
               - **Updated**: 2026-01-01T00:00:00Z\n\
               - **Status**: Complete\n- **Completed**: 2020-01-01T00:00:00Z\n" in
    let tasks, warnings = parse_document_with_rejections doc in
    check (list string) "valid completion has no warning" [] warnings;
    check bool "valid completion remains archive eligible" true
      (Task.should_archive ~clock:(fun () -> 1578268800.) (List.hd tasks))

  let required_block ?(id = "task") ?(description = "task") () =
    Printf.sprintf
      "- **ID**: %s\n- **Description**: %s\n\
       - **Created**: 2026-01-01T00:00:00Z\n\
       - **Updated**: 2026-01-01T00:00:00Z\n" id description

  let find_substring haystack needle =
    let rec search i =
      if i + String.length needle > String.length haystack then None
      else if String.sub haystack i (String.length needle) = needle then Some i
      else search (i + 1) in
    search 0

  let readme_storage_example () =
    let readme = In_channel.with_open_text "README.md" In_channel.input_all in
    match find_substring readme "## Storage" with
    | None -> fail "README has no Storage section"
    | Some start ->
        let tail = String.sub readme start (String.length readme - start) in
        match find_substring tail "```markdown\n" with
        | None -> fail "README storage example missing"
        | Some offset ->
            let open_at = start + offset + String.length "```markdown\n" in
            let body = String.sub readme open_at (String.length readme - open_at) in
            match find_substring body "```" with
            | None -> fail "README storage example is not closed"
            | Some close -> String.sub body 0 close

  let test_grammar_accepts_recognised_bullets () =
    List.iter (fun bullet ->
      let doc = Printf.sprintf
          "### task\n%s **ID**: task\n%s **Description**: task\n\
           %s **Created**: 2026-01-01T00:00:00Z\n\
           %s **Updated**: 2026-01-01T00:00:00Z\n"
          bullet bullet bullet bullet in
      let tasks, warnings = parse_document_with_rejections doc in
      check (list string) (bullet ^ " bullet accepted") [] warnings;
      check int (bullet ^ " bullet loads one task") 1 (List.length tasks);
      let task = List.hd tasks in
      check string "id" "task" task.id;
      check string "description" "task" task.description)
      ["-"; "*"; "+"];
    let header = format_document [Task.create ~description:"canonical" ()] in
    let tasks, warnings = parse_document_with_rejections header in
    check (list string) "canonical document accepted" [] warnings;
    check int "canonical document loads" 1 (List.length tasks);
    let indented = "### task\n  - **ID**: task\n\t- **Description**: task\n\
                    \    - **Created**: 2026-01-01T00:00:00Z\n\
                    \  - **Updated**: 2026-01-01T00:00:00Z\n" in
    let tasks, warnings = parse_document_with_rejections indented in
    check (list string) "indentation tolerated" [] warnings;
    check int "indented record loads" 1 (List.length tasks)

  let test_grammar_requires_identity_fields () =
    let required = [
      "ID", "- **ID**: legacy";
      "Description", "- **Description**: legacy";
      "Created", "- **Created**: 2026-01-01T00:00:00Z";
      "Updated", "- **Updated**: 2026-01-01T00:00:00Z";
    ] in
    List.iter (fun (label, _dropped) ->
      let lines = List.filter_map (fun (name, line) ->
        if name = label then None else Some line) required in
      let doc = "### legacy\n" ^ String.concat "\n" lines ^ "\n" in
      let tasks, warnings = parse_document_with_rejections doc in
      check int (label ^ " missing refuses the record") 0 (List.length tasks);
      check (list string) (label ^ " names the task, line and field")
        [Printf.sprintf "Task legacy, line 1: missing required field %s" label] warnings)
      required;
    let mixed = "### good\n" ^ required_block ~id:"good" ~description:"good" () ^
                "\n### legacy\n- **Description**: legacy\n" in
    let tasks, warnings = parse_document_with_rejections mixed in
    check int "valid record preserved" 1 (List.length tasks);
    check string "valid record identity" "good" (List.hd tasks).id;
    check (list string) "missing fields reported for the bad record"
      ["Task legacy, line 7: missing required field ID";
       "Task legacy, line 7: missing required field Created";
       "Task legacy, line 7: missing required field Updated"] warnings

  let test_grammar_rejects_unknown_lines () =
    let cases = [
      "- **Tags**: alpha, beta";
      "some prose here";
      "-**ID**: task";
      "- **status**: Complete";
    ] in
    List.iter (fun offending ->
      let doc = "### good\n" ^ required_block ~id:"good" ~description:"good" () ^
                "\n### bad\n" ^ required_block ~id:"bad" ~description:"bad" () ^
                offending ^ "\n" in
      let tasks, warnings = parse_document_with_rejections doc in
      check int ("valid sibling preserved for " ^ offending) 1 (List.length tasks);
      check string "sibling identity" "good" (List.hd tasks).id;
      check (list string) ("offending line refused: " ^ offending)
        [Printf.sprintf "Task bad, line 12: unrecognised line: %s" offending] warnings)
      cases

  let test_grammar_rejects_unrecognised_document_lines () =
    let doc = "# Mark Task\n\n## Tasks\nstray prose\n" in
    let tasks, warnings = parse_document_with_rejections doc in
    check int "stray prose loads no tasks" 0 (List.length tasks);
    check (list string) "stray prose refused with line and text"
      ["line 4: unrecognised line: stray prose"] warnings;
    let _, warnings = parse_document_with_rejections "# Mark Task\n## Other\n" in
    check (list string) "unknown heading refused"
      ["line 2: unrecognised line: ## Other"] warnings

  let test_grammar_strips_leading_bom () =
    let doc = "\xEF\xBB\xBF### task\n" ^ required_block () in
    let tasks, warnings = parse_document_with_rejections doc in
    check (list string) "BOM strip warns nothing" [] warnings;
    check int "BOM document loads its record" 1 (List.length tasks);
    check string "BOM record identity" "task" (List.hd tasks).id

  let test_grammar_normalises_bullets_on_write () =
    let doc = "### task\n* **ID**: task\n+ **Description**: task\n\
               * **Created**: 2026-01-01T00:00:00Z\n\
               + **Updated**: 2026-01-01T00:00:00Z\n" in
    let tasks, warnings = parse_document_with_rejections doc in
    check (list string) "star and plus load" [] warnings;
    check int "star and plus load one task" 1 (List.length tasks);
    let written = format_document tasks in
    let starts_with_bullet bullet line =
      String.starts_with ~prefix:bullet (String.trim line) in
    let lines = String.split_on_char '\n' written in
    check bool "canonical bullet written" true
      (List.exists (starts_with_bullet "- ") lines);
    check bool "star bullet not written" false
      (List.exists (starts_with_bullet "* ") lines);
    check bool "plus bullet not written" false
      (List.exists (starts_with_bullet "+ ") lines);
    let rewritten, rewritten_warnings = parse_document_with_rejections written in
    check (list string) "rewritten document loads" [] rewritten_warnings;
    check string "canonical round trip is stable" written (format_document rewritten)

  let test_grammar_readme_storage_example () =
    let example = readme_storage_example () in
    let tasks, warnings = parse_document_with_rejections example in
    check (list string) "README storage example loads clean" [] warnings;
    check int "README storage example has one task" 1 (List.length tasks);
    let task = List.hd tasks in
    check bool "README example carries Created" true (task.created_at <> "");
    check bool "README example carries Updated" true (task.updated_at <> "")

  let test_grammar_header_label_must_match_id () =
    let labelled = "### Fix the login bug\n" ^
                   required_block ~id:"abc" ~description:"login bug" () in
    let tasks, warnings = parse_document_with_rejections labelled in
    check int "labelled header refuses the record" 0 (List.length tasks);
    check (list string) "mismatch names header, line and reason"
      ["Task Fix the login bug, line 1: header label does not match ID: abc"] warnings;
    let matching = "### abc\n" ^ required_block ~id:"abc" ~description:"login bug" () in
    let tasks, warnings = parse_document_with_rejections matching in
    check (list string) "id header accepted" [] warnings;
    check int "id header loads" 1 (List.length tasks);
    let written = format_document tasks in
    check bool "id header survives a rewrite" true
      (String.starts_with ~prefix:"# Mark Task\n\n## Tasks\n### abc\n" written);
    let mixed = "### good\n" ^ required_block ~id:"good" ~description:"good" () ^
                "\n### Fix the login bug\n" ^
                required_block ~id:"bad" ~description:"bad" () in
    let tasks, warnings = parse_document_with_rejections mixed in
    check int "mixed labels keep the matching record" 1 (List.length tasks);
    check string "mixed labels keep the good record" "good" (List.hd tasks).id;
    check (list string) "mixed labels refuse the label record"
      ["Task Fix the login bug, line 7: header label does not match ID: bad"] warnings

  let test_grammar_label_less_records_are_refused () =
    let no_label = required_block ~id:"abc" ~description:"abc" () in
    let tasks, warnings = parse_document_with_rejections no_label in
    check int "no header loads nothing" 0 (List.length tasks);
    check (list string) "no header refuses every field line"
      ["line 1: unrecognised line: - **ID**: abc";
       "line 2: unrecognised line: - **Description**: abc";
       "line 3: unrecognised line: - **Created**: 2026-01-01T00:00:00Z";
       "line 4: unrecognised line: - **Updated**: 2026-01-01T00:00:00Z"] warnings;
    let empty_label = "### \n" ^ required_block ~id:"abc" ~description:"abc" () in
    let tasks, warnings = parse_document_with_rejections empty_label in
    check int "empty header loads nothing" 0 (List.length tasks);
    check (list string) "empty header is unrecognised and its fields refused"
      ["line 1: unrecognised line: ###";
       "line 2: unrecognised line: - **ID**: abc";
       "line 3: unrecognised line: - **Description**: abc";
       "line 4: unrecognised line: - **Created**: 2026-01-01T00:00:00Z";
       "line 5: unrecognised line: - **Updated**: 2026-01-01T00:00:00Z"] warnings

  let test_format_guard_matches_reader () =
    let task = { (Task.create ~description:"x" ()) with description = "" } in
    check_raises "empty description guard"
      (Invalid_argument "Description cannot be empty")
      (fun () -> ignore (Markdown.format_document [task]));
    let task = { (Task.create ~description:"x" ()) with description = "   " } in
    check_raises "whitespace description guard"
      (Invalid_argument "Description cannot be empty")
      (fun () -> ignore (Markdown.format_document [task]))

  let test_backtick_values_are_normalised_on_write () =
    let task = { (Task.create ~description:"`hello`" ()) with
      id = "task"; category = Some "`Work`" } in
    let written = format_document [task] in
    let lines = String.split_on_char '\n' written in
    check bool "description loses the wrapper" true
      (List.mem "- **Description**: hello" lines);
    check bool "category loses the wrapper" true
      (List.mem "- **Category**: Work" lines);
    let reparsed = List.hd (parse_document written) in
    check string "stored description" "hello" reparsed.description;
    check (option string) "stored category" (Some "Work") reparsed.category;
    check string "save/load is byte-stable" written (format_document [reparsed])

  let test_backtick_hand_edit_is_reported () =
    let doc = "### task\n- **ID**: task\n- **Description**: `hello now`\n\
               - **Created**: 2026-01-01T00:00:00Z\n\
               - **Updated**: 2026-01-01T00:00:00Z\n" in
    let result = parse_document_result doc in
    check (list string) "normalisation is reported"
      ["Task task, line 3: stripped backticks from Description"]
      result.normalisations;
    check (list string) "no rejection" [] result.rejections;
    check string "stored value is the stripped value" "hello now"
      (List.hd result.tasks).description;
    let written = format_document result.tasks in
    check bool "writer emits the stored value" true
      (List.mem "- **Description**: hello now" (String.split_on_char '\n' written));
    check string "rewrite is a fixpoint" written
      (format_document (parse_document written))

  let test_grammar_accepts_tab_bullets () =
    let doc = "### task\n-\t**ID**: task\n-\t**Description**: task\n\
               -\t**Created**: 2026-01-01T00:00:00Z\n\
               -\t**Updated**: 2026-01-01T00:00:00Z\n" in
    let tasks, warnings = parse_document_with_rejections doc in
    check (list string) "tab bullets accepted" [] warnings;
    check int "tab bullets load one task" 1 (List.length tasks);
    check string "tab bullet id" "task" (List.hd tasks).id;
    let written = format_document tasks in
    check bool "tab bullet rewritten as the canonical bullet" true
      (List.mem "- **ID**: `task`" (String.split_on_char '\n' written))

  let test_grammar_accepts_duplicate_headings () =
    let doc = "# Mark Task\n\n## Tasks\n\n### one\n" ^
              required_block ~id:"one" ~description:"one" () ^
              "\n## Tasks\n\n### two\n" ^
              required_block ~id:"two" ~description:"two" () in
    let tasks, warnings = parse_document_with_rejections doc in
    check (list string) "duplicate heading warns nothing" [] warnings;
    check int "both records load" 2 (List.length tasks);
    let written = format_document tasks in
    let headings = List.filter (fun line -> String.trim line = "## Tasks")
        (String.split_on_char '\n' written) in
    check int "the save keeps one Tasks heading" 1 (List.length headings);
    let _, warnings = parse_document_with_rejections written in
    check (list string) "the rewrite loads clean" [] warnings

  let test_grammar_refuses_bullet_less_fields () =
    (* Intended compatibility break: the bullet-less shape is refused by this
       grammar. The refusal is documented in the README. *)
    let doc = "### legacy-no-bullets\n\
               **ID**: legacy\n\
               **Description**: legacy record\n\
               **Created**: 2026-01-01T00:00:00Z\n\
               **Updated**: 2026-01-01T00:00:00Z\n" in
    let tasks, warnings = parse_document_with_rejections doc in
    check int "bullet-less record refused" 0 (List.length tasks);
    check (list string) "each bullet-less line refused"
      ["Task legacy-no-bullets, line 2: unrecognised line: **ID**: legacy";
       "Task legacy-no-bullets, line 3: unrecognised line: **Description**: legacy record";
       "Task legacy-no-bullets, line 4: unrecognised line: **Created**: 2026-01-01T00:00:00Z";
       "Task legacy-no-bullets, line 5: unrecognised line: **Updated**: 2026-01-01T00:00:00Z";
       "Task legacy-no-bullets, line 1: missing required field ID";
       "Task legacy-no-bullets, line 1: missing required field Description";
       "Task legacy-no-bullets, line 1: missing required field Created";
       "Task legacy-no-bullets, line 1: missing required field Updated"]
      warnings;
    let readme = In_channel.with_open_text "README.md" In_channel.input_all in
    check bool "README records the release note" true
      (find_substring readme "Field lines without a bullet" <> None)

  let sample_value key = match key with
    | "id" | "description" -> "task"
    | "category" -> "Work"
    | "status" -> "Complete"
    | "priority" -> "High"
    | "due_date" | "created_at" -> "2026-01-01T00:00:00Z"
    | "complexity" -> "3"
    | "effort" -> "4"
    | "updated_at" -> "2026-01-02T00:00:00Z"
    | "completed_at" -> "2026-01-03T00:00:00Z"
    | "recurrence" -> "daily"
    | _ -> "value"

  let test_grammar_field_table_is_single_source () =
    List.iter (fun (spec : Markdown.field_spec) ->
      let lines = List.filter_map (fun (other : Markdown.field_spec) ->
        if other.key = spec.key then None
        else Some (Printf.sprintf "- **%s**: %s" other.label
                      (sample_value other.key))) Markdown.field_specs in
      let doc = "### task\n" ^ String.concat "\n" lines ^ "\n" in
      let tasks, warnings = parse_document_with_rejections doc in
      if spec.required then (
        check int (spec.label ^ " absence refuses") 0 (List.length tasks);
        check (list string) (spec.label ^ " absence is reported")
          [Printf.sprintf "Task task, line 1: missing required field %s" spec.label]
          warnings)
      else (
        check (list string) (spec.label ^ " absence is accepted") [] warnings;
        check int (spec.label ^ " absence loads") 1 (List.length tasks)))
      Markdown.field_specs

  let tests = [
    "grammar_field_table_is_single_source", `Quick, test_grammar_field_table_is_single_source;
    "grammar_accepts_recognised_bullets", `Quick, test_grammar_accepts_recognised_bullets;
    "grammar_requires_identity_fields", `Quick, test_grammar_requires_identity_fields;
    "grammar_rejects_unknown_lines", `Quick, test_grammar_rejects_unknown_lines;
    "grammar_rejects_unrecognised_document_lines", `Quick, test_grammar_rejects_unrecognised_document_lines;
    "grammar_strips_leading_bom", `Quick, test_grammar_strips_leading_bom;
    "grammar_normalises_bullets_on_write", `Quick, test_grammar_normalises_bullets_on_write;
    "grammar_readme_storage_example", `Quick, test_grammar_readme_storage_example;
    "grammar_header_label_must_match_id", `Quick, test_grammar_header_label_must_match_id;
    "grammar_label_less_records_are_refused", `Quick, test_grammar_label_less_records_are_refused;
    "format_guard_matches_reader", `Quick, test_format_guard_matches_reader;
    "backtick_values_are_normalised_on_write", `Quick, test_backtick_values_are_normalised_on_write;
    "backtick_hand_edit_is_reported", `Quick, test_backtick_hand_edit_is_reported;
    "grammar_accepts_tab_bullets", `Quick, test_grammar_accepts_tab_bullets;
    "grammar_refuses_bullet_less_fields", `Quick, test_grammar_refuses_bullet_less_fields;
    "grammar_accepts_duplicate_headings", `Quick, test_grammar_accepts_duplicate_headings;
    "all_invalid_and_valid", `Quick, test_all_invalid_and_valid;
    "tight_colon_forms_rejected", `Quick, test_tight_colon_forms_rejected;
    "invalid_values_rejected_for_every_attribute", `Quick, test_invalid_values_rejected_for_every_attribute;
    "all_invalid_reports_every_warning", `Quick, test_all_invalid_reports_every_warning;
    "valid_completed_still_archives", `Quick, test_valid_completed_still_archives;
    "parse_format", `Quick, test_parse_format;
    "id_roundtrips", `Quick, test_id_roundtrips;
    "empty_file", `Quick, test_empty_file;
    "format_rejects_newline", `Quick, test_format_rejects_newline;
    "invalid_file_attributes", `Quick, test_invalid_file_attributes;
  ]
end

module Storage_tests = struct
  open Alcotest

  let test_load_save () = 
    let path = Filename.temp_file "mark-task-test" ".md" in
    let task = Task.create ~description:"Test" () in
    write_file path [task];
    let loaded = parse_file path in
    check int "load save" 1 (List.length loaded);
    Sys.remove path

  let tests = [
    "load_save", `Quick, test_load_save;
  ]
end

module Atomic_write_tests = struct
  open Alcotest

  let with_dir f =
    let dir = Filename.temp_file "atomic-write" "" in
    Sys.remove dir;
    Unix.mkdir dir 0o700;
    Fun.protect ~finally:(fun () ->
      Array.iter (fun name -> Sys.remove (Filename.concat dir name)) (Sys.readdir dir);
      Unix.rmdir dir) (fun () -> f dir)

  let test_archive_path () = with_dir (fun dir ->
    let path = Filename.concat dir "tasks.md" in
    let expected = Filename.concat dir "archive.md" in
    let observed = ref None in
    let task = { (Task.create ~description:"completed" ()) with
      status = Complete; completed_at = Some "2020-01-01T00:00:00Z" } in
    Atomic_write.hook := (fun event target ->
      if event = Atomic_write.Temp_created then (observed := Some target; raise Exit));
    Fun.protect ~finally:(fun () -> Atomic_write.hook := (fun _ _ -> ())) (fun () ->
      (try ignore (Archive.archive_completed_tasks ~_path:path [task]) with Exit -> ());
      check (option string) "archive beside selected file" (Some expected) !observed))

  let test_archive_rejects_invalid_destination () = with_dir (fun dir ->
    let path = Filename.concat dir "tasks.md" in
    let archive = Filename.concat dir "archive.md" in
    let active = "### eligible\n- **ID**: eligible\n- **Description**: archive me\n- **Status**: Complete\n- **Completed**: 2020-01-01T00:00:00Z\n" in
    let invalid = "### old-invalid\n- **ID**: old-invalid\n- **Description**: preserve me\n- **Status**: typo\n" in
    Out_channel.with_open_bin path (fun oc -> output_string oc active);
    Out_channel.with_open_bin archive (fun oc -> output_string oc invalid);
    let manager = Task_manager.FileTaskManager.create ~path () in
    let result = Task_manager.FileTaskManager.archive_completed manager in
    check bool "invalid archive refused" true (Result.is_error result);
    check string "active unchanged" active (In_channel.with_open_bin path In_channel.input_all);
    check string "archive unchanged" invalid (In_channel.with_open_bin archive In_channel.input_all))

  let test_archive_valid_and_missing () = with_dir (fun dir ->
    let path = Filename.concat dir "tasks.md" in
    let archive = Filename.concat dir "archive.md" in
    let completed description = { (Task.create ~description ()) with
      status = Complete; completed_at = Some "2020-01-01T00:00:00Z" } in
    let first = completed "first" and second = completed "second" in
    check int "missing archive succeeds" 0
      (List.length (Result.get_ok (Archive.archive_completed_tasks ~_path:path [first])));
    check int "valid archive succeeds" 0
      (List.length (Result.get_ok (Archive.archive_completed_tasks ~_path:path [second])));
    check (list string) "both records retained" ["first"; "second"]
      (List.map (fun task -> task.description) (Markdown.parse_file archive)))

  let test_archive_reopened () = with_dir (fun dir ->
    let path = Filename.concat dir "tasks.md" in
    let completed = { (Task.create ~description:"reopened" ()) with
      status = Complete; completed_at = Some "2020-01-01T00:00:00Z" } in
    let reopened = Task.mark_status Backlog completed in
    check (option string) "completion timestamp retained" completed.completed_at reopened.completed_at;
    let remaining = Result.get_ok (Archive.archive_completed_tasks ~_path:path ~days:3 [reopened]) in
    check (list string) "reopened task remains" [reopened.id] (List.map (fun task -> task.id) remaining);
    check bool "archive not written" false (Sys.file_exists (Filename.concat dir "archive.md")))

  let test_serialize_failure () = with_dir (fun dir ->
    let path = Filename.concat dir "tasks.md" in
    Out_channel.with_open_bin path (fun oc -> output_string oc "old");
    let before = ref false in
    Atomic_write.hook := (fun event target ->
      if event = Atomic_write.Before_rename && target = path then before := true);
    Fun.protect ~finally:(fun () -> Atomic_write.hook := (fun _ _ -> ())) (fun () ->
      check_raises "mid-serialize error" Exit (fun () ->
        Atomic_write.write path (fun oc -> output_string oc "partial"; raise Exit));
      check string "target unchanged" "old" (In_channel.with_open_bin path In_channel.input_all);
      check (list string) "no temporary file" ["tasks.md"]
        (Array.to_list (Sys.readdir dir));
      check bool "rename not reached" false !before))

  let test_sites_and_sync () = with_dir (fun dir ->
    let path = Filename.concat dir "tasks.md" in
    let config_path = Filename.concat dir "config.json" in
    let events = ref [] in
    Out_channel.with_open_bin path (fun oc -> output_string oc "old task data");
    Atomic_write.hook := (fun event target ->
      if event = Atomic_write.Before_rename && target = path then
        check string "target intact before rename" "old task data"
          (In_channel.with_open_bin target In_channel.input_all);
      events := (event, target) :: !events);
    Fun.protect ~finally:(fun () -> Atomic_write.hook := (fun _ _ -> ())) (fun () ->
      Markdown.write_file path [];
      Config.save default_config ~config_path ();
      ignore (Result.get_ok (Storage.save_history ~path [] []));
      let task = Task.create ~description:"overdue" ~due_date:(Some "2020-01-01T00:00:00Z") () in
      ignore (Result.get_ok (Notifications.check_due_tasks ~path ~send:(fun _ _ _ -> false)
        default_config [task]));
      List.iter (fun target ->
        List.iter (fun event -> check bool "write stage reached" true
          (List.mem (event, target) !events))
          [Atomic_write.Temp_created; Atomic_write.File_synced;
           Atomic_write.Before_rename; Atomic_write.Dir_synced])
        [path; config_path; path ^ ".history"; path ^ ".notifications"];
      check (list string) "no temporary files" []
        (Array.to_list (Sys.readdir dir) |> List.filter (String.starts_with ~prefix:".atomic-"))))

  let tests = [
    "archive_path", `Quick, test_archive_path;
    "archive_rejects_invalid_destination", `Quick, test_archive_rejects_invalid_destination;
    "archive_valid_and_missing", `Quick, test_archive_valid_and_missing;
    "archive_reopened", `Quick, test_archive_reopened;
    "serialize_failure", `Quick, test_serialize_failure;
    "sites_and_sync", `Quick, test_sites_and_sync;
  ]
end

module Validation_tests = struct
  open Alcotest

  let test_validate_task () = 
    let result = validate_task "Test" () in
    check bool "valid task" (result = Ok ()) true;
    
    let result = validate_task "" () in
    check bool "empty description" (result = Error EmptyDescription) true

  let test_reject_newline_description () =
    match validate_task "first\n### phantom" () with
    | Ok () -> fail "newline description accepted"
    | Error error ->
      check bool "error names description" true
        (String.starts_with ~prefix:"Description" (string_of_error error))

  let test_reject_invalid_attributes () =
    check bool "invalid priority"
      (validate_task "T" ~priority:"Ultra" () = Error (InvalidPriority "Ultra")) true;
    check bool "invalid complexity"
      (validate_task "T" ~complexity:"9" () = Error (InvalidComplexity "9")) true;
    check bool "invalid effort"
      (validate_task "T" ~effort:"0" () = Error (InvalidEffort "0")) true;
    check bool "invalid due date"
      (validate_task "T" ~due_date:"notadate" () = Error (InvalidDueDate "notadate")) true;
    check bool "invalid status"
      (validate_task "T" ~status:"Bogus" () = Error (InvalidStatus "Bogus")) true

  let test_accept_valid_attributes () =
    check bool "case-insensitive priority"
      (validate_task "T" ~priority:"HIGH" () = Ok ()) true;
    check bool "spaced status"
      (validate_task "T" ~status:"In Progress" () = Ok ()) true;
    check bool "valid due date"
      (validate_task "T" ~due_date:"2026-12-31T23:59:59Z" () = Ok ()) true

  let test_reject_empty_category () =
    check bool "empty category refused" true
      (validate_task "T" ~category:"" () =
       Error (OtherError "Category cannot be empty"));
    check bool "spaces-only category refused" true
      (validate_task "T" ~category:"   " () =
       Error (OtherError "Category cannot be empty"))

  let tests = [
    "validate_task", `Quick, test_validate_task;
    "reject_newline_description", `Quick, test_reject_newline_description;
    "reject_invalid_attributes", `Quick, test_reject_invalid_attributes;
    "reject_empty_category", `Quick, test_reject_empty_category;
    "accept_valid_attributes", `Quick, test_accept_valid_attributes;
  ]
end

module Task_manager_tests = struct
  open Alcotest

  let fresh_path () =
    let path = Filename.temp_file "mark-task-test" ".md" in
    Sys.remove path;
    let history = path ^ ".history" in
    if Sys.file_exists history then Sys.remove history;
    path

  let test_mixed_file_refuses_write () =
    let path = fresh_path () in
    let original = "### valid\n- **ID**: valid\n- **Description**: valid\n\
                    - **Created**: 2026-01-01T00:00:00Z\n\
                    - **Updated**: 2026-01-01T00:00:00Z\n\n\
                    ### rejected\n- **ID**: rejected\n- **Description**: rejected\n\
                    - **Created**: 2026-01-01T00:00:00Z\n\
                    - **Updated**: 2026-01-01T00:00:00Z\n\
                    - **Priority**: Ultra\n" in
    Out_channel.with_open_bin path (fun oc -> Out_channel.output_string oc original);
    let m = FileTaskManager.create ~path () in
    let result = FileTaskManager.mark m "valid" Complete in
    let after = In_channel.with_open_bin path In_channel.input_all in
    check string "file remains byte-identical" original after;
    (match result with
     | Ok _ -> fail "mark must be refused"
     | Error reason ->
         List.iter (fun part ->
           check bool ("reason includes " ^ part) true
             (let len = String.length part in
              let rec contains i =
                i + len <= String.length reason &&
                (String.sub reason i len = part || contains (i + 1)) in
              contains 0)) ["rejected"; "Priority"; "Ultra"; "12"]);
    Sys.remove path;
    if Sys.file_exists (path ^ ".history") then Sys.remove (path ^ ".history")

  let test_empty_backticks_cannot_self_lock () =
    Atomic_write_tests.with_dir (fun dir ->
      let path = Filename.concat dir "tasks.md" in
      let m = FileTaskManager.create ~path () in
      let refused = Repl.execute_command m (Repl.Add ("``", [])) in
      check bool "add refuses empty backticks" true (Result.is_error refused);
      check bool "refused add writes no file" false (Sys.file_exists path);
      let m = FileTaskManager.create ~path () in
      let added = Repl.execute_command m (Repl.Add ("`hello`", [])) in
      check bool "wrapped description accepted" true (Result.is_ok added);
      let written = In_channel.with_open_bin path In_channel.input_all in
      check bool "stored description is bare" true
        (let lines = String.split_on_char '\n' written in
         List.mem "- **Description**: hello" lines);
      let reloaded = FileTaskManager.create ~path () in
      check int "writer output loads again" 1
        (List.length (FileTaskManager.get_tasks reloaded));
      check string "reloaded description" "hello"
        (List.hd (FileTaskManager.get_tasks reloaded)).description)

  let test_read_only_and_reload () =
    let path = fresh_path () in
    let original = "### valid\n- **ID**: valid\n- **Description**: valid\n\
                    - **Created**: 2026-01-01T00:00:00Z\n\
                    - **Updated**: 2026-01-01T00:00:00Z\n\
                    ### bad\n- **ID**: bad\n- **Description**: bad\n\
                    - **Created**: 2026-01-01T00:00:00Z\n\
                    - **Updated**: 2026-01-01T00:00:00Z\n\
                    - **Effort**: 0\n" in
    Out_channel.with_open_bin path (fun oc -> Out_channel.output_string oc original);
    let m = FileTaskManager.create ~path () in
    let task = List.hd (FileTaskManager.get_tasks m) in
    let operations = [
      (fun () -> FileTaskManager.save m);
      (fun () -> FileTaskManager.add m task);
      (fun () -> FileTaskManager.delete m "valid");
      (fun () -> FileTaskManager.update m task);
      (fun () -> FileTaskManager.mark m "valid" Complete);
      (fun () -> FileTaskManager.undo m);
      (fun () -> FileTaskManager.redo m);
      (fun () -> FileTaskManager.archive_completed m);
    ] in
    List.iter (fun op -> match op () with
      | Error reason -> check bool "rejection reason" true
          (String.length reason > 0 && String.starts_with ~prefix:"Read-only:" reason)
      | Ok _ -> fail "write must be refused") operations;
    check string "unchanged after all writes" original
      (In_channel.with_open_bin path In_channel.input_all);
    check bool "no history written" false (Sys.file_exists (path ^ ".history"));
    Out_channel.with_open_bin path (fun oc ->
      Out_channel.output_string oc "### valid\n- **ID**: valid\n- **Description**: valid\n\
                           - **Created**: 2026-01-01T00:00:00Z\n\
                           - **Updated**: 2026-01-01T00:00:00Z\n");
    let clean = FileTaskManager.create ~path () in
    check bool "reload enables writes" true
      (Result.is_ok (FileTaskManager.mark clean "valid" Complete));
    Sys.remove path;
    if Sys.file_exists (path ^ ".history") then Sys.remove (path ^ ".history")

  let test_add_undo_redo_across_processes () =
    let path = fresh_path () in
    let m = FileTaskManager.create ~path () in
    let task = Task.create ~description:"Undo me" () in
    let m, _ = Result.get_ok (FileTaskManager.add m task) in
    check int "one task after add" 1
      (List.length (FileTaskManager.get_tasks m));
    (* New process: reload from disk, then undo the add *)
    let reloaded = FileTaskManager.create ~path () in
    let reloaded, _ = Result.get_ok (FileTaskManager.undo reloaded) in
    check int "undo removes task" 0
      (List.length (FileTaskManager.get_tasks reloaded));
    (* New process: redo restores it *)
    let reloaded_again = FileTaskManager.create ~path () in
    let reloaded_again, _ = Result.get_ok (FileTaskManager.redo reloaded_again) in
    check int "redo restores task" 1
      (List.length (FileTaskManager.get_tasks reloaded_again));
    Sys.remove path;
    Sys.remove (path ^ ".history")

  let readme_contains text =
    let readme = In_channel.with_open_text "README.md" In_channel.input_all in
    let rec search i =
      i + String.length text <= String.length readme &&
      (String.sub readme i (String.length text) = text || search (i + 1)) in
    search 0

  let test_controlled_repeat () =
    Atomic_write_tests.with_dir (fun dir ->
      let path = Filename.concat dir "tasks.md" in
      let task = { (Task.create ~description:"repeat" ()) with id = "repeat" } in
      let m = FileTaskManager.create ~path () in
      let m, _ = Result.get_ok (FileTaskManager.add m task) in
      let m, _ = Result.get_ok (FileTaskManager.mark ~clock:(fun () -> 1577836800.) m task.id Complete) in
      check (option string) "first completion recorded" (Some "2020-01-01T00:00:00Z")
        (List.hd (Markdown.parse_file path)).completed_at;
      let _, _ = Result.get_ok (FileTaskManager.mark ~clock:(fun () -> 1577923200.) m task.id Complete) in
      check (option string) "repeat completion preserves first completion"
        (Some "2020-01-01T00:00:00Z") (List.hd (Markdown.parse_file path)).completed_at)

  let test_controlled_recompletion () =
    Atomic_write_tests.with_dir (fun dir ->
      let path = Filename.concat dir "tasks.md" in
      let task = { (Task.create ~description:"recomplete" ()) with id = "recomplete" } in
      let m = FileTaskManager.create ~path () in
      let m, _ = Result.get_ok (FileTaskManager.add m task) in
      let m, _ = Result.get_ok (FileTaskManager.mark ~clock:(fun () -> 1577836800.) m task.id Complete) in
      let m, _ = Result.get_ok (FileTaskManager.mark m task.id Backlog) in
      let _, _ = Result.get_ok (FileTaskManager.mark ~clock:(fun () -> 1577923200.) m task.id Complete) in
      check (option string) "recompletion records new completion"
        (Some "2020-01-02T00:00:00Z") (List.hd (Markdown.parse_file path)).completed_at)

  let test_controlled_aged_archive () =
    Atomic_write_tests.with_dir (fun dir ->
      let path = Filename.concat dir "tasks.md" in
      let task = { (Task.create ~description:"aged" ()) with id = "aged" } in
      let m = FileTaskManager.create ~path () in
      let m, _ = Result.get_ok (FileTaskManager.add m task) in
      let m, _ = Result.get_ok (FileTaskManager.mark ~clock:(fun () -> 1577836800.) m task.id Complete) in
      let m, _ = Result.get_ok (FileTaskManager.archive_completed ~clock:(fun () -> 1578268800.) m) in
      check int "aged completion archived" 0 (List.length (FileTaskManager.get_tasks m));
      check int "archive contains task" 1
        (List.length (Markdown.parse_file (Filename.concat dir "archive.md"))))

  let test_controlled_completion_and_archive () =
    Atomic_write_tests.with_dir (fun dir ->
      let path = Filename.concat dir "tasks.md" in
      let clock seconds () = seconds in
      let t1 = clock 1577836800. and t2 = clock 1577923200. in
      let after = clock 1578268800. in
      let task = { (Task.create ~description:"timed" ()) with id = "timed" } in
      let m = FileTaskManager.create ~path () in
      let m, _ = Result.get_ok (FileTaskManager.add m task) in
      let m, _ = Result.get_ok (FileTaskManager.mark ~clock:t1 m task.id Complete) in
      let completed () = (List.hd (Markdown.parse_file path)).completed_at in
      check (option string) "first completion" (Some "2020-01-01T00:00:00Z") (completed ());
      let m, _ = Result.get_ok (FileTaskManager.mark ~clock:t2 m task.id Complete) in
      check (option string) "repeat preserves first time" (Some "2020-01-01T00:00:00Z") (completed ());
      let m, _ = Result.get_ok (FileTaskManager.mark m task.id Backlog) in
      check (option string) "reopen retains first time" (Some "2020-01-01T00:00:00Z") (completed ());
      check bool "reopened task not archived" false
        (Task.should_archive ~clock:after (List.hd (FileTaskManager.get_tasks m)));
      let m, _ = Result.get_ok (FileTaskManager.mark ~clock:t2 m task.id Complete) in
      check (option string) "recompletion records second time" (Some "2020-01-02T00:00:00Z") (completed ());
      let m, _ = Result.get_ok (FileTaskManager.archive_completed ~clock:after m) in
      check int "aged task removed" 0 (List.length (FileTaskManager.get_tasks m));
      let archived = Markdown.parse_file (Filename.concat dir "archive.md") in
      check (option string) "archive retains completion" (Some "2020-01-02T00:00:00Z")
        (List.hd archived).completed_at)

  let test_completion_transition () =
    let path = fresh_path () in
    let old = "2020-01-01T00:00:00Z" in
    let task = { (Task.create ~description:"completion" ()) with
      id = "completion"; completed_at = Some old } in
    let m = FileTaskManager.create ~path () in
    let m, _ = Result.get_ok (FileTaskManager.add m task) in
    let m, _ = Result.get_ok (FileTaskManager.mark m task.id Complete) in
    let completed = List.hd (Markdown.parse_file path) in
    check bool "completion recorded" true
      (Option.is_some completed.completed_at && completed.completed_at <> Some old);
    let m, _ = Result.get_ok (FileTaskManager.mark m task.id Complete) in
    check (option string) "repeat preserves timestamp" completed.completed_at
      (List.hd (Markdown.parse_file path)).completed_at;
    let m, _ = Result.get_ok (FileTaskManager.mark m task.id Backlog) in
    check (option string) "reopen retains timestamp" completed.completed_at
      (List.hd (Markdown.parse_file path)).completed_at;
    let m, _ = Result.get_ok (FileTaskManager.undo m) in
    check (option string) "undo restores completion timestamp" completed.completed_at
      (List.hd (FileTaskManager.get_tasks m)).completed_at;
    Sys.remove path;
    Sys.remove (path ^ ".history")

  let test_completion_consumers () =
    let path = fresh_path () in
    let task = { (Task.create ~description:"daily" ~due_date:(Some "2026-12-01T12:00:00Z")
        ~recurrence:(Some "daily") ()) with id = "daily" } in
    let m = FileTaskManager.create ~path () in
    let m, _ = Result.get_ok (FileTaskManager.add m task) in
    ignore (Result.get_ok (FileTaskManager.mark m task.id Complete));
    let completed = List.hd (Markdown.parse_file path) in
    check bool "completion persisted" true (Option.is_some completed.completed_at);
    let regenerated = List.hd (FileTaskManager.get_tasks (FileTaskManager.create ~path ())) in
    check bool "recurrence regenerated" true (regenerated.status = Backlog);
    let aged = { completed with recurrence = None; completed_at = Some "2020-01-01T00:00:00Z" } in
    check bool "aged completion is archive eligible" true (Task.should_archive ~days:3 aged);
    Sys.remove path;
    Sys.remove (path ^ ".history")

  let test_identifier_history_and_display () =
    let path = fresh_path () in
    let task = { (Task.create ~description:"identity" ()) with id = "12345678-abcd" } in
    let m = FileTaskManager.create ~path () in
    ignore (Result.get_ok (FileTaskManager.add m task));
    let m = FileTaskManager.create ~path () in
    check bool "exact id resolves after reload" true
      (Option.is_some (FileTaskManager.find_by_id m task.id));
    let m, _ = Result.get_ok (FileTaskManager.undo m) in
    let m, _ = Result.get_ok (FileTaskManager.redo m) in
    check string "history restores exact id" task.id (List.hd (FileTaskManager.get_tasks m)).id;
    let rendered = Ui.format_task default_config.color_theme task in
    check bool "display uses eight-character id" true
      (String.starts_with ~prefix:"12345678 |" rendered);
    Sys.remove path;
    Sys.remove (path ^ ".history")

  let test_history_cap () =
    let h = History.TaskHistory.create 5 in
    for i = 0 to 7 do
      ignore (History.TaskHistory.push h [{ (Task.create ~description:"h" ()) with id = string_of_int i }])
    done;
    check (list string) "newest six retained" ["7"; "6"; "5"; "4"; "3"; "2"]
      (List.map (fun state -> (List.hd state).id) (History.TaskHistory.states h));
    for i = 6 downto 2 do
      let state, _ = History.TaskHistory.undo h in
      check string "one step undo" (string_of_int i) (List.hd (Option.get state)).id
    done;
    check bool "sixth undo refused" true (fst (History.TaskHistory.undo h) = None);
    check bool "redo available" true (Option.is_some (fst (History.TaskHistory.redo h)))

  let test_stale_history_ignored () =
    Atomic_write_tests.with_dir (fun dir ->
      let path = Filename.concat dir "tasks.md" in
      let m = FileTaskManager.create ~path () in
      let m, _ = Result.get_ok (FileTaskManager.add m (Task.create ~description:"first" ())) in
      Atomic_write.hook := (fun event target ->
        if event = Atomic_write.Before_rename && target = path ^ ".history" then raise Exit);
      let result = Fun.protect ~finally:(fun () -> Atomic_write.hook := (fun _ _ -> ()))
        (fun () -> FileTaskManager.add m (Task.create ~description:"second" ())) in
      (match result with
       | Error reason -> check bool "partial success reported" true
           (String.starts_with ~prefix:"Task file saved, but history was not saved" reason)
       | Ok _ -> fail "sidecar failure was not reported");
      check int "both tasks saved" 2 (List.length (Markdown.parse_file path));
      let reloaded = FileTaskManager.create ~path () in
      check bool "stale history not used" true (Result.is_error (FileTaskManager.undo reloaded));
      check int "reload retains tasks" 2 (List.length (FileTaskManager.get_tasks reloaded)))

  let test_history_boundaries () =
    let state i = [{ (Task.create ~description:"h" ()) with id = string_of_int i }] in
    check_raises "negative cap refused" (Invalid_argument "invalid history size")
      (fun () -> ignore (History.TaskHistory.create (-1)));
    let zero = History.TaskHistory.create 0 in
    ignore (History.TaskHistory.push zero (state 0));
    ignore (History.TaskHistory.push zero (state 1));
    check bool "zero cap refuses undo" false (History.TaskHistory.can_undo zero);
    let restored = History.TaskHistory.restore ~max_size:2
      [state 5; state 4; state 3; state 2] [state 6; state 7] in
    check int "restore bounded" 3 (History.TaskHistory.size restored);
    check bool "excess redo discarded" false (History.TaskHistory.can_redo restored);
    ignore (History.TaskHistory.undo restored);
    ignore (History.TaskHistory.push restored (state 9));
    check bool "new change clears redo" false (History.TaskHistory.can_redo restored)

  let test_history_write_failure () =
    Atomic_write_tests.with_dir (fun dir ->
      let path = Filename.concat dir "tasks.md" in
      Unix.mkdir (path ^ ".history") 0o700;
      let m = FileTaskManager.create ~path () in
      let result = FileTaskManager.add m (Task.create ~description:"kept" ()) in
      check bool "sidecar failure reported" true (Result.is_error result);
      check int "task was saved" 1 (List.length (Markdown.parse_file path));
      Unix.rmdir (path ^ ".history");
      check bool "reload refuses undo" true
        (Result.is_error (FileTaskManager.undo (FileTaskManager.create ~path ()))))

  let test_repl_row_resolution () =
    Atomic_write_tests.with_dir (fun dir ->
      let path = Filename.concat dir "tasks.md" in
      let m = FileTaskManager.create ~path () in
      let add m id category =
        let task = { (Task.create ~description:id ~category:(Some category) ()) with id } in
        fst (Result.get_ok (FileTaskManager.add m task)) in
      let m = add m "first-id" "work" in
      let m = add m "second-id" "home" in
      let m = add m "third-id" "home" in
      let filtered = FileTaskManager.filter_by_category m "work" in
      check string "filtered row differs" "first-id" (List.hd filtered).id;
      let m, _ = Result.get_ok (Repl.execute_command m (Repl.Mark ("1", "Complete"))) in
      check bool "row uses unfiltered order" true
        ((Option.get (FileTaskManager.find_by_id m "third-id")).status = Complete);
      check bool "invalid status refused" true
        (Result.is_error (Repl.execute_command m (Repl.Mark ("1", "bogus"))));
      check bool "out of range refused" true
        (Result.is_error (Repl.execute_command m (Repl.Delete "99")));
      let m, _ = Result.get_ok (Repl.execute_command m (Repl.Delete "1")) in
      check bool "row deleted" true (FileTaskManager.find_by_id m "third-id" = None);
      let m, _ = Result.get_ok (Repl.execute_command m (Repl.Mark ("first-id", "Complete"))) in
      let m, _ = Result.get_ok (Repl.execute_command m (Repl.Delete "first-id")) in
      check bool "exact id deleted" true (FileTaskManager.find_by_id m "first-id" = None))

  let edit_fixture path =
    let task = { (Task.create ~description:"editable" ~category:(Some "Work")
                   ~priority:High ~due_date:(Some "2030-06-01T09:00:00Z")
                   ~complexity:C3 ~effort:E4 ~recurrence:(Some "daily") ()) with
                 id = "editable" } in
    let m = FileTaskManager.create ~path () in
    fst (Result.get_ok (FileTaskManager.add m task))

  let edited_task m =
    Option.get (FileTaskManager.find_by_id m "editable")

  let test_edit_readme_example_preserves_fields () =
    Atomic_write_tests.with_dir (fun dir ->
      let path = Filename.concat dir "tasks.md" in
      let m = edit_fixture path in
      let m, _ = Result.get_ok
          (Repl.execute_command m (Repl.Edit ("1", ["--priority"; "Medium"]))) in
      let task = edited_task m in
      check bool "priority updated" true (task.priority = Medium);
      check (option string) "due date preserved" (Some "2030-06-01T09:00:00Z") task.due_date;
      check (option string) "recurrence preserved" (Some "daily") task.recurrence;
      check (option string) "category preserved" (Some "Work") task.category;
      check bool "complexity preserved" true (task.complexity = C3);
      check bool "effort preserved" true (task.effort = E4))

  let test_edit_flags_change_only_named_field () =
    Atomic_write_tests.with_dir (fun dir ->
      let cases = [
        "category", ["--category"; "Home"], `Category;
        "priority", ["--priority"; "Low"], `Priority;
        "due", ["--due"; "2031-01-01T00:00:00Z"], `Due;
        "complexity", ["--complexity"; "5"], `Complexity;
        "effort", ["--effort"; "1"], `Effort;
        "recurrence", ["--recurring"; "weekly"], `Recurrence;
      ] in
      List.iter (fun (name, attrs, expected) ->
        let path = Filename.concat dir ("tasks-" ^ name ^ ".md") in
        let m = edit_fixture path in
        let m, _ = Result.get_ok (Repl.execute_command m (Repl.Edit ("1", attrs))) in
        let task = edited_task m in
        (match expected with
         | `Category -> check (option string) (name ^ " applied") (Some "Home") task.category
         | `Priority -> check bool (name ^ " applied") true (task.priority = Low)
         | `Due -> check (option string) (name ^ " applied")
             (Some "2031-01-01T00:00:00Z") task.due_date
         | `Complexity -> check bool (name ^ " applied") true (task.complexity = C5)
         | `Effort -> check bool (name ^ " applied") true (task.effort = E1)
         | `Recurrence -> check (option string) (name ^ " applied")
             (Some "weekly") task.recurrence);
        if name <> "category" then
          check (option string) (name ^ " preserves category") (Some "Work") task.category;
        if name <> "priority" then
          check bool (name ^ " preserves priority") true (task.priority = High);
        if name <> "due" then
          check (option string) (name ^ " preserves due") (Some "2030-06-01T09:00:00Z") task.due_date;
        if name <> "complexity" then
          check bool (name ^ " preserves complexity") true (task.complexity = C3);
        if name <> "effort" then
          check bool (name ^ " preserves effort") true (task.effort = E4);
        if name <> "recurrence" then
          check (option string) (name ^ " preserves recurrence") (Some "daily") task.recurrence)
        cases)

  let test_edit_clear_flags () =
    Atomic_write_tests.with_dir (fun dir ->
      let run name attrs =
        let path = Filename.concat dir ("tasks-" ^ name ^ ".md") in
        let m = edit_fixture path in
        let m, _ = Result.get_ok (Repl.execute_command m (Repl.Edit ("1", attrs))) in
        edited_task m in
      let due_cleared = run "clear-due" ["--clear-due"] in
      check (option string) "clear due clears due" None due_cleared.due_date;
      check (option string) "clear due keeps recurrence" (Some "daily") due_cleared.recurrence;
      let recurrence_cleared = run "clear-recurring" ["--clear-recurring"] in
      check (option string) "clear recurring keeps due"
        (Some "2030-06-01T09:00:00Z") recurrence_cleared.due_date;
      check (option string) "clear recurring clears recurrence" None recurrence_cleared.recurrence;
      let both = run "clear-both"
          ["--clear-due"; "--clear-recurring"; "--priority"; "Low"] in
      check (option string) "combined clear due" None both.due_date;
      check (option string) "combined clear recurrence" None both.recurrence;
      check bool "combined update applies" true (both.priority = Low);
      check (option string) "combined clear keeps category" (Some "Work") both.category)

  let test_edit_rejects_invalid_provided_values () =
    Atomic_write_tests.with_dir (fun dir ->
      let path = Filename.concat dir "tasks.md" in
      let m = edit_fixture path in
      let original = In_channel.with_open_bin path In_channel.input_all in
      (match Repl.execute_command m (Repl.Edit ("1", ["--due"; "garbage"])) with
       | Error _ -> ()
       | Ok _ -> fail "invalid due date accepted");
      (match Repl.execute_command m (Repl.Edit ("1", ["--priority"; "Ultra"])) with
       | Error _ -> ()
       | Ok _ -> fail "invalid priority accepted");
      check string "rejected edits leave the file unchanged" original
        (In_channel.with_open_bin path In_channel.input_all);
      let m, _ = Result.get_ok
          (Repl.execute_command m (Repl.Edit ("1", ["--category"; "Home"]))) in
      let task = edited_task m in
      check (option string) "category fallback updated" (Some "Home") task.category;
      check bool "priority fallback preserved" true (task.priority = High);
      check bool "complexity fallback preserved" true (task.complexity = C3);
      check bool "effort fallback preserved" true (task.effort = E4);
      check (option string) "provided-edit keeps due" (Some "2030-06-01T09:00:00Z") task.due_date;
      check (option string) "provided-edit keeps recurrence" (Some "daily") task.recurrence)

  let test_invalid_completed_refuses_writes_and_archive () =
    Atomic_write_tests.with_dir (fun dir ->
      let path = Filename.concat dir "tasks.md" in
      let archive = Filename.concat dir "archive.md" in
      let original =
        "### valid\n- **ID**: valid\n- **Description**: valid\n\
         - **Created**: 2026-01-01T00:00:00Z\n\
         - **Updated**: 2026-01-01T00:00:00Z\n\n\
         ### bad\n- **ID**: bad\n- **Description**: bad\n\
         - **Created**: 2026-01-01T00:00:00Z\n\
         - **Updated**: 2026-01-01T00:00:00Z\n\
         - **Status**: Complete\n- **Completed**: garbage\n" in
      Out_channel.with_open_bin path (fun oc -> Out_channel.output_string oc original);
      let m = FileTaskManager.create ~path () in
      let contains haystack needle =
        let len = String.length needle in
        let rec loop i =
          i + len <= String.length haystack &&
          (String.sub haystack i len = needle || loop (i + 1)) in
        loop 0 in
      (match FileTaskManager.mark m "valid" Complete with
       | Error reason ->
           List.iter (fun part ->
             check bool ("reason includes " ^ part) true (contains reason part))
             ["Read-only:"; "bad"; "Completed"; "garbage"; "13"]
       | Ok _ -> fail "write accepted an invalid Completed attribute");
      (match FileTaskManager.archive_completed m with
       | Error reason -> check bool "archive refused" true
           (String.starts_with ~prefix:"Read-only:" reason)
       | Ok _ -> fail "archive accepted an invalid Completed attribute");
      check string "task file byte-identical" original
        (In_channel.with_open_bin path In_channel.input_all);
      check bool "no archive file created" false (Sys.file_exists archive))

  let test_manual_archive_documented () =
    check bool "manual archive documented" true
      (readme_contains "Archive completed tasks with `mark-task archive`")

  let test_display_filter_documented () =
    check bool "display filtering documented" true
      (readme_contains "Performance modes filter the task list display")

  let test_help_file_exception_documented () =
    check bool "help file exception documented" true
      (readme_contains "Every command except `help` accepts `--file PATH`")

  let test_nothing_to_undo_on_fresh_file () =
    let path = fresh_path () in
    let m = FileTaskManager.create ~path () in
    check bool "fresh file has nothing to undo"
      (Result.is_error (FileTaskManager.undo m)) true;
    if Sys.file_exists path then Sys.remove path;
    if Sys.file_exists (path ^ ".history") then Sys.remove (path ^ ".history")

  let test_grammar_unknown_line_refuses_writes () =
    Atomic_write_tests.with_dir (fun dir ->
      let path = Filename.concat dir "tasks.md" in
      let original =
        "### legacy\n- **ID**: legacy\n- **Description**: legacy\n\
         - **Created**: 2026-01-01T00:00:00Z\n\
         - **Updated**: 2026-01-01T00:00:00Z\n\
         - **Tags**: alpha, beta\n" in
      Out_channel.with_open_bin path (fun oc -> Out_channel.output_string oc original);
      let m = FileTaskManager.create ~path () in
      (match FileTaskManager.mark m "legacy" Complete with
       | Error reason ->
           check bool "read-only names the refusal" true
             (String.length reason > 0 && String.starts_with ~prefix:"Read-only:" reason)
       | Ok _ -> fail "write accepted an unrecognised record line");
      check string "refused write leaves the file byte-identical" original
        (In_channel.with_open_bin path In_channel.input_all);
      check bool "no history written" false (Sys.file_exists (path ^ ".history")))

  let contains haystack needle =
    let len = String.length needle in
    let rec search i =
      i + len <= String.length haystack &&
      (String.sub haystack i len = needle || search (i + 1)) in
    search 0

  let test_truth_due_date_example_runs () =
    check bool "README example uses an accepted timestamp" true
      (readme_contains
         "add Stuff to do, Work, high, 2026-06-02T09:00:00Z, 1, 1, weekly");
    Atomic_write_tests.with_dir (fun dir ->
      let path = Filename.concat dir "tasks.md" in
      let m = FileTaskManager.create ~path () in
      let m, _ = Result.get_ok
          (Repl.execute_command m
             (Repl.Add
                ("Stuff to do, Work, high, 2026-06-02T09:00:00Z, 1, 1, weekly", []))) in
      let task = List.hd (FileTaskManager.get_tasks m) in
      check (option string) "README due date accepted"
        (Some "2026-06-02T09:00:00Z") task.due_date;
      check (option string) "README category stored as written"
        (Some "Work") task.category)

  let test_truth_help_contracts () =
    check bool "help documents exact category matching" true
      (contains Repl.help_text "Category filters match the stored text exactly");
    check bool "help no longer claims case-insensitive categories" false
      (contains Repl.help_text "case-insensitive")

  let test_truth_edit_preserves_category_text () =
    Atomic_write_tests.with_dir (fun dir ->
      let path = Filename.concat dir "tasks.md" in
      let m = edit_fixture path in
      let m, _ = Result.get_ok
          (Repl.execute_command m (Repl.Edit ("1", ["--category"; "WORK"]))) in
      check (option string) "stored value preserved" (Some "WORK")
        (edited_task m).category;
      check int "exact filter finds it" 1
        (List.length (FileTaskManager.filter_by_category m "WORK"));
      check int "lower-case filter is a different value" 0
        (List.length (FileTaskManager.filter_by_category m "work")))

  let test_truth_add_preserves_category_text () =
    Atomic_write_tests.with_dir (fun dir ->
      let path = Filename.concat dir "tasks.md" in
      let m = FileTaskManager.create ~path () in
      let m, _ = Result.get_ok
          (Repl.execute_command m (Repl.Add ("thing, Work", []))) in
      check (option string) "REPL add stores the category as written"
        (Some "Work") (List.hd (FileTaskManager.get_tasks m)).category)

  let tests = [
    "truth_due_date_example_runs", `Quick, test_truth_due_date_example_runs;
    "truth_help_contracts", `Quick, test_truth_help_contracts;
    "truth_edit_preserves_category_text", `Quick, test_truth_edit_preserves_category_text;
    "truth_add_preserves_category_text", `Quick, test_truth_add_preserves_category_text;
    "grammar_unknown_line_refuses_writes", `Quick, test_grammar_unknown_line_refuses_writes;
    "empty_backticks_cannot_self_lock", `Quick, test_empty_backticks_cannot_self_lock;
    "mixed_file_refuses_write", `Quick, test_mixed_file_refuses_write;
    "read_only_and_reload", `Quick, test_read_only_and_reload;
    "add_undo_redo_across_processes", `Quick, test_add_undo_redo_across_processes;
    "controlled_repeat", `Quick, test_controlled_repeat;
    "controlled_recompletion", `Quick, test_controlled_recompletion;
    "controlled_aged_archive", `Quick, test_controlled_aged_archive;
    "controlled_completion_and_archive", `Quick, test_controlled_completion_and_archive;
    "completion_transition", `Quick, test_completion_transition;
    "completion_consumers", `Quick, test_completion_consumers;
    "identifier_history_and_display", `Quick, test_identifier_history_and_display;
    "history_cap", `Quick, test_history_cap;
    "history_write_failure", `Quick, test_history_write_failure;
    "history_boundaries", `Quick, test_history_boundaries;
    "stale_history_ignored", `Quick, test_stale_history_ignored;
    "repl_row_resolution", `Quick, test_repl_row_resolution;
    "edit_readme_example_preserves_fields", `Quick, test_edit_readme_example_preserves_fields;
    "edit_flags_change_only_named_field", `Quick, test_edit_flags_change_only_named_field;
    "edit_clear_flags", `Quick, test_edit_clear_flags;
    "edit_rejects_invalid_provided_values", `Quick, test_edit_rejects_invalid_provided_values;
    "invalid_completed_refuses_writes_and_archive", `Quick, test_invalid_completed_refuses_writes_and_archive;
    "manual_archive_documented", `Quick, test_manual_archive_documented;
    "display_filter_documented", `Quick, test_display_filter_documented;
    "help_file_exception_documented", `Quick, test_help_file_exception_documented;
    "nothing_to_undo_on_fresh_file", `Quick, test_nothing_to_undo_on_fresh_file;
  ]
end

module Recurring_tests = struct
  open Alcotest

  let task due completed recurrence =
    { (Task.create ~description:"Recurring" ~due_date:due
         ~recurrence:(Some recurrence) ()) with
      status = Complete; completed_at = completed }

  let test_next_due () =
    let t = task (Some "2026-01-01T12:00:00Z")
        (Some "2026-01-03T12:00:00Z") "daily" in
    match Recurring.regenerate_if_needed t with
    | None -> fail "expected next occurrence"
    | Some next -> check (option string) "skip missed dates"
        (Some "2026-01-04T12:00:00Z") next.due_date

  let test_regeneration_lands_on_due_grid () =
    let cases = [
      "2026-01-03T13:00:00Z", "2026-01-04T12:00:00Z";
      "2025-12-31T12:00:00Z", "2026-01-02T12:00:00Z"
    ] in
    List.iter (fun (completed, expected) ->
      let t = task (Some "2026-01-01T12:00:00Z") (Some completed) "daily" in
      match Recurring.regenerate_if_needed t with
      | None -> fail "expected occurrence"
      | Some next -> check (option string) "due grid" (Some expected) next.due_date) cases

  let test_everyn_due_anchored_creation_resets () =
    let t = { (task (Some "2026-01-01T12:00:00Z")
                 (Some "2026-01-04T13:00:00Z") "every 2 days") with
              created_at = "2025-12-20T09:00:00Z" } in
    match Recurring.regenerate_if_needed t with
    | None -> fail "expected occurrence"
    | Some next ->
      check (option string) "due anchored to original due" (Some "2026-01-05T12:00:00Z") next.due_date;
      check bool "creation time resets" true (next.created_at <> t.created_at)

  let test_recurrence_parse_refusals () =
    let cases = [
      "weekly monday", "ambiguous";
      "mon,wed,fri", "multi-day schedules are not supported"
    ] in
    List.iter (fun (text, reason) ->
      let contains s part =
        let rec search i = i + String.length part <= String.length s &&
          (String.sub s i (String.length part) = part || search (i + 1)) in
        search 0 in
      match Validation.validate_task "task" ~recurrence:text () with
      | Ok () -> fail ("accepted " ^ text)
      | Error error ->
        let message = Validation.string_of_error error in
        check bool "offending text" true (contains message text);
        check bool "reason" true (contains message reason)) cases;
    check bool "every day alias" true (Recurring.parse_recurrence "every day" = Recurring.Daily)

  let test_recurrence_parse_and_load () =
    List.iter (fun text ->
      let refused = try ignore (Recurring.parse_recurrence text); false
        with Invalid_argument _ -> true in
      check bool text true refused;
      let doc = "### task\n- **ID**: task\n- **Description**: task\n\
                 - **Created**: 2026-01-01T00:00:00Z\n\
                 - **Updated**: 2026-01-01T00:00:00Z\n\
                 - **Recurrence**: " ^ text ^ "\n" in
      let tasks, warnings = Markdown.parse_document_with_rejections doc in
      check int "invalid recurrence not loaded" 0 (List.length tasks);
      check int "invalid recurrence reported" 1 (List.length warnings))
      ["weekly monday"; "mon,wed,fri"; "monday wednesday"; "every 0 days"];
    check bool "single day accepted" true
      (Recurring.parse_recurrence "monday" = Recurring.Weekly [1])

  let test_time_base_mixing () =
    let parse text = match Ptime.of_rfc3339 text with
      | Ok (value, _, _) -> value | Error _ -> fail "invalid fixture" in
    let cases = [
      "2026-01-01T23:30:00Z", 1, false;
      "2026-01-02T00:30:00Z", 2, true;
      "2026-01-02T04:30:00Z", 2, true;
      "2026-01-02T05:30:00Z", 2, true
    ] in
    let t = { (task (Some "2026-01-01T23:30:00Z") None "friday") with
              created_at = "2026-01-01T23:30:00Z" } in
    List.iter (fun (text, day, expected) ->
      let instant = parse text in
      let seconds = Ptime.to_float_s instant in
      let local = Unix.localtime seconds in
      let stamped = Task.now_str ~clock:(fun () -> seconds) () in
      let (_, _, stamped_day) = Ptime.to_date (parse stamped) in
      check int "now_str calendar day" day stamped_day;
      check bool "Friday in UTC" expected (Recurring.should_regenerate ~today:instant t);
      if text = "2026-01-02T00:30:00Z" &&
         Sys.getenv_opt "TZ" = Some "America/New_York" then
        check int "New York calendar day differs" 1 local.Unix.tm_mday) cases;
    let before = Unix.gettimeofday () in
    let created = Task.create ~description:"Clock" () in
    let after = Unix.gettimeofday () in
    let instant = Ptime.to_float_s (parse created.created_at) in
    check bool "now_str UTC instant within clock interval" true
      (instant >= floor before && instant <= after)

  let test_missing_timestamp () =
    let t = task None None "daily" in
    check bool "retain completed task" true
      (Recurring.process_recurring_tasks [t] = [t])

  let test_cadence () =
    let cases = [
      "daily", "2026-01-01T12:00:00Z", "2026-01-03T13:00:00Z", "2026-01-04T12:00:00Z";
      "every 2 days", "2026-01-01T12:00:00Z", "2026-01-03T13:00:00Z", "2026-01-05T12:00:00Z";
      "weekly", "2026-01-01T12:00:00Z", "2026-01-15T12:00:00Z", "2026-01-22T12:00:00Z";
    ] in
    List.iter (fun (pattern, due, completed, expected) ->
      let t = task (Some due) (Some completed) pattern in
      match Recurring.regenerate_if_needed t with
      | None -> fail "expected occurrence"
      | Some next ->
        check (option string) pattern (Some expected) next.due_date;
        check bool "new UUID" true (next.id <> t.id)) cases

  let test_calendar () =
    let cases = [
      "monthly", "2026-01-31T12:00:00Z", "2026-01-31T12:00:00Z", "2026-03-31T12:00:00Z";
      "yearly", "2024-02-29T12:00:00Z", "2024-02-29T12:00:00Z", "2028-02-29T12:00:00Z";
      "monthly", "2026-01-31T12:00:00.123400Z", "2026-01-31T12:00:00Z", "2026-03-31T12:00:00.1234Z";
      "daily", "2026-01-01T12:00:00.000000000001Z", "2026-01-01T12:00:00Z", "2026-01-02T12:00:00.000000000001Z";
    ] in
    List.iter (fun (pattern, due, completed, expected) ->
      let t = task (Some due) (Some completed) pattern in
      match Recurring.regenerate_if_needed t with
      | None -> fail pattern
      | Some next -> check (option string) pattern (Some expected) next.due_date) cases

  let tests = ["regeneration_lands_on_due_grid", `Quick, test_regeneration_lands_on_due_grid;
               "everyn_due_anchored_creation_resets", `Quick, test_everyn_due_anchored_creation_resets;
               "recurrence_parse_refusals", `Quick, test_recurrence_parse_refusals;
               "recurrence_parse_and_load", `Quick, test_recurrence_parse_and_load;
               "time_base_mixing", `Quick, test_time_base_mixing;
               "next_due", `Quick, test_next_due;
               "missing_timestamp", `Quick, test_missing_timestamp;
               "cadence", `Quick, test_cadence;
               "calendar", `Quick, test_calendar]
end

module Notification_tests = struct
  open Alcotest

  let capture f =
    let path = Filename.temp_file "notifications" ".out" in
    let saved = Unix.dup Unix.stdout in
    let fd = Unix.openfile path [Unix.O_WRONLY; Unix.O_TRUNC] 0o600 in
    Unix.dup2 fd Unix.stdout; Unix.close fd;
    Fun.protect ~finally:(fun () -> flush stdout; Unix.dup2 saved Unix.stdout;
      Unix.close saved) f;
    let output = In_channel.with_open_bin path In_channel.input_all in
    Sys.remove path; output

  let test_repeated_load () =
    let path = Filename.temp_file "notification-tasks" ".md" in
    let t = Task.create ~description:"Overdue" ~due_date:(Some "2020-01-01T00:00:00Z") () in
    Markdown.write_file path [t];
    let first = capture (fun () -> ignore (FileTaskManager.create ~path ())) in
    let second = capture (fun () -> ignore (FileTaskManager.create ~path ())) in
    check bool "first delivery" true (String.length first > 0);
    check string "no repeated delivery" "" second;
    Sys.remove path;
    Sys.remove (path ^ ".notifications")

  let test_identity_and_failure () =
    let path = Filename.temp_file "notification-state" ".md" in
    let parse s = match Ptime.of_rfc3339 s with Ok (t, _, _) -> t | Error _ -> assert false in
    let due = "2026-01-02T12:00:00Z" in
    let t = Task.create ~description:"Task" ~due_date:(Some due) () in
    let count = ref 0 in
    let send _ _ _ = incr count; true in
    let check_at now task = Notifications.check_due_tasks ~path ~now:(parse now) ~send default_config [task] in
    ignore (Result.get_ok (check_at "2026-01-02T11:00:00Z" t));
    ignore (Result.get_ok (check_at "2026-01-02T11:00:00Z" t));
    check int "reopen retains due-soon" 1 !count;
    ignore (Result.get_ok (check_at "2026-01-02T13:00:00Z" t));
    check int "transition to overdue" 2 !count;
    ignore (Result.get_ok (check_at "2026-01-02T13:00:00Z" {t with status = Complete}));
    check int "completed suppressed" 2 !count;
    ignore (Result.get_ok (check_at "2026-01-02T13:00:00Z" {t with due_date = Some "2026-01-02T11:30:00Z"}));
    check int "changed due" 3 !count;
    let failed = {t with id = "failed"} in
    let send_fail _ _ _ = incr count; false in
    ignore (Result.get_ok (Notifications.check_due_tasks ~path ~now:(parse "2026-01-02T13:00:00Z") ~send:send_fail default_config [failed]));
    ignore (Result.get_ok (check_at "2026-01-02T13:00:00Z" failed));
    check int "failed delivery retried" 5 !count;
    Sys.remove (path ^ ".notifications"); Sys.remove path;
    let bad_path = path ^ "/missing" in
    check bool "persistence failure reported" true (Result.is_error
      (Notifications.check_due_tasks ~path:bad_path ~now:(parse "2026-01-02T13:00:00Z") ~send default_config [t]));
    check int "persistence failure suppresses delivery" 5 !count

  let test_pending_persisted_and_cleared () =
    let path = Filename.temp_file "notification-pending" ".md" in
    let t = Task.create ~description:"Task" ~due_date:(Some "2020-01-01T00:00:00Z") () in
    let observed = ref false in
    let send _ _ _ =
      observed := (Yojson.Safe.from_file (path ^ ".notifications") <> `List []);
      false in
    ignore (Result.get_ok (Notifications.check_due_tasks ~path ~send default_config [t]));
    check bool "pending exists before delivery" true !observed;
    check bool "failed delivery retains pending" true
      (Yojson.Safe.from_file (path ^ ".notifications") <> `List []);
    ignore (Result.get_ok (Notifications.check_due_tasks ~path default_config [{t with status = Complete}]));
    check bool "completion clears pending" true
      (Yojson.Safe.from_file (path ^ ".notifications") = `List []);
    Sys.remove (path ^ ".notifications"); Sys.remove path

  let test_superseded_pending () =
    let path = Filename.temp_file "notification-superseded" ".md" in
    let parse s = match Ptime.of_rfc3339 s with Ok (t, _, _) -> t | Error _ -> assert false in
    let due = "2026-01-02T12:00:00Z" in
    let t = Task.create ~description:"Task" ~due_date:(Some due) () in
    let titles = ref [] in
    let send _ title _ = titles := title :: !titles; false in
    let run now = Result.get_ok (Notifications.check_due_tasks ~path ~now:(parse now) ~send default_config [t]) in
    ignore (run "2026-01-02T11:00:00Z");
    titles := [];
    ignore (run "2026-01-02T13:00:00Z");
    check (list string) "only overdue attempted" ["Mark Task: OVERDUE"] !titles;
    let key = `List [`String t.id; `String (Task.format_timestamp (parse due)); `String "overdue"] in
    check bool "only overdue remains pending" true
      (Yojson.Safe.from_file (path ^ ".notifications") = `List [`Assoc [("pending", key)]]);
    Sys.remove (path ^ ".notifications"); Sys.remove path

  let test_both_partial_failure () =
    let path = Filename.temp_file "notification-channels" ".md" in
    let task = Task.create ~description:"Task" ~due_date:(Some "2020-01-01T00:00:00Z") () in
    let system = ref 0 and terminal = ref 0 in
    let send mode _ _ =
      let send_system () = incr system; !system > 1 in
      let send_terminal () = incr terminal; true in
      match mode with
      | System -> send_system ()
      | Terminal -> send_terminal ()
      | Both -> let s = send_system () in let t = send_terminal () in s && t in
    let config = {default_config with notification_mode = Both} in
    Fun.protect ~finally:(fun () ->
      if Sys.file_exists (path ^ ".notifications") then Sys.remove (path ^ ".notifications");
      Sys.remove path) (fun () ->
      ignore (Result.get_ok (Notifications.check_due_tasks ~path ~send config [task]));
      check int "first terminal delivery" 1 !terminal;
      ignore (Result.get_ok (Notifications.check_due_tasks ~path ~send config [task]));
      check int "failed system retried" 2 !system;
      check int "delivered terminal not repeated" 1 !terminal;
      ignore (Result.get_ok (Notifications.check_due_tasks ~path ~send config [task]));
      check int "successful system not repeated" 2 !system;
      check int "terminal remains once" 1 !terminal)

  let test_fractional_due_identity () =
    let path = Filename.temp_file "notification-fractions" ".md" in
    let t = Task.create ~description:"Task" ~due_date:(Some "2020-01-01T00:00:00.1Z") () in
    let count = ref 0 in
    let send _ _ _ = incr count; true in
    ignore (Result.get_ok (Notifications.check_due_tasks ~path ~send default_config [t]));
    ignore (Result.get_ok (Notifications.check_due_tasks ~path ~send default_config
      [{t with due_date = Some "2020-01-01T00:00:00.2Z"}]));
    check int "distinct fractional due timestamps" 2 !count;
    Sys.remove (path ^ ".notifications"); Sys.remove path

  let tests = ["both_partial_failure", `Quick, test_both_partial_failure;
               "repeated_load", `Quick, test_repeated_load;
               "identity_and_failure", `Quick, test_identity_and_failure;
               "pending_persisted_and_cleared", `Quick, test_pending_persisted_and_cleared;
               "superseded_pending", `Quick, test_superseded_pending;
               "fractional_due_identity", `Quick, test_fractional_due_identity]
end

let () = 
  let open Alcotest in
  run "Mark Task" [
    "Task", Task_tests.tests;
    "Config", Config_tests.tests;
    "Markdown", Markdown_tests.tests;
    "Storage", Storage_tests.tests;
    "Atomic_write", Atomic_write_tests.tests;
    "Validation", Validation_tests.tests;
    "Task_manager", Task_manager_tests.tests;
    "Recurring", Recurring_tests.tests;
    "Notification", Notification_tests.tests;
  ]
