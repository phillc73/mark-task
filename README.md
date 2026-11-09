# Mark Task

CLI task manager written in OCaml. Tasks live in a CommonMark markdown
file that is human-readable and hand-editable.

## Features

- Task CRUD with UUIDs, categories, priorities, due dates, and 1-5
  complexity and effort ratings
- Statuses: Backlog (default), In Progress, On Hold, Complete, Idea
- Search, filtering by category/status/priority
- Undo/redo of the last 5 operations, persisted across invocations
- Recurring tasks (daily, weekly, monthly, yearly, every N units)
- Desktop notifications for overdue and soon-due tasks
- Performance modes filter the task list display to the next 24/48/96
  hours; all tasks are loaded and overdue tasks remain visible in every
  mode
- Archive completed tasks with `mark-task archive`
- Interactive REPL with command aliases
- Present invalid input is rejected with an error, never silently
  coerced; absent optional attributes take their documented defaults

## Build

Requires OCaml 5 and dune 3.1+.

```sh
opam install cmdliner yojson uuidm ptime alcotest
dune build
```

## Usage

```sh
mark-task add "Complete project proposal" --category Work --priority High \
  --due 2026-12-31T23:59:59Z --complexity 3 --effort 4
mark-task list
mark-task list --category Work
mark-task edit 1 --priority Medium
mark-task mark 1 Complete
mark-task search proposal
mark-task undo
mark-task redo
mark-task archive
mark-task interactive
mark-task help
```

Row numbers in edit, mark and delete refer to the unfiltered task order.
A filtered list can show different row numbers; use the full task ID or an
unfiltered list when selecting a task.

`edit` changes only the fields you name. Omit a field to keep its current
value. Use `--clear-due` or `--clear-recurring` to remove a due date or a
recurrence. Category filters match the stored text exactly, and category
values are stored as written. Due dates are RFC 3339 timestamps
(e.g. `2026-12-31T23:59:59Z`).

Marking a task Complete records its completion time. Marking an already
complete task Complete preserves that time. Reopening retains the last
completion time; completing the reopened task records a new time.

Every command except `help` accepts `--file PATH` (or `-f`) to use a
different markdown file. The REPL supports the same commands plus
comma-separated add syntax:

```
add Stuff to do, Work, high, 2026-06-02T09:00:00Z, 1, 1, weekly
```

## Storage

Tasks are stored at `~/.local/share/mark-task/tasks.md` by default:

```markdown
# Mark Task

## Tasks

### <uuid>
- **ID**: `<uuid>`
- **Description**: Complete project proposal
- **Category**: Work
- **Status**: In Progress
- **Priority**: High
- **Due Date**: 2026-12-31T23:59:59Z
- **Complexity**: 3
- **Effort**: 4
- **Created**: 2026-01-01T09:00:00Z
- **Updated**: 2026-01-01T09:00:00Z
```

The record grammar is strict. A record is a `### ` header whose label is the
record's ID; a header label that differs from the record's `ID` is refused
with its line and both values. Inside a record, after optional indentation,
every non-blank line is one of the fields above in the form
`- **Field**: value`; the CommonMark bullets `*` and `+` are accepted and
rewritten as `-`, and a space or a tab may follow the bullet. `ID`,
`Description`, `Created` and `Updated` are required.
A leading UTF-8 BOM is stripped when the file is read. Surrounding whitespace
and a backtick wrapper around a value are Markdown decoration, not data: they
are removed when the value is read (reported as a `Note:`) and on input, so a
stored value is stable across save and load and a description that normalises
to an empty value is refused. Any other non-blank line is rejected with its
line number and text, and the file is read-only for writes until it is
repaired; valid records still load.

Field lines without a bullet (`**ID**: ...`) are refused. The refusal is
intended: every accepted field line has one canonical form, and such a file
loads as read-only instead of being silently rewritten on the next save.

Undo history is kept in a `tasks.md.history` sidecar next to the task
file. The `archive` command moves completed tasks older than the configured
threshold to `archive.md` in the same directory.

A save preserves the ID stored in the file, and backticks appended to an ID
are not removed automatically. Before repairing an affected file, back up the
task file, its history and notification sidecars and the archive. Compare each
ID with trusted records and check for collisions. If an ID is ambiguous, leave
it unchanged until it can be identified. History snapshots can contain
different ID versions and may restore them on undo or redo. Notification
markers can also use different IDs; changing an ID can cause another
notification. Review those sidecars before choosing whether to retain or
discard them. Discarding history removes undo and redo; discarding notification
state can cause repeat notifications.

## Configuration

JSON configuration at `~/.config/mark-task/config.yaml`:

```json
{
  "markdown_location": "~/.local/share/mark-task/tasks.md",
  "notification_mode": "terminal",
  "performance_mode": "all",
  "max_history_size": 5,
  "auto_archive_days": 3,
  "aliases": { "ls": "list" },
  "color_theme": { "status_backlog": "gray" }
}
```

- `notification_mode`: `system` (notify-send), `terminal`, or `both`.
  Notifications are retried per channel until delivery is recorded; a crash
  after sending can cause a duplicate.
- `performance_mode`: `all`, `next24h`, `next48h`, or `next96h`
- `max_history_size`: undo states to keep, `0..1000` (`0` disables undo; default `5`)
- `auto_archive_days`: completion-age threshold in days, `0..36500`
  (`0` archives every complete task; default `3`)
- `aliases`: REPL command shortcuts

A wrong-typed or out-of-range integer setting warns with the key, the value,
the accepted range and the default, and that key uses its default; the other
keys still apply.

Configuration keys are independent. A missing key uses its default. A
malformed file or a wrong-typed value produces a warning; that key falls
back to its default and the other keys still apply. A value beyond the
integer range fails JSON parsing and falls back for the whole file, with a
warning that names the file.

Duplicate configuration keys resolve first-wins. Duplicate attributes in a
task record resolve last-wins.

The archive command writes `tasks.md` and `archive.md` as two atomic files,
but the pair is not one atomic operation: a failure between the two writes
returns an error and can duplicate a record on retry. An abrupt kill can
leave an inert `.atomic-*` temporary file; the target file is never partial.

Exit codes are not part of the command contract.

## Tests

```sh
TZ=America/New_York dune runtest
```

## Formal specifications

Quint models live in `specs/`; see `specs/README.md` for the per-model
verdict and the commands. The models are instrument evidence, not a proof:
9 pass bounded instrument gates; 10 are unsound for scope (they model
interactions the product does not ship or lack their input environment) and
make no claim about the product. Verification is bounded at 3 steps.

## Release notes (v0.1.0)

- The record grammar is strict: a `###` label must equal the record ID;
  required fields are ID, Description, Created and Updated; a leading BOM
  is stripped; any other non-blank line is refused and the file is read-only
  until repaired.
- Field lines without a bullet are refused; the refusal is intended.
- `max_history_size` accepts 0..1000 (0 disables undo; default 5).
  `auto_archive_days` accepts 0..36500 (0 archives every complete task;
  default 3). Wrong-typed and out-of-range values warn and fall back per key.
- Accepted hand edits are normalised on the first save: bullet style,
  backtick wrappers and surrounding whitespace. Duplicate `## Tasks`
  headings are dropped, CRLF becomes LF, and a trailing blank line is
  removed. Writer output is a save/load fixpoint.
- Duplicate configuration keys are first-wins; duplicate task-record
  attributes are last-wins.
- Notifications are at-least-once: a crash between sending and recording
  delivery can duplicate a notification.
- Exit codes are not part of the command contract.

## Licence

AGPL-3.0-only. The full licence text is in `LICENSE`.

Task files and configuration you create with mark-task are your data and
are not covered by this licence.

## AI assistance

The code in this repository was written with the assistance of a large
language model and reviewed by its maintainer.
