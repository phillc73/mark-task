(lang dune 3.1)
(name mark-task)
(version 0.1.0)
(generate_opam_files true)

(package
  (name mark-task)
  (synopsis "CLI task manager with markdown storage")
  (description
    "Mark Task manages tasks in a CommonMark file with status, priority, \
     categories, due dates, complexity and effort ratings, recurring \
     tasks, search, undo/redo, and an interactive REPL.")
  (authors "phillc73")
  (maintainers "phillc73")
  (license AGPL-3.0-only)
  (homepage "https://github.com/phillc73/mark-task")
  (bug_reports "https://github.com/phillc73/mark-task/issues")
  (depends
    dune
    cmdliner
    yojson
    uuidm
    ptime
    odoc
    (alcotest :with-test)))
