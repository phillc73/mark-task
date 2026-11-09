module ValidationErrorHandling {
  type Record = { task: str, line: int }
  type Warning = { task: str, line: int, attribute: str }

  pure val records = Set(
    { task: "alpha", line: 1 },
    { task: "beta", line: 2 }
  )
  pure val attributes = Set("status", "priority")
  pure val possibleDefects = records.fold(Set(), (acc, r) =>
    acc.union(attributes.map(a => { task: r.task, line: r.line, attribute: a })))

  var defects: Set[Warning]
  var pending: Set[Record]
  var loaded: Set[Record]
  var rejected: Set[Record]
  var warnings: Set[Warning]

  pure def recordDefects(ds: Set[Warning], r: Record): Set[Warning] =
    ds.filter(d => d.task == r.task and d.line == r.line)

  action initWith(ds) = all {
    defects' = ds,
    pending' = records,
    loaded' = Set(),
    rejected' = Set(),
    warnings' = Set()
  }

  action init = {
    nondet ds = possibleDefects.powerset().oneOf()
    initWith(ds)
  }

  action process(r) = all {
    pending.contains(r),
    defects' = defects,
    pending' = pending.exclude(Set(r)),
    loaded' = if (recordDefects(defects, r) == Set()) loaded.union(Set(r)) else loaded,
    rejected' = if (recordDefects(defects, r) == Set()) rejected else rejected.union(Set(r)),
    warnings' = warnings.union(recordDefects(defects, r))
  }

  // Defect mutant: invalid records are loaded without warnings.
  action processMutant(r) = all {
    pending.contains(r),
    defects' = defects,
    pending' = pending.exclude(Set(r)),
    loaded' = loaded.union(Set(r)),
    rejected' = rejected,
    warnings' = warnings
  }

  action idle = all {
    pending == Set(),
    defects' = defects,
    pending' = pending,
    loaded' = loaded,
    rejected' = rejected,
    warnings' = warnings
  }

  action step = any {
    nondet r = records.oneOf()
    process(r),
    idle
  }

  action mutantStep = any {
    nondet r = records.oneOf()
    processMutant(r),
    idle
  }

  val inv = all {
    pending.union(loaded).union(rejected) == records,
    pending.intersect(loaded) == Set(),
    pending.intersect(rejected) == Set(),
    loaded.intersect(rejected) == Set(),
    loaded.forall(r => recordDefects(defects, r) == Set()),
    rejected.forall(r => recordDefects(defects, r) != Set()),
    warnings == defects.filter(d => rejected.contains({ task: d.task, line: d.line }))
  }

  run mixed = initWith(Set({ task: "beta", line: 2, attribute: "status" }))
    .then(process({ task: "beta", line: 2 }))
    .then(process({ task: "alpha", line: 1 }))
    .expect(all {
      assert(loaded == Set({ task: "alpha", line: 1 })),
      assert(rejected == Set({ task: "beta", line: 2 })),
      assert(warnings == Set({ task: "beta", line: 2, attribute: "status" })),
      assert(inv)
    })

  run allInvalid = initWith(Set(
      { task: "alpha", line: 1, attribute: "priority" },
      { task: "beta", line: 2, attribute: "status" }
    ))
    .then(process({ task: "alpha", line: 1 }))
    .then(process({ task: "beta", line: 2 }))
    .expect(all {
      assert(loaded == Set()),
      assert(rejected == records),
      assert(warnings == defects),
      assert(inv)
    })

  run mutantWitness = initWith(Set({ task: "beta", line: 2, attribute: "status" }))
    .then(processMutant({ task: "beta", line: 2 }))
    .expect(all {
      assert(loaded.contains({ task: "beta", line: 2 })),
      assert(not(inv))
    })
}

module DescriptionValidation {
  type Task = { id: str, description: str }
  pure val descriptions = Set("", "clean", "WITH_LF", "WITH_CR")
  pure def invalidDescription(description) = description == "WITH_LF" or description == "WITH_CR"
  pure def validDescription(description) = description != "" and not(invalidDescription(description))
  var tasks: Set[Task]
  var lastError: str
  var inputId: str
  var inputDescription: str

  action init = all {
    tasks' = Set(), lastError' = "", inputId' = "", inputDescription' = ""
  }

  action input(id, description) = all {
    tasks' = tasks, lastError' = lastError,
    inputId' = id, inputDescription' = description
  }

  action addTask = all {
    inputId != "", validDescription(inputDescription),
    tasks' = tasks.union(Set({ id: inputId, description: inputDescription })),
    lastError' = "", inputId' = "", inputDescription' = ""
  }

  action rejectInvalidDescription = all {
    not(validDescription(inputDescription)),
    tasks' = tasks, lastError' = (if (inputDescription == "") "empty description" else "newline description"),
    inputId' = inputId, inputDescription' = inputDescription
  }

  action clearError = all {
    lastError != "", tasks' = tasks, lastError' = "",
    inputId' = inputId, inputDescription' = inputDescription
  }

  action step = any {
    nondet id = Set("", "task").oneOf()
    nondet description = descriptions.oneOf()
    input(id, description), addTask, rejectInvalidDescription, clearError
  }

  val inv = tasks.forall(t => validDescription(t.description))

  run newlineDescription = init
    .then(input("task", "WITH_LF"))
    .then(rejectInvalidDescription)
    .expect(all { assert(tasks == Set()), assert(lastError == "newline description") })

  run carriageReturnDescription = init
    .then(input("task", "WITH_CR"))
    .then(rejectInvalidDescription)
    .expect(all { assert(tasks == Set()), assert(lastError == "newline description") })

  run emptyDescription = init
    .then(input("task", ""))
    .then(rejectInvalidDescription)
    .expect(all { assert(tasks == Set()), assert(lastError == "empty description"), assert(inv) })
    .then(clearError)
    .then(input("task", "clean"))
    .then(addTask)
    .expect(all { assert(tasks == Set({ id: "task", description: "clean" })), assert(lastError == ""), assert(inv) })
}

module ReadOnlyUntilClean {
  pure val operations = Set("save", "add", "delete", "update", "mark", "undo", "redo", "archive")
  var diskClean: bool
  var readOnly: bool
  var fileWrites: int
  var historyWrites: int
  var refused: bool
  var violation: bool

  action init = all {
    diskClean' = false, readOnly' = true, fileWrites' = 0,
    historyWrites' = 0, refused' = false, violation' = false
  }

  action repair = all {
    diskClean' = true, readOnly' = readOnly, fileWrites' = fileWrites,
    historyWrites' = historyWrites, refused' = refused, violation' = violation
  }

  action reload = all {
    diskClean' = diskClean, readOnly' = not(diskClean), fileWrites' = fileWrites,
    historyWrites' = historyWrites, refused' = false, violation' = violation
  }

  action write(operation) = all {
    operations.contains(operation), diskClean' = diskClean, readOnly' = readOnly,
    fileWrites' = if (readOnly) fileWrites else fileWrites + 1,
    historyWrites' = if (readOnly) historyWrites else historyWrites + 1,
    refused' = readOnly, violation' = (violation or (not(readOnly) and not(diskClean)))
  }

  action step = any {
    repair, reload, write("mark"), write("save")
  }

  val inv = all { not(violation), readOnly or diskClean }

  run readOnlyThenReload = init
    .then(write("mark"))
    .expect(all { assert(refused), assert(fileWrites == 0), assert(historyWrites == 0) })
    .then(repair)
    .then(write("save"))
    .expect(all { assert(refused), assert(fileWrites == 0), assert(historyWrites == 0) })
    .then(reload)
    .then(write("mark"))
    .expect(all { assert(not(refused)), assert(fileWrites == 1), assert(historyWrites == 1), assert(inv) })
}
