module ConflictResolution {
  type TaskID = str
  type Description = str
  type Status = Backlog | InProgress | OnHold | Complete | Idea

  type Task = {
    id: TaskID,
    description: Description,
    status: Status
  }

  type ConflictResolution = KeepMemory | KeepFile | None

  var tasks: Set[Task]
  var fileTasks: Set[Task]
  var conflictResolution: ConflictResolution
  var inputFileTasks: Set[Task]

  action init = all {
    tasks' = Set(),
    fileTasks' = Set(),
    conflictResolution' = None,
    inputFileTasks' = Set()
  }

  // Reloading a file that changed underneath us raises a conflict
  action loadFromFile = all {
    inputFileTasks != Set(),
    tasks' = tasks,
    fileTasks' = inputFileTasks,
    conflictResolution' = if (tasks != inputFileTasks) KeepMemory else None,
    inputFileTasks' = Set()
  }

  action chooseKeepFile = all {
    conflictResolution == KeepMemory,
    tasks' = tasks,
    fileTasks' = fileTasks,
    conflictResolution' = KeepFile,
    inputFileTasks' = inputFileTasks
  }

  action resolveKeepMemory = all {
    conflictResolution == KeepMemory,
    tasks' = tasks,
    fileTasks' = tasks,
    conflictResolution' = None,
    inputFileTasks' = inputFileTasks
  }

  action resolveKeepFile = all {
    conflictResolution == KeepFile,
    tasks' = fileTasks,
    fileTasks' = fileTasks,
    conflictResolution' = None,
    inputFileTasks' = inputFileTasks
  }

  action step = any { loadFromFile, chooseKeepFile, resolveKeepMemory, resolveKeepFile }

  val conflictResolutionValid =
    conflictResolution == KeepMemory or conflictResolution == KeepFile or
    conflictResolution == None
  // Once no conflict is pending, memory and file agree
  val noPendingConflict =
    (conflictResolution != None) or (tasks == fileTasks)
  val inv = and {
    conflictResolutionValid,
    noPendingConflict
  }
}
