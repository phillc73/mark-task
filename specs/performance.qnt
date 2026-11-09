module Performance {
  type TaskID = str
  // Hours from now until the task is due; negative means overdue
  type DueDate = int
  type PerformanceMode = All | Next24h | Next48h | Next96h
  type Task = {
    id: TaskID,
    dueDate: DueDate
  }

  var tasks: Set[Task]
  var loadedTasks: Set[Task]
  var performanceMode: PerformanceMode
  var inputMode: PerformanceMode
  var inputTaskID: TaskID
  var inputDueDate: DueDate

  action init = all {
    tasks' = Set(),
    loadedTasks' = Set(),
    performanceMode' = All,
    inputMode' = All,
    inputTaskID' = "",
    inputDueDate' = 0
  }

  def withinWindow(mode, t) =
    mode == All or
    (mode == Next24h and t.dueDate <= 24) or
    (mode == Next48h and t.dueDate <= 48) or
    (mode == Next96h and t.dueDate <= 96)

  action setPerformanceMode = all {
    tasks' = tasks,
    loadedTasks' = filter(tasks, t => withinWindow(inputMode, t)),
    performanceMode' = inputMode,
    inputMode' = All,
    inputTaskID' = inputTaskID,
    inputDueDate' = inputDueDate
  }

  action addTask = all {
    inputTaskID != "",
    tasks' = union(tasks, Set({ id: inputTaskID, dueDate: inputDueDate })),
    loadedTasks' = filter(union(tasks, Set({ id: inputTaskID, dueDate: inputDueDate })),
      t => withinWindow(performanceMode, t)),
    performanceMode' = performanceMode,
    inputMode' = inputMode,
    inputTaskID' = "",
    inputDueDate' = 0
  }

  action step = any { setPerformanceMode, addTask }

  val loadedSubset = forall(loadedTasks, l => exists(tasks, t => t == l))
  val loadedWithinWindow = forall(loadedTasks, t => withinWindow(performanceMode, t))
  val inv = and {
    loadedSubset,
    loadedWithinWindow
  }
}
