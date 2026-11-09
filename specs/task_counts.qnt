module TaskCounts {
  type TaskID = str
  type Description = str
  type DueDate = str
  type Status = Backlog | InProgress | OnHold | Complete | Idea
  type Task = {
    id: TaskID,
    description: Description,
    dueDate: DueDate,
    status: Status
  }

  var tasks: Set[Task]
  var dailyCompletionCount: int
  var lastResetTime: DueDate
  var currentTime: DueDate
  var inputTaskID: TaskID
  var inputDescription: Description

  action init = all {
    tasks' = Set(),
    dailyCompletionCount' = 0,
    lastResetTime' = "2023-01-01T00:00:00",
    currentTime' = "2023-01-01T00:00:00",
    inputTaskID' = "",
    inputDescription' = ""
  }

  action addTask = all {
    inputTaskID != "",
    tasks' = tasks.union(Set({
      id: inputTaskID,
      description: inputDescription,
      dueDate: "",
      status: Backlog
    })),
    dailyCompletionCount' = dailyCompletionCount,
    lastResetTime' = lastResetTime,
    currentTime' = currentTime,
    inputTaskID' = "",
    inputDescription' = ""
  }

  // Only a task that exists and is not already complete counts
  action markTaskComplete = all {
    inputTaskID != "",
    exists(tasks, t => t.id == inputTaskID and t.status != Complete),
    tasks' = map(tasks, t =>
      if (t.id == inputTaskID)
        { id: t.id, description: t.description, dueDate: t.dueDate, status: Complete }
        else t),
    dailyCompletionCount' = dailyCompletionCount + 1,
    lastResetTime' = lastResetTime,
    currentTime' = currentTime,
    inputTaskID' = "",
    inputDescription' = inputDescription
  }

  action resetDailyCompletionCount = all {
    currentTime != lastResetTime,
    tasks' = tasks,
    dailyCompletionCount' = 0,
    lastResetTime' = currentTime,
    currentTime' = currentTime,
    inputTaskID' = inputTaskID,
    inputDescription' = inputDescription
  }

  action step = any { addTask, markTaskComplete, resetDailyCompletionCount }

  val nonNegativeCount = dailyCompletionCount >= 0
  // Each task can be completed at most once, so the count is bounded
  val countBoundedByTasks = dailyCompletionCount <= size(tasks)
  val inv = and {
    nonNegativeCount,
    countBoundedByTasks
  }
}
