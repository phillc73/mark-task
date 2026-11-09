module TaskArchiving {
  type TaskID = str
  type Description = str
  type DueDate = str
  type Status = Backlog | InProgress | OnHold | Complete | Idea
  type Task = {
    id: TaskID,
    description: Description,
    dueDate: DueDate,
    status: Status,
    completedAt: int
  }

  var tasks: Set[Task]
  var archivedTasks: Set[Task]
  var currentTime: int
  pure val archiveHours = 72
  pure def eligible(t, now) = t.status == Complete and t.completedAt >= 0 and now - t.completedAt >= archiveHours
  var inputTaskID: TaskID
  var inputDescription: Description
  var inputDueDate: DueDate

  action init = all {
    tasks' = Set(),
    archivedTasks' = Set(),
    currentTime' = 100,
    inputTaskID' = "",
    inputDescription' = "",
    inputDueDate' = ""
  }

  action addTask = all {
    inputTaskID != "",
    inputDescription != "",
    tasks' = tasks.union(Set({
      id: inputTaskID,
      description: inputDescription,
      dueDate: inputDueDate,
      status: Backlog,
      completedAt: -1
    })),
    archivedTasks' = archivedTasks,
    currentTime' = currentTime,
    inputTaskID' = "",
    inputDescription' = "",
    inputDueDate' = ""
  }

  action markComplete = all {
    inputTaskID != "",
    exists(tasks, t => t.id == inputTaskID),
    tasks' = map(tasks, t =>
      if (t.id == inputTaskID)
        { id: t.id, description: t.description, dueDate: t.dueDate, status: Complete, completedAt: currentTime }
        else t),
    archivedTasks' = archivedTasks,
    currentTime' = currentTime,
    inputTaskID' = "",
    inputDescription' = inputDescription,
    inputDueDate' = inputDueDate
  }

  // Completed tasks move to the archive and leave the active list
  action autoArchive = all {
    filter(tasks, t => eligible(t, currentTime)) != Set(),
    archivedTasks' = union(archivedTasks, filter(tasks, t => eligible(t, currentTime))),
    tasks' = exclude(tasks, filter(tasks, t => eligible(t, currentTime))),
    currentTime' = currentTime,
    inputTaskID' = inputTaskID,
    inputDescription' = inputDescription,
    inputDueDate' = inputDueDate
  }

  action step = any { addTask, markComplete, autoArchive }

  action archiveFixture = all {
    tasks' = Set(
      { id: "old", description: "old", dueDate: "", status: Complete, completedAt: 28 },
      { id: "recent", description: "recent", dueDate: "", status: Complete, completedAt: 29 },
      { id: "reopened", description: "reopened", dueDate: "", status: Backlog, completedAt: 0 },
      { id: "missing", description: "missing", dueDate: "", status: Complete, completedAt: -1 }
    ),
    archivedTasks' = Set(), currentTime' = 100,
    inputTaskID' = "", inputDescription' = "", inputDueDate' = ""
  }

  run recentCompletedRemains = archiveFixture.then(autoArchive)
    .expect(all { assert(tasks.exists(t => t.id == "recent")), assert(tasks.exists(t => t.id == "reopened")), assert(tasks.exists(t => t.id == "missing")), assert(archivedTasks.exists(t => t.id == "old")), assert(archivedTasks.size() == 1) })

  val noTaskInBoth = forall(tasks, t => not(exists(archivedTasks, a => a == t)))
  val archivedWereComplete = forall(archivedTasks, t => eligible(t, currentTime))
  val inv = and {
    noTaskInBoth,
    archivedWereComplete
  }
}
