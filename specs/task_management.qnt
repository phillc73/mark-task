module TaskManagement {
  type TaskID = str
  type Description = str
  type Status = Backlog | InProgress | OnHold | Complete | Idea
  type Task = {
    id: TaskID,
    description: Description,
    status: Status
  }

  var tasks: Set[Task]
  var history: List[Set[Task]]
  var inputTaskID: TaskID
  var inputDescription: Description
  var inputStatus: Status

  action init = all {
    tasks' = Set(),
    history' = List(),
    inputTaskID' = "",
    inputDescription' = "",
    inputStatus' = Backlog
  }

  action addTask = all {
    inputTaskID != "",
    inputDescription != "",
    tasks' = tasks.union(Set({
      id: inputTaskID,
      description: inputDescription,
      status: inputStatus
    })),
    history' = append(history, tasks),
    inputTaskID' = "",
    inputDescription' = "",
    inputStatus' = Backlog
  }

  action deleteTask = all {
    inputTaskID != "",
    exists(tasks, t => t.id == inputTaskID),
    tasks' = filter(tasks, t => t.id != inputTaskID),
    history' = append(history, tasks),
    inputTaskID' = "",
    inputDescription' = inputDescription,
    inputStatus' = inputStatus
  }

  action markTask = all {
    inputTaskID != "",
    exists(tasks, t => t.id == inputTaskID),
    tasks' = map(tasks, t =>
      if (t.id == inputTaskID)
        { id: t.id, description: t.description, status: inputStatus }
        else t),
    history' = append(history, tasks),
    inputTaskID' = "",
    inputDescription' = inputDescription,
    inputStatus' = Backlog
  }

  action step = any { addTask, deleteTask, markTask }

  val idsNotEmpty = forall(tasks, t => t.id != "")
  val descriptionsNotEmpty = forall(tasks, t => t.description != "")
  val inv = and {
    idsNotEmpty,
    descriptionsNotEmpty
  }
}
