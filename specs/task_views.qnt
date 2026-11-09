module TaskViews {
  type TaskID = str
  type Description = str
  type Category = str
  type Status = Backlog | InProgress | OnHold | Complete | Idea
  type Priority = Low | Medium | High
  type Task = {
    id: TaskID,
    description: Description,
    category: Category,
    status: Status,
    priority: Priority
  }

  var tasks: Set[Task]
  var viewTasks: Set[Task]
  var inputTaskID: TaskID
  var inputDescription: Description
  var inputCategory: Category
  var inputStatus: Status
  var inputPriority: Priority

  action init = all {
    tasks' = Set(),
    viewTasks' = Set(),
    inputTaskID' = "",
    inputDescription' = "",
    inputCategory' = "",
    inputStatus' = Backlog,
    inputPriority' = Low
  }

  action addTask = all {
    inputTaskID != "",
    inputDescription != "",
    tasks' = tasks.union(Set({
      id: inputTaskID,
      description: inputDescription,
      category: inputCategory,
      status: inputStatus,
      priority: inputPriority
    })),
    viewTasks' = viewTasks,
    inputTaskID' = "",
    inputDescription' = "",
    inputCategory' = "",
    inputStatus' = inputStatus,
    inputPriority' = inputPriority
  }

  action filterByCategory = all {
    inputCategory != "",
    tasks' = tasks,
    viewTasks' = filter(tasks, t => t.category == inputCategory),
    inputTaskID' = inputTaskID,
    inputDescription' = inputDescription,
    inputCategory' = "",
    inputStatus' = inputStatus,
    inputPriority' = inputPriority
  }

  action filterByStatus = all {
    tasks' = tasks,
    viewTasks' = filter(tasks, t => t.status == inputStatus),
    inputTaskID' = inputTaskID,
    inputDescription' = inputDescription,
    inputCategory' = inputCategory,
    inputStatus' = inputStatus,
    inputPriority' = inputPriority
  }

  action filterByPriority = all {
    tasks' = tasks,
    viewTasks' = filter(tasks, t => t.priority == inputPriority),
    inputTaskID' = inputTaskID,
    inputDescription' = inputDescription,
    inputCategory' = inputCategory,
    inputStatus' = inputStatus,
    inputPriority' = inputPriority
  }

  action showAll = all {
    tasks' = tasks,
    viewTasks' = tasks,
    inputTaskID' = inputTaskID,
    inputDescription' = inputDescription,
    inputCategory' = inputCategory,
    inputStatus' = inputStatus,
    inputPriority' = inputPriority
  }

  action step = any { addTask, filterByCategory, filterByStatus, filterByPriority, showAll }

  val viewSubsetOfTasks = forall(viewTasks, v => exists(tasks, t => t == v))
  val inv = viewSubsetOfTasks
}
