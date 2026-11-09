module UserInterface {
  type TaskID = str
  type Description = str
  type Status = Backlog | InProgress | OnHold | Complete | Idea
  type Priority = Low | Medium | High
  type Color = str
  type Task = {
    id: TaskID,
    description: Description,
    status: Status,
    priority: Priority
  }

  var colorTheme: str -> Color
  var tasks: Set[Task]
  var displayed: Set[TaskID]
  var inputTaskID: TaskID
  var inputDescription: Description

  action init = all {
    colorTheme' = Map(
      ("backlog", "gray"),
      ("inprogress", "yellow"),
      ("onhold", "blue"),
      ("complete", "green"),
      ("idea", "cyan")),
    tasks' = Set(),
    displayed' = Set(),
    inputTaskID' = "",
    inputDescription' = ""
  }

  action addTask = all {
    inputTaskID != "",
    inputDescription != "",
    colorTheme' = colorTheme,
    tasks' = union(tasks, Set({
      id: inputTaskID,
      description: inputDescription,
      status: Backlog,
      priority: Low
    })),
    displayed' = displayed,
    inputTaskID' = "",
    inputDescription' = ""
  }

  // Only existing tasks can be rendered
  action renderTask = all {
    inputTaskID != "",
    exists(tasks, t => t.id == inputTaskID),
    colorTheme' = colorTheme,
    tasks' = tasks,
    displayed' = union(displayed, Set(inputTaskID)),
    inputTaskID' = "",
    inputDescription' = inputDescription
  }

  action clearDisplay = all {
    colorTheme' = colorTheme,
    tasks' = tasks,
    displayed' = Set(),
    inputTaskID' = inputTaskID,
    inputDescription' = inputDescription
  }

  action step = any { addTask, renderTask, clearDisplay }

  val displayedTasksExist =
    forall(displayed, id => exists(tasks, t => t.id == id))
  val inv = displayedTasksExist
}
