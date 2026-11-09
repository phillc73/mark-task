module DataStorage {
  type TaskID = str
  type Description = str
  type Task = {
    id: TaskID,
    description: Description
  }

  // In-memory tasks and the persisted markdown document
  var tasks: Set[Task]
  var savedTasks: Set[Task]
  var markdownLocation: str
  var inputTaskID: TaskID
  var inputDescription: Description

  action init = all {
    tasks' = Set(),
    savedTasks' = Set(),
    markdownLocation' = "/default/location/tasks.md",
    inputTaskID' = "",
    inputDescription' = ""
  }

  action addTask = all {
    inputTaskID != "",
    inputDescription != "",
    tasks' = tasks.union(Set({ id: inputTaskID, description: inputDescription })),
    savedTasks' = savedTasks,
    markdownLocation' = markdownLocation,
    inputTaskID' = "",
    inputDescription' = ""
  }

  action save = all {
    savedTasks' = tasks,
    tasks' = tasks,
    markdownLocation' = markdownLocation,
    inputTaskID' = inputTaskID,
    inputDescription' = inputDescription
  }

  action load = all {
    tasks' = savedTasks,
    savedTasks' = savedTasks,
    markdownLocation' = markdownLocation,
    inputTaskID' = inputTaskID,
    inputDescription' = inputDescription
  }

  action step = any { addTask, save, load }

  val locationSet = markdownLocation != ""
  val inv = locationSet
}
