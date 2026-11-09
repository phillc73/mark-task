module UndoFunctionality {
  type TaskID = str
  type Description = str
  type Task = {
    id: TaskID,
    description: Description
  }

  var tasks: Set[Task]
  // Past states, most recent first. The current state is not included.
  var history: List[Set[Task]]
  var redoStack: List[Set[Task]]
  var maxHistorySize: int
  var inputTaskID: TaskID
  var inputDescription: Description

  action init = all {
    tasks' = Set(),
    history' = List(),
    redoStack' = List(),
    maxHistorySize' = 5,
    inputTaskID' = "",
    inputDescription' = ""
  }

  action addTask = all {
    inputTaskID != "",
    inputDescription != "",
    tasks' = tasks.union(Set({ id: inputTaskID, description: inputDescription })),
    history' = concat(List(tasks), history),
    redoStack' = List(),
    maxHistorySize' = maxHistorySize,
    inputTaskID' = "",
    inputDescription' = ""
  }

  action deleteTask = all {
    inputTaskID != "",
    exists(tasks, t => t.id == inputTaskID),
    tasks' = filter(tasks, t => t.id != inputTaskID),
    history' = concat(List(tasks), history),
    redoStack' = List(),
    maxHistorySize' = maxHistorySize,
    inputTaskID' = "",
    inputDescription' = inputDescription
  }

  action undo = all {
    history != List(),
    tasks' = head(history),
    history' = tail(history),
    redoStack' = concat(List(tasks), redoStack),
    maxHistorySize' = maxHistorySize,
    inputTaskID' = inputTaskID,
    inputDescription' = inputDescription
  }

  action redo = all {
    redoStack != List(),
    tasks' = head(redoStack),
    history' = concat(List(tasks), history),
    redoStack' = tail(redoStack),
    maxHistorySize' = maxHistorySize,
    inputTaskID' = inputTaskID,
    inputDescription' = inputDescription
  }

  action step = any { addTask, deleteTask, undo, redo }

  val idsNotEmpty = forall(tasks, t => t.id != "")
  val inv = idsNotEmpty
}
