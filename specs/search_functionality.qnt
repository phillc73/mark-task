module SearchFunctionality {
  type TaskID = str
  type Description = str
  type Category = str
  type Status = Backlog | InProgress | OnHold | Complete | Idea
  type Task = {
    id: TaskID,
    description: Description,
    category: Category,
    status: Status
  }

  var tasks: Set[Task]
  var searchQuery: str
  var results: Set[Task]
  var inputTaskID: TaskID
  var inputDescription: Description
  var inputCategory: Category
  var inputStatus: Status

  action init = all {
    tasks' = Set(),
    searchQuery' = "",
    results' = Set(),
    inputTaskID' = "",
    inputDescription' = "",
    inputCategory' = "",
    inputStatus' = Backlog
  }

  action addTask = all {
    inputTaskID != "",
    inputDescription != "",
    tasks' = tasks.union(Set({
      id: inputTaskID,
      description: inputDescription,
      category: inputCategory,
      status: inputStatus
    })),
    searchQuery' = searchQuery,
    results' = results,
    inputTaskID' = "",
    inputDescription' = "",
    inputCategory' = "",
    inputStatus' = inputStatus
  }

  // The matching predicate is abstracted to equality on description or
  // category; the implementation realises it as case-insensitive
  // substring matching.
  action search = all {
    searchQuery != "",
    tasks' = tasks,
    results' = filter(tasks, t =>
      t.description == searchQuery or t.category == searchQuery),
    searchQuery' = "",
    inputTaskID' = inputTaskID,
    inputDescription' = inputDescription,
    inputCategory' = inputCategory,
    inputStatus' = inputStatus
  }

  action clearResults = all {
    tasks' = tasks,
    results' = Set(),
    searchQuery' = searchQuery,
    inputTaskID' = inputTaskID,
    inputDescription' = inputDescription,
    inputCategory' = inputCategory,
    inputStatus' = inputStatus
  }

  action step = any { addTask, search, clearResults }

  val resultsSubsetOfTasks = forall(results, r => exists(tasks, t => t == r))
  val inv = resultsSubsetOfTasks
}
