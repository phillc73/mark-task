module TaskAttributes {
  type TaskID = str
  type Description = str
  type Category = str
  type Status = Backlog | InProgress | OnHold | Complete | Idea
  type Priority = Low | Medium | High
  type Rating = int
  type Task = {
    id: TaskID,
    description: Description,
    category: Category,
    status: Status,
    priority: Priority,
    complexity: Rating,
    effort: Rating
  }

  var tasks: Set[Task]
  var inputTaskID: TaskID
  var inputDescription: Description
  var inputCategory: Category
  var inputStatus: Status
  var inputPriority: Priority
  var inputComplexity: Rating
  var inputEffort: Rating

  action init = all {
    tasks' = Set(),
    inputTaskID' = "",
    inputDescription' = "",
    inputCategory' = "",
    inputStatus' = Backlog,
    inputPriority' = Low,
    inputComplexity' = 1,
    inputEffort' = 1
  }

  action addTask = all {
    inputTaskID != "",
    inputDescription != "",
    inputComplexity >= 1 and inputComplexity <= 5,
    inputEffort >= 1 and inputEffort <= 5,
    tasks' = tasks.union(Set({
      id: inputTaskID,
      description: inputDescription,
      category: inputCategory,
      status: inputStatus,
      priority: inputPriority,
      complexity: inputComplexity,
      effort: inputEffort
    })),
    inputTaskID' = "",
    inputDescription' = "",
    inputCategory' = "",
    inputStatus' = inputStatus,
    inputPriority' = inputPriority,
    inputComplexity' = inputComplexity,
    inputEffort' = inputEffort
  }

  action editTask = all {
    inputTaskID != "",
    exists(tasks, t => t.id == inputTaskID),
    inputComplexity >= 1 and inputComplexity <= 5,
    inputEffort >= 1 and inputEffort <= 5,
    tasks' = map(tasks, t =>
      if (t.id == inputTaskID)
        {
          id: t.id,
          description:
            if (inputDescription != "") inputDescription else t.description,
          category: if (inputCategory != "") inputCategory else t.category,
          status: inputStatus,
          priority: inputPriority,
          complexity: inputComplexity,
          effort: inputEffort
        }
        else t),
    inputTaskID' = "",
    inputDescription' = "",
    inputCategory' = "",
    inputStatus' = inputStatus,
    inputPriority' = inputPriority,
    inputComplexity' = inputComplexity,
    inputEffort' = inputEffort
  }

  action step = any { addTask, editTask }

  val ratingsInRange = forall(tasks, t =>
    t.complexity >= 1 and t.complexity <= 5 and t.effort >= 1 and t.effort <= 5)
  val inv = ratingsInRange
}
