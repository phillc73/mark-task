module DefaultValues {
  type Status = Backlog | InProgress | OnHold | Complete | Idea
  type Priority = Low | Medium | High
  type Complexity = C1 | C2 | C3 | C4 | C5
  type Effort = E1 | E2 | E3 | E4 | E5

  var defaultStatus: Status
  var defaultPriority: Priority
  var defaultComplexity: Complexity
  var defaultEffort: Effort
  var inputStatus: Status

  action init = all {
    defaultStatus' = Backlog,
    defaultPriority' = Low,
    defaultComplexity' = C1,
    defaultEffort' = E1,
    inputStatus' = Backlog
  }

  action setDefaultStatus = all {
    inputStatus != defaultStatus,
    defaultStatus' = inputStatus,
    defaultPriority' = defaultPriority,
    defaultComplexity' = defaultComplexity,
    defaultEffort' = defaultEffort,
    inputStatus' = Backlog
  }

  action step = any { setDefaultStatus }

  val statusValid =
    defaultStatus == Backlog or defaultStatus == InProgress or
    defaultStatus == OnHold or defaultStatus == Complete or
    defaultStatus == Idea
  val priorityValid =
    defaultPriority == Low or defaultPriority == Medium or defaultPriority == High
  val complexityValid =
    defaultComplexity == C1 or defaultComplexity == C2 or
    defaultComplexity == C3 or defaultComplexity == C4 or
    defaultComplexity == C5
  val effortValid =
    defaultEffort == E1 or defaultEffort == E2 or defaultEffort == E3 or
    defaultEffort == E4 or defaultEffort == E5
  val inv = and {
    statusValid,
    priorityValid,
    complexityValid,
    effortValid
  }
}
