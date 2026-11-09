module RecurringTasks {
  type Status = Backlog | Complete
  type Task = { id: str, description: str, dueDate: int, periodDays: int, status: Status }

  // UTC minutes since an epoch whose day zero is Thursday. Periods are
  // whole UTC days; seven-day multiples therefore preserve the weekday.
  pure val day = 1440
  pure def nextDue(due, completed, periodDays) = {
    val period = day * periodDays
    if (completed < due) due + period
    else due + ((completed - due) / period + 1) * period
  }
  pure def completionAnchored(completed, periodDays) = completed + day * periodDays
  pure def weekday(time) = (4 + time / day) % 7

  var task: Task
  var currentTime: int
  var originalDue: int
  var lastDue: int
  var lastCompletion: int
  var regenerated: bool

  action initWith(periodDays) = all {
    task' = { id: "a", description: "task", dueDate: 600, periodDays: periodDays, status: Backlog },
    currentTime' = 0,
    originalDue' = 600,
    lastDue' = 600,
    lastCompletion' = 0,
    regenerated' = false
  }
  action init = {
    nondet periodDays = Set(1, 2, 7, 14).oneOf()
    initWith(periodDays)
  }

  action completeTask(completed) = all {
    task.status == Backlog,
    completed >= currentTime,
    task' = task.with("status", Complete),
    currentTime' = completed,
    originalDue' = originalDue,
    lastDue' = lastDue,
    lastCompletion' = lastCompletion,
    regenerated' = regenerated
  }

  action regenerate = all {
    task.status == Complete,
    task' = task.with("id", if (task.id == "a") "b" else "a")
      .with("dueDate", nextDue(task.dueDate, currentTime, task.periodDays))
      .with("status", Backlog),
    currentTime' = currentTime,
    originalDue' = originalDue,
    lastDue' = task.dueDate,
    lastCompletion' = currentTime,
    regenerated' = true
  }

  action regenerateCompletionMutant = all {
    task.status == Complete,
    task' = task.with("id", if (task.id == "a") "b" else "a")
      .with("dueDate", completionAnchored(currentTime, task.periodDays))
      .with("status", Backlog),
    currentTime' = currentTime,
    originalDue' = originalDue,
    lastDue' = task.dueDate,
    lastCompletion' = currentTime,
    regenerated' = true
  }

  action step = {
    nondet delay = Set(0, 60, 1440, 5000).oneOf()
    any { completeTask(currentTime + delay), regenerate }
  }

  val idsNotEmpty = task.id != ""
  val cadenceStable = task.periodDays > 0 and task.dueDate >= originalDue and
    (task.dueDate - originalDue) % (day * task.periodDays) == 0
  val utcTimePreserved = task.dueDate % day == originalDue % day
  val weekdayPreserved = task.periodDays % 7 != 0 or weekday(task.dueDate) == weekday(originalDue)
  val firstFutureOccurrence = not(regenerated) or all {
    task.dueDate > lastDue,
    task.dueDate > lastCompletion,
    task.dueDate - day * task.periodDays <= lastCompletion or
      task.dueDate - day * task.periodDays == lastDue
  }
  val inv = and { idsNotEmpty, cadenceStable, utcTimePreserved, weekdayPreserved, firstFutureOccurrence }

  run scheduled_anchor_witness = initWith(2)
    .then(completeTask(600 + 3 * day + 60))
    .then(regenerate)
    .expect(all {
      assert(task.dueDate == 600 + 4 * day),
      assert(task.dueDate != completionAnchored(currentTime, task.periodDays)),
      assert(inv)
    })
    .then(completeTask(600 + 4 * day))
    .then(regenerate)
    .expect(all { assert(task.dueDate == 600 + 6 * day), assert(inv) })

  run completion_mutant_witness = initWith(2)
    .then(completeTask(600 + 3 * day + 60))
    .then(regenerateCompletionMutant)
    .expect(all { assert(not(cadenceStable)) })

  run early_completion = initWith(7)
    .then(completeTask(60))
    .then(regenerate)
    .expect(all { assert(task.dueDate == 600 + 7 * day), assert(inv) })
}

// Calendar dates use a 32-slot month. Empty slots are not dates.
module CalendarRecurringTasks {
  type Status = Backlog | Complete
  pure val months = 0.to(119)
  pure def leap(year) = year % 4 == 0 and (year % 100 != 0 or year % 400 == 0)
  pure def monthLength(month) = {
    val m = month % 12
    if (m == 1) { if (leap(month / 12)) 29 else 28 }
    else if (Set(3, 5, 8, 10).contains(m)) 30
    else 31
  }
  pure def date(month, day) = month * 32 + day
  pure def occurrence(month, anchorMonth, anchorDay, interval) = all {
    month >= anchorMonth,
    (month - anchorMonth) % interval == 0,
    anchorDay <= monthLength(month)
  }

  var anchorMonth: int
  var anchorDay: int
  var interval: int
  var due: int
  var now: int
  var status: Status
  var lastDue: int
  var lastCompletion: int
  var regenerated: bool

  action initWith(month, day, period) = all {
    months.contains(month), day >= 1, day <= monthLength(month),
    Set(1, 12).contains(period),
    anchorMonth' = month, anchorDay' = day, interval' = period,
    due' = date(month, day), now' = 0, status' = Backlog,
    lastDue' = date(month, day), lastCompletion' = 0, regenerated' = false
  }
  action init = {
    nondet anchor = Set(0, 1, 3).oneOf()
    nondet period = Set(1, 12).oneOf()
    initWith(anchor, if (anchor == 1) 29 else if (anchor == 3) 30 else 31, period)
  }
  action completeTask(completed) = all {
    status == Backlog, completed >= now, completed <= 2000,
    status' = Complete, now' = completed,
    anchorMonth' = anchorMonth, anchorDay' = anchorDay, interval' = interval,
    due' = due, lastDue' = lastDue, lastCompletion' = lastCompletion,
    regenerated' = regenerated
  }
  action regenerate = {
    nondet month = months.filter(candidate =>
      occurrence(candidate, anchorMonth, anchorDay, interval) and
      date(candidate, anchorDay) > due and date(candidate, anchorDay) > now and
      months.forall(m =>
        not(occurrence(m, anchorMonth, anchorDay, interval)) or
        date(m, anchorDay) <= due or date(m, anchorDay) <= now or m >= candidate)).oneOf()
    all {
      status == Complete,
      due' = date(month, anchorDay), status' = Backlog,
      lastDue' = due, lastCompletion' = now, regenerated' = true,
      anchorMonth' = anchorMonth, anchorDay' = anchorDay, interval' = interval,
      now' = now
    }
  }
  action regenerateClampMutant = {
    val month = due / 32 + interval
    val clampedDay = if (anchorDay <= monthLength(month)) anchorDay else monthLength(month)
    all {
      status == Complete, months.contains(month), date(month, clampedDay) > now,
      due' = date(month, clampedDay), status' = Backlog,
      lastDue' = due, lastCompletion' = now, regenerated' = true,
      anchorMonth' = anchorMonth, anchorDay' = anchorDay, interval' = interval,
      now' = now
    }
  }
  action step = {
    nondet delay = Set(0, 1, 32, 100).oneOf()
    any { completeTask(now + delay), regenerate }
  }
  val calendarCadence = all {
    due % 32 == anchorDay,
    occurrence(due / 32, anchorMonth, anchorDay, interval)
  }
  val firstFutureOccurrence = not(regenerated) or all {
    due > lastDue, due > lastCompletion,
    months.forall(m =>
      not(occurrence(m, anchorMonth, anchorDay, interval)) or
      date(m, anchorDay) <= lastDue or date(m, anchorDay) <= lastCompletion or
      date(m, anchorDay) >= due)
  }
  val inv = calendarCadence and firstFutureOccurrence

  run monthly_skip_invalid = initWith(0, 31, 1)
    .then(completeTask(31)).then(regenerate)
    .expect(all { assert(due == date(2, 31)), assert(inv) })
    .then(completeTask(date(5, 1))).then(regenerate)
    .expect(all { assert(due == date(6, 31)), assert(inv) })

  run yearly_leap_skip = initWith(1, 29, 12)
    .then(completeTask(date(1, 29))).then(regenerate)
    .expect(all { assert(due == date(49, 29)), assert(inv) })

  run early_completion = initWith(0, 31, 1)
    .then(completeTask(1)).then(regenerate)
    .expect(all { assert(due == date(2, 31)), assert(inv) })

  run clamp_mutant_witness = initWith(0, 31, 1)
    .then(completeTask(31)).then(regenerateClampMutant)
    .expect(all { assert(due == date(1, 29)), assert(not(inv)) })
}
