module DesktopNotifications {
  type NotificationMode = System | Terminal | Both
  type TaskState = DueSoon | Overdue | Complete
  type Key = { file: str, uuid: str, due: int, state: TaskState }

  pure val files = Set("a", "b")
  pure def key(file, state) =
    { file: file, uuid: "task-1", due: 1, state: state }
  pure def eligible(state) = state != Complete

  // The file selects the persistent store; identity within that store is
  // (UUID, due, state). Delivery success alone records that identity.
  var states: str -> TaskState
  var delivered: Set[Key]
  var notifications: Set[Key]
  var notificationMode: NotificationMode
  var retained: Set[Key]

  action init = all {
    states' = files.mapBy(_ => DueSoon),
    delivered' = Set(),
    notifications' = Set(),
    notificationMode' = Terminal,
    retained' = Set()
  }

  action setNotificationMode(mode) = all {
    states' = states,
    delivered' = delivered,
    notifications' = notifications,
    notificationMode' = mode,
    retained' = retained
  }

  action changeState(file, state) = all {
    states' = states.set(file, state),
    delivered' = delivered,
    notifications' = notifications.filter(k => k.file != file),
    notificationMode' = notificationMode,
    retained' = retained
  }

  action checkDueTasks(file, success) = checkWithPersistence(file, success, true)

  action checkWithPersistence(file, success, persistenceSuccess) = {
    val k = key(file, states.get(file))
    all {
      states' = states,
      delivered' = if (eligible(k.state) and not(delivered.contains(k)) and success and persistenceSuccess)
        delivered.union(Set(k)) else delivered,
      notifications' = if (eligible(k.state) and not(delivered.contains(k)) and success and persistenceSuccess)
        notifications.union(Set(k)) else notifications,
      notificationMode' = notificationMode,
    retained' = retained
    }
  }

  // Mutant: omit state from the persistent identity comparison.
  action checkDueTasksWithoutState(file, success) = {
    val k = key(file, states.get(file))
    val alreadyDelivered = delivered.exists(d =>
      d.file == k.file and d.uuid == k.uuid and d.due == k.due)
    all {
      states' = states,
      delivered' = if (eligible(k.state) and not(alreadyDelivered) and success)
        delivered.union(Set(k)) else delivered,
      notifications' = if (eligible(k.state) and not(alreadyDelivered) and success)
        notifications.union(Set(k)) else notifications,
      notificationMode' = notificationMode,
    retained' = retained
    }
  }

  action reopen = all {
    states' = states,
    delivered' = delivered,
    notifications' = Set(),
    notificationMode' = notificationMode,
    retained' = retained.union(delivered)
  }

  action reopenResetMutant = all {
    states' = states,
    delivered' = Set(),
    notifications' = Set(),
    notificationMode' = notificationMode,
    retained' = retained.union(delivered)
  }

  action sendOnPersistenceFailureMutant(file) = {
    val k = key(file, states.get(file))
    all {
      eligible(k.state) and not(delivered.contains(k)),
      states' = states,
      delivered' = delivered,
      notifications' = notifications.union(Set(k)),
      notificationMode' = notificationMode,
      retained' = retained
    }
  }

  action clearNotifications = all {
    states' = states,
    delivered' = delivered,
    notifications' = Set(),
    notificationMode' = notificationMode,
    retained' = retained
  }

  action step = {
    nondet file = files.oneOf()
    nondet state = Set(DueSoon, Overdue, Complete).oneOf()
    nondet mode = Set(System, Terminal, Both).oneOf()
    nondet success = Set(true, false).oneOf()
    nondet persistenceSuccess = Set(true, false).oneOf()
    any {
      setNotificationMode(mode),
      changeState(file, state),
      checkWithPersistence(file, success, persistenceSuccess),
      reopen,
      clearNotifications
    }
  }

  val modeValid = Set(System, Terminal, Both).contains(notificationMode)
  val notificationsAreForDueTasks = notifications.forall(k =>
    eligible(k.state) and states.get(k.file) == k.state)
  val notificationsWereDelivered = notifications.subseteq(delivered)
  val reopenRetainsIdentities = retained.subseteq(delivered)
  val inv = and { modeValid, notificationsAreForDueTasks, notificationsWereDelivered, reopenRetainsIdentities }

  action resetMutantStep = any { step, reopenResetMutant }
  action persistenceMutantStep = {
    nondet file = files.oneOf()
    any { step, sendOnPersistenceFailureMutant(file) }
  }

  run reopen_retains_identities = init
    .then(checkDueTasks("a", true))
    .then(checkDueTasks("b", true))
    .then(reopen)
    .then(checkDueTasks("a", true))
    .expect(all {
      assert(delivered == Set(key("a", DueSoon), key("b", DueSoon))),
      assert(retained == delivered),
      assert(notifications == Set()),
      assert(inv)
    })

  run reset_on_reopen_mutant_witness = init
    .then(checkDueTasks("a", true))
    .then(reopenResetMutant)
    .expect(all { assert(not(reopenRetainsIdentities)), assert(not(inv)) })

  run persistence_failure_suppresses_delivery = init
    .then(checkWithPersistence("a", true, false))
    .expect(all { assert(delivered == Set()), assert(notifications == Set()), assert(inv) })
    .then(checkWithPersistence("a", true, true))
    .expect(all { assert(delivered.contains(key("a", DueSoon))), assert(notifications.contains(key("a", DueSoon))) })

  run send_on_persistence_failure_mutant_witness = init
    .then(sendOnPersistenceFailureMutant("a"))
    .expect(all { assert(not(notificationsWereDelivered)), assert(not(inv)) })

  run transition_and_mutant_witness = init
    .then(checkDueTasks("a", true))
    .then(changeState("a", Overdue))
    .expect(all { assert(not(delivered.contains({ file: "a", uuid: "task-1", due: 1, state: Overdue }))) })
    .then(checkDueTasks("a", true))
    .expect(all { assert(notifications.contains({ file: "a", uuid: "task-1", due: 1, state: Overdue })) })

  run state_omitting_mutant_witness = init
    .then(checkDueTasksWithoutState("a", true))
    .expect(all { assert(notifications.contains(key("a", DueSoon))) })
    .then(changeState("a", Overdue))
    .then(checkDueTasksWithoutState("a", true))
    .expect(all {
      assert(delivered.contains(key("a", DueSoon))),
      assert(not(delivered.contains(key("a", Overdue)))),
      assert(notifications == Set())
    })

  run failure_retry_and_complete = init
    .then(checkDueTasks("a", false))
    .expect(all { assert(delivered == Set()), assert(notifications == Set()) })
    .then(checkDueTasks("a", true))
    .then(clearNotifications)
    .then(checkDueTasks("a", true))
    .expect(all { assert(notifications == Set()) })
    .then(changeState("a", Complete))
    .then(checkDueTasks("a", true))
    .expect(all { assert(not(delivered.contains({ file: "a", uuid: "task-1", due: 1, state: Complete }))), assert(notifications == Set()) })

  run per_file_persistence = init
    .then(checkDueTasks("a", true))
    .then(clearNotifications)
    .then(checkDueTasks("b", true))
    .expect(all {
      assert(delivered.contains({ file: "a", uuid: "task-1", due: 1, state: DueSoon })),
      assert(delivered.contains({ file: "b", uuid: "task-1", due: 1, state: DueSoon })),
      assert(notifications == Set({ file: "b", uuid: "task-1", due: 1, state: DueSoon }))
    })
}

module DurableNotificationOwner {
  type Status = Eligible | Complete
  type Key = { file: str, uuid: str }
  pure val taskKeys = Set({ file: "a", uuid: "task-1" }, { file: "b", uuid: "task-1" })
  pure val a = { file: "a", uuid: "task-1" }
  pure val b = { file: "b", uuid: "task-1" }

  var status: Key -> Status
  var pending: Set[Key]
  var delivered: Set[Key]
  var sent: Set[Key]
  var duplicate: bool
  var unprotectedSend: bool

  action init = all {
    status' = taskKeys.mapBy(_ => Eligible),
    pending' = Set(), delivered' = Set(), sent' = Set(),
    duplicate' = false, unprotectedSend' = false
  }

  action persistPending(k, success) = all {
    status.get(k) == Eligible and not(delivered.contains(k)),
    status' = status,
    pending' = if (success) pending.union(Set(k)) else pending,
    delivered' = delivered, sent' = sent,
    duplicate' = duplicate, unprotectedSend' = unprotectedSend
  }

  action sendFailure(k) = all {
    pending.contains(k) and status.get(k) == Eligible,
    status' = status, pending' = pending, delivered' = delivered, sent' = sent,
    duplicate' = duplicate, unprotectedSend' = unprotectedSend
  }

  // A crash after the external send can precede the durable delivered write.
  action sendSuccess(k, crash) = all {
    pending.contains(k) and status.get(k) == Eligible,
    status' = status,
    pending' = if (crash) pending else pending.exclude(Set(k)),
    delivered' = if (crash) delivered else delivered.union(Set(k)),
    sent' = sent.union(Set(k)),
    duplicate' = (duplicate or sent.contains(k)),
    unprotectedSend' = (unprotectedSend or not(pending.contains(k)))
  }

  action complete(k) = all {
    status' = status.set(k, Complete),
    pending' = pending.exclude(Set(k)), delivered' = delivered, sent' = sent,
    duplicate' = duplicate, unprotectedSend' = unprotectedSend
  }

  action reopen = all {
    status' = status, pending' = pending, delivered' = delivered, sent' = sent,
    duplicate' = duplicate, unprotectedSend' = unprotectedSend
  }

  action skipPendingMutant(k) = all {
    status.get(k) == Eligible and not(pending.contains(k)) and not(delivered.contains(k)),
    status' = status, pending' = pending, delivered' = delivered.union(Set(k)),
    sent' = sent.union(Set(k)), duplicate' = (duplicate or sent.contains(k)),
    unprotectedSend' = (unprotectedSend or not(pending.contains(k)))
  }

  action step = {
    nondet k = taskKeys.oneOf()
    nondet success = Set(true, false).oneOf()
    any { persistPending(k, success), sendFailure(k), sendSuccess(k, success), complete(k), reopen }
  }

  action mutantStep = {
    nondet k = taskKeys.oneOf()
    any { step, skipPendingMutant(k) }
  }

  val sendRequiresDurablePending = not(unprotectedSend)
  val pendingEligible = pending.forall(k => status.get(k) == Eligible)
  val pendingNotDelivered = pending.intersect(delivered) == Set()
  val inv = and { sendRequiresDurablePending, pendingEligible, pendingNotDelivered }

  run persistence_failure_no_send = init
    .then(persistPending(a, false))
    .expect(all { assert(pending == Set()), assert(sent == Set()), assert(inv) })
    .then(persistPending(a, true))
    .expect(all { assert(pending == Set(a)), assert(sent == Set()), assert(inv) })

  run retry_success_and_completion = init
    .then(persistPending(a, true))
    .then(persistPending(b, true))
    .then(sendFailure(a))
    .then(reopen)
    .expect(all { assert(pending == Set(a, b)), assert(sent == Set()), assert(inv) })
    .then(sendSuccess(a, false))
    .expect(all { assert(pending == Set(b)), assert(delivered == Set(a)), assert(sent == Set(a)), assert(inv) })
    .then(complete(b))
    .expect(all { assert(pending == Set()), assert(delivered == Set(a)), assert(inv) })

  run crash_allows_duplicate = init
    .then(persistPending(a, true))
    .then(sendSuccess(a, true))
    .then(reopen)
    .expect(all { assert(pending == Set(a)), assert(delivered == Set()), assert(sent == Set(a)), assert(inv) })
    .then(sendSuccess(a, false))
    .expect(all { assert(duplicate), assert(pending == Set()), assert(delivered == Set(a)), assert(inv) })

  run skip_pending_mutant_witness = init
    .then(skipPendingMutant(a))
    .expect(all { assert(not(sendRequiresDurablePending)), assert(not(inv)) })
}

module NotificationSupersession {
  type State = DueSoon | Overdue
  var current: State
  var pending: Set[State]
  var sent: Set[State]
  var violation: bool

  action init = all { current' = DueSoon, pending' = Set(DueSoon), sent' = Set(), violation' = false }
  action advance = all { current' = Overdue, pending' = pending, sent' = sent, violation' = violation }
  action persist = all { current' = current, pending' = Set(current), sent' = sent, violation' = violation }
  action send = all {
    pending.contains(current), current' = current, pending' = pending,
    sent' = sent.union(Set(current)), violation' = (violation or pending != Set(current))
  }
  action mutantPersist = all {
    current' = current, pending' = pending.union(Set(current)), sent' = sent, violation' = violation
  }
  action step = any { advance, persist, send }
  action mutantStep = any { step, mutantPersist }
  val inv = not(violation)

  run supersede = init.then(advance).then(persist).then(send)
    .expect(all { assert(pending == Set(Overdue)), assert(sent == Set(Overdue)), assert(inv) })
  run supersede_mutant = init.then(advance).then(mutantPersist).then(send)
    .expect(all { assert(not(inv)) })
}

// Per-channel delivery: each channel retries until success is recorded; a
// crash after send may duplicate delivery (accepted, at-least-once).
// NOTE: a boolean rhs in an all{} conjunct list must be parenthesised - a bare
// `x' = a or b` parses `or` as a temporal operator and poisons the action's
// effects (QNT200 cascade).
module NotificationChannels {
  type Channel = System | Terminal
  val channels = Set(System, Terminal)
  var pending: bool
  var delivered: Set[Channel]
  var sent: Set[Channel]
  var repeatedRecorded: bool
  var duplicate: bool

  action init = all {
    pending' = false, delivered' = Set(), sent' = Set(),
    repeatedRecorded' = false, duplicate' = false
  }
  action prepare = all {
    pending' = true, delivered' = delivered, sent' = sent,
    repeatedRecorded' = repeatedRecorded, duplicate' = duplicate
  }
  action send(c, success, crash) = all {
    pending and not(delivered.contains(c)),
    pending' = pending,
    delivered' = if (success and not(crash)) delivered.union(Set(c)) else delivered,
    sent' = if (success) sent.union(Set(c)) else sent,
    repeatedRecorded' = repeatedRecorded,
    duplicate' = (duplicate or (success and sent.contains(c)))
  }
  action mutantSend(c) = all {
    pending, pending' = pending, delivered' = delivered.union(Set(c)),
    sent' = sent.union(Set(c)),
    repeatedRecorded' = (repeatedRecorded or delivered.contains(c)),
    duplicate' = (duplicate or sent.contains(c))
  }
  action step = {
    nondet c = channels.oneOf()
    nondet success = Set(true, false).oneOf()
    nondet crash = Set(true, false).oneOf()
    any { prepare, send(c, success, crash) }
  }
  action mutantStep = {
    nondet c = channels.oneOf()
    any { step, mutantSend(c) }
  }
  val inv = not(repeatedRecorded)
  run partial_retry_once = init.then(prepare)
    .then(send(System, false, false)).then(send(Terminal, true, false))
    .then(send(System, true, false))
    .expect(all { assert(delivered == channels), assert(not(duplicate)), assert(inv) })
  run crash_window = init.then(prepare).then(send(Terminal, true, true))
    .then(send(Terminal, true, false))
    .expect(all { assert(duplicate), assert(inv) })
  run repeated_recorded_mutant = init.then(prepare).then(send(Terminal, true, false))
    .then(mutantSend(Terminal)).expect(all { assert(not(inv)) })
}
