module AtomicWrite {
  type Phase = Idle | Started | Written | Renamed | Done | Aborted
  type Content = Old | Partial | New

  var phase: Phase
  var target: Content
  var tempExists: bool

  action init = all { phase' = Idle, target' = Old, tempExists' = false }
  action start = all {
    phase == Idle, phase' = Started, target' = target, tempExists' = true
  }
  action writeTemp = all {
    phase == Started, phase' = Written, target' = target, tempExists' = true
  }
  action rename = all {
    phase == Written, phase' = Renamed, target' = New, tempExists' = false
  }
  action finish = all {
    phase == Renamed, phase' = Done, target' = target, tempExists' = false
  }
  action abort = all {
    phase == Started or phase == Written,
    phase' = Aborted, target' = target, tempExists' = false
  }
  action idle = all {
    phase == Done or phase == Aborted,
    phase' = phase, target' = target, tempExists' = tempExists
  }
  action step = any { start, writeTemp, rename, finish, abort, idle }
  val inv = and {
    target != Partial,
    (phase == Idle or phase == Started or phase == Written or phase == Aborted).implies(target == Old),
    (phase == Renamed or phase == Done).implies(target == New),
    (phase == Done or phase == Aborted).implies(not(tempExists))
  }
  run success = init.then(start).then(writeTemp).then(rename).then(finish)
    .expect(all { assert(target == New), assert(not(tempExists)), assert(inv) })
  run abort_started = init.then(start).then(abort)
    .expect(all { assert(target == Old), assert(not(tempExists)), assert(inv) })
  run abort_written = init.then(start).then(writeTemp).then(abort)
    .expect(all { assert(target == Old), assert(not(tempExists)), assert(inv) })
}

module AtomicWriteMutant {
  import AtomicWrite.*
  action truncate = all {
    phase == Started, phase' = Written, target' = Partial, tempExists' = true
  }
  action mutantStep = any { start, truncate, rename, finish, abort, idle }
  run truncation_witness = init.then(start).then(truncate)
    .expect(all { assert(target == Partial), assert(not(inv)) })
}
