# Formal specifications

Quint models for mark-task, checked with Quint 0.33.1 and Apalache 0.62.4.
Verification is bounded at 3 steps, so the models are instrument evidence,
not a proof.

Status at v0.1.0: 9 VERIFIED-INSTRUMENT, 10 UNSOUND-SCOPE, 0 VERIFIED.

- **VERIFIED-INSTRUMENT**: the bounded instrument gate passes and the named
  runs pass. Semantic review is incomplete.
- **UNSOUND-SCOPE**: the model describes an interaction the product does not
  ship, or its input or command environment is absent, so the model
  deadlocks. These models are not verified and make no claim about the
  product.

The models predate later grammar and configuration changes. They are not
updated to those changes, so the specification debt stands.

| File | Verdict | Note |
| --- | --- | --- |
| `alias_commands.qnt` | UNSOUND-SCOPE | Input environment absent. |
| `atomic_write.qnt` | VERIFIED-INSTRUMENT | Bounded gate and mutant witness pass; semantic review incomplete. |
| `configuration.qnt` | UNSOUND-SCOPE | Input environment absent. |
| `conflict_resolution.qnt` | UNSOUND-SCOPE | Models interactive resolution, which the product does not ship. |
| `data_storage.qnt` | VERIFIED-INSTRUMENT | Bounded gate passes; semantic review incomplete. |
| `default_values.qnt` | UNSOUND-SCOPE | Input environment absent. |
| `desktop_notifications.qnt` | VERIFIED-INSTRUMENT | Named runs and bounded gates pass; semantic review incomplete. |
| `interactive_mode.qnt` | UNSOUND-SCOPE | Command environment absent. |
| `performance.qnt` | VERIFIED-INSTRUMENT | Bounded gate passes; the display-versus-load mapping is unreviewed. |
| `recurring_tasks.qnt` | VERIFIED-INSTRUMENT | Named runs and bounded gates pass; semantic review incomplete. |
| `search_functionality.qnt` | VERIFIED-INSTRUMENT | Bounded gate passes; semantic review incomplete. |
| `task_archiving.qnt` | UNSOUND-SCOPE | The original environment deadlocks; the eligibility repair is covered by the executable suite, not by a bounded verification. |
| `task_attributes.qnt` | UNSOUND-SCOPE | Input environment absent. |
| `task_counts.qnt` | UNSOUND-SCOPE | Command and clock environment absent. |
| `task_management.qnt` | UNSOUND-SCOPE | Input environment absent. |
| `task_views.qnt` | VERIFIED-INSTRUMENT | Bounded gate passes; semantic review incomplete. |
| `undo_functionality.qnt` | UNSOUND-SCOPE | Input environment absent. |
| `user_interface.qnt` | VERIFIED-INSTRUMENT | Bounded gate passes; semantic review incomplete. |
| `validation_error_handling.qnt` | VERIFIED-INSTRUMENT | Refusal models pass after repair; full semantic review incomplete. |

Run a model with:

```sh
quint typecheck specs/<file>.qnt
quint test specs/<file>.qnt --main=<Module> --match='<selector>'
quint verify specs/<file>.qnt --main=<Module> --invariant=inv --max-steps=3
```

The bounded runs do not prove the model for every step. A deadlock is a
failed gate, not an invariant pass.
