# Technical Debt Backlog

This backlog converts the recent debt audit into execution-ready work items.

## Working Agreement

- Prioritize reliability and deterministic behavior over feature expansion.
- Keep each item independently shippable where possible.
- Every item must include tests or explicit test rationale.
- No item is complete without docs updates if behavior changes.

## Status Legend

- `ready`: scoped and can be started now
- `needs-design`: requires design decision before implementation
- `blocked`: depends on another item
- `done`: implemented and verified

## Backlog (Prioritized)

| ID | Priority | Status | Area | Summary | Depends On |
|---|---|---|---|---|---|
| TD-001 | P0 | done | Runtime Safety | Add hard termination controls to recursive generation loop | - |
| TD-002 | P0 | done | Error Handling | Replace silent `ignore-errors` in critical paths with typed/logged failures | - |
| TD-003 | P0 | done | Security/Config | Remove hardcoded OpenAI auth header, switch to config/env | - |
| TD-004 | P0 | done | Repo Hygiene | Remove editor backup artifacts and tighten ignore patterns | - |
| TD-005 | P1 | done | Thread Lifecycle | Add start/stop lifecycle for heartbeat/background threads | - |
| TD-006 | P1 | done | Tests | Expand integration tests for generate-content and backend adapters | TD-001, TD-002, TD-003 |
| TD-007 | P1 | done | State Model | Replace global conversation/model/config state with explicit session object | TD-006 |
| TD-008 | P1 | done | Observability | Introduce structured logging facade and route trace output through it | - |
| TD-009 | P1 | done | Adapter Layer | Unify Gemini/OpenAI payload normalization and validation | TD-003 |
| TD-010 | P2 | done | Rate Limiting | Improve fixed-delay limiter with 429-aware backoff strategy | - |
| TD-011 | P2 | done | API Validation | Add payload validation before transport submission | TD-009 |
| TD-012 | P2 | done | Build Modularity | Reduce coupling in ASDF graph into bounded modules | TD-007, TD-009 |
| TD-013 | P2 | done | Observability | Second-pass cleanup: migrate remaining ad hoc trace output to logging facade and normalize severity usage | TD-008 |

---

## Item Details

### TD-001: Hard termination controls for recursive generation loop

- Priority: P0
- Status: `done`
- Files in scope:
  - `gemini-core.lisp`
  - `tests/main.lisp`
- Problem:
  - `%generate-content` currently uses soft loop signaling and can continue through deep recursion.
- Deliverables:
  - Add absolute recursion cap (configurable default, e.g. 24-40).
  - Add explicit typed condition on cap exceed.
  - Ensure tool-call recursion and thin-response recursion share same budget.
- Acceptance criteria:
  - Recursive loops terminate deterministically.
  - Caller receives actionable error message with depth and trigger reason.
  - New tests cover: repeated thin response, repeated function-call chain, mixed chain.
- Estimation: 0.5-1 day

### TD-002: Replace silent critical `ignore-errors`

- Priority: P0
- Status: `done`
- Files in scope:
  - `gemini-core.lisp`
  - `gemini-iridium.lisp`
  - `uroboros.lisp`
  - `tests/main.lisp`
- Problem:
  - Silent error suppression obscures production failures.
- Deliverables:
  - Replace critical `ignore-errors` with `handler-case` and structured diagnostics.
  - Distinguish recoverable vs fatal failures.
  - Keep graceful behavior where intended, but emit explicit warning/error events.
- Acceptance criteria:
  - No silent swallow of critical path failures.
  - Tests assert expected handling for failure modes.
- Estimation: 1-1.5 days

### TD-003: Externalize OpenAI auth header

- Priority: P0
- Status: `done`
- Files in scope:
  - `gemini-openai.lisp`
  - `vars.lisp`
  - `README.md`
  - `tests/main.lisp`
- Problem:
  - Hardcoded `Bearer lm-studio` is inflexible and unsafe.
- Deliverables:
  - Add configuration lookup (persona config/env var/fallback).
  - Keep local default behavior optional, not hardcoded.
  - Document config and migration path.
- Acceptance criteria:
  - Header source is configurable.
  - Existing local setup can still work with explicit default option.
  - Tests validate header construction behavior.
- Estimation: 0.5 day

### TD-004: Repository hygiene cleanup

- Priority: P0
- Status: `done`
- Files in scope:
  - `.gitignore`
  - root backup artifacts (`*~`, `#*#`, etc.)
- Problem:
  - Backup/temp files in repo create noise and risk accidental edits.
- Deliverables:
  - Delete stale backup artifacts.
  - Ensure ignore rules cover editor lock/temp patterns in use.
- Acceptance criteria:
  - Tree has no backup/lock artifacts tracked.
  - Ignore rules prevent recurrence.
- Estimation: 0.25 day

### TD-005: Background thread lifecycle controls

- Priority: P1
- Status: `done`
- Files in scope:
  - `gemini-chatbot.lisp`
  - `gemini-iridium.lisp`
  - `mcp.lisp`
  - `tests/main.lisp`
- Problem:
  - Long-lived/background threads start with weak shutdown semantics.
- Deliverables:
  - Add explicit start/stop APIs and idempotent guards.
  - Register cleanup in unload/test teardown paths.
- Acceptance criteria:
  - No orphan threads after tests.
  - Repeated init/teardown cycles are stable.
- Estimation: 1-2 days

### TD-006: Integration test expansion

- Priority: P1
- Status: `done`
- Files in scope:
  - `tests/main.lisp`
  - optional split into `tests/integration/*.lisp`
- Problem:
  - Single test file currently carries broad coverage responsibilities.
- Deliverables:
  - Add scenario-level tests for:
    - generation loop stop behavior
    - backend safety/guideline stop behavior
    - payload translation invariants
    - persona load/reload isolation
- Acceptance criteria:
  - Core regressions covered by deterministic tests.
  - Test layout reflects unit vs integration intent.
- Estimation: 2-3 days

### TD-007: Session object replaces global runtime state

- Priority: P1
- Status: `done`
- Files in scope:
  - `vars.lisp`
  - `gemini-core.lisp`
  - `gemini.lisp`
  - `llm-repl.lisp`
  - `mcp.lisp`
- Problem:
  - Global state prevents clean multi-session concurrency and predictability.
- Deliverables:
  - Introduce explicit `session` object for context/model/runtime flags.
  - Thread API surface through key entry points incrementally.
- Acceptance criteria:
  - Session isolation across concurrent flows.
  - Backward compatibility shim exists for existing call sites.
- Estimation: 1-3 weeks

### TD-008: Structured logging facade

- Priority: P1
- Status: `done`
- Files in scope:
  - `gemini-core.lisp`
  - `gemini-openai.lisp`
  - `gemini-iridium.lisp`
  - `jsonrpc.lisp`
- Problem:
  - Ad hoc trace logging is noisy and hard to route/filter.
- Deliverables:
  - Add lightweight logging wrapper (`debug`, `info`, `warn`, `error`).
  - Route high-value events through wrapper first.
- Acceptance criteria:
  - Log verbosity can be controlled centrally.
  - Existing diagnostics remain available.
- Estimation: 1-2 days

### TD-009: Unified adapter normalization layer

- Priority: P1
- Status: `done`
- Files in scope:
  - `gemini-core.lisp`
  - `gemini-openai.lisp`
  - `object.lisp`
- Problem:
  - Duplicate payload and response normalization logic increases drift risk.
- Deliverables:
  - Shared conversion/validation module.
  - Provider-specific edge handling isolated to adapter boundaries.
- Acceptance criteria:
  - Reduced duplicate mappings.
  - Shared tests assert normalization parity.
- Estimation: 1-2 weeks

### TD-010: 429-aware adaptive rate limiting

- Priority: P2
- Status: `done`
- Files in scope:
  - `gemini-core.lisp`
  - `tests/main.lisp`
- Problem:
  - Fixed-delay model-based limiter is coarse.
- Deliverables:
  - Add exponential backoff and jitter on quota/rate responses.
  - Keep deterministic reservation logic for local scheduling.
- Acceptance criteria:
  - Better behavior under burst and transient quota failures.
- Estimation: 1 day

### TD-011: Preflight payload validation

- Priority: P2
- Status: `done`
- Files in scope:
  - `gemini-core.lisp`
  - `gemini-openai.lisp`
  - `tests/main.lisp`
- Problem:
  - Some malformed payloads only fail at remote endpoint.
- Deliverables:
  - Validate required fields/types before send.
  - Emit clear local errors with field path context.
- Acceptance criteria:
  - Invalid payloads fail fast locally.
- Estimation: 1-2 days

### TD-012: Modularize ASDF dependency graph

- Priority: P2
- Status: `done`
- Files in scope:
  - `gemini.asd`
  - package/module files as needed
- Problem:
  - Broad inter-module coupling raises refactor risk.
- Deliverables:
  - Carve out bounded modules (transport, adapters, orchestration, tools).
  - Simplify load order and dependency edges.
- Acceptance criteria:
  - Cleaner dependency graph with reduced cross-module coupling.
- Estimation: 1-3 weeks

### TD-013: Second-pass logging cleanup and severity normalization

- Priority: P2
- Status: `done`
- Files in scope:
  - `gemini-core.lisp`
  - `gemini-openai.lisp`
  - `gemini-iridium.lisp`
  - `jsonrpc.lisp`
  - additional files with `format *trace-output*` as discovered
  - `tests/main.lisp`
- Problem:
  - TD-008 routed high-value events, but residual ad hoc trace-output writes still create inconsistency in format, level semantics, and filterability.
- Deliverables:
  - Migrate remaining direct trace-output diagnostics to `log-debug`/`log-info`/`log-warn`/`log-error` where appropriate.
  - Normalize severity mapping for recurring classes of events (transport lifecycle, retries, timeout/cancel, decode/parse failures).
  - Keep intentionally user-facing transcript output unchanged where it is not diagnostic logging.
  - Add regression tests for level filtering in migrated hotspots.
- Acceptance criteria:
  - No remaining diagnostic-only `format *trace-output*` calls in scoped files.
  - Emitted severity aligns with event semantics and is centrally filterable.
  - Full test suite passes after migration.
- Estimation: 0.5-1.5 days

---

## Execution Plan (Initial)

### Phase 1 (Stabilize: 3-5 days)

- TD-001
- TD-002
- TD-003
- TD-004
- TD-005

Exit criteria:

- Deterministic recursion safety in place.
- Critical silent failures replaced.
- Auth configuration no longer hardcoded.
- Thread lifecycle has explicit controls.
- Repository hygiene baseline established.

### Phase 2 (Confidence: 3-5 days)

- TD-006
- TD-008
- TD-010
- TD-011

Exit criteria:

- Integration tests cover core risk paths.
- Logging is centrally controllable.
- Rate limiting/backoff and preflight validation reduce runtime incidents.

### Phase 3 (Structural: 2-6 weeks)

- TD-007
- TD-009
- TD-012

Exit criteria:

- Sessionized runtime model.
- Unified adapter layer.
- Reduced system coupling.

---

## Ready Queue (Start Next)

1. TD-001
2. TD-002
3. TD-003
4. TD-004
5. TD-005

## Preparation Checklist for Each Item

- [x] Confirm scope and touched files
- [x] Define test updates before code changes
- [x] Implement smallest safe slice
- [x] Run full test suite
- [x] Update docs/changelog notes
- [x] Mark backlog item status and follow-up tasks
