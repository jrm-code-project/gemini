# Technical Debt Backlog

This backlog converts the recent debt audit into execution-ready work items.

## Current State

- The Interactions subsystem has been split across focused session, events, payload, and transport modules.
- Session-first APIs now exist for the main generator, chat, analysis, improvement, and Interactions entry points.
- The default `gemini-tests` run is hermetic, with live backend coverage isolated in `interaction-live-tests`.
- The remaining observability work is mostly limited to intentionally interactive or app-facing output paths rather than silent library cleanup.

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
| TD-014 | P2 | done | Repo Hygiene | Refactor physical source directory structure into subdirectories | TD-012 |
| TD-015 | P2 | done | Model Registry | Move statically-defined entry points to dynamic configuration-driven registry | TD-007 |
| TD-016 | P1 | done | State Model | Deprecate ambient/global state variables in favor of explicit session parameters | TD-007 |
| TD-017 | P1 | done | Transport | Implement self-healing connection watchdogs for stdio MCP sub-processes | - |
| TD-018 | P2 | done | Tests | Implement fully-isolated mock environments for side-effect-heavy tools (git, shell, fs) | - |

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

### TD-014: Refactor physical source directory structure into subdirectories

- Priority: P2
- Status: `done`
- Files in scope:
  - All `.lisp` files in the flat root directory
  - `gemini.asd`
- Problem:
  - Although the ASDF system defines distinct modules, all source files are crowded in the flat root directory.
- Deliverables:
  - Relocate `.lisp` files to corresponding subdirectories matching the system module boundaries (e.g., `infrastructure/`, `transport/`, `adapters/`, `tools/`, `orchestration/`, `apps/`).
  - Update `gemini.asd` pathname configuration to reference the physical folders rather than virtual paths.
- Acceptance criteria:
  - Clean and organized repository layout matching modern Common Lisp standards.
  - Complete ASDF load and test suites compile and execute successfully after relocation.
- Estimation: 2-3 days

### TD-015: Dynamic configuration-driven model registry

- Priority: P2
- Status: `done`
- Files in scope:
  - `gemini.lisp`
  - `config.lisp`
- Problem:
  - Entry points like `invoke-gemini`, `gemini-flash-lite`, and `qwen` are statically defined using compile-time macrolets. Adding support for a new model requires codebase modification and code recompilation.
- Deliverables:
  - Implement a configuration-driven, runtime registry for model configurations (specifying parameters, endpoint urls, and default behaviors).
  - Create a generic model-invocation dispatch mechanism (`invoke-model`).
  - Keep backward-compatible dynamic wrappers for existing functions.
- Acceptance criteria:
  - Models can be added, updated, or custom-configured at runtime without recompilation.
- Estimation: 3-5 days

### TD-016: Deprecate ambient/global state variables

- Priority: P1
- Status: `done`
- Files in scope:
  - `vars.lisp`
  - `gemini-core.lisp`
  - `gemini-chatbot.lisp`
- Problem:
  - Implicit globals (e.g., `*current-session*`, `*chat-persona*`, and package-level state) still drive high-level flows, introducing thread safety concerns and state leaks across chatbot sessions.
- Deliverables:
  - Establish a plan for complete, explicit session propagation across the main invocation stack.
  - Progressively deprecate the global state fallbacks.
- Acceptance criteria:
  - Multi-session concurrency is completely isolated, predictable, and robust.
- Estimation: 1-2 weeks

### TD-017: Implement self-healing connection watchdogs for stdio MCP sub-processes

- Priority: P1
- Status: `done`
- Files in scope:
  - `mcp.lisp`
  - `jsonrpc.lisp`
- Problem:
  - MCP stdio-based transport assumes child processes are highly stable. A single crashed, stalled, or high-latency MCP tool can lock or freeze the parent Lisp process.
- Deliverables:
  - Add connection heartbeat/watchdog timers to stdio transport channels.
  - Implement automatic restart and self-healing logic for crashed or dead MCP server processes.
- Acceptance criteria:
  - Child process failures are isolated, log clear warnings, and self-heal automatically without thread freezes.
- Estimation: 3-4 days

### TD-018: Mock testing expansion for tool side-effects

- Priority: P2
- Status: `done`
- Files in scope:
  - `git-tools.lisp`
  - `shell-tools.lisp`
  - `filesystem-tools.lisp`
  - `tests/misc.lisp`
- Problem:
  - Tool behaviors that write files, query git, or run CLI commands have sparse test isolation, risking modifications to the host environment or failing due to host configuration drift.
- Deliverables:
  - Implement a 100% mocked, virtualized filesystem and environment helper for the test suite.
  - Isolate side-effecting CLI and VCS calls during testing.
- Acceptance criteria:
  - No side effects can leak to the host filesystem or git repositories during local test execution.
- Estimation: 3-5 days

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

### Phase 4 (Modularization & Extension: 1-2 weeks)

- TD-014
- TD-015
- TD-016
- TD-017
- TD-018

Exit criteria:

- Physical repository layout fully organized into subdirectories.
- Dynamic data-driven model registry replaces macrolet static helpers.
- Implicit ambient globals are fully deprecated in favor of explicit session propagation.
- Watchdog monitors prevent stdio MCP socket/transport freezes.
- Side-effect-heavy tools run inside 100% mocked test environments.

---

## Ready Queue (Start Next)

None (all backlog items completed!)

## Preparation Checklist for Each Item

- [x] Confirm scope and touched files
- [x] Define test updates before code changes
- [x] Implement smallest safe slice
- [x] Run full test suite
- [x] Update docs/changelog notes
- [x] Mark backlog item status and follow-up tasks
