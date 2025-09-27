# Project Overview

## Architecture
- ASDF defines the main `lisp-mcp-server` system and a dedicated test system that delegates `asdf:test-op` to Rove while loading the `src/` modules in serial order (package, logging, REPL, protocol, TCP, core, run).
- A single public package exports `run`, `version`, logging controls, REPL, protocol, and TCP entry points, keeping the external surface intentionally narrow for downstream clients.
- Version metadata is centralized via `+server-version+` and `version`, ensuring a consistent semantic version across transports and protocol responses.
- The README frames the server as a minimal, test-first MCP implementation that speaks newline-delimited JSON-RPC over stdio or TCP, with structured logging and a REPL-backed tools surface.

## Core Runtime
- Logging emits JSON events to `*error-output*`, with severity and error-detail policies controllable via `MCP_LOG_LEVEL` and `MCP_ERROR_DETAIL` environment variables; optional backtraces support debugging without code changes.
- Protocol handling uses a lightweight `server-state` to manage initialization status, MCP version negotiation, capability advertisement, and routing of `initialize`, `tools/list`, `tools/call`, `ping`, and notification messages with JSON-RPC compliant error mapping.
- `repl-eval` reads all forms through a fresh readtable, evaluates sequentially inside a caller-selected package, and returns both printed and raw values, forming the initial `tools/call` implementation.
- Transport loops share the protocol processor: `run` drives a stdio session with per-stream state and structured logs, while `serve-tcp` accepts sockets, logs peer metadata, and reuses the same line-processing loop per client.

## Testing & Tooling
- Rove-based suites cover version reporting, REPL semantics, protocol handshakes, logging behavior, error translation, and TCP interactions, providing regression protection for protocol and transport changes.
- Logging tests assert that debug-level execution emits `rpc.dispatch` and `rpc.result` events, keeping observability wired through refactors.
- TCP integration tests spawn the server in a background thread, validate handshake responses, and ensure sockets close cleanly, preventing regressions in connection management.
- The stdio↔TCP bridge test launches the Python helper script, validates idle durability, and exercises end-to-end transport bridging, demonstrating expected tooling composition.

## Design Guidance
- Per-connection state isolation ensures initialization, tool listing, and error policies remain consistent across transports; future work can extend this state with authentication or rate limiting without cross-session leakage.
- Tool dispatch normalizes names (handling dotted and slashed namespaces) and wraps failures into JSON-RPC `-32000` responses with optional backtraces, aligning server ergonomics with client expectations.
- Environment-driven logging and error disclosure support operational tuning; existing tests assert that detail suppression modes behave as configured.
- Security notes emphasize that reader and runtime evaluation are enabled, constraining current deployment to trusted environments and signaling the roadmap for condition taxonomies, tool allowlists, and resource caps.
