TODO
- [x] Protocol: Wrap `process-json-line` with `handler-case`; return -32700 for bad JSON, -32600 for malformed requests, -32601 for unknown methods; add regression tests in `tests/protocol-test.lisp` for invalid JSON and unknown methods.
- [x] TCP: Add accept/read timeouts and per-connection error handling in `serve-tcp`. When `accept-once` is NIL, spawn a per-connection thread so one slow client doesn’t block others. Stop via flag + listener close instead of `destroy-thread`. Add timeout and multi-client tests in `tests/tcp-test.lisp`.
- [x] REPL: Add evaluation timeout, max output length, and a safe mode that disables `*read-eval*`; surface timeout/cancel via `tools/call` so `(loop)` can’t wedge the server.
- [x] FS: Make `fs-read-file` do partial reads (respect `limit`) and reject negative/huge offsets. Resolve symlinks (`truename`) on writes to keep everything under `*project-root*`. Add large-file and path traversal tests.
- [x] Logging: Default log level to `:info` with env override; include request id, client address, and connection id in logs for traceability.
- [x] Protocol negotiation: When an unsupported `protocolVersion` is requested, return an explicit error or documented fallback; align returned `capabilities` with the negotiated version and add tests.
- [x] CI ops: Because `tests/tcp-test.lisp` can fail in network-restricted sandboxes, split TCP tests into a separate job/optional run; keep `rove lisp-mcp-server.asd` for core suites and run targeted files independently.
