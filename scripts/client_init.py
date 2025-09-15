#!/usr/bin/env python3
"""
Minimal MCP TCP client for initialize handshake.

Usage:
  python3 scripts/client_init.py --host 127.0.0.1 --port 12345 \
      [--method initialize] [--id 1] [--params '{"clientInfo":{"name":"py-client","version":"0.1"}}'] [--raw]

Sends one newline-delimited JSON-RPC request and prints the response.
"""

import argparse
import json
import os
import socket
import sys


def build_request(method: str, req_id: int, params_json: str | None) -> str:
    if params_json:
        try:
            params = json.loads(params_json)
        except Exception as e:
            raise SystemExit(f"Invalid --params JSON: {e}")
    else:
        params = {}
    msg = {"jsonrpc": "2.0", "id": req_id, "method": method, "params": params}
    return json.dumps(msg, separators=(",", ":")) + "\n"


def main() -> int:
    ap = argparse.ArgumentParser(description="MCP TCP client (initialize)")
    ap.add_argument("--host", default=os.environ.get("MCP_HOST", "127.0.0.1"))
    ap.add_argument("--port", type=int, default=int(os.environ.get("MCP_PORT", "12345")))
    ap.add_argument("--method", default="initialize")
    ap.add_argument("--id", type=int, default=1, dest="req_id")
    ap.add_argument("--params", default=None, help="JSON string for params")
    ap.add_argument("--raw", action="store_true", help="print raw line only")
    args = ap.parse_args()

    payload = build_request(args.method, args.req_id, args.params)

    try:
        with socket.create_connection((args.host, args.port), timeout=5) as sock:
            sock.sendall(payload.encode("utf-8"))
            # Read one line (newline-delimited protocol)
            buf = bytearray()
            while True:
                b = sock.recv(1)
                if not b:
                    break
                buf += b
                if b == b"\n":
                    break
            line = buf.decode("utf-8", errors="replace").rstrip("\n")
    except ConnectionRefusedError:
        print(f"Connection refused to {args.host}:{args.port}", file=sys.stderr)
        return 2
    except socket.timeout:
        print("Timed out waiting for response", file=sys.stderr)
        return 3
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        return 4

    if args.raw:
        print(line)
        return 0

    try:
        obj = json.loads(line)
    except Exception:
        print(line)
        return 0

    print(json.dumps(obj, indent=2, ensure_ascii=False))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

