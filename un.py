# PUBLIC DOMAIN - NO LICENSE, NO WARRANTY
#
# This is free public domain software for the public good of a permacomputer hosted
# at permacomputer.com - an always-on computer by the people, for the people. One
# which is durable, easy to repair, and distributed like tap water for machine
# learning intelligence.
#
# The permacomputer is community-owned infrastructure optimized around four values:
#
#   TRUTH    - First principles, math & science, open source code freely distributed
#   FREEDOM  - Voluntary partnerships, freedom from tyranny & corporate control
#   HARMONY  - Minimal waste, self-renewing systems with diverse thriving connections
#   LOVE     - Be yourself without hurting others, cooperation through natural law
#
# This software contributes to that vision by enabling code execution across 42+
# programming languages through a unified interface, accessible to all. Code is
# seeds to sprout on any abandoned technology.
#
# Learn more: https://www.permacomputer.com
#
# Anyone is free to copy, modify, publish, use, compile, sell, or distribute this
# software, either in source code form or as a compiled binary, for any purpose,
# commercial or non-commercial, and by any means.
#
# NO WARRANTY. THE SOFTWARE IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY KIND.
#
# That said, our permacomputer's digital membrane stratum continuously runs unit,
# integration, and functional tests on all of it's own software - with our
# permacomputer monitoring itself, repairing itself, with minimal human in the
# loop guidance. Our agents do their best.
#
# Copyright 2025 TimeHexOn & foxhop & russell@unturf
# https://www.timehexon.com
# https://www.foxhop.net
# https://www.unturf.com/software

#!/usr/bin/env python3
"""
un.py - Unsandbox CLI Client (Python Implementation)

Full-featured CLI matching un.c capabilities:
- Execute code with env vars, input files, artifacts
- Interactive sessions with shell/REPL support
- Persistent services with domains and ports

Usage:
  un.py [options] <source_file>
  un.py session [options]
  un.py service [options]

Requires: UNSANDBOX_API_KEY environment variable
"""

import sys
import os
import json
import base64
import argparse
import urllib.request
import urllib.error
import webbrowser
import hmac
import hashlib
import time

API_BASE = "https://api.unsandbox.com"
PORTAL_BASE = "https://unsandbox.com"
BLUE = "\033[34m"
RED = "\033[31m"
GREEN = "\033[32m"
YELLOW = "\033[33m"
RESET = "\033[0m"

# Extension to language mapping
EXT_MAP = {
    ".py": "python", ".js": "javascript", ".ts": "typescript",
    ".rb": "ruby", ".php": "php", ".pl": "perl", ".lua": "lua",
    ".sh": "bash", ".go": "go", ".rs": "rust", ".c": "c",
    ".cpp": "cpp", ".cc": "cpp", ".cxx": "cpp",
    ".java": "java", ".kt": "kotlin", ".cs": "csharp", ".fs": "fsharp",
    ".hs": "haskell", ".ml": "ocaml", ".clj": "clojure", ".scm": "scheme",
    ".lisp": "commonlisp", ".erl": "erlang", ".ex": "elixir", ".exs": "elixir",
    ".jl": "julia", ".r": "r", ".R": "r", ".cr": "crystal",
    ".d": "d", ".nim": "nim", ".zig": "zig", ".v": "v",
    ".dart": "dart", ".groovy": "groovy", ".scala": "scala",
    ".f90": "fortran", ".f95": "fortran", ".cob": "cobol",
    ".pro": "prolog", ".forth": "forth", ".4th": "forth",
    ".tcl": "tcl", ".raku": "raku", ".m": "objc",
}


def get_api_keys(args_key=None):
    """Get API keys from args or environment. Returns (public_key, secret_key)."""
    # Try new split key format first
    public_key = os.environ.get("UNSANDBOX_PUBLIC_KEY")
    secret_key = os.environ.get("UNSANDBOX_SECRET_KEY")

    # Fall back to old single key format for backwards compatibility
    if not public_key or not secret_key:
        old_key = args_key or os.environ.get("UNSANDBOX_API_KEY")
        if old_key:
            # Old format: use the key as secret, derive public from it or use as-is
            public_key = old_key
            secret_key = old_key
        else:
            print(f"{RED}Error: UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY not set{RESET}", file=sys.stderr)
            print(f"{RED}       (or legacy UNSANDBOX_API_KEY for backwards compatibility){RESET}", file=sys.stderr)
            sys.exit(1)

    return public_key, secret_key


def detect_language(filename, exit_on_error=True):
    """Detect language from file extension"""
    ext = os.path.splitext(filename)[1].lower()
    lang = EXT_MAP.get(ext)
    if not lang:
        # Try reading shebang
        try:
            with open(filename, 'r') as f:
                first_line = f.readline()
                if first_line.startswith('#!'):
                    if 'python' in first_line: return 'python'
                    if 'node' in first_line: return 'javascript'
                    if 'ruby' in first_line: return 'ruby'
                    if 'perl' in first_line: return 'perl'
                    if 'bash' in first_line or '/sh' in first_line: return 'bash'
                    if 'lua' in first_line: return 'lua'
                    if 'php' in first_line: return 'php'
        except:
            pass
        if exit_on_error:
            print(f"{RED}Error: Cannot detect language for {filename}{RESET}", file=sys.stderr)
            sys.exit(1)
        return None
    return lang


def read_file(filepath):
    """Read file contents - helper for tests"""
    with open(filepath, 'r') as f:
        return f.read()


def execute_code(language, code, public_key=None, secret_key=None):
    """Execute code and return result - helper for tests"""
    if not public_key:
        public_key, secret_key = get_api_keys()
    return api_request("/execute", method="POST", data={"language": language, "code": code}, public_key=public_key, secret_key=secret_key)


def api_request(endpoint, method="GET", data=None, public_key=None, secret_key=None):
    """Make API request with HMAC authentication"""
    url = f"{API_BASE}{endpoint}"

    # Prepare body
    body = json.dumps(data) if data else ""

    # Generate HMAC signature
    timestamp = str(int(time.time()))
    signature_input = f"{timestamp}:{method}:{endpoint}:{body}"
    signature = hmac.new(
        secret_key.encode('utf-8'),
        signature_input.encode('utf-8'),
        hashlib.sha256
    ).hexdigest()

    headers = {
        "Authorization": f"Bearer {public_key}",
        "X-Timestamp": timestamp,
        "X-Signature": signature,
        "Content-Type": "application/json"
    }

    req = urllib.request.Request(url, method=method, headers=headers)
    if data:
        req.data = body.encode('utf-8')

    try:
        with urllib.request.urlopen(req, timeout=300) as resp:
            return json.loads(resp.read().decode('utf-8'))
    except urllib.error.HTTPError as e:
        error_body = e.read().decode('utf-8') if e.fp else str(e)
        if e.code == 401 and 'timestamp' in error_body.lower():
            print(f"{RED}Error: Request timestamp expired (must be within 5 minutes of server time){RESET}", file=sys.stderr)
            print(f"{YELLOW}Your computer's clock may have drifted.{RESET}", file=sys.stderr)
            print("Check your system time and sync with NTP if needed:", file=sys.stderr)
            print("  Linux:   sudo ntpdate -s time.nist.gov", file=sys.stderr)
            print("  macOS:   sudo sntp -sS time.apple.com", file=sys.stderr)
            print("  Windows: w32tm /resync", file=sys.stderr)
        else:
            print(f"{RED}Error: HTTP {e.code} - {error_body}{RESET}", file=sys.stderr)
        sys.exit(1)
    except urllib.error.URLError as e:
        print(f"{RED}Error: {e.reason}{RESET}", file=sys.stderr)
        sys.exit(1)


def cmd_execute(args):
    """Execute source code"""
    public_key, secret_key = get_api_keys(args.api_key)

    # Check for inline mode: -s/--shell specified, or source_file doesn't exist
    inline_mode = False
    if args.exec_shell:
        inline_mode = True
        language = args.exec_shell
        code = args.source_file  # The "file" argument is actually the code
    elif not os.path.exists(args.source_file):
        # File doesn't exist - treat as inline bash code
        inline_mode = True
        language = "bash"
        code = args.source_file

    if not inline_mode:
        # Read source file
        try:
            with open(args.source_file, 'r') as f:
                code = f.read()
        except FileNotFoundError:
            print(f"{RED}Error: File not found: {args.source_file}{RESET}", file=sys.stderr)
            sys.exit(1)

        language = detect_language(args.source_file)

    # Build request payload
    payload = {
        "language": language,
        "code": code
    }

    # Add environment variables
    if args.env:
        env_vars = {}
        for e in args.env:
            if '=' in e:
                k, v = e.split('=', 1)
                env_vars[k] = v
        if env_vars:
            payload["env"] = env_vars

    # Add input files
    if args.files:
        input_files = []
        for filepath in args.files:
            try:
                with open(filepath, 'rb') as f:
                    content = base64.b64encode(f.read()).decode('utf-8')
                input_files.append({
                    "filename": os.path.basename(filepath),
                    "content_base64": content
                })
            except FileNotFoundError:
                print(f"{RED}Error: Input file not found: {filepath}{RESET}", file=sys.stderr)
                sys.exit(1)
        if input_files:
            payload["input_files"] = input_files

    # Add options
    if args.artifacts:
        payload["return_artifacts"] = True
    if args.network:
        payload["network"] = args.network
    if args.vcpu:
        payload["vcpu"] = args.vcpu

    # Execute
    result = api_request("/execute", method="POST", data=payload, public_key=public_key, secret_key=secret_key)

    # Print output
    if result.get("stdout"):
        print(f"{BLUE}{result['stdout']}{RESET}", end='')
    if result.get("stderr"):
        print(f"{RED}{result['stderr']}{RESET}", end='', file=sys.stderr)

    # Save artifacts
    if args.artifacts and result.get("artifacts"):
        out_dir = args.output_dir or "."
        os.makedirs(out_dir, exist_ok=True)
        for artifact in result["artifacts"]:
            filename = artifact.get("filename", "artifact")
            content = base64.b64decode(artifact["content_base64"])
            path = os.path.join(out_dir, filename)
            with open(path, 'wb') as f:
                f.write(content)
            os.chmod(path, 0o755)
            print(f"{GREEN}Saved: {path}{RESET}", file=sys.stderr)

    sys.exit(result.get("exit_code", 0))


def cmd_session(args):
    """Manage interactive sessions"""
    public_key, secret_key = get_api_keys(args.api_key)

    if args.list:
        result = api_request("/sessions", public_key=public_key, secret_key=secret_key)
        sessions = result.get("sessions", [])
        if not sessions:
            print("No active sessions")
        else:
            print(f"{'ID':<40} {'Shell':<10} {'Status':<10} {'Created'}")
            for s in sessions:
                print(f"{s.get('id', 'N/A'):<40} {s.get('shell', 'N/A'):<10} {s.get('status', 'N/A'):<10} {s.get('created_at', 'N/A')}")
        return

    if args.kill:
        result = api_request(f"/sessions/{args.kill}", method="DELETE", public_key=public_key, secret_key=secret_key)
        print(f"{GREEN}Session terminated: {args.kill}{RESET}")
        return

    if args.snapshot:
        payload = {}
        if args.snapshot_name:
            payload["name"] = args.snapshot_name
        if args.hot:
            payload["hot"] = True

        print(f"{YELLOW}Creating snapshot of session {args.snapshot}...{RESET}", file=sys.stderr)
        result = api_request(f"/sessions/{args.snapshot}/snapshot", method="POST", data=payload, public_key=public_key, secret_key=secret_key)
        print(f"{GREEN}Snapshot created successfully{RESET}")
        print(f"Snapshot ID: {result.get('id', 'N/A')}")
        return

    if args.restore:
        # --restore takes snapshot ID directly, calls /snapshots/:id/restore
        print(f"{YELLOW}Restoring from snapshot {args.restore}...{RESET}", file=sys.stderr)
        result = api_request(f"/snapshots/{args.restore}/restore", method="POST", public_key=public_key, secret_key=secret_key)
        print(f"{GREEN}Session restored from snapshot{RESET}")
        if result.get('session_id'):
            print(f"New session ID: {result.get('session_id')}")
        return

    if args.attach:
        print(f"{YELLOW}Attaching to session {args.attach}...{RESET}")
        print(f"{YELLOW}(Interactive sessions require WebSocket - use un2 for full support){RESET}")
        return

    # Create new session
    payload = {
        "shell": args.shell or "bash"
    }
    if args.network:
        payload["network"] = args.network
    if args.vcpu:
        payload["vcpu"] = args.vcpu
    if args.tmux:
        payload["persistence"] = "tmux"
    if args.screen:
        payload["persistence"] = "screen"
    if args.audit:
        payload["audit"] = True

    # Add input files
    if args.files:
        input_files = []
        for filepath in args.files:
            try:
                with open(filepath, 'rb') as f:
                    content = base64.b64encode(f.read()).decode('utf-8')
                input_files.append({
                    "filename": os.path.basename(filepath),
                    "content_base64": content
                })
            except FileNotFoundError:
                print(f"{RED}Error: Input file not found: {filepath}{RESET}", file=sys.stderr)
                sys.exit(1)
        if input_files:
            payload["input_files"] = input_files

    print(f"{YELLOW}Creating session...{RESET}")
    result = api_request("/sessions", method="POST", data=payload, public_key=public_key, secret_key=secret_key)
    print(f"{GREEN}Session created: {result.get('id', 'N/A')}{RESET}")
    print(f"{YELLOW}(Interactive sessions require WebSocket - use un2 for full support){RESET}")


def validate_key(public_key, secret_key, extend=False):
    """Validate API key and display information"""
    url = f"{PORTAL_BASE}/keys/validate"

    # Generate HMAC signature for portal request
    timestamp = str(int(time.time()))
    endpoint = "/keys/validate"
    body = ""
    signature_input = f"{timestamp}:POST:{endpoint}:{body}"
    signature = hmac.new(
        secret_key.encode('utf-8'),
        signature_input.encode('utf-8'),
        hashlib.sha256
    ).hexdigest()

    headers = {
        "Authorization": f"Bearer {public_key}",
        "X-Timestamp": timestamp,
        "X-Signature": signature,
        "Content-Type": "application/json"
    }

    req = urllib.request.Request(url, method="POST", headers=headers)

    try:
        with urllib.request.urlopen(req, timeout=30) as resp:
            result = json.loads(resp.read().decode('utf-8'))

            # Handle --extend flag
            if extend:
                public_key = result.get("public_key")
                if public_key:
                    extend_url = f"{PORTAL_BASE}/keys/extend?pk={public_key}"
                    print(f"{BLUE}Opening browser to extend key...{RESET}")
                    webbrowser.open(extend_url)
                    return
                else:
                    print(f"{RED}Error: Could not retrieve public key{RESET}", file=sys.stderr)
                    sys.exit(1)

            # Check if key is expired
            if result.get("expired", False):
                print(f"{RED}Expired{RESET}")
                print(f"Public Key: {result.get('public_key', 'N/A')}")
                print(f"Tier: {result.get('tier', 'N/A')}")
                print(f"Expired: {result.get('expires_at', 'N/A')}")
                print(f"{YELLOW}To renew: Visit https://unsandbox.com/keys/extend{RESET}")
                sys.exit(1)

            # Valid key
            print(f"{GREEN}Valid{RESET}")
            print(f"Public Key: {result.get('public_key', 'N/A')}")
            print(f"Tier: {result.get('tier', 'N/A')}")
            print(f"Status: {result.get('status', 'N/A')}")
            print(f"Expires: {result.get('expires_at', 'N/A')}")
            print(f"Time Remaining: {result.get('time_remaining', 'N/A')}")
            print(f"Rate Limit: {result.get('rate_limit', 'N/A')}")
            print(f"Burst: {result.get('burst', 'N/A')}")
            print(f"Concurrency: {result.get('concurrency', 'N/A')}")

    except urllib.error.HTTPError as e:
        error_body = e.read().decode('utf-8') if e.fp else str(e)
        try:
            error_json = json.loads(error_body)
            reason = error_json.get("error", error_body)
        except:
            reason = error_body
        print(f"{RED}Invalid{RESET}")
        print(f"Reason: {reason}")
        sys.exit(1)
    except urllib.error.URLError as e:
        print(f"{RED}Error: {e.reason}{RESET}", file=sys.stderr)
        sys.exit(1)


def cmd_key(args):
    """Validate API key"""
    public_key, secret_key = get_api_keys(args.key)
    validate_key(public_key, secret_key, extend=args.extend)


def cmd_snapshot(args):
    """Manage snapshots"""
    public_key, secret_key = get_api_keys(args.api_key)

    if args.list:
        result = api_request("/snapshots", public_key=public_key, secret_key=secret_key)
        snapshots = result.get("snapshots", [])
        if not snapshots:
            print("No snapshots found")
        else:
            print(f"{'ID':<40} {'Name':<20} {'Type':<12} {'Source ID':<30} {'Size':<10}")
            for s in snapshots:
                print(f"{s.get('id', 'N/A'):<40} {s.get('name', '-'):<20} {s.get('source_type', 'N/A'):<12} {s.get('source_id', 'N/A'):<30} {s.get('size', 'N/A'):<10}")
        return

    if args.info:
        result = api_request(f"/snapshots/{args.info}", public_key=public_key, secret_key=secret_key)
        print(f"{BLUE}Snapshot Details{RESET}\n")
        print(f"Snapshot ID: {result.get('id', 'N/A')}")
        print(f"Name: {result.get('name', '-')}")
        print(f"Source Type: {result.get('source_type', 'N/A')}")
        print(f"Source ID: {result.get('source_id', 'N/A')}")
        print(f"Size: {result.get('size', 'N/A')}")
        print(f"Created: {result.get('created_at', 'N/A')}")
        print(f"Hot Snapshot: {result.get('hot', 'N/A')}")
        return

    if args.delete:
        result = api_request(f"/snapshots/{args.delete}", method="DELETE", public_key=public_key, secret_key=secret_key)
        print(f"{GREEN}Snapshot deleted successfully{RESET}")
        return

    if args.clone:
        if not args.type:
            print(f"{RED}Error: --type required for --clone (session or service){RESET}", file=sys.stderr)
            sys.exit(1)
        if args.type not in ["session", "service"]:
            print(f"{RED}Error: --type must be 'session' or 'service'{RESET}", file=sys.stderr)
            sys.exit(1)

        payload = {"type": args.type}
        if args.name:
            payload["name"] = args.name
        if args.shell:
            payload["shell"] = args.shell
        if args.ports:
            payload["ports"] = [int(p) for p in args.ports.split(',')]

        result = api_request(f"/snapshots/{args.clone}/clone", method="POST", data=payload, public_key=public_key, secret_key=secret_key)

        if args.type == "session":
            print(f"{GREEN}Session created from snapshot{RESET}")
            print(f"Session ID: {result.get('id', 'N/A')}")
        else:
            print(f"{GREEN}Service created from snapshot{RESET}")
            print(f"Service ID: {result.get('id', 'N/A')}")
        return

    print(f"{RED}Error: Specify --list, --info ID, --delete ID, or --clone ID --type TYPE{RESET}", file=sys.stderr)
    sys.exit(1)


def cmd_service(args):
    """Manage persistent services"""
    public_key, secret_key = get_api_keys(args.api_key)

    if args.list:
        result = api_request("/services", public_key=public_key, secret_key=secret_key)
        services = result.get("services", [])
        if not services:
            print("No services")
        else:
            print(f"{'ID':<20} {'Name':<15} {'Status':<10} {'Ports':<15} {'Domains'}")
            for s in services:
                ports = ','.join(map(str, s.get('ports', [])))
                domains = ','.join(s.get('domains', []))
                print(f"{s.get('id', 'N/A'):<20} {s.get('name', 'N/A'):<15} {s.get('status', 'N/A'):<10} {ports:<15} {domains}")
        return

    if args.info:
        result = api_request(f"/services/{args.info}", public_key=public_key, secret_key=secret_key)
        print(json.dumps(result, indent=2))
        return

    if args.logs:
        result = api_request(f"/services/{args.logs}/logs", public_key=public_key, secret_key=secret_key)
        print(result.get("logs", ""))
        return

    if args.tail:
        result = api_request(f"/services/{args.tail}/logs?lines=9000", public_key=public_key, secret_key=secret_key)
        print(result.get("logs", ""))
        return

    if args.sleep:
        result = api_request(f"/services/{args.sleep}/sleep", method="POST", public_key=public_key, secret_key=secret_key)
        print(f"{GREEN}Service sleeping: {args.sleep}{RESET}")
        return

    if args.wake:
        result = api_request(f"/services/{args.wake}/wake", method="POST", public_key=public_key, secret_key=secret_key)
        print(f"{GREEN}Service waking: {args.wake}{RESET}")
        return

    if args.destroy:
        result = api_request(f"/services/{args.destroy}", method="DELETE", public_key=public_key, secret_key=secret_key)
        print(f"{GREEN}Service destroyed: {args.destroy}{RESET}")
        return

    if args.snapshot:
        payload = {}
        if args.snapshot_name:
            payload["name"] = args.snapshot_name
        if args.hot:
            payload["hot"] = True

        print(f"{YELLOW}Creating snapshot of service {args.snapshot}...{RESET}", file=sys.stderr)
        result = api_request(f"/services/{args.snapshot}/snapshot", method="POST", data=payload, public_key=public_key, secret_key=secret_key)
        print(f"{GREEN}Snapshot created successfully{RESET}")
        print(f"Snapshot ID: {result.get('id', 'N/A')}")
        return

    if args.restore:
        # --restore takes snapshot ID directly, calls /snapshots/:id/restore
        print(f"{YELLOW}Restoring from snapshot {args.restore}...{RESET}", file=sys.stderr)
        result = api_request(f"/snapshots/{args.restore}/restore", method="POST", public_key=public_key, secret_key=secret_key)
        print(f"{GREEN}Service restored from snapshot{RESET}")
        if result.get('service_id'):
            print(f"New service ID: {result.get('service_id')}")
        return

    if args.execute:
        payload = {"command": args.command}
        result = api_request(f"/services/{args.execute}/execute", method="POST", data=payload, public_key=public_key, secret_key=secret_key)
        if result.get("stdout"):
            print(f"{BLUE}{result['stdout']}{RESET}", end='')
        if result.get("stderr"):
            print(f"{RED}{result['stderr']}{RESET}", end='', file=sys.stderr)
        return

    if args.dump_bootstrap:
        print(f"Fetching bootstrap script from {args.dump_bootstrap}...", file=sys.stderr)
        payload = {"command": "cat /tmp/bootstrap.sh"}
        result = api_request(f"/services/{args.dump_bootstrap}/execute", method="POST", data=payload, public_key=public_key, secret_key=secret_key)

        if result.get("stdout"):
            bootstrap = result["stdout"]
            if args.dump_file:
                # Write to file
                try:
                    with open(args.dump_file, 'w') as f:
                        f.write(bootstrap)
                    os.chmod(args.dump_file, 0o755)
                    print(f"Bootstrap saved to {args.dump_file}")
                except IOError as e:
                    print(f"{RED}Error: Could not write to {args.dump_file}: {e}{RESET}", file=sys.stderr)
                    sys.exit(1)
            else:
                # Print to stdout
                print(bootstrap, end='')
        else:
            print(f"{RED}Error: Failed to fetch bootstrap (service not running or no bootstrap file){RESET}", file=sys.stderr)
            sys.exit(1)
        return

    # Create new service
    if args.name:
        payload = {"name": args.name}
        if args.ports:
            payload["ports"] = [int(p) for p in args.ports.split(',')]
        if args.domains:
            payload["domains"] = args.domains.split(',')
        if args.service_type:
            payload["service_type"] = args.service_type
        if args.bootstrap:
            payload["bootstrap"] = args.bootstrap
        if args.bootstrap_file:
            if not os.path.exists(args.bootstrap_file):
                print(f"{RED}Error: Bootstrap file not found: {args.bootstrap_file}{RESET}", file=sys.stderr)
                sys.exit(1)
            with open(args.bootstrap_file, 'r') as f:
                payload["bootstrap_content"] = f.read()
        if args.files:
            input_files = []
            for filepath in args.files:
                try:
                    with open(filepath, 'rb') as f:
                        content = base64.b64encode(f.read()).decode('utf-8')
                    input_files.append({
                        "filename": os.path.basename(filepath),
                        "content_base64": content
                    })
                except FileNotFoundError:
                    print(f"{RED}Error: Input file not found: {filepath}{RESET}", file=sys.stderr)
                    sys.exit(1)
            if input_files:
                payload["input_files"] = input_files
        if args.network:
            payload["network"] = args.network
        if args.vcpu:
            payload["vcpu"] = args.vcpu

        result = api_request("/services", method="POST", data=payload, public_key=public_key, secret_key=secret_key)
        print(f"{GREEN}Service created: {result.get('id', 'N/A')}{RESET}")
        print(f"Name: {result.get('name', 'N/A')}")
        if result.get('url'):
            print(f"URL: {result.get('url')}")
        return

    print(f"{RED}Error: Specify --name to create a service, or use --list, --info, etc.{RESET}", file=sys.stderr)
    sys.exit(1)


def main():
    parser = argparse.ArgumentParser(
        description="Unsandbox CLI - Execute code in secure sandboxes",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  %(prog)s script.py                    Execute Python script
  %(prog)s -e DEBUG=1 script.py         With environment variable
  %(prog)s -f data.csv process.py       With input file
  %(prog)s -a -o ./bin main.c           Save compiled artifacts
  %(prog)s session                      Interactive bash session
  %(prog)s session --shell python3      Python REPL
  %(prog)s session --list               List active sessions
  %(prog)s service --name web --ports 80 --bootstrap "python -m http.server"
  %(prog)s service --name app --ports 8000 --bootstrap-file ./setup.sh
  %(prog)s service --list               List all services
        """
    )

    # Common options
    parser.add_argument("-k", "--api-key", help="API key (or set UNSANDBOX_API_KEY)")
    parser.add_argument("-n", "--network", choices=["zerotrust", "semitrusted"], help="Network mode")
    parser.add_argument("-v", "--vcpu", type=int, choices=range(1, 9), help="vCPU count (1-8)")

    subparsers = parser.add_subparsers(dest="command")

    # Key subcommand
    key_parser = subparsers.add_parser("key", help="Validate API key")
    key_parser.add_argument("-k", "--key", help="API key to validate (or set UNSANDBOX_API_KEY)")
    key_parser.add_argument("--extend", action="store_true", help="Open browser to extend key")

    # Session subcommand
    session_parser = subparsers.add_parser("session", help="Interactive shell/REPL sessions")
    session_parser.add_argument("-s", "--shell", help="Shell/REPL to use (default: bash)")
    session_parser.add_argument("-l", "--list", action="store_true", help="List active sessions")
    session_parser.add_argument("--attach", metavar="ID", help="Reconnect to session")
    session_parser.add_argument("--kill", metavar="ID", help="Terminate session")
    session_parser.add_argument("--snapshot", metavar="SESSION_ID", help="Create snapshot of session")
    session_parser.add_argument("--restore", metavar="SNAPSHOT_ID", help="Restore from snapshot ID")
    session_parser.add_argument("--snapshot-name", metavar="NAME", help="Name for snapshot")
    session_parser.add_argument("--hot", action="store_true", help="Hot snapshot (no freeze)")
    session_parser.add_argument("--audit", action="store_true", help="Record session")
    session_parser.add_argument("--tmux", action="store_true", help="Enable tmux persistence")
    session_parser.add_argument("--screen", action="store_true", help="Enable screen persistence")
    session_parser.add_argument("-f", "--files", action="append", metavar="FILE", help="Add input file")
    session_parser.add_argument("-n", "--network", choices=["zerotrust", "semitrusted"])
    session_parser.add_argument("-v", "--vcpu", type=int, choices=range(1, 9))
    session_parser.add_argument("-k", "--api-key")

    # Service subcommand
    service_parser = subparsers.add_parser("service", help="Persistent services")
    service_parser.add_argument("--name", help="Service name")
    service_parser.add_argument("--ports", help="Comma-separated ports")
    service_parser.add_argument("--domains", help="Comma-separated custom domains")
    service_parser.add_argument("--type", dest="service_type", help="Service type for SRV records (minecraft, mumble, teamspeak, source, tcp, udp)")
    service_parser.add_argument("--bootstrap", help="Bootstrap command or URI")
    service_parser.add_argument("--bootstrap-file", dest="bootstrap_file", help="Upload local file as bootstrap script")
    service_parser.add_argument("-f", "--files", action="append", metavar="FILE", help="Add input file")
    service_parser.add_argument("-l", "--list", action="store_true", help="List services")
    service_parser.add_argument("--info", metavar="ID", help="Get service details")
    service_parser.add_argument("--tail", metavar="ID", help="Get last 9000 lines of logs")
    service_parser.add_argument("--logs", metavar="ID", help="Get all logs")
    service_parser.add_argument("--freeze", metavar="ID", help="Freeze service")
    service_parser.add_argument("--unfreeze", metavar="ID", help="Unfreeze service")
    service_parser.add_argument("--destroy", metavar="ID", help="Destroy service")
    service_parser.add_argument("--snapshot", metavar="SERVICE_ID", help="Create snapshot of service")
    service_parser.add_argument("--restore", metavar="SNAPSHOT_ID", help="Restore from snapshot ID")
    service_parser.add_argument("--snapshot-name", metavar="NAME", help="Name for snapshot")
    service_parser.add_argument("--hot", action="store_true", help="Hot snapshot (no freeze)")
    service_parser.add_argument("--execute", metavar="ID", help="Execute command in service")
    service_parser.add_argument("--command", help="Command to execute (with --execute)")
    service_parser.add_argument("--dump-bootstrap", metavar="ID", help="Dump bootstrap script")
    service_parser.add_argument("--dump-file", metavar="FILE", help="File to save bootstrap (with --dump-bootstrap)")
    service_parser.add_argument("-n", "--network", choices=["zerotrust", "semitrusted"])
    service_parser.add_argument("-v", "--vcpu", type=int, choices=range(1, 9))
    service_parser.add_argument("-k", "--api-key")

    # Snapshot subcommand
    snapshot_parser = subparsers.add_parser("snapshot", help="Manage container snapshots")
    snapshot_parser.add_argument("-l", "--list", action="store_true", help="List all snapshots")
    snapshot_parser.add_argument("--info", metavar="ID", help="Get snapshot details")
    snapshot_parser.add_argument("--delete", metavar="ID", help="Delete a snapshot")
    snapshot_parser.add_argument("--clone", metavar="ID", help="Clone snapshot to new session/service")
    snapshot_parser.add_argument("--type", help="Type for clone (session or service)")
    snapshot_parser.add_argument("--name", help="Name for cloned session/service")
    snapshot_parser.add_argument("--shell", help="Shell for cloned session")
    snapshot_parser.add_argument("--ports", help="Ports for cloned service")
    snapshot_parser.add_argument("-k", "--api-key")

    # Execute options (default command)
    parser.add_argument("source_file", nargs="?", help="Source file to execute")
    parser.add_argument("-s", "--shell", dest="exec_shell", metavar="LANG", help="Execute inline code with specified language (defaults to bash if arg is not a file)")
    parser.add_argument("-e", "--env", action="append", metavar="KEY=VALUE", help="Set environment variable")
    parser.add_argument("-f", "--files", action="append", metavar="FILE", help="Add input file")
    parser.add_argument("-a", "--artifacts", action="store_true", help="Return artifacts")
    parser.add_argument("-o", "--output-dir", help="Output directory for artifacts")
    parser.add_argument("-y", "--yes", action="store_true", help="Skip confirmations")

    args = parser.parse_args()

    if args.command == "key":
        cmd_key(args)
    elif args.command == "session":
        cmd_session(args)
    elif args.command == "service":
        cmd_service(args)
    elif args.command == "snapshot":
        cmd_snapshot(args)
    elif args.source_file:
        cmd_execute(args)
    else:
        parser.print_help()
        sys.exit(1)


if __name__ == "__main__":
    main()
