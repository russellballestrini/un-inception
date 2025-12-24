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

API_BASE = "https://api.unsandbox.com"
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


def get_api_key(args_key=None):
    """Get API key from args or environment"""
    key = args_key or os.environ.get("UNSANDBOX_API_KEY")
    if not key:
        print(f"{RED}Error: UNSANDBOX_API_KEY not set{RESET}", file=sys.stderr)
        sys.exit(1)
    return key


def detect_language(filename):
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
        print(f"{RED}Error: Cannot detect language for {filename}{RESET}", file=sys.stderr)
        sys.exit(1)
    return lang


def api_request(endpoint, method="GET", data=None, api_key=None):
    """Make API request and return response"""
    url = f"{API_BASE}{endpoint}"
    headers = {
        "Authorization": f"Bearer {api_key}",
        "Content-Type": "application/json"
    }

    req = urllib.request.Request(url, method=method, headers=headers)
    if data:
        req.data = json.dumps(data).encode('utf-8')

    try:
        with urllib.request.urlopen(req, timeout=300) as resp:
            return json.loads(resp.read().decode('utf-8'))
    except urllib.error.HTTPError as e:
        error_body = e.read().decode('utf-8') if e.fp else str(e)
        print(f"{RED}Error: HTTP {e.code} - {error_body}{RESET}", file=sys.stderr)
        sys.exit(1)
    except urllib.error.URLError as e:
        print(f"{RED}Error: {e.reason}{RESET}", file=sys.stderr)
        sys.exit(1)


def cmd_execute(args):
    """Execute source code"""
    api_key = get_api_key(args.api_key)

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
    result = api_request("/execute", method="POST", data=payload, api_key=api_key)

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
    api_key = get_api_key(args.api_key)

    if args.list:
        result = api_request("/sessions", api_key=api_key)
        sessions = result.get("sessions", [])
        if not sessions:
            print("No active sessions")
        else:
            print(f"{'ID':<40} {'Shell':<10} {'Status':<10} {'Created'}")
            for s in sessions:
                print(f"{s.get('id', 'N/A'):<40} {s.get('shell', 'N/A'):<10} {s.get('status', 'N/A'):<10} {s.get('created_at', 'N/A')}")
        return

    if args.kill:
        result = api_request(f"/sessions/{args.kill}", method="DELETE", api_key=api_key)
        print(f"{GREEN}Session terminated: {args.kill}{RESET}")
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

    print(f"{YELLOW}Creating session...{RESET}")
    result = api_request("/sessions", method="POST", data=payload, api_key=api_key)
    print(f"{GREEN}Session created: {result.get('id', 'N/A')}{RESET}")
    print(f"{YELLOW}(Interactive sessions require WebSocket - use un2 for full support){RESET}")


def cmd_service(args):
    """Manage persistent services"""
    api_key = get_api_key(args.api_key)

    if args.list:
        result = api_request("/services", api_key=api_key)
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
        result = api_request(f"/services/{args.info}", api_key=api_key)
        print(json.dumps(result, indent=2))
        return

    if args.logs:
        result = api_request(f"/services/{args.logs}/logs", api_key=api_key)
        print(result.get("logs", ""))
        return

    if args.tail:
        result = api_request(f"/services/{args.tail}/logs?lines=9000", api_key=api_key)
        print(result.get("logs", ""))
        return

    if args.sleep:
        result = api_request(f"/services/{args.sleep}/sleep", method="POST", api_key=api_key)
        print(f"{GREEN}Service sleeping: {args.sleep}{RESET}")
        return

    if args.wake:
        result = api_request(f"/services/{args.wake}/wake", method="POST", api_key=api_key)
        print(f"{GREEN}Service waking: {args.wake}{RESET}")
        return

    if args.destroy:
        result = api_request(f"/services/{args.destroy}", method="DELETE", api_key=api_key)
        print(f"{GREEN}Service destroyed: {args.destroy}{RESET}")
        return

    if args.execute:
        payload = {"command": args.command}
        result = api_request(f"/services/{args.execute}/execute", method="POST", data=payload, api_key=api_key)
        if result.get("stdout"):
            print(f"{BLUE}{result['stdout']}{RESET}", end='')
        if result.get("stderr"):
            print(f"{RED}{result['stderr']}{RESET}", end='', file=sys.stderr)
        return

    # Create new service
    if args.name:
        payload = {"name": args.name}
        if args.ports:
            payload["ports"] = [int(p) for p in args.ports.split(',')]
        if args.domains:
            payload["domains"] = args.domains.split(',')
        if args.bootstrap:
            # Check if bootstrap is a file
            if os.path.exists(args.bootstrap):
                with open(args.bootstrap, 'r') as f:
                    payload["bootstrap"] = f.read()
            else:
                payload["bootstrap"] = args.bootstrap
        if args.network:
            payload["network"] = args.network
        if args.vcpu:
            payload["vcpu"] = args.vcpu

        result = api_request("/services", method="POST", data=payload, api_key=api_key)
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
  %(prog)s service --list               List all services
        """
    )

    # Common options
    parser.add_argument("-k", "--api-key", help="API key (or set UNSANDBOX_API_KEY)")
    parser.add_argument("-n", "--network", choices=["zerotrust", "semitrusted"], help="Network mode")
    parser.add_argument("-v", "--vcpu", type=int, choices=range(1, 9), help="vCPU count (1-8)")

    subparsers = parser.add_subparsers(dest="command")

    # Session subcommand
    session_parser = subparsers.add_parser("session", help="Interactive shell/REPL sessions")
    session_parser.add_argument("-s", "--shell", help="Shell/REPL to use (default: bash)")
    session_parser.add_argument("-l", "--list", action="store_true", help="List active sessions")
    session_parser.add_argument("--attach", metavar="ID", help="Reconnect to session")
    session_parser.add_argument("--kill", metavar="ID", help="Terminate session")
    session_parser.add_argument("--audit", action="store_true", help="Record session")
    session_parser.add_argument("--tmux", action="store_true", help="Enable tmux persistence")
    session_parser.add_argument("--screen", action="store_true", help="Enable screen persistence")
    session_parser.add_argument("-n", "--network", choices=["zerotrust", "semitrusted"])
    session_parser.add_argument("-v", "--vcpu", type=int, choices=range(1, 9))
    session_parser.add_argument("-k", "--api-key")

    # Service subcommand
    service_parser = subparsers.add_parser("service", help="Persistent services")
    service_parser.add_argument("--name", help="Service name")
    service_parser.add_argument("--ports", help="Comma-separated ports")
    service_parser.add_argument("--domains", help="Comma-separated custom domains")
    service_parser.add_argument("--bootstrap", help="Bootstrap command/file")
    service_parser.add_argument("-l", "--list", action="store_true", help="List services")
    service_parser.add_argument("--info", metavar="ID", help="Get service details")
    service_parser.add_argument("--tail", metavar="ID", help="Get last 9000 lines of logs")
    service_parser.add_argument("--logs", metavar="ID", help="Get all logs")
    service_parser.add_argument("--sleep", metavar="ID", help="Freeze service")
    service_parser.add_argument("--wake", metavar="ID", help="Unfreeze service")
    service_parser.add_argument("--destroy", metavar="ID", help="Destroy service")
    service_parser.add_argument("--execute", metavar="ID", help="Execute command in service")
    service_parser.add_argument("--command", help="Command to execute (with --execute)")
    service_parser.add_argument("-n", "--network", choices=["zerotrust", "semitrusted"])
    service_parser.add_argument("-v", "--vcpu", type=int, choices=range(1, 9))
    service_parser.add_argument("-k", "--api-key")

    # Execute options (default command)
    parser.add_argument("source_file", nargs="?", help="Source file to execute")
    parser.add_argument("-e", "--env", action="append", metavar="KEY=VALUE", help="Set environment variable")
    parser.add_argument("-f", "--files", action="append", metavar="FILE", help="Add input file")
    parser.add_argument("-a", "--artifacts", action="store_true", help="Return artifacts")
    parser.add_argument("-o", "--output-dir", help="Output directory for artifacts")
    parser.add_argument("-y", "--yes", action="store_true", help="Skip confirmations")

    args = parser.parse_args()

    if args.command == "session":
        cmd_session(args)
    elif args.command == "service":
        cmd_service(args)
    elif args.source_file:
        cmd_execute(args)
    else:
        parser.print_help()
        sys.exit(1)


if __name__ == "__main__":
    main()
