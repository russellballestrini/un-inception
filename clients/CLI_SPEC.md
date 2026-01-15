# Unsandbox CLI Specification

**Every SDK implementation MUST include a CLI interface identical to un.c.**

One file. One CLI spec. 42+ languages. All implementing the same API.

## Core Principle

Each SDK file (un.py, un.js, un.rb, etc.) is BOTH:
1. **A library** - Can be imported/required by other code
2. **A CLI tool** - Can be executed directly from command line

```bash
# Library usage
python -c "from un import execute_code; print(execute_code('python', 'print(1)'))"

# CLI usage
python un.py script.py
python un.py -s bash 'echo hello'
python un.py session --tmux
```

## CLI Entry Points

Each language must detect when run as main:

```python
# Python
if __name__ == "__main__":
    cli_main()
```

```javascript
// JavaScript (Node.js)
if (require.main === module) {
    cliMain();
}
```

```ruby
# Ruby
if __FILE__ == $0
  cli_main
end
```

```go
// Go - separate main.go that imports the library
func main() {
    un.CliMain()
}
```

## Command Structure

```
un [options] <source_file>        # Execute code file
un session [options]              # Interactive session
un service [options]              # Manage services
un snapshot [options]             # Manage snapshots
un key                            # Check API key
```

## Global Options

| Option | Short | Description |
|--------|-------|-------------|
| `--shell LANG` | `-s` | Language for inline code |
| `--env KEY=VAL` | `-e` | Set environment variable |
| `--file FILE` | `-f` | Add input file to /tmp/ |
| `--file-path FILE` | `-F` | Add input file with path preserved |
| `--artifacts` | `-a` | Return compiled artifacts |
| `--output DIR` | `-o` | Output directory for artifacts |
| `--public-key KEY` | `-p` | API public key |
| `--secret-key KEY` | `-k` | API secret key |
| `--network MODE` | `-n` | Network: zerotrust or semitrusted |
| `--vcpu N` | `-v` | vCPU count (1-8) |
| `--yes` | `-y` | Skip confirmation prompts |
| `--help` | `-h` | Show help |

## Execute Command (Default)

```bash
un script.py                      # Execute Python script
un -s bash 'echo hello'           # Inline bash command
un -e DEBUG=1 script.py           # With environment variable
un -f data.csv process.py         # With input file
un -a -o ./bin main.c             # Save compiled artifacts
un -n semitrusted crawler.py      # With network access
```

## Session Command

```bash
un session                        # Interactive bash
un session --shell python3        # Python REPL
un session --tmux                 # Persistent session (can reconnect)
un session --screen               # Persistent with screen
un session --list                 # List active sessions
un session --attach ID            # Reconnect to session
un session --kill ID              # Terminate session
un session --freeze ID            # Pause session
un session --unfreeze ID          # Resume session
un session --boost ID             # Add resources
un session --unboost ID           # Remove boost
un session --snapshot ID          # Create snapshot
un session -n semitrusted         # With network access
```

### Session Options

| Option | Description |
|--------|-------------|
| `--shell SHELL` | Shell/REPL to use (default: bash) |
| `--list`, `-l` | List active sessions |
| `--attach ID` | Reconnect to existing session |
| `--kill ID` | Terminate a session |
| `--freeze ID` | Pause session |
| `--unfreeze ID` | Resume session |
| `--boost ID` | Add vCPUs/RAM |
| `--unboost ID` | Remove boost |
| `--tmux` | Enable persistence with tmux |
| `--screen` | Enable persistence with screen |
| `--snapshot ID` | Create snapshot |
| `--snapshot-name NAME` | Name for snapshot |
| `--hot` | Live snapshot (no freeze) |
| `--audit` | Record session |

## Service Command

```bash
un service --list                 # List all services
un service --name myapp --ports 80 --bootstrap "python -m http.server 80"
un service --info ID              # Get service details
un service --logs ID              # Get bootstrap logs
un service --freeze ID            # Pause service
un service --unfreeze ID          # Resume service
un service --destroy ID           # Delete service
un service --lock ID              # Prevent deletion
un service --unlock ID            # Allow deletion
un service --execute ID 'cmd'     # Run command in service
un service --redeploy ID          # Re-run bootstrap
un service --snapshot ID          # Create snapshot
```

### Service Options

| Option | Description |
|--------|-------------|
| `--name NAME` | Service name (creates new) |
| `--ports PORTS` | Comma-separated ports |
| `--domains DOMAINS` | Custom domains |
| `--type TYPE` | Service type (minecraft, tcp, udp) |
| `--bootstrap CMD` | Bootstrap command |
| `--bootstrap-file FILE` | Bootstrap from file |
| `--env-file FILE` | Load env from .env file |
| `--list`, `-l` | List all services |
| `--info ID` | Get service details |
| `--logs ID` | Get all logs |
| `--tail ID` | Get last 9000 lines |
| `--freeze ID` | Pause service |
| `--unfreeze ID` | Resume service |
| `--destroy ID` | Delete service |
| `--lock ID` | Prevent deletion |
| `--unlock ID` | Allow deletion |
| `--resize ID` | Resize (with --vcpu) |
| `--redeploy ID` | Re-run bootstrap |
| `--execute ID CMD` | Run command |
| `--snapshot ID` | Create snapshot |

### Service Environment Vault

```bash
un service env status ID          # Show vault status
un service env set ID             # Set from --env-file or stdin
un service env export ID          # Export to stdout
un service env delete ID          # Delete vault
```

## Snapshot Command

```bash
un snapshot --list                # List all snapshots
un snapshot --info ID             # Get details
un snapshot --delete ID           # Delete snapshot
un snapshot --lock ID             # Prevent deletion
un snapshot --unlock ID           # Allow deletion
un snapshot --clone ID            # Clone to new session/service
un snapshot --clone ID --type service --name myapp --ports 80
```

### Snapshot Options

| Option | Description |
|--------|-------------|
| `--list`, `-l` | List all snapshots |
| `--info ID` | Get snapshot details |
| `--delete ID` | Delete snapshot |
| `--lock ID` | Prevent deletion |
| `--unlock ID` | Allow deletion |
| `--clone ID` | Clone snapshot |
| `--type TYPE` | Clone type: session or service |
| `--name NAME` | Name for cloned service |
| `--shell SHELL` | Shell for cloned session |
| `--ports PORTS` | Ports for cloned service |

## Key Command

```bash
un key                            # Check API key validity
```

## Output Formatting

### Execute Output
```
stdout content here
---
Exit code: 0
Execution time: 123ms
```

### List Output (sessions, services, snapshots)
```
ID                                    NAME              STATUS    CREATED
abc123-def456-789                     my-session        running   2024-01-15 10:30:00
```

### Error Output
```
Error: <message>
```

Errors go to stderr, exit code 1.

## Exit Codes

| Code | Meaning |
|------|---------|
| 0 | Success |
| 1 | General error |
| 2 | Invalid arguments |
| 3 | Authentication error |
| 4 | API error |
| 5 | Timeout |

## Implementation Requirements

1. **Argument parsing** - Use language-appropriate library (argparse, commander, clap, etc.)
2. **Subcommands** - session, service, snapshot, key
3. **Consistent output** - Same format across all languages
4. **Error handling** - Proper exit codes, errors to stderr
5. **Credential resolution** - 4-tier system (-p/-k flags > env > ~/.unsandbox > ./accounts.csv)

## Available Shells/REPLs

```
Shells: bash, dash, sh, zsh, fish, ksh, tcsh, csh, elvish, xonsh, ash
REPLs:  python3, bpython, ipython, node, ruby, irb, lua, php, perl
        guile, ghci, erl, iex, sbcl, clisp, r, julia, clojure
```

## File Size Target

With CLI included, each SDK should be approximately:
- **2,000-3,000 lines** for higher-level languages (Python, Ruby, JavaScript)
- **3,000-4,000 lines** for verbose languages (Java, Go, Rust)

Reference: un.c = 6,354 lines (includes manual HTTP, JSON, crypto)
