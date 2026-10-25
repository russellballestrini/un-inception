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
un image [options]                # Manage images
un languages [options]            # List available languages
un key                            # Check API key
un paas logs [options]            # Platform logs (partner keys)
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
un service --add-domain ID --domains example.com      # Add domains
un service --remove-domain ID --domains example.com   # Remove domains
un service --set-domains ID --domains a.com,b.com     # Replace all domains
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
| `--add-domain ID` | Add domains (requires --domains) |
| `--remove-domain ID` | Remove domains (requires --domains) |
| `--set-domains ID` | Replace all domains (requires --domains) |
| `--auto-unfreeze ID` | Enable auto-unfreeze on HTTP request |
| `--no-auto-unfreeze ID` | Disable auto-unfreeze |
| `--show-freeze-page ID` | Show payment page when frozen (default) |
| `--no-show-freeze-page ID` | Return JSON error when frozen |
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

## Image Command

```bash
un image --list                   # List all images
un image --info ID                # Get image details
un image --delete ID              # Delete image
un image --lock ID                # Prevent deletion
un image --unlock ID              # Allow deletion
un image --publish ID             # Publish from service/snapshot
un image --visibility ID public   # Set visibility (private/unlisted/public)
un image --spawn ID               # Spawn new service from image
un image --clone ID               # Clone image
```

### Image Options

| Option | Description |
|--------|-------------|
| `--list`, `-l` | List all images |
| `--info ID` | Get image details |
| `--delete ID` | Delete image |
| `--lock ID` | Prevent deletion |
| `--unlock ID` | Allow deletion |
| `--publish ID` | Publish image from service or snapshot |
| `--visibility ID MODE` | Set visibility: private, unlisted, or public |
| `--spawn ID` | Spawn new service from image |
| `--clone ID` | Clone image |
| `--name NAME` | Name for spawned/cloned resource |
| `--ports PORTS` | Ports for spawned service |
| `--source-type TYPE` | Source type: service or snapshot (for publish) |

## Languages Command

```bash
un languages                      # List all available languages (one per line)
un languages --json               # Output as JSON array for scripts
```

### Languages Options

| Option | Description |
|--------|-------------|
| `--json` | Output as JSON array instead of one per line |

The languages command fetches the list from the API and caches it in `~/.unsandbox/languages.json` for 1 hour.

**Default output** (one language per line, suitable for piping):
```
python
javascript
go
rust
...
```

**JSON output** (`--json`):
```json
["python", "javascript", "go", "rust", ...]
```

## Key Command

```bash
un key                            # Check API key validity
un key --rotate                   # Rotate this key's secret (sudo OTP), update accounts.csv
un key --rotate --otp CODE        # Finish a rotation started without a terminal
```

### `un key` output

`POST /keys/validate` on the API base, empty body, HMAC-signed
(`timestamp:POST:/keys/validate:`). Response handling:

- **200** — our key validated. Payload (fields we display; others such as
  `rate_per_minute`, `burst`, `concurrency`, `api_key_id` may follow and get ignored):

  | Field | Type | Meaning |
  |-------|------|---------|
  | `valid` | bool | `true` |
  | `status` | string | `"active"` (the API answers 401 for any other state today) |
  | `tier` | int | key tier |
  | `valid_through` | int | expiry, **milliseconds** since the Unix epoch |

  The payload carries **no public key and no `expires_at` / `time_remaining`** (those
  came from an older portal endpoint). Print our own public key (the one we signed
  with) and derive expiry from `valid_through`:

  ```
  Valid
  Public Key: unsb-pk-xxxx-xxxx-xxxx-xxxx
  Tier: 5
  Status: active
  Expires: 2027-06-19T12:45:27Z
  Time Remaining: 236d 4h 12m
  ```

  `Expires` = `valid_through` as UTC ISO-8601 (`YYYY-MM-DDTHH:MM:SSZ`, whole seconds).
  `Time Remaining` = `valid_through` minus now, floored: `<days>d <hours>h <minutes>m`;
  `expired` when zero or negative. Print `N/A` for `Tier`, `Status`, `Expires` or `Time
  Remaining` only when the payload lacks its source field. Exit 0.
  A 200 whose `status` differs from `"active"` gets handled like the matching 401 below
  (`expired` → Expired, `suspended` → Suspended, anything else → Invalid with
  `Reason: status <status>`).
- **401** `{"error": ..., "valid": false}` — our key failed. Map `error`:
  - `":expired"` (or `"expired"`) → print `Expired`, `Public Key: <pk>`,
    `To renew: Visit https://unsandbox.com/keys/extend`.
  - `":suspended"` (or `"suspended"`) → print `Suspended`, `Public Key: <pk>`.
  - anything else (`"Invalid API key"`, `":invalid_signature"`, missing headers, ...) →
    print `Invalid`, `Public Key: <pk>`, `Reason: <error>`.

  Exit 3 (authentication error) for all three.
- Other non-2xx: `Error: <message>` to stderr, exit 4. No response: `Error: <message>`,
  exit 1.

### `un key --rotate`

Replaces the key's secret. Anyone holding the current secret may rotate it; if
the key has an email, the API emails a one-time code (sudo OTP) and the rotation
completes only with that code — the human in the loop.

Flow (byte-identical across all implementations):

1. `POST /keys/self/rotate-secret`, body `{}`, header `Content-Type: application/json`,
   HMAC-signed as usual (`timestamp:POST:/keys/self/rotate-secret:{}`).
2. Response handling:
   - **428** `{"challenge_id": ...}`: an OTP was emailed.
     - stdin is a terminal and no `--otp`: print
       `One-time code emailed to the key owner.` and prompt `Code: ` on stderr; read one
       line; go to step 3.
     - otherwise: save the challenge id to
       `~/.unsandbox/rotate-<first 16 hex of sha256(public_key)>.challenge` (dir 0700,
       file 0600) and print `One-time code emailed to the key owner. Finish with: un key
       --rotate --otp CODE`; exit 0.
   - **403** `{"error":"email_required"}`: print `Error: this key has no email; add one
     or use POST /keys/recover` to stderr; exit 3.
   - **401**: `Error: authentication failed`, exit 3. Other non-2xx: `Error: <message>`, exit 4.
3. With a code (prompted, or `--otp CODE` + the saved challenge file): repeat step 1 with
   a fresh timestamp/signature plus headers `X-Sudo-OTP: CODE` and
   `X-Sudo-Challenge: <challenge_id>`. Delete the challenge file after any final answer.
4. **200** `{"secret": NEW}`:
   - If the credentials came from an accounts.csv file (tiers 3/4): rewrite that file —
     replace the secret (2nd field) on the line whose 1st field equals the public key;
     keep every other line, field (labels), comment and the line order unchanged; write a
     temp file in the same directory with mode 0600, then rename over the original.
     Print `Secret rotated. Updated <path> (public key <pk>).` — **never print the new
     secret.**
   - If they came from flags or environment variables: there is nowhere to persist it;
     print `New secret (shown once, store it now): <secret>` and
     `Warning: UNSANDBOX_SECRET_KEY / -k still hold the old, now invalid, secret.`
   Exit 0.

The old secret stops working immediately. Finding and updating other places the key
is used is the key owner's job, not the tool's.

### API base URL

Every implementation reads `UNSANDBOX_API_URL` (default `https://api.unsandbox.com`,
no trailing slash) for all requests, so CLIs can be pointed at a staging or local
endpoint.

### Portal base URL

Every implementation reads `UNSANDBOX_PORTAL_URL` (default `https://unsandbox.com`,
trailing slashes stripped) for portal requests: `un paas logs` and the
`/keys/extend` renewal link.

## PaaS Logs Command

Platform logs live on the **portal**, not the API host: `GET {PORTAL}/logs/{source}`
and `GET {PORTAL}/logs/{source}/stream`, where source is `api`, `portal`,
`pool/<node>` or `all`. (`/paas/logs` exists on neither host.) Access needs a
partner API key with log access enabled; anything else answers 401/403.

```bash
un paas logs                          # last 100 lines, all sources
un paas logs --api -n 500             # API server
un paas logs --portal --grep error    # portal, lines matching 'error'
un paas logs --pool cammy --since 1h  # one pool node
un paas logs -l warning               # warning and above
un paas logs --follow                 # stream (Ctrl+C to stop)
```

| Option | Description |
|--------|-------------|
| `--all` / `--api` / `--portal` | Source (default `all`) |
| `--pool NODE` | Source `pool/NODE` |
| `--source SRC` | Source string: `api`, `portal`, `pool/NODE`, `all` |
| `--lines N`, `-n N` | Lines (default 100, clamped 1..10000) |
| `--since T` | Window: `30s`, `5m`, `1h`, `1d` (default: cursor, else `5m`) |
| `--grep PAT` | Server-side filter |
| `--level L`, `-l L` | Min level: `debug info notice warning err crit alert emerg` |
| `--follow`, `-f` | Stream via SSE |
| `--json` | Print the raw JSON response |
| `-p KEY` / `-k KEY` / `--account N` | Credentials, as for every command |

Request: query `lines`, `since`, `grep`, `level` (percent-encoded; omit empty
ones). HMAC signs the **path only**: `timestamp:GET:/logs/{source}:` (empty body;
the server verifies `conn.request_path`, no query string).

Response (batch): single source `{"source","host","unit","lines":["..."],"count"}`
— print each line as is. `all` returns `{"sources":[...],"lines":[{"source","line"}],"count"}`
— print `%-12s %s` (source, line). Non-200: print `Error: HTTP <code> — <error>` to
stderr and exit 1. On success write the current Unix time to
`~/.unsandbox/paas_log_cursor`; without `--since`, the next fetch asks for the
seconds since that cursor (minimum `5s`), so repeated calls return only new lines.

Stream: each event `data: {"source","line"}` prints as `%-12s %s`; comment lines
(`: keepalive`) print nothing. When the connection drops (or ends with 200, or a 5xx),
reconnect with fresh signature headers after a backoff (2 s doubling to 30 s),
adding `since=<seconds since last line + 5, min 10>s`. On 401/403 print
`Error: HTTP <code> (auth failed)` and exit 1.

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
6. **No secret on any command line** - `ps` and `/proc/<pid>/cmdline` show every process's
   argv. Compute HMAC in-process, or feed the key to a child through a pipe or its environment
   (never `openssl dgst -hmac KEY`, `jq --arg`, or `-e SECRET=...`). Never park a response
   that may carry a secret (rotate-secret does) in a fixed `/tmp` path. Enforced by
   `scripts/lint-secret-argv.sh` (`make lint-secrets`, CI job `lint-secret-argv`).

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
