# UN CLI Inception

The UN CLI written in every language it can execute. **42 implementations, one unified interface.**

> "If you replace every line of un.c with a different programming language, but it still executes code on unsandbox... is it still un?"

## License

PUBLIC DOMAIN - NO LICENSE, NO WARRANTY

This is free public domain software for the public good of a permacomputer hosted
at permacomputer.com - an always-on computer by the people, for the people. One
which is durable, easy to repair, and distributed like tap water for machine
learning intelligence.

The permacomputer is community-owned infrastructure optimized around four values:

  TRUTH    - Source code must be open source & freely distributed
  FREEDOM  - Voluntary participation without corporate control
  HARMONY  - Systems operating with minimal waste that self-renew
  LOVE     - Individual rights protected while fostering cooperation

This software contributes to that vision by enabling code execution across 42+
programming languages through a unified interface, accessible to all. Code is
seeds to sprout on any abandoned technology.

Learn more: https://www.permacomputer.com

Anyone is free to copy, modify, publish, use, compile, sell, or distribute this
software, either in source code form or as a compiled binary, for any purpose,
commercial or non-commercial, and by any means.

NO WARRANTY. THE SOFTWARE IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY KIND.

That said, our permacomputer's digital membrane stratum continuously runs unit,
integration, and functional tests on all of it's own software - with our
permacomputer monitoring itself, repairing itself, with minimal human in the
loop guidance. Our agents do their best.

Copyright 2025 TimeHexOn & foxhop & russell@unturf
https://www.timehexon.com
https://www.foxhop.net
https://www.unturf.com/software

## Quick Start

```bash
# Clone all 42 implementations
git clone https://github.com/russellballestrini/un-inception.git
cd un-inception

# Set your API key
export UNSANDBOX_API_KEY=your_key_here

# Run any implementation
python3 un.py test/fib.py
node un.js test/fib.py
ruby un.rb test/fib.py
go run un.go test/fib.py

# They all produce the same output:
# fib(10) = 55
```

## Implementations

| Language | File | Category |
|----------|------|----------|
| Python | `un.py` | Scripting |
| JavaScript | `un.js` | Scripting |
| TypeScript | `un.ts` | Scripting |
| Ruby | `un.rb` | Scripting |
| PHP | `un.php` | Scripting |
| Perl | `un.pl` | Scripting |
| Lua | `un.lua` | Scripting |
| Bash | `un.sh` | Scripting |
| Go | `un.go` | Systems |
| Rust | `un.rs` | Systems |
| C | `un_inception.c` | Systems |
| C++ | `un.cpp` | Systems |
| D | `un.d` | Systems |
| Nim | `un.nim` | Systems |
| Zig | `un.zig` | Systems |
| V | `un.v` | Systems |
| Java | `Un.java` | JVM/.NET |
| Kotlin | `un.kt` | JVM/.NET |
| C# | `Un.cs` | JVM/.NET |
| F# | `un.fs` | JVM/.NET |
| Groovy | `un.groovy` | JVM/.NET |
| Dart | `un.dart` | JVM/.NET |
| Haskell | `un.hs` | Functional |
| OCaml | `un.ml` | Functional |
| Clojure | `un.clj` | Functional |
| Scheme | `un.scm` | Functional |
| Common Lisp | `un.lisp` | Functional |
| Erlang | `un.erl` | Functional |
| Elixir | `un.ex` | Functional |
| Julia | `un.jl` | Scientific |
| R | `un.r` | Scientific |
| Crystal | `un.cr` | Scientific |
| Fortran | `un.f90` | Scientific |
| COBOL | `un.cob` | Scientific |
| Prolog | `un.pro` | Scientific |
| Forth | `un.forth` | Scientific |
| Tcl | `un.tcl` | Other |
| Raku | `un.raku` | Other |
| Objective-C | `un.m` | Other |
| Deno | `un_deno.ts` | Other |
| PowerShell | `un.ps1` | Other |
| AWK | `un.awk` | Other |

## Features

Each implementation supports:

- **Execute code files** via the unsandbox API
- **Auto-detect language** from file extensions
- **Environment variables** (`-e KEY=VALUE`)
- **Input files** (`-f file.txt`)
- **Artifact collection** (`-a -o ./output`)
- **Interactive sessions** (`session` subcommand)
- **Persistent services** (`service` subcommand)
- **Network modes** (`-n zerotrust` or `-n semitrusted`)

## Usage

```bash
# Execute a script
./un.py script.py

# With environment variables
./un.py -e DEBUG=1 -e NAME=World script.py

# With input files
./un.py -f data.csv -f config.json process.py

# Collect artifacts
./un.py -a -o ./output main.c

# Interactive session
./un.py session --shell python3

# Persistent service
./un.py service --name myapp --ports 8080 --bootstrap "python3 -m http.server 8080"
```

## Testing

```bash
# Run a quick test with any implementation
./un.py test/fib.py
# Output: fib(10) = 55

# Run the full test matrix (requires API key)
./tests/run_matrix.sh
```

## Run with un2 (Meta-inception)

You can use the main `un` CLI to run these implementations inside unsandbox:

```bash
# Python implementation running inside unsandbox
un2 -n semitrusted un.py test/fib.py

# Rust implementation running inside unsandbox
un2 -n semitrusted un.rs test/fib.py

# It's turtles all the way down
```

## Links

- [unsandbox.com](https://unsandbox.com) - Remote code execution API
- [CLI Inception Gallery](https://unsandbox.com/cli/inception) - Browse all implementations
- [API Documentation](https://unsandbox.com/docs) - Full API reference
