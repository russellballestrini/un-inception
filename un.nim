// PUBLIC DOMAIN - NO LICENSE, NO WARRANTY
//
// This is free public domain software for the public good of a permacomputer hosted
// at permacomputer.com - an always-on computer by the people, for the people. One
// which is durable, easy to repair, and distributed like tap water for machine
// learning intelligence.
//
// The permacomputer is community-owned infrastructure optimized around four values:
//
//   TRUTH    - First principles, math & science, open source code freely distributed
//   FREEDOM  - Voluntary partnerships, freedom from tyranny & corporate control
//   HARMONY  - Minimal waste, self-renewing systems with diverse thriving connections
//   LOVE     - Be yourself without hurting others, cooperation through natural law
//
// This software contributes to that vision by enabling code execution across 42+
// programming languages through a unified interface, accessible to all. Code is
// seeds to sprout on any abandoned technology.
//
// Learn more: https://www.permacomputer.com
//
// Anyone is free to copy, modify, publish, use, compile, sell, or distribute this
// software, either in source code form or as a compiled binary, for any purpose,
// commercial or non-commercial, and by any means.
//
// NO WARRANTY. THE SOFTWARE IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY KIND.
//
// That said, our permacomputer's digital membrane stratum continuously runs unit,
// integration, and functional tests on all of it's own software - with our
// permacomputer monitoring itself, repairing itself, with minimal human in the
// loop guidance. Our agents do their best.
//
// Copyright 2025 TimeHexOn & foxhop & russell@unturf
// https://www.timehexon.com
// https://www.foxhop.net
// https://www.unturf.com/software


# UN CLI - Nim Implementation (using curl subprocess)
# Compile: nim c -d:release un.nim
# Usage:
#   un.nim script.py
#   un.nim -e KEY=VALUE script.py
#   un.nim session --list
#   un.nim service --name web --ports 8080

import os, strutils, osproc, strformat, times

const
  API_BASE = "https://api.unsandbox.com"
  PORTAL_BASE = "https://unsandbox.com"
  BLUE = "\x1b[34m"
  RED = "\x1b[31m"
  GREEN = "\x1b[32m"
  YELLOW = "\x1b[33m"
  RESET = "\x1b[0m"

let langMap = {
  ".py": "python", ".js": "javascript", ".ts": "typescript",
  ".go": "go", ".rs": "rust", ".c": "c", ".cpp": "cpp",
  ".d": "d", ".zig": "zig", ".nim": "nim", ".v": "v",
  ".rb": "ruby", ".php": "php", ".sh": "bash"
}.toTable

proc detectLanguage(filename: string): string =
  let ext = splitFile(filename).ext
  result = langMap.getOrDefault(ext, "")

proc escapeJson(s: string): string =
  result = ""
  for c in s:
    case c
    of '"': result.add("\\\"")
    of '\\': result.add("\\\\")
    of '\n': result.add("\\n")
    of '\r': result.add("\\r")
    of '\t': result.add("\\t")
    else: result.add(c)

proc computeHmac(secretKey: string, message: string): string =
  let cmd = fmt"echo -n '{message}' | openssl dgst -sha256 -hmac '{secretKey}' -hex 2>/dev/null | sed 's/.*= //'"
  result = execProcess(cmd).strip()

proc getTimestamp(): string =
  result = $toUnix(getTime())

proc buildAuthHeaders(meth: string, path: string, body: string, publicKey: string, secretKey: string): string =
  if secretKey == "":
    # Legacy mode: use public_key as bearer token
    return fmt"-H 'Authorization: Bearer {publicKey}'"

  # HMAC mode
  let timestamp = getTimestamp()
  let message = fmt"{timestamp}:{meth}:{path}:{body}"
  let signature = computeHmac(secretKey, message)

  return fmt"-H 'Authorization: Bearer {publicKey}' -H 'X-Timestamp: {timestamp}' -H 'X-Signature: {signature}'"

proc execCurl(cmd: string): string =
  result = execProcess(cmd)

  # Check for timestamp authentication errors
  if result.contains("timestamp") and
     (result.contains("401") or result.contains("expired") or result.contains("invalid")):
    stderr.writeLine(RED & "Error: Request timestamp expired (must be within 5 minutes of server time)" & RESET)
    stderr.writeLine(YELLOW & "Your computer's clock may have drifted." & RESET)
    stderr.writeLine("Check your system time and sync with NTP if needed:")
    stderr.writeLine("  Linux:   sudo ntpdate -s time.nist.gov")
    stderr.writeLine("  macOS:   sudo sntp -sS time.apple.com")
    stderr.writeLine("  Windows: w32tm /resync")
    quit(1)

proc cmdExecute(sourceFile: string, envs: seq[string], artifacts: bool, network: string, vcpu: int, publicKey: string, secretKey: string) =
  let lang = detectLanguage(sourceFile)
  if lang == "":
    stderr.writeLine(RED & "Error: Cannot detect language" & RESET)
    quit(1)

  let code = readFile(sourceFile)
  var json = fmt"""{"language":"{lang}","code":"{escapeJson(code)}""""

  if envs.len > 0:
    json.add(""","env":{""")
    for i, e in envs:
      let parts = e.split('=', 1)
      if parts.len == 2:
        if i > 0: json.add(",")
        json.add(fmt""""{parts[0]}":"{escapeJson(parts[1])}"""")
    json.add("}")

  if artifacts: json.add(""","return_artifacts":true""")
  if network != "": json.add(fmt""","network":"{network}"""")
  if vcpu > 0: json.add(fmt""","vcpu":{vcpu}""")
  json.add("}")

  let authHeaders = buildAuthHeaders("POST", "/execute", json, publicKey, secretKey)
  let cmd = fmt"""curl -s -X POST '{API_BASE}/execute' -H 'Content-Type: application/json' {authHeaders} -d '{json}'"""
  echo execCurl(cmd)

proc cmdSession(list: bool, kill, shell, network: string, vcpu: int, tmux, screen: bool, publicKey: string, secretKey: string) =
  if list:
    let authHeaders = buildAuthHeaders("GET", "/sessions", "", publicKey, secretKey)
    let cmd = fmt"""curl -s -X GET '{API_BASE}/sessions' {authHeaders}"""
    echo execCurl(cmd)
    return

  if kill != "":
    let path = fmt"/sessions/{kill}"
    let authHeaders = buildAuthHeaders("DELETE", path, "", publicKey, secretKey)
    let cmd = fmt"""curl -s -X DELETE '{API_BASE}/sessions/{kill}' {authHeaders}"""
    discard execCurl(cmd)
    echo GREEN & "Session terminated: " & kill & RESET
    return

  var json = fmt"""{"shell":"{if shell != "": shell else: "bash"}""""
  if network != "": json.add(fmt""","network":"{network}"""")
  if vcpu > 0: json.add(fmt""","vcpu":{vcpu}""")
  if tmux: json.add(""","persistence":"tmux"""")
  if screen: json.add(""","persistence":"screen"""")
  json.add("}")

  echo YELLOW & "Creating session..." & RESET
  let authHeaders = buildAuthHeaders("POST", "/sessions", json, publicKey, secretKey)
  let cmd = fmt"""curl -s -X POST '{API_BASE}/sessions' -H 'Content-Type: application/json' {authHeaders} -d '{json}'"""
  echo execCurl(cmd)

proc cmdService(name, ports, bootstrap, serviceType: string, list: bool, info, logs, tail, sleep, wake, destroy, execute, command, dumpBootstrap, dumpFile, network: string, vcpu: int, publicKey: string, secretKey: string) =
  if list:
    let authHeaders = buildAuthHeaders("GET", "/services", "", publicKey, secretKey)
    let cmd = fmt"""curl -s -X GET '{API_BASE}/services' {authHeaders}"""
    echo execCurl(cmd)
    return

  if info != "":
    let path = fmt"/services/{info}"
    let authHeaders = buildAuthHeaders("GET", path, "", publicKey, secretKey)
    let cmd = fmt"""curl -s -X GET '{API_BASE}/services/{info}' {authHeaders}"""
    echo execCurl(cmd)
    return

  if logs != "":
    let path = fmt"/services/{logs}/logs"
    let authHeaders = buildAuthHeaders("GET", path, "", publicKey, secretKey)
    let cmd = fmt"""curl -s -X GET '{API_BASE}/services/{logs}/logs' {authHeaders}"""
    stdout.write(execCurl(cmd))
    return

  if tail != "":
    let path = fmt"/services/{tail}/logs"
    let authHeaders = buildAuthHeaders("GET", path, "", publicKey, secretKey)
    let cmd = fmt"""curl -s -X GET '{API_BASE}/services/{tail}/logs?lines=9000' {authHeaders}"""
    stdout.write(execCurl(cmd))
    return

  if sleep != "":
    let path = fmt"/services/{sleep}/sleep"
    let authHeaders = buildAuthHeaders("POST", path, "", publicKey, secretKey)
    let cmd = fmt"""curl -s -X POST '{API_BASE}/services/{sleep}/sleep' {authHeaders}"""
    discard execCurl(cmd)
    echo GREEN & "Service sleeping: " & sleep & RESET
    return

  if wake != "":
    let path = fmt"/services/{wake}/wake"
    let authHeaders = buildAuthHeaders("POST", path, "", publicKey, secretKey)
    let cmd = fmt"""curl -s -X POST '{API_BASE}/services/{wake}/wake' {authHeaders}"""
    discard execCurl(cmd)
    echo GREEN & "Service waking: " & wake & RESET
    return

  if destroy != "":
    let path = fmt"/services/{destroy}"
    let authHeaders = buildAuthHeaders("DELETE", path, "", publicKey, secretKey)
    let cmd = fmt"""curl -s -X DELETE '{API_BASE}/services/{destroy}' {authHeaders}"""
    discard execCurl(cmd)
    echo GREEN & "Service destroyed: " & destroy & RESET
    return

  if execute != "":
    let json = fmt"""{"command":"{escapeJson(command)}"}"""
    let path = fmt"/services/{execute}/execute"
    let authHeaders = buildAuthHeaders("POST", path, json, publicKey, secretKey)
    let cmd = fmt"""curl -s -X POST '{API_BASE}/services/{execute}/execute' -H 'Content-Type: application/json' {authHeaders} -d '{json}'"""
    let result = execCurl(cmd)

    # Simple parsing for stdout/stderr
    let stdoutStart = result.find("\"stdout\":\"")
    if stdoutStart >= 0:
      let start = stdoutStart + 10
      var endPos = start
      while endPos < result.len:
        if result[endPos] == '"' and (endPos == 0 or result[endPos-1] != '\\'):
          break
        inc endPos
      if endPos > start:
        var output = result[start..<endPos]
        output = output.replace("\\n", "\n").replace("\\r", "\r").replace("\\t", "\t").replace("\\\"", "\"").replace("\\\\", "\\")
        stdout.write(output)

    let stderrStart = result.find("\"stderr\":\"")
    if stderrStart >= 0:
      let start = stderrStart + 10
      var endPos = start
      while endPos < result.len:
        if result[endPos] == '"' and (endPos == 0 or result[endPos-1] != '\\'):
          break
        inc endPos
      if endPos > start:
        var errout = result[start..<endPos]
        errout = errout.replace("\\n", "\n").replace("\\r", "\r").replace("\\t", "\t").replace("\\\"", "\"").replace("\\\\", "\\")
        stderr.write(errout)
    return

  if dumpBootstrap != "":
    stderr.writeLine("Fetching bootstrap script from " & dumpBootstrap & "...")
    let json = """{"command":"cat /tmp/bootstrap.sh"}"""
    let path = fmt"/services/{dumpBootstrap}/execute"
    let authHeaders = buildAuthHeaders("POST", path, json, publicKey, secretKey)
    let cmd = fmt"""curl -s -X POST '{API_BASE}/services/{dumpBootstrap}/execute' -H 'Content-Type: application/json' {authHeaders} -d '{json}'"""
    let result = execCurl(cmd)

    let stdoutStart = result.find("\"stdout\":\"")
    if stdoutStart >= 0:
      let start = stdoutStart + 10
      var endPos = start
      while endPos < result.len:
        if result[endPos] == '"' and (endPos == 0 or result[endPos-1] != '\\'):
          break
        inc endPos
      if endPos > start:
        var bootstrapScript = result[start..<endPos]
        bootstrapScript = bootstrapScript.replace("\\n", "\n").replace("\\r", "\r").replace("\\t", "\t").replace("\\\"", "\"").replace("\\\\", "\\")

        if dumpFile != "":
          try:
            writeFile(dumpFile, bootstrapScript)
            when defined(posix):
              import os
              setFilePermissions(dumpFile, {fpUserExec, fpUserWrite, fpUserRead, fpGroupExec, fpGroupRead, fpOthersExec, fpOthersRead})
            echo "Bootstrap saved to " & dumpFile
          except IOError as e:
            stderr.writeLine(RED & "Error: Could not write to " & dumpFile & ": " & e.msg & RESET)
            quit(1)
        else:
          stdout.write(bootstrapScript)
      else:
        stderr.writeLine(RED & "Error: Failed to fetch bootstrap (service not running or no bootstrap file)" & RESET)
        quit(1)
    else:
      stderr.writeLine(RED & "Error: Failed to fetch bootstrap (service not running or no bootstrap file)" & RESET)
      quit(1)
    return

  if name != "":
    var json = fmt"""{"name":"{name}""""
    if ports != "": json.add(fmt""","ports":[{ports}]""")
    if bootstrap != "":
      if fileExists(bootstrap):
        let bootCode = readFile(bootstrap)
        json.add(fmt""","bootstrap":"{escapeJson(bootCode)}"""")
      else:
        json.add(fmt""","bootstrap":"{escapeJson(bootstrap)}"""")
    if serviceType != "": json.add(fmt""","service_type":"{serviceType}"""")
    if network != "": json.add(fmt""","network":"{network}"""")
    if vcpu > 0: json.add(fmt""","vcpu":{vcpu}""")
    json.add("}")

    echo YELLOW & "Creating service..." & RESET
    let authHeaders = buildAuthHeaders("POST", "/services", json, publicKey, secretKey)
    let cmd = fmt"""curl -s -X POST '{API_BASE}/services' -H 'Content-Type: application/json' {authHeaders} -d '{json}'"""
    echo execCurl(cmd)
    return

  stderr.writeLine(RED & "Error: Specify --name to create a service" & RESET)
  quit(1)

proc cmdKey(extend: bool, publicKey: string, secretKey: string) =
  let authHeaders = buildAuthHeaders("POST", "/keys/validate", "", publicKey, secretKey)
  let cmd = fmt"""curl -s -X POST '{PORTAL_BASE}/keys/validate' {authHeaders}"""
  let response = execCurl(cmd)

  # Parse JSON response manually (simple approach)
  if response.contains("\"status\":\"valid\""):
    echo GREEN & "Valid" & RESET
    # Extract and display key info
    if response.contains("\"public_key\":"):
      let pkStart = response.find("\"public_key\":\"") + 14
      let pkEnd = response.find("\"", pkStart)
      if pkEnd > pkStart:
        let pubKey = response[pkStart..<pkEnd]
        echo "Public Key: " & pubKey
        if extend:
          let extendUrl = fmt"{PORTAL_BASE}/keys/extend?pk={pubKey}"
          echo BLUE & "Opening browser to extend key..." & RESET
          discard execProcess(fmt"xdg-open '{extendUrl}'")

    # Extract tier
    if response.contains("\"tier\":"):
      let tierStart = response.find("\"tier\":\"") + 8
      let tierEnd = response.find("\"", tierStart)
      if tierEnd > tierStart:
        echo "Tier: " & response[tierStart..<tierEnd]

    # Extract expires_at
    if response.contains("\"expires_at\":"):
      let expiresStart = response.find("\"expires_at\":\"") + 14
      let expiresEnd = response.find("\"", expiresStart)
      if expiresEnd > expiresStart:
        echo "Expires: " & response[expiresStart..<expiresEnd]

  elif response.contains("\"status\":\"expired\""):
    echo RED & "Expired" & RESET

    # Extract and display key info
    var pubKey = ""
    if response.contains("\"public_key\":"):
      let pkStart = response.find("\"public_key\":\"") + 14
      let pkEnd = response.find("\"", pkStart)
      if pkEnd > pkStart:
        pubKey = response[pkStart..<pkEnd]
        echo "Public Key: " & pubKey

    # Extract tier
    if response.contains("\"tier\":"):
      let tierStart = response.find("\"tier\":\"") + 8
      let tierEnd = response.find("\"", tierStart)
      if tierEnd > tierStart:
        echo "Tier: " & response[tierStart..<tierEnd]

    # Extract expires_at
    if response.contains("\"expires_at\":"):
      let expiresStart = response.find("\"expires_at\":\"") + 14
      let expiresEnd = response.find("\"", expiresStart)
      if expiresEnd > expiresStart:
        echo "Expired: " & response[expiresStart..<expiresEnd]

    echo ""
    echo YELLOW & "To renew: Visit " & PORTAL_BASE & "/keys/extend" & RESET

    if extend and pubKey != "":
      let extendUrl = fmt"{PORTAL_BASE}/keys/extend?pk={pubKey}"
      echo BLUE & "Opening browser to extend key..." & RESET
      discard execProcess(fmt"xdg-open '{extendUrl}'")

  else:
    echo RED & "Invalid" & RESET
    if response.contains("\"error\":"):
      let errStart = response.find("\"error\":\"") + 9
      let errEnd = response.find("\"", errStart)
      if errEnd > errStart:
        echo "Error: " & response[errStart..<errEnd]

proc main() =
  var publicKey = getEnv("UNSANDBOX_PUBLIC_KEY", "")
  var secretKey = getEnv("UNSANDBOX_SECRET_KEY", "")

  # Fall back to UNSANDBOX_API_KEY for backwards compatibility
  if publicKey == "":
    publicKey = getEnv("UNSANDBOX_API_KEY", "")

  let args = commandLineParams()

  if args.len < 1:
    stderr.writeLine("Usage: un.nim [options] <source_file>")
    stderr.writeLine("       un.nim session [options]")
    stderr.writeLine("       un.nim service [options]")
    stderr.writeLine("       un.nim key [options]")
    quit(1)

  if args[0] == "key":
    var extend = false
    var i = 1
    while i < args.len:
      case args[i]
      of "--extend": extend = true
      of "-k": publicKey = args[i+1]; inc i
      inc i
    cmdKey(extend, publicKey, secretKey)
    return

  if args[0] == "session":
    var list = false
    var kill, shell, network = ""
    var vcpu = 0
    var tmux, screen = false
    var i = 1
    while i < args.len:
      case args[i]
      of "--list": list = true
      of "--kill": kill = args[i+1]; inc i
      of "--shell": shell = args[i+1]; inc i
      of "-n": network = args[i+1]; inc i
      of "-v": vcpu = parseInt(args[i+1]); inc i
      of "--tmux": tmux = true
      of "--screen": screen = true
      of "-k": publicKey = args[i+1]; inc i
      inc i
    cmdSession(list, kill, shell, network, vcpu, tmux, screen, publicKey, secretKey)
    return

  if args[0] == "service":
    var name, ports, bootstrap, serviceType = ""
    var list = false
    var info, logs, tail, sleep, wake, destroy, execute, command, dumpBootstrap, dumpFile, network = ""
    var vcpu = 0
    var i = 1
    while i < args.len:
      case args[i]
      of "--name": name = args[i+1]; inc i
      of "--ports": ports = args[i+1]; inc i
      of "--bootstrap": bootstrap = args[i+1]; inc i
      of "--type": serviceType = args[i+1]; inc i
      of "--list": list = true
      of "--info": info = args[i+1]; inc i
      of "--logs": logs = args[i+1]; inc i
      of "--tail": tail = args[i+1]; inc i
      of "--freeze": sleep = args[i+1]; inc i
      of "--unfreeze": wake = args[i+1]; inc i
      of "--destroy": destroy = args[i+1]; inc i
      of "--execute": execute = args[i+1]; inc i
      of "--command": command = args[i+1]; inc i
      of "--dump-bootstrap": dumpBootstrap = args[i+1]; inc i
      of "--dump-file": dumpFile = args[i+1]; inc i
      of "-n": network = args[i+1]; inc i
      of "-v": vcpu = parseInt(args[i+1]); inc i
      of "-k": publicKey = args[i+1]; inc i
      inc i
    cmdService(name, ports, bootstrap, serviceType, list, info, logs, tail, sleep, wake, destroy, execute, command, dumpBootstrap, dumpFile, network, vcpu, publicKey, secretKey)
    return

  # Execute mode
  var envs: seq[string] = @[]
  var artifacts = false
  var network, sourceFile = ""
  var vcpu = 0
  var i = 0
  while i < args.len:
    case args[i]
    of "-e": envs.add(args[i+1]); inc i
    of "-a": artifacts = true
    of "-n": network = args[i+1]; inc i
    of "-v": vcpu = parseInt(args[i+1]); inc i
    of "-k": publicKey = args[i+1]; inc i
    else:
      if not args[i].startsWith("-"):
        sourceFile = args[i]
    inc i

  if sourceFile == "":
    stderr.writeLine(RED & "Error: No source file specified" & RESET)
    quit(1)

  cmdExecute(sourceFile, envs, artifacts, network, vcpu, publicKey, secretKey)

when isMainModule:
  main()
