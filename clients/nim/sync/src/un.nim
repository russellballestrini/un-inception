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

proc base64EncodeFile(filename: string): string =
  let cmd = fmt"base64 -w0 '{filename}'"
  result = execProcess(cmd).strip()

proc buildInputFilesJson(files: seq[string]): string =
  if files.len == 0:
    return ""
  var entries: seq[string] = @[]
  for f in files:
    let basename = extractFilename(f)
    let content = base64EncodeFile(f)
    entries.add(fmt"""{{"filename":"{basename}","content":"{content}"}}""")
  result = fmt""","input_files":[{entries.join(",")}]"""

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

proc execCurlPut(endpoint, body, publicKey, secretKey: string): bool =
  let tmpFile = fmt"/tmp/un_nim_{epochTime().int mod 999999}.txt"
  writeFile(tmpFile, body)
  let authHeaders = buildAuthHeaders("PUT", endpoint, body, publicKey, secretKey)
  let cmd = fmt"""curl -s -o /dev/null -w '%{{http_code}}' -X PUT '{API_BASE}{endpoint}' -H 'Content-Type: text/plain' {authHeaders} -d @{tmpFile}"""
  let output = execProcess(cmd).strip()
  removeFile(tmpFile)
  try:
    let status = parseInt(output)
    return status >= 200 and status < 300
  except:
    return false

const MAX_ENV_CONTENT_SIZE = 65536

proc readEnvFile(path: string): string =
  if not fileExists(path):
    stderr.writeLine(RED & "Error: Env file not found: " & path & RESET)
    quit(1)
  return readFile(path)

proc buildEnvContent(envs: seq[string], envFile: string): string =
  var lines: seq[string] = envs
  if envFile != "":
    let content = readEnvFile(envFile)
    for line in content.splitLines():
      let trimmed = line.strip()
      if trimmed.len > 0 and not trimmed.startsWith("#"):
        lines.add(trimmed)
  return lines.join("\n")

proc serviceEnvStatus(serviceId, publicKey, secretKey: string): string =
  let path = fmt"/services/{serviceId}/env"
  let authHeaders = buildAuthHeaders("GET", path, "", publicKey, secretKey)
  let cmd = fmt"""curl -s -X GET '{API_BASE}/services/{serviceId}/env' {authHeaders}"""
  return execCurl(cmd)

proc serviceEnvSet(serviceId, envContent, publicKey, secretKey: string): bool =
  if envContent.len > MAX_ENV_CONTENT_SIZE:
    stderr.writeLine(RED & "Error: Env content exceeds maximum size of 64KB" & RESET)
    return false
  return execCurlPut(fmt"/services/{serviceId}/env", envContent, publicKey, secretKey)

proc serviceEnvExport(serviceId, publicKey, secretKey: string): string =
  let path = fmt"/services/{serviceId}/env/export"
  let authHeaders = buildAuthHeaders("POST", path, "{}", publicKey, secretKey)
  let cmd = fmt"""curl -s -X POST '{API_BASE}/services/{serviceId}/env/export' -H 'Content-Type: application/json' {authHeaders} -d '{{}}'"""
  return execCurl(cmd)

proc serviceEnvDelete(serviceId, publicKey, secretKey: string): bool =
  let path = fmt"/services/{serviceId}/env"
  let authHeaders = buildAuthHeaders("DELETE", path, "", publicKey, secretKey)
  let cmd = fmt"""curl -s -o /dev/null -w '%{{http_code}}' -X DELETE '{API_BASE}/services/{serviceId}/env' {authHeaders}"""
  let output = execProcess(cmd).strip()
  try:
    let status = parseInt(output)
    return status >= 200 and status < 300
  except:
    return false

proc extractJsonField(response, field: string): string =
  let fieldStart = response.find("\"" & field & "\":\"")
  if fieldStart >= 0:
    let start = fieldStart + field.len + 4
    var endPos = start
    while endPos < response.len:
      if response[endPos] == '"' and (endPos == 0 or response[endPos-1] != '\\'):
        break
      inc endPos
    if endPos > start:
      return response[start..<endPos]
  return ""

proc cmdServiceEnv(action, target: string, envs: seq[string], envFile, publicKey, secretKey: string) =
  case action
  of "status":
    if target == "":
      stderr.writeLine(RED & "Error: service env status requires service ID" & RESET)
      quit(1)
    let response = serviceEnvStatus(target, publicKey, secretKey)
    if response.contains("\"has_vault\":true"):
      echo GREEN & "Vault: configured" & RESET
      let envCount = extractJsonField(response, "env_count")
      if envCount != "": echo "Variables: " & envCount
      let updatedAt = extractJsonField(response, "updated_at")
      if updatedAt != "": echo "Updated: " & updatedAt
    else:
      echo YELLOW & "Vault: not configured" & RESET
  of "set":
    if target == "":
      stderr.writeLine(RED & "Error: service env set requires service ID" & RESET)
      quit(1)
    if envs.len == 0 and envFile == "":
      stderr.writeLine(RED & "Error: service env set requires -e or --env-file" & RESET)
      quit(1)
    let envContent = buildEnvContent(envs, envFile)
    if serviceEnvSet(target, envContent, publicKey, secretKey):
      echo GREEN & "Vault updated for service " & target & RESET
    else:
      stderr.writeLine(RED & "Error: Failed to update vault" & RESET)
      quit(1)
  of "export":
    if target == "":
      stderr.writeLine(RED & "Error: service env export requires service ID" & RESET)
      quit(1)
    let response = serviceEnvExport(target, publicKey, secretKey)
    let content = extractJsonField(response, "content")
    if content != "":
      var output = content.replace("\\n", "\n").replace("\\r", "\r").replace("\\t", "\t").replace("\\\"", "\"").replace("\\\\", "\\")
      stdout.write(output)
  of "delete":
    if target == "":
      stderr.writeLine(RED & "Error: service env delete requires service ID" & RESET)
      quit(1)
    if serviceEnvDelete(target, publicKey, secretKey):
      echo GREEN & "Vault deleted for service " & target & RESET
    else:
      stderr.writeLine(RED & "Error: Failed to delete vault" & RESET)
      quit(1)
  else:
    stderr.writeLine(RED & "Error: Unknown env action: " & action & RESET)
    stderr.writeLine("Usage: un.nim service env <status|set|export|delete> <service_id>")
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

proc cmdSession(list: bool, kill, shell, network: string, vcpu: int, tmux, screen: bool, inputFiles: seq[string], publicKey: string, secretKey: string) =
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
  json.add(buildInputFilesJson(inputFiles))
  json.add("}")

  echo YELLOW & "Creating session..." & RESET
  let authHeaders = buildAuthHeaders("POST", "/sessions", json, publicKey, secretKey)
  let cmd = fmt"""curl -s -X POST '{API_BASE}/sessions' -H 'Content-Type: application/json' {authHeaders} -d '{json}'"""
  echo execCurl(cmd)

proc cmdService(name, ports, bootstrap, bootstrapFile, serviceType: string, list: bool, info, logs, tail, sleep, wake, destroy, resize: string, resizeVcpu: int, execute, command, dumpBootstrap, dumpFile, network: string, vcpu: int, inputFiles: seq[string], svcEnvs: seq[string], svcEnvFile, envAction, envTarget: string, publicKey: string, secretKey: string) =
  # Handle env subcommand
  if envAction != "":
    cmdServiceEnv(envAction, envTarget, svcEnvs, svcEnvFile, publicKey, secretKey)
    return

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
    let path = fmt"/services/{sleep}/freeze"
    let authHeaders = buildAuthHeaders("POST", path, "", publicKey, secretKey)
    let cmd = fmt"""curl -s -X POST '{API_BASE}/services/{sleep}/freeze' {authHeaders}"""
    discard execCurl(cmd)
    echo GREEN & "Service frozen: " & sleep & RESET
    return

  if wake != "":
    let path = fmt"/services/{wake}/unfreeze"
    let authHeaders = buildAuthHeaders("POST", path, "", publicKey, secretKey)
    let cmd = fmt"""curl -s -X POST '{API_BASE}/services/{wake}/unfreeze' {authHeaders}"""
    discard execCurl(cmd)
    echo GREEN & "Service unfreezing: " & wake & RESET
    return

  if destroy != "":
    let path = fmt"/services/{destroy}"
    let authHeaders = buildAuthHeaders("DELETE", path, "", publicKey, secretKey)
    let cmd = fmt"""curl -s -X DELETE '{API_BASE}/services/{destroy}' {authHeaders}"""
    discard execCurl(cmd)
    echo GREEN & "Service destroyed: " & destroy & RESET
    return

  if resize != "":
    if resizeVcpu <= 0:
      stderr.writeLine(RED & "Error: --resize requires --vcpu or -v" & RESET)
      quit(1)
    if resizeVcpu < 1 or resizeVcpu > 8:
      stderr.writeLine(RED & "Error: vCPU must be between 1 and 8" & RESET)
      quit(1)
    let json = fmt"""{{"vcpu":{resizeVcpu}}}"""
    let path = fmt"/services/{resize}"
    let authHeaders = buildAuthHeaders("PATCH", path, json, publicKey, secretKey)
    let cmd = fmt"""curl -s -X PATCH '{API_BASE}/services/{resize}' -H 'Content-Type: application/json' {authHeaders} -d '{json}'"""
    discard execCurl(cmd)
    let ram = resizeVcpu * 2
    echo GREEN & "Service resized to " & $resizeVcpu & " vCPU, " & $ram & " GB RAM" & RESET
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
      json.add(fmt""","bootstrap":"{escapeJson(bootstrap)}"""")
    if bootstrapFile != "":
      if fileExists(bootstrapFile):
        let bootCode = readFile(bootstrapFile)
        json.add(fmt""","bootstrap_content":"{escapeJson(bootCode)}"""")
      else:
        stderr.writeLine(RED & "Error: Bootstrap file not found: " & bootstrapFile & RESET)
        quit(1)
    if serviceType != "": json.add(fmt""","service_type":"{serviceType}"""")
    if network != "": json.add(fmt""","network":"{network}"""")
    if vcpu > 0: json.add(fmt""","vcpu":{vcpu}""")
    json.add(buildInputFilesJson(inputFiles))
    json.add("}")

    echo YELLOW & "Creating service..." & RESET
    let authHeaders = buildAuthHeaders("POST", "/services", json, publicKey, secretKey)
    let cmd = fmt"""curl -s -X POST '{API_BASE}/services' -H 'Content-Type: application/json' {authHeaders} -d '{json}'"""
    let response = execCurl(cmd)
    echo response

    # Auto-set vault if -e or --env-file provided
    if svcEnvs.len > 0 or svcEnvFile != "":
      let serviceId = extractJsonField(response, "service_id")
      if serviceId != "":
        let envContent = buildEnvContent(svcEnvs, svcEnvFile)
        if serviceEnvSet(serviceId, envContent, publicKey, secretKey):
          echo GREEN & "Vault configured for service " & serviceId & RESET
        else:
          stderr.writeLine(YELLOW & "Warning: Failed to set vault" & RESET)
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
    stderr.writeLine("       un.nim service env <action> <service_id> [options]")
    stderr.writeLine("       un.nim key [options]")
    stderr.writeLine("")
    stderr.writeLine("Service env commands:")
    stderr.writeLine("  env status <id>     Show vault status")
    stderr.writeLine("  env set <id>        Set vault (-e KEY=VALUE or --env-file FILE)")
    stderr.writeLine("  env export <id>     Export vault contents")
    stderr.writeLine("  env delete <id>     Delete vault")
    stderr.writeLine("")
    stderr.writeLine("Service options:")
    stderr.writeLine("  -e KEY=VALUE        Set environment variable (for vault)")
    stderr.writeLine("  --env-file FILE     Load env vars from file (for vault)")
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
    var inputFiles: seq[string] = @[]
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
      of "-f":
        let file = args[i+1]
        if fileExists(file):
          inputFiles.add(file)
        else:
          stderr.writeLine("Error: File not found: " & file)
          quit(1)
        inc i
      else: discard
      inc i
    cmdSession(list, kill, shell, network, vcpu, tmux, screen, inputFiles, publicKey, secretKey)
    return

  if args[0] == "service":
    var name, ports, bootstrap, bootstrapFile, serviceType = ""
    var list = false
    var info, logs, tail, sleep, wake, destroy, resize, execute, command, dumpBootstrap, dumpFile, network = ""
    var vcpu = 0
    var resizeVcpu = 0
    var inputFiles: seq[string] = @[]
    var svcEnvs: seq[string] = @[]
    var svcEnvFile = ""
    var envAction, envTarget = ""
    var i = 1

    # Check for env subcommand
    if args.len > 1 and args[1] == "env":
      if args.len > 2:
        envAction = args[2]
      if args.len > 3:
        envTarget = args[3]
      i = 4
      while i < args.len:
        case args[i]
        of "-e": svcEnvs.add(args[i+1]); inc i
        of "--env-file": svcEnvFile = args[i+1]; inc i
        of "-k": publicKey = args[i+1]; inc i
        else: discard
        inc i
      cmdService(name, ports, bootstrap, bootstrapFile, serviceType, list, info, logs, tail, sleep, wake, destroy, resize, resizeVcpu, execute, command, dumpBootstrap, dumpFile, network, vcpu, inputFiles, svcEnvs, svcEnvFile, envAction, envTarget, publicKey, secretKey)
      return

    while i < args.len:
      case args[i]
      of "--name": name = args[i+1]; inc i
      of "--ports": ports = args[i+1]; inc i
      of "--bootstrap": bootstrap = args[i+1]; inc i
      of "--bootstrap-file": bootstrapFile = args[i+1]; inc i
      of "--type": serviceType = args[i+1]; inc i
      of "--list": list = true
      of "--info": info = args[i+1]; inc i
      of "--logs": logs = args[i+1]; inc i
      of "--tail": tail = args[i+1]; inc i
      of "--freeze": sleep = args[i+1]; inc i
      of "--unfreeze": wake = args[i+1]; inc i
      of "--destroy": destroy = args[i+1]; inc i
      of "--resize": resize = args[i+1]; inc i
      of "--vcpu": resizeVcpu = parseInt(args[i+1]); inc i
      of "--execute": execute = args[i+1]; inc i
      of "--command": command = args[i+1]; inc i
      of "--dump-bootstrap": dumpBootstrap = args[i+1]; inc i
      of "--dump-file": dumpFile = args[i+1]; inc i
      of "-n": network = args[i+1]; inc i
      of "-v": vcpu = parseInt(args[i+1]); inc i
      of "-k": publicKey = args[i+1]; inc i
      of "-e": svcEnvs.add(args[i+1]); inc i
      of "--env-file": svcEnvFile = args[i+1]; inc i
      of "-f":
        let file = args[i+1]
        if fileExists(file):
          inputFiles.add(file)
        else:
          stderr.writeLine("Error: File not found: " & file)
          quit(1)
        inc i
      else: discard
      inc i
    cmdService(name, ports, bootstrap, bootstrapFile, serviceType, list, info, logs, tail, sleep, wake, destroy, resize, resizeVcpu, execute, command, dumpBootstrap, dumpFile, network, vcpu, inputFiles, svcEnvs, svcEnvFile, envAction, envTarget, publicKey, secretKey)
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
      if args[i].startsWith("-"):
        stderr.writeLine(RED & "Unknown option: " & args[i] & RESET)
        quit(1)
      else:
        sourceFile = args[i]
    inc i

  if sourceFile == "":
    stderr.writeLine(RED & "Error: No source file specified" & RESET)
    quit(1)

  cmdExecute(sourceFile, envs, artifacts, network, vcpu, publicKey, secretKey)

when isMainModule:
  main()
