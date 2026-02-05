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
  LANGUAGES_CACHE_TTL = 3600  # 1 hour in seconds

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

proc getLanguagesCachePath(): string =
  let home = getEnv("HOME", "")
  if home == "":
    return ""
  return joinPath(home, ".unsandbox", "languages.json")

proc loadLanguagesCache(): string =
  let cachePath = getLanguagesCachePath()
  if cachePath == "" or not fileExists(cachePath):
    return ""

  try:
    let content = readFile(cachePath)
    # Parse timestamp from JSON
    let tsStart = content.find("\"timestamp\":")
    if tsStart < 0:
      return ""

    let numStart = tsStart + 12  # Length of "\"timestamp\":"
    var numEnd = numStart
    while numEnd < content.len and content[numEnd] in {'0'..'9'}:
      inc numEnd

    if numEnd == numStart:
      return ""

    let cachedTime = parseInt(content[numStart..<numEnd])
    let currentTime = toUnix(getTime())

    # Check if cache is still valid (within TTL)
    if currentTime - cachedTime < LANGUAGES_CACHE_TTL:
      return content
  except:
    # Cache read failed, return empty to fetch fresh
    discard

  return ""

proc saveLanguagesCache(response: string) =
  let cachePath = getLanguagesCachePath()
  if cachePath == "":
    return

  try:
    # Ensure directory exists
    let cacheDir = parentDir(cachePath)
    if not dirExists(cacheDir):
      createDir(cacheDir)

    # Find the languages array in response
    let langStart = response.find("\"languages\":")
    if langStart < 0:
      return

    var bracketStart = response.find("[", langStart)
    if bracketStart < 0:
      return

    # Find matching closing bracket
    var depth = 1
    var bracketEnd = bracketStart + 1
    while bracketEnd < response.len and depth > 0:
      if response[bracketEnd] == '[': inc depth
      elif response[bracketEnd] == ']': dec depth
      inc bracketEnd

    let languagesArray = response[bracketStart..<bracketEnd]

    # Build cache JSON with timestamp
    let timestamp = toUnix(getTime())
    let cacheJson = fmt"""{{"languages":{languagesArray},"timestamp":{timestamp}}}"""
    writeFile(cachePath, cacheJson)
  except:
    # Cache write failed, ignore
    discard

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

proc execCurlWithStatus(cmd: string): (int, string) =
  # Execute curl and capture HTTP status code and response body
  let result = execProcess(cmd)
  return (0, result)  # For non-status commands, we parse response

proc handleSudoChallenge(responseData, meth, endpoint, body, publicKey, secretKey: string): bool =
  # Extract challenge_id from response
  let challengeId = extractJsonField(responseData, "challenge_id")

  stderr.writeLine(YELLOW & "Confirmation required. Check your email for a one-time code." & RESET)
  stderr.write("Enter OTP: ")

  var otp = ""
  try:
    otp = stdin.readLine().strip()
  except:
    stderr.writeLine(RED & "Error: Failed to read OTP" & RESET)
    return false

  if otp.len == 0:
    stderr.writeLine(RED & "Error: Operation cancelled" & RESET)
    return false

  # Build retry command with sudo headers
  var otpHeader = fmt"-H 'X-Sudo-OTP: {otp}'"
  var challengeHeader = ""
  if challengeId != "":
    challengeHeader = fmt" -H 'X-Sudo-Challenge: {challengeId}'"

  let authHeaders = buildAuthHeaders(meth, endpoint, body, publicKey, secretKey)
  var cmd: string
  if meth == "DELETE":
    cmd = fmt"""curl -s -o /dev/null -w '%{{http_code}}' -X DELETE '{API_BASE}{endpoint}' {authHeaders} {otpHeader}{challengeHeader}"""
  elif meth == "POST":
    let contentType = if body != "": "-H 'Content-Type: application/json'" else: ""
    cmd = fmt"""curl -s -o /dev/null -w '%{{http_code}}' -X POST '{API_BASE}{endpoint}' {contentType} {authHeaders} {otpHeader}{challengeHeader} -d '{body}'"""
  else:
    return false

  let output = execProcess(cmd).strip()
  try:
    let status = parseInt(output)
    if status >= 200 and status < 300:
      echo GREEN & "Operation completed successfully" & RESET
      return true
    else:
      stderr.writeLine(RED & "Error: HTTP " & $status & RESET)
      return false
  except:
    stderr.writeLine(RED & "Error: Failed to retry operation" & RESET)
    return false

proc execCurlDeleteWithSudo(endpoint, publicKey, secretKey: string): int =
  let authHeaders = buildAuthHeaders("DELETE", endpoint, "", publicKey, secretKey)
  let cmd = fmt"""curl -s -w '\n%{{http_code}}' -X DELETE '{API_BASE}{endpoint}' {authHeaders}"""
  let output = execProcess(cmd)

  # Parse response body and status code
  let lines = output.strip().split('\n')
  if lines.len < 1:
    return 500

  let statusLine = lines[^1]
  let responseBody = if lines.len > 1: lines[0..^2].join("\n") else: ""

  try:
    let status = parseInt(statusLine)
    if status == 428:
      if handleSudoChallenge(responseBody, "DELETE", endpoint, "", publicKey, secretKey):
        return 200
      else:
        return 428
    return status
  except:
    return 500

proc execCurlPostWithSudo(endpoint, body, publicKey, secretKey: string): int =
  let authHeaders = buildAuthHeaders("POST", endpoint, body, publicKey, secretKey)
  let cmd = fmt"""curl -s -w '\n%{{http_code}}' -X POST '{API_BASE}{endpoint}' -H 'Content-Type: application/json' {authHeaders} -d '{body}'"""
  let output = execProcess(cmd)

  # Parse response body and status code
  let lines = output.strip().split('\n')
  if lines.len < 1:
    return 500

  let statusLine = lines[^1]
  let responseBody = if lines.len > 1: lines[0..^2].join("\n") else: ""

  try:
    let status = parseInt(statusLine)
    if status == 428:
      if handleSudoChallenge(responseBody, "POST", endpoint, body, publicKey, secretKey):
        return 200
      else:
        return 428
    return status
  except:
    return 500

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

proc cmdSession(list: bool, kill, info, freeze, unfreeze, boost, unboost, execute, command, shell, network: string, vcpu: int, tmux, screen: bool, inputFiles: seq[string], publicKey: string, secretKey: string) =
  if list:
    let authHeaders = buildAuthHeaders("GET", "/sessions", "", publicKey, secretKey)
    let cmd = fmt"""curl -s -X GET '{API_BASE}/sessions' {authHeaders}"""
    echo execCurl(cmd)
    return

  if info != "":
    let path = fmt"/sessions/{info}"
    let authHeaders = buildAuthHeaders("GET", path, "", publicKey, secretKey)
    let cmd = fmt"""curl -s -X GET '{API_BASE}/sessions/{info}' {authHeaders}"""
    echo execCurl(cmd)
    return

  if kill != "":
    let path = fmt"/sessions/{kill}"
    let authHeaders = buildAuthHeaders("DELETE", path, "", publicKey, secretKey)
    let cmd = fmt"""curl -s -X DELETE '{API_BASE}/sessions/{kill}' {authHeaders}"""
    discard execCurl(cmd)
    echo GREEN & "Session terminated: " & kill & RESET
    return

  if freeze != "":
    let path = fmt"/sessions/{freeze}/freeze"
    let authHeaders = buildAuthHeaders("POST", path, "", publicKey, secretKey)
    let cmd = fmt"""curl -s -X POST '{API_BASE}/sessions/{freeze}/freeze' {authHeaders}"""
    discard execCurl(cmd)
    echo GREEN & "Session frozen: " & freeze & RESET
    return

  if unfreeze != "":
    let path = fmt"/sessions/{unfreeze}/unfreeze"
    let authHeaders = buildAuthHeaders("POST", path, "", publicKey, secretKey)
    let cmd = fmt"""curl -s -X POST '{API_BASE}/sessions/{unfreeze}/unfreeze' {authHeaders}"""
    discard execCurl(cmd)
    echo GREEN & "Session unfreezing: " & unfreeze & RESET
    return

  if boost != "":
    let boostVcpu = if vcpu > 0: vcpu else: 2
    let json = fmt"""{{"vcpu":{boostVcpu}}}"""
    let path = fmt"/sessions/{boost}/boost"
    let authHeaders = buildAuthHeaders("POST", path, json, publicKey, secretKey)
    let cmd = fmt"""curl -s -X POST '{API_BASE}/sessions/{boost}/boost' -H 'Content-Type: application/json' {authHeaders} -d '{json}'"""
    discard execCurl(cmd)
    echo GREEN & "Session boosted to " & $boostVcpu & " vCPU: " & boost & RESET
    return

  if unboost != "":
    let path = fmt"/sessions/{unboost}/unboost"
    let authHeaders = buildAuthHeaders("POST", path, "", publicKey, secretKey)
    let cmd = fmt"""curl -s -X POST '{API_BASE}/sessions/{unboost}/unboost' {authHeaders}"""
    discard execCurl(cmd)
    echo GREEN & "Session unboosted: " & unboost & RESET
    return

  if execute != "":
    let json = fmt"""{"command":"{escapeJson(command)}"}"""
    let path = fmt"/sessions/{execute}/execute"
    let authHeaders = buildAuthHeaders("POST", path, json, publicKey, secretKey)
    let cmd = fmt"""curl -s -X POST '{API_BASE}/sessions/{execute}/execute' -H 'Content-Type: application/json' {authHeaders} -d '{json}'"""
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

proc setServiceUnfreezeOnDemand(serviceId: string, enabled: bool, publicKey: string, secretKey: string): bool =
  let enabledStr = if enabled: "true" else: "false"
  let json = fmt"""{{"unfreeze_on_demand":{enabledStr}}}"""
  let path = fmt"/services/{serviceId}"
  let authHeaders = buildAuthHeaders("PATCH", path, json, publicKey, secretKey)
  let cmd = fmt"""curl -s -o /dev/null -w '%{{http_code}}' -X PATCH '{API_BASE}/services/{serviceId}' -H 'Content-Type: application/json' {authHeaders} -d '{json}'"""
  let output = execProcess(cmd).strip()
  try:
    let status = parseInt(output)
    return status >= 200 and status < 300
  except:
    return false

proc cmdService(name, ports, bootstrap, bootstrapFile, serviceType: string, list: bool, info, logs, tail, sleep, wake, destroy, resize: string, resizeVcpu: int, execute, command, dumpBootstrap, dumpFile, network: string, vcpu: int, unfreezeOnDemand: bool, setUnfreezeOnDemand, setUnfreezeOnDemandEnabled, lock, unlock, redeploy: string, inputFiles: seq[string], svcEnvs: seq[string], svcEnvFile, envAction, envTarget: string, publicKey: string, secretKey: string) =
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
    let status = execCurlDeleteWithSudo(path, publicKey, secretKey)
    if status >= 200 and status < 300:
      echo GREEN & "Service destroyed: " & destroy & RESET
    elif status != 428:  # 428 already handled in execCurlDeleteWithSudo
      stderr.writeLine(RED & "Error: Failed to destroy service" & RESET)
      quit(1)
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

  if setUnfreezeOnDemand != "":
    let enabled = setUnfreezeOnDemandEnabled == "true" or setUnfreezeOnDemandEnabled == "1"
    if setServiceUnfreezeOnDemand(setUnfreezeOnDemand, enabled, publicKey, secretKey):
      let status = if enabled: "enabled" else: "disabled"
      echo GREEN & "Unfreeze-on-demand " & status & " for service: " & setUnfreezeOnDemand & RESET
    else:
      stderr.writeLine(RED & "Error: Failed to update unfreeze-on-demand setting" & RESET)
      quit(1)
    return

  if lock != "":
    let path = fmt"/services/{lock}/lock"
    let authHeaders = buildAuthHeaders("POST", path, "", publicKey, secretKey)
    let cmd = fmt"""curl -s -X POST '{API_BASE}/services/{lock}/lock' {authHeaders}"""
    discard execCurl(cmd)
    echo GREEN & "Service locked: " & lock & RESET
    return

  if unlock != "":
    let path = fmt"/services/{unlock}/unlock"
    let status = execCurlPostWithSudo(path, "{}", publicKey, secretKey)
    if status >= 200 and status < 300:
      echo GREEN & "Service unlocked: " & unlock & RESET
    elif status != 428:
      stderr.writeLine(RED & "Error: Failed to unlock service" & RESET)
      quit(1)
    return

  if redeploy != "":
    var json = "{"
    var hasContent = false
    if bootstrap != "":
      json.add(fmt""""bootstrap":"{escapeJson(bootstrap)}"""")
      hasContent = true
    if bootstrapFile != "":
      if fileExists(bootstrapFile):
        let bootCode = readFile(bootstrapFile)
        if hasContent: json.add(",")
        json.add(fmt""""bootstrap_content":"{escapeJson(bootCode)}"""")
      else:
        stderr.writeLine(RED & "Error: Bootstrap file not found: " & bootstrapFile & RESET)
        quit(1)
    json.add("}")
    let path = fmt"/services/{redeploy}/redeploy"
    let authHeaders = buildAuthHeaders("POST", path, json, publicKey, secretKey)
    let cmd = fmt"""curl -s -X POST '{API_BASE}/services/{redeploy}/redeploy' -H 'Content-Type: application/json' {authHeaders} -d '{json}'"""
    let response = execCurl(cmd)
    echo GREEN & "Service redeployed: " & redeploy & RESET
    echo response
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
    if unfreezeOnDemand: json.add(""","unfreeze_on_demand":true""")
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

proc cmdLanguages(jsonOutput: bool, publicKey: string, secretKey: string) =
  # Try to load from cache first
  var response = loadLanguagesCache()

  if response == "":
    # Fetch from API
    let authHeaders = buildAuthHeaders("GET", "/languages", "", publicKey, secretKey)
    let cmd = fmt"""curl -s -X GET '{API_BASE}/languages' {authHeaders}"""
    response = execCurl(cmd)

    # Save to cache
    saveLanguagesCache(response)

  if jsonOutput:
    # Extract languages array and output as JSON
    let langStart = response.find("\"languages\":")
    if langStart >= 0:
      var bracketStart = response.find("[", langStart)
      if bracketStart >= 0:
        var depth = 1
        var bracketEnd = bracketStart + 1
        while bracketEnd < response.len and depth > 0:
          if response[bracketEnd] == '[': inc depth
          elif response[bracketEnd] == ']': dec depth
          inc bracketEnd
        if bracketEnd <= response.len:
          echo response[bracketStart..<bracketEnd]
        else:
          echo "[]"
      else:
        echo "[]"
    else:
      echo "[]"
  else:
    # Extract each language and print one per line
    let langStart = response.find("\"languages\":")
    if langStart >= 0:
      var bracketStart = response.find("[", langStart)
      if bracketStart >= 0:
        var pos = bracketStart + 1
        while pos < response.len:
          # Skip whitespace
          while pos < response.len and response[pos] in {' ', '\n', '\r', '\t', ','}: inc pos
          if pos >= response.len or response[pos] == ']': break
          # Find quoted string
          if response[pos] == '"':
            let start = pos + 1
            var endPos = start
            while endPos < response.len and response[endPos] != '"':
              inc endPos
            if endPos > start:
              echo response[start..<endPos]
            pos = endPos + 1
          else:
            inc pos

proc cmdSnapshot(list: bool, infoId, sessionId, serviceId, restoreId, deleteId, lockId, unlockId, cloneId, cloneType, name, ports: string, hot: bool, publicKey, secretKey: string) =
  if list:
    let authHeaders = buildAuthHeaders("GET", "/snapshots", "", publicKey, secretKey)
    let cmd = fmt"""curl -s -X GET '{API_BASE}/snapshots' {authHeaders}"""
    echo execCurl(cmd)
    return

  if infoId != "":
    let path = fmt"/snapshots/{infoId}"
    let authHeaders = buildAuthHeaders("GET", path, "", publicKey, secretKey)
    let cmd = fmt"""curl -s -X GET '{API_BASE}/snapshots/{infoId}' {authHeaders}"""
    echo execCurl(cmd)
    return

  if sessionId != "":
    var json = "{"
    var hasContent = false
    if name != "":
      json.add(fmt""""name":"{name}"""")
      hasContent = true
    if hot:
      if hasContent: json.add(",")
      json.add(""""hot":true""")
    json.add("}")
    let path = fmt"/sessions/{sessionId}/snapshot"
    let authHeaders = buildAuthHeaders("POST", path, json, publicKey, secretKey)
    let cmd = fmt"""curl -s -X POST '{API_BASE}/sessions/{sessionId}/snapshot' -H 'Content-Type: application/json' {authHeaders} -d '{json}'"""
    let response = execCurl(cmd)
    echo GREEN & "Snapshot created" & RESET
    echo response
    return

  if serviceId != "":
    var json = "{"
    var hasContent = false
    if name != "":
      json.add(fmt""""name":"{name}"""")
      hasContent = true
    if hot:
      if hasContent: json.add(",")
      json.add(""""hot":true""")
    json.add("}")
    let path = fmt"/services/{serviceId}/snapshot"
    let authHeaders = buildAuthHeaders("POST", path, json, publicKey, secretKey)
    let cmd = fmt"""curl -s -X POST '{API_BASE}/services/{serviceId}/snapshot' -H 'Content-Type: application/json' {authHeaders} -d '{json}'"""
    let response = execCurl(cmd)
    echo GREEN & "Snapshot created" & RESET
    echo response
    return

  if restoreId != "":
    let path = fmt"/snapshots/{restoreId}/restore"
    let authHeaders = buildAuthHeaders("POST", path, "{}", publicKey, secretKey)
    let cmd = fmt"""curl -s -X POST '{API_BASE}/snapshots/{restoreId}/restore' -H 'Content-Type: application/json' {authHeaders} -d '{{}}'"""
    let response = execCurl(cmd)
    echo GREEN & "Snapshot restored" & RESET
    echo response
    return

  if deleteId != "":
    let path = fmt"/snapshots/{deleteId}"
    let status = execCurlDeleteWithSudo(path, publicKey, secretKey)
    if status >= 200 and status < 300:
      echo GREEN & "Snapshot deleted: " & deleteId & RESET
    elif status != 428:
      stderr.writeLine(RED & "Error: Failed to delete snapshot" & RESET)
      quit(1)
    return

  if lockId != "":
    let path = fmt"/snapshots/{lockId}/lock"
    let authHeaders = buildAuthHeaders("POST", path, "", publicKey, secretKey)
    let cmd = fmt"""curl -s -X POST '{API_BASE}/snapshots/{lockId}/lock' {authHeaders}"""
    discard execCurl(cmd)
    echo GREEN & "Snapshot locked: " & lockId & RESET
    return

  if unlockId != "":
    let path = fmt"/snapshots/{unlockId}/unlock"
    let status = execCurlPostWithSudo(path, "{}", publicKey, secretKey)
    if status >= 200 and status < 300:
      echo GREEN & "Snapshot unlocked: " & unlockId & RESET
    elif status != 428:
      stderr.writeLine(RED & "Error: Failed to unlock snapshot" & RESET)
      quit(1)
    return

  if cloneId != "":
    var json = fmt"""{{"clone_type":"{if cloneType != "": cloneType else: "session"}"""""
    if name != "": json.add(fmt""","name":"{name}"""")
    if ports != "": json.add(fmt""","ports":[{ports}]""")
    json.add("}")
    let path = fmt"/snapshots/{cloneId}/clone"
    let authHeaders = buildAuthHeaders("POST", path, json, publicKey, secretKey)
    let cmd = fmt"""curl -s -X POST '{API_BASE}/snapshots/{cloneId}/clone' -H 'Content-Type: application/json' {authHeaders} -d '{json}'"""
    let response = execCurl(cmd)
    echo GREEN & "Snapshot cloned" & RESET
    echo response
    return

  stderr.writeLine(RED & "Error: Use --list, --info, --session, --service, --restore, --delete, --lock, --unlock, or --clone" & RESET)
  quit(1)

proc cmdLogs(source: string, lines: int, since, grep: string, follow: bool, publicKey, secretKey: string) =
  let sourceParam = if source != "": source else: "all"
  let linesParam = if lines > 0: lines else: 100
  let sinceParam = if since != "": since else: "1h"

  if follow:
    var endpoint = fmt"/paas/logs/stream?source={sourceParam}"
    if grep != "": endpoint.add(fmt"&grep={grep}")
    let authHeaders = buildAuthHeaders("GET", endpoint, "", publicKey, secretKey)
    let cmd = fmt"""curl -s -N -X GET '{PORTAL_BASE}{endpoint}' -H 'Accept: text/event-stream' {authHeaders}"""
    # Stream logs - this will block
    let output = execProcess(cmd)
    echo output
  else:
    var endpoint = fmt"/paas/logs?source={sourceParam}&lines={linesParam}&since={sinceParam}"
    if grep != "": endpoint.add(fmt"&grep={grep}")
    let authHeaders = buildAuthHeaders("GET", endpoint, "", publicKey, secretKey)
    let cmd = fmt"""curl -s -X GET '{PORTAL_BASE}{endpoint}' {authHeaders}"""
    echo execCurl(cmd)

proc cmdHealth() =
  let cmd = fmt"""curl -s -X GET '{API_BASE}/health'"""
  let response = execProcess(cmd)
  if response.contains("\"status\":\"healthy\"") or response.contains("\"ok\":true"):
    echo GREEN & "API is healthy" & RESET
  else:
    echo RED & "API may be unhealthy" & RESET
  echo response

proc cmdVersion() =
  echo "un.nim version 1.0.0"
  echo "API: " & API_BASE
  echo "Portal: " & PORTAL_BASE

proc cmdImage(list: bool, infoId, deleteId, lockId, unlockId, publishId, sourceType, visibilityId, visibilityMode, spawnId, cloneId, name, ports, grantId, revokeId, trustedId, trustedKey, transferId, toKey, publicKey, secretKey: string) =
  if list:
    let authHeaders = buildAuthHeaders("GET", "/images", "", publicKey, secretKey)
    let cmd = fmt"""curl -s -X GET '{API_BASE}/images' {authHeaders}"""
    echo execCurl(cmd)
    return

  if infoId != "":
    let path = fmt"/images/{infoId}"
    let authHeaders = buildAuthHeaders("GET", path, "", publicKey, secretKey)
    let cmd = fmt"""curl -s -X GET '{API_BASE}/images/{infoId}' {authHeaders}"""
    echo execCurl(cmd)
    return

  if deleteId != "":
    let path = fmt"/images/{deleteId}"
    let status = execCurlDeleteWithSudo(path, publicKey, secretKey)
    if status >= 200 and status < 300:
      echo GREEN & "Image deleted: " & deleteId & RESET
    elif status != 428:  # 428 already handled
      stderr.writeLine(RED & "Error: Failed to delete image" & RESET)
      quit(1)
    return

  if lockId != "":
    let path = fmt"/images/{lockId}/lock"
    let authHeaders = buildAuthHeaders("POST", path, "", publicKey, secretKey)
    let cmd = fmt"""curl -s -X POST '{API_BASE}/images/{lockId}/lock' {authHeaders}"""
    discard execCurl(cmd)
    echo GREEN & "Image locked: " & lockId & RESET
    return

  if unlockId != "":
    let path = fmt"/images/{unlockId}/unlock"
    let status = execCurlPostWithSudo(path, "{}", publicKey, secretKey)
    if status >= 200 and status < 300:
      echo GREEN & "Image unlocked: " & unlockId & RESET
    elif status != 428:  # 428 already handled
      stderr.writeLine(RED & "Error: Failed to unlock image" & RESET)
      quit(1)
    return

  if publishId != "":
    if sourceType == "":
      stderr.writeLine(RED & "Error: --publish requires --source-type (service or snapshot)" & RESET)
      quit(1)
    var json = fmt"""{{"source_type":"{sourceType}","source_id":"{publishId}"""""
    if name != "": json.add(fmt""","name":"{name}"""")
    json.add("}")
    let authHeaders = buildAuthHeaders("POST", "/images/publish", json, publicKey, secretKey)
    let cmd = fmt"""curl -s -X POST '{API_BASE}/images/publish' -H 'Content-Type: application/json' {authHeaders} -d '{json}'"""
    let response = execCurl(cmd)
    echo GREEN & "Image published" & RESET
    echo response
    return

  if visibilityId != "" and visibilityMode != "":
    let json = fmt"""{{"visibility":"{visibilityMode}"}}"""
    let path = fmt"/images/{visibilityId}/visibility"
    let authHeaders = buildAuthHeaders("POST", path, json, publicKey, secretKey)
    let cmd = fmt"""curl -s -X POST '{API_BASE}/images/{visibilityId}/visibility' -H 'Content-Type: application/json' {authHeaders} -d '{json}'"""
    discard execCurl(cmd)
    echo GREEN & "Image visibility set to " & visibilityMode & ": " & visibilityId & RESET
    return

  if spawnId != "":
    var json = "{"
    var hasContent = false
    if name != "":
      json.add(fmt""""name":"{name}"""")
      hasContent = true
    if ports != "":
      if hasContent: json.add(",")
      json.add(fmt""""ports":[{ports}]""")
    json.add("}")
    let path = fmt"/images/{spawnId}/spawn"
    let authHeaders = buildAuthHeaders("POST", path, json, publicKey, secretKey)
    let cmd = fmt"""curl -s -X POST '{API_BASE}/images/{spawnId}/spawn' -H 'Content-Type: application/json' {authHeaders} -d '{json}'"""
    let response = execCurl(cmd)
    echo GREEN & "Service spawned from image" & RESET
    echo response
    return

  if cloneId != "":
    var json = "{"
    if name != "":
      json.add(fmt""""name":"{name}"""")
    json.add("}")
    let path = fmt"/images/{cloneId}/clone"
    let authHeaders = buildAuthHeaders("POST", path, json, publicKey, secretKey)
    let cmd = fmt"""curl -s -X POST '{API_BASE}/images/{cloneId}/clone' -H 'Content-Type: application/json' {authHeaders} -d '{json}'"""
    let response = execCurl(cmd)
    echo GREEN & "Image cloned" & RESET
    echo response
    return

  if grantId != "":
    if trustedKey == "":
      stderr.writeLine(RED & "Error: --grant requires --trusted-key" & RESET)
      quit(1)
    let json = fmt"""{{"trusted_api_key":"{trustedKey}"}}"""
    let path = fmt"/images/{grantId}/grant"
    let authHeaders = buildAuthHeaders("POST", path, json, publicKey, secretKey)
    let cmd = fmt"""curl -s -X POST '{API_BASE}/images/{grantId}/grant' -H 'Content-Type: application/json' {authHeaders} -d '{json}'"""
    discard execCurl(cmd)
    echo GREEN & "Access granted to " & trustedKey & RESET
    return

  if revokeId != "":
    if trustedKey == "":
      stderr.writeLine(RED & "Error: --revoke requires --trusted-key" & RESET)
      quit(1)
    let json = fmt"""{{"trusted_api_key":"{trustedKey}"}}"""
    let path = fmt"/images/{revokeId}/revoke"
    let authHeaders = buildAuthHeaders("POST", path, json, publicKey, secretKey)
    let cmd = fmt"""curl -s -X POST '{API_BASE}/images/{revokeId}/revoke' -H 'Content-Type: application/json' {authHeaders} -d '{json}'"""
    discard execCurl(cmd)
    echo GREEN & "Access revoked from " & trustedKey & RESET
    return

  if trustedId != "":
    let path = fmt"/images/{trustedId}/trusted"
    let authHeaders = buildAuthHeaders("GET", path, "", publicKey, secretKey)
    let cmd = fmt"""curl -s -X GET '{API_BASE}/images/{trustedId}/trusted' {authHeaders}"""
    echo execCurl(cmd)
    return

  if transferId != "":
    if toKey == "":
      stderr.writeLine(RED & "Error: --transfer requires --to-key" & RESET)
      quit(1)
    let json = fmt"""{{"to_api_key":"{toKey}"}}"""
    let path = fmt"/images/{transferId}/transfer"
    let status = execCurlPostWithSudo(path, json, publicKey, secretKey)
    if status >= 200 and status < 300:
      echo GREEN & "Image transferred to " & toKey & RESET
    elif status != 428:
      stderr.writeLine(RED & "Error: Failed to transfer image" & RESET)
      quit(1)
    return

  stderr.writeLine(RED & "Error: Use --list, --info, --delete, --lock, --unlock, --publish, --visibility, --spawn, --clone, --grant, --revoke, --trusted, or --transfer" & RESET)
  quit(1)

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
    stderr.writeLine("       un.nim image [options]")
    stderr.writeLine("       un.nim service env <action> <service_id> [options]")
    stderr.writeLine("       un.nim languages [--json]")
    stderr.writeLine("       un.nim key [options]")
    stderr.writeLine("")
    stderr.writeLine("Languages options:")
    stderr.writeLine("  --json              Output as JSON array")
    stderr.writeLine("")
    stderr.writeLine("Image options:")
    stderr.writeLine("  --list, -l          List all images")
    stderr.writeLine("  --info ID           Get image details")
    stderr.writeLine("  --delete ID         Delete an image")
    stderr.writeLine("  --lock ID           Lock image to prevent deletion")
    stderr.writeLine("  --unlock ID         Unlock image")
    stderr.writeLine("  --publish ID        Publish image from service/snapshot")
    stderr.writeLine("  --source-type TYPE  Source type: service or snapshot")
    stderr.writeLine("  --visibility ID MODE  Set visibility: private, unlisted, or public")
    stderr.writeLine("  --spawn ID          Spawn new service from image")
    stderr.writeLine("  --clone ID          Clone an image")
    stderr.writeLine("  --name NAME         Name for spawned service or cloned image")
    stderr.writeLine("  --ports PORTS       Ports for spawned service")
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

  if args[0] == "languages":
    var jsonOutput = false
    var i = 1
    while i < args.len:
      case args[i]
      of "--json": jsonOutput = true
      of "-k": publicKey = args[i+1]; inc i
      else: discard
      inc i
    cmdLanguages(jsonOutput, publicKey, secretKey)
    return

  if args[0] == "image":
    var list = false
    var infoId, deleteId, lockId, unlockId, publishId, sourceType = ""
    var visibilityId, visibilityMode, spawnId, cloneId, name, ports = ""
    var grantId, revokeId, trustedId, trustedKey, transferId, toKey = ""
    var i = 1
    while i < args.len:
      case args[i]
      of "--list", "-l": list = true
      of "--info": infoId = args[i+1]; inc i
      of "--delete": deleteId = args[i+1]; inc i
      of "--lock": lockId = args[i+1]; inc i
      of "--unlock": unlockId = args[i+1]; inc i
      of "--publish": publishId = args[i+1]; inc i
      of "--source-type": sourceType = args[i+1]; inc i
      of "--visibility":
        visibilityId = args[i+1]
        visibilityMode = args[i+2]
        inc i, 2
      of "--spawn": spawnId = args[i+1]; inc i
      of "--clone": cloneId = args[i+1]; inc i
      of "--name": name = args[i+1]; inc i
      of "--ports": ports = args[i+1]; inc i
      of "--grant": grantId = args[i+1]; inc i
      of "--revoke": revokeId = args[i+1]; inc i
      of "--trusted": trustedId = args[i+1]; inc i
      of "--trusted-key": trustedKey = args[i+1]; inc i
      of "--transfer": transferId = args[i+1]; inc i
      of "--to-key": toKey = args[i+1]; inc i
      of "-k": publicKey = args[i+1]; inc i
      else: discard
      inc i
    cmdImage(list, infoId, deleteId, lockId, unlockId, publishId, sourceType, visibilityId, visibilityMode, spawnId, cloneId, name, ports, grantId, revokeId, trustedId, trustedKey, transferId, toKey, publicKey, secretKey)
    return

  if args[0] == "snapshot":
    var list = false
    var infoId, sessionId, serviceId, restoreId, deleteId, lockId, unlockId = ""
    var cloneId, cloneType, name, ports = ""
    var hot = false
    var i = 1
    while i < args.len:
      case args[i]
      of "--list", "-l": list = true
      of "--info": infoId = args[i+1]; inc i
      of "--session": sessionId = args[i+1]; inc i
      of "--service": serviceId = args[i+1]; inc i
      of "--restore": restoreId = args[i+1]; inc i
      of "--delete": deleteId = args[i+1]; inc i
      of "--lock": lockId = args[i+1]; inc i
      of "--unlock": unlockId = args[i+1]; inc i
      of "--clone": cloneId = args[i+1]; inc i
      of "--clone-type": cloneType = args[i+1]; inc i
      of "--name": name = args[i+1]; inc i
      of "--ports": ports = args[i+1]; inc i
      of "--hot": hot = true
      of "-k": publicKey = args[i+1]; inc i
      else: discard
      inc i
    cmdSnapshot(list, infoId, sessionId, serviceId, restoreId, deleteId, lockId, unlockId, cloneId, cloneType, name, ports, hot, publicKey, secretKey)
    return

  if args[0] == "logs":
    var source, since, grep = ""
    var lines = 0
    var follow = false
    var i = 1
    while i < args.len:
      case args[i]
      of "--source": source = args[i+1]; inc i
      of "--lines": lines = parseInt(args[i+1]); inc i
      of "--since": since = args[i+1]; inc i
      of "--grep": grep = args[i+1]; inc i
      of "--follow", "-f": follow = true
      of "-k": publicKey = args[i+1]; inc i
      else: discard
      inc i
    cmdLogs(source, lines, since, grep, follow, publicKey, secretKey)
    return

  if args[0] == "health":
    cmdHealth()
    return

  if args[0] == "version":
    cmdVersion()
    return

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
    var kill, info, freeze, unfreeze, boost, unboost, execute, command, shell, network = ""
    var vcpu = 0
    var tmux, screen = false
    var inputFiles: seq[string] = @[]
    var i = 1
    while i < args.len:
      case args[i]
      of "--list": list = true
      of "--kill": kill = args[i+1]; inc i
      of "--info": info = args[i+1]; inc i
      of "--freeze": freeze = args[i+1]; inc i
      of "--unfreeze": unfreeze = args[i+1]; inc i
      of "--boost": boost = args[i+1]; inc i
      of "--unboost": unboost = args[i+1]; inc i
      of "--execute": execute = args[i+1]; inc i
      of "--command": command = args[i+1]; inc i
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
    cmdSession(list, kill, info, freeze, unfreeze, boost, unboost, execute, command, shell, network, vcpu, tmux, screen, inputFiles, publicKey, secretKey)
    return

  if args[0] == "service":
    var name, ports, bootstrap, bootstrapFile, serviceType = ""
    var list = false
    var info, logs, tail, sleep, wake, destroy, resize, execute, command, dumpBootstrap, dumpFile, network = ""
    var lock, unlock, redeploy = ""
    var vcpu = 0
    var resizeVcpu = 0
    var unfreezeOnDemand = false
    var setUnfreezeOnDemand, setUnfreezeOnDemandEnabled = ""
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
      cmdService(name, ports, bootstrap, bootstrapFile, serviceType, list, info, logs, tail, sleep, wake, destroy, resize, resizeVcpu, execute, command, dumpBootstrap, dumpFile, network, vcpu, unfreezeOnDemand, setUnfreezeOnDemand, setUnfreezeOnDemandEnabled, lock, unlock, redeploy, inputFiles, svcEnvs, svcEnvFile, envAction, envTarget, publicKey, secretKey)
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
      of "--lock": lock = args[i+1]; inc i
      of "--unlock": unlock = args[i+1]; inc i
      of "--redeploy": redeploy = args[i+1]; inc i
      of "-n": network = args[i+1]; inc i
      of "-v": vcpu = parseInt(args[i+1]); inc i
      of "-k": publicKey = args[i+1]; inc i
      of "-e": svcEnvs.add(args[i+1]); inc i
      of "--env-file": svcEnvFile = args[i+1]; inc i
      of "--unfreeze-on-demand": unfreezeOnDemand = true
      of "--set-unfreeze-on-demand":
        setUnfreezeOnDemand = args[i+1]
        setUnfreezeOnDemandEnabled = args[i+2]
        inc i, 2
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
    cmdService(name, ports, bootstrap, bootstrapFile, serviceType, list, info, logs, tail, sleep, wake, destroy, resize, resizeVcpu, execute, command, dumpBootstrap, dumpFile, network, vcpu, unfreezeOnDemand, setUnfreezeOnDemand, setUnfreezeOnDemandEnabled, lock, unlock, redeploy, inputFiles, svcEnvs, svcEnvFile, envAction, envTarget, publicKey, secretKey)
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
