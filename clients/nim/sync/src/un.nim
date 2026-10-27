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


# UN CLI - Nim Implementation (using curl subprocess)
# Compile: nim c -d:release un.nim
# Usage:
#   un.nim script.py
#   un.nim -e KEY=VALUE script.py
#   un.nim session --list
#   un.nim service --name web --ports 8080

import os, strutils, osproc, strformat, times, terminal, tables, json, streams
from posix import nil

const
  BLUE = "\x1b[34m"
  RED = "\x1b[31m"
  GREEN = "\x1b[32m"
  YELLOW = "\x1b[33m"
  RESET = "\x1b[0m"
  LANGUAGES_CACHE_TTL = 3600  # 1 hour in seconds

# API base URL: UNSANDBOX_API_URL (trailing slash stripped), default https://api.unsandbox.com
let API_BASE = block:
  let v = getEnv("UNSANDBOX_API_URL", "").strip(leading = false, chars = {'/'})
  if v.len > 0: v else: "https://api.unsandbox.com"

# Portal base URL: UNSANDBOX_PORTAL_URL (trailing slash stripped), default https://unsandbox.com.
# Serves /logs/{source} (partner log access) and /keys/extend.
let PORTAL_BASE = block:
  let v = getEnv("UNSANDBOX_PORTAL_URL", "").strip(leading = false, chars = {'/'})
  if v.len > 0: v else: "https://unsandbox.com"

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

# SHA-256 / HMAC-SHA256 computed in-process (FIPS 180-4, RFC 2104). Never shell
# out for this: a subprocess would expose the secret key in argv (ps) and
# interpolate the message into a shell.
const sha256K: array[64, uint32] = [
  0x428a2f98'u32, 0x71374491'u32, 0xb5c0fbcf'u32, 0xe9b5dba5'u32, 0x3956c25b'u32, 0x59f111f1'u32, 0x923f82a4'u32, 0xab1c5ed5'u32,
  0xd807aa98'u32, 0x12835b01'u32, 0x243185be'u32, 0x550c7dc3'u32, 0x72be5d74'u32, 0x80deb1fe'u32, 0x9bdc06a7'u32, 0xc19bf174'u32,
  0xe49b69c1'u32, 0xefbe4786'u32, 0x0fc19dc6'u32, 0x240ca1cc'u32, 0x2de92c6f'u32, 0x4a7484aa'u32, 0x5cb0a9dc'u32, 0x76f988da'u32,
  0x983e5152'u32, 0xa831c66d'u32, 0xb00327c8'u32, 0xbf597fc7'u32, 0xc6e00bf3'u32, 0xd5a79147'u32, 0x06ca6351'u32, 0x14292967'u32,
  0x27b70a85'u32, 0x2e1b2138'u32, 0x4d2c6dfc'u32, 0x53380d13'u32, 0x650a7354'u32, 0x766a0abb'u32, 0x81c2c92e'u32, 0x92722c85'u32,
  0xa2bfe8a1'u32, 0xa81a664b'u32, 0xc24b8b70'u32, 0xc76c51a3'u32, 0xd192e819'u32, 0xd6990624'u32, 0xf40e3585'u32, 0x106aa070'u32,
  0x19a4c116'u32, 0x1e376c08'u32, 0x2748774c'u32, 0x34b0bcb5'u32, 0x391c0cb3'u32, 0x4ed8aa4a'u32, 0x5b9cca4f'u32, 0x682e6ff3'u32,
  0x748f82ee'u32, 0x78a5636f'u32, 0x84c87814'u32, 0x8cc70208'u32, 0x90befffa'u32, 0xa4506ceb'u32, 0xbef9a3f7'u32, 0xc67178f2'u32]

proc rotr32(x: uint32, n: int): uint32 {.inline.} = (x shr n) or (x shl (32 - n))

proc sha256Raw(data: string): string =
  ## Raw 32-byte SHA-256 digest of data.
  var h: array[8, uint32] = [0x6a09e667'u32, 0xbb67ae85'u32, 0x3c6ef372'u32, 0xa54ff53a'u32,
                             0x510e527f'u32, 0x9b05688c'u32, 0x1f83d9ab'u32, 0x5be0cd19'u32]
  var msg = data
  let bitLen = uint64(data.len) * 8
  msg.add('\x80')
  while msg.len mod 64 != 56:
    msg.add('\x00')
  for i in countdown(7, 0):
    msg.add(char((bitLen shr (i * 8)) and 0xff))
  var w: array[64, uint32]
  var chunk = 0
  while chunk < msg.len:
    for t in 0 ..< 16:
      let o = chunk + t * 4
      w[t] = (uint32(ord(msg[o])) shl 24) or (uint32(ord(msg[o+1])) shl 16) or
             (uint32(ord(msg[o+2])) shl 8) or uint32(ord(msg[o+3]))
    for t in 16 ..< 64:
      let s0 = rotr32(w[t-15], 7) xor rotr32(w[t-15], 18) xor (w[t-15] shr 3)
      let s1 = rotr32(w[t-2], 17) xor rotr32(w[t-2], 19) xor (w[t-2] shr 10)
      w[t] = w[t-16] + s0 + w[t-7] + s1
    var a = h[0]; var b = h[1]; var c = h[2]; var d = h[3]
    var e = h[4]; var f = h[5]; var g = h[6]; var hh = h[7]
    for t in 0 ..< 64:
      let S1 = rotr32(e, 6) xor rotr32(e, 11) xor rotr32(e, 25)
      let ch = (e and f) xor ((not e) and g)
      let temp1 = hh + S1 + ch + sha256K[t] + w[t]
      let S0 = rotr32(a, 2) xor rotr32(a, 13) xor rotr32(a, 22)
      let maj = (a and b) xor (a and c) xor (b and c)
      let temp2 = S0 + maj
      hh = g; g = f; f = e; e = d + temp1
      d = c; c = b; b = a; a = temp1 + temp2
    h[0] += a; h[1] += b; h[2] += c; h[3] += d
    h[4] += e; h[5] += f; h[6] += g; h[7] += hh
    chunk += 64
  result = newStringOfCap(32)
  for v in h:
    for i in countdown(3, 0):
      result.add(char((v shr (i * 8)) and 0xff))

proc sha256Hex(data: string): string =
  result = sha256Raw(data).toHex().toLowerAscii()

proc computeHmac(secretKey: string, message: string): string =
  var key = secretKey
  if key.len > 64:
    key = sha256Raw(key)
  key.setLen(64)  # zero-padded
  var ipad = newString(64)
  var opad = newString(64)
  for i in 0 ..< 64:
    ipad[i] = char(ord(key[i]) xor 0x36)
    opad[i] = char(ord(key[i]) xor 0x5c)
  result = sha256Raw(opad & sha256Raw(ipad & message)).toHex().toLowerAscii()

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
  # The server verifies against the request path without the query string
  # (conn.request_path), so sign only the part before '?'.
  let signPath = path.split('?', 1)[0]
  let message = fmt"{timestamp}:{meth}:{signPath}:{body}"
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

# Target OS from the global --os flag: "" (field omitted, API default linux),
# "linux" or "freebsd". Sent as "os" in /execute, /sessions and /services bodies.
var targetOs = ""

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
  var json = fmt"""{{"language":"{lang}","code":"{escapeJson(code)}""""

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
  if targetOs != "": json.add(fmt""","os":"{targetOs}"""")
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
    let json = fmt"""{{"command":"{escapeJson(command)}"}}"""
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

  let shellName = if shell != "": shell else: "bash"
  var json = fmt"""{{"shell":"{shellName}""""
  if network != "": json.add(fmt""","network":"{network}"""")
  if vcpu > 0: json.add(fmt""","vcpu":{vcpu}""")
  if targetOs != "": json.add(fmt""","os":"{targetOs}"""")
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
    let json = fmt"""{{"command":"{escapeJson(command)}"}}"""
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
    var json = fmt"""{{"name":"{name}""""
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
    if targetOs != "": json.add(fmt""","os":"{targetOs}"""")
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
  if extend:
    let extendUrl = fmt"{PORTAL_BASE}/keys/extend?pk={publicKey}"
    echo BLUE & "Opening browser to extend key..." & RESET
    discard execProcess(fmt"xdg-open '{extendUrl}'")
    return
  # Display per CLI_SPEC.md "`un key` output": the API returns no public key,
  # so we print ours; expiry derives from valid_through (ms since epoch).
  let authHeaders = buildAuthHeaders("POST", "/keys/validate", "", publicKey, secretKey)
  let cmd = "curl -s -w '\\n%{http_code}' -X POST '" & API_BASE & "/keys/validate' " & authHeaders
  let (rawOut, rc) = execCmdEx(cmd)
  let raw = rawOut.strip(leading = false, chars = {'\n', '\r'})  # execCmdEx appends a newline
  let nl = raw.rfind('\n')
  if rc != 0 or nl < 0:
    stderr.writeLine(RED & "Error: request to " & API_BASE & "/keys/validate failed" & RESET)
    quit(1)
  let code = raw[nl+1..^1].strip()
  var kr = newJObject()
  try:
    let parsed = parseJson(raw[0..<nl])
    if parsed.kind == JObject: kr = parsed
  except CatchableError:
    discard
  var state = "invalid"
  var reason = ""
  if code == "200":
    state = if kr.hasKey("status"): kr["status"].getStr() elif kr{"valid"}.getBool(): "active" else: "invalid"
    reason = "status " & state
  elif code == "401":
    reason = if kr.hasKey("error"): kr["error"].getStr() else: "unauthorized"
    state = case reason
      of ":expired", "expired": "expired"
      of ":suspended", "suspended": "suspended"
      else: "invalid"
  else:
    stderr.writeLine(RED & "Error: HTTP " & code & " " & kr{"error"}.getStr() & RESET)
    quit(4)
  case state
  of "active": discard
  of "expired":
    echo RED & "Expired" & RESET
    echo "Public Key: " & publicKey
    echo YELLOW & "To renew: Visit " & PORTAL_BASE & "/keys/extend" & RESET
    quit(3)
  of "suspended":
    echo RED & "Suspended" & RESET
    echo "Public Key: " & publicKey
    quit(3)
  else:
    echo RED & "Invalid" & RESET
    echo "Public Key: " & publicKey
    echo "Reason: " & reason
    quit(3)
  let tier = if kr.hasKey("tier") and kr["tier"].kind != JNull: ($kr["tier"]).strip(chars = {'"'}) else: "N/A"
  let status = if kr.hasKey("status"): kr["status"].getStr() else: "N/A"
  var expires = "N/A"
  var remaining = "N/A"
  if kr.hasKey("valid_through") and kr["valid_through"].kind in {JInt, JFloat}:
    let vt = if kr["valid_through"].kind == JInt: kr["valid_through"].getBiggestInt() else: int64(kr["valid_through"].getFloat())
    let secs = vt div 1000
    expires = fromUnix(secs).utc.format("yyyy-MM-dd'T'HH:mm:ss'Z'")
    let left = secs - getTime().toUnix()
    remaining = if left <= 0: "expired" else: $(left div 86400) & "d " & $(left mod 86400 div 3600) & "h " & $(left mod 3600 div 60) & "m"
  echo GREEN & "Valid" & RESET
  echo "Public Key: " & publicKey
  echo "Tier: " & tier
  echo "Status: " & status
  echo "Expires: " & expires
  echo "Time Remaining: " & remaining

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
    let cloneTypeName = if cloneType != "": cloneType else: "session"
    var json = fmt"""{{"clone_type":"{cloneTypeName}""""
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

# ---------------------------------------------------------------------------
# PaaS logs (CLI_SPEC.md "PaaS Logs Command"): GET {PORTAL}/logs/{source}[/stream].
# HMAC covers "ts:GET:/logs/{source}:" (path only). curl runs from an argv list
# (no shell); the secret never leaves this process, only its signature does.
# ---------------------------------------------------------------------------

proc pctEncode(v: string): string =
  for c in v:
    if c in {'A'..'Z', 'a'..'z', '0'..'9', '-', '_', '.', '~'}: result.add(c)
    else: result.add('%' & toHex(ord(c), 2))

proc addQuery(url: var string, key, val: string) =
  if val == "": return
  url.add(if '?' in url: '&' else: '?')
  url.add(key & "=" & pctEncode(val))

proc logsPath(source: string, stream: bool): string =
  let src = if source == "": "all" else: source
  result = "/logs/" & src
  if stream: result.add("/stream")

proc portalAuthArgs(path, publicKey, secretKey: string): seq[string] =
  result = @["-H", "Authorization: Bearer " & publicKey]
  if secretKey != "":
    let ts = getTimestamp()
    let sig = computeHmac(secretKey, ts & ":GET:" & path & ":")
    result.add(@["-H", "X-Timestamp: " & ts, "-H", "X-Signature: " & sig])

const STATUS_MARK = "__UNSB_HTTP_STATUS__"

# Raw fetch: (status, body); status 0 = curl never got a response.
proc logsFetchRaw(source: string, lines: int, since, grep, level, publicKey, secretKey: string): (int, string) =
  let path = logsPath(source, false)
  var url = PORTAL_BASE & path & "?lines=" & $lines
  url.addQuery("since", since)
  url.addQuery("grep", grep)
  url.addQuery("level", level)
  let args = @["-s", "-X", "GET", url] & portalAuthArgs(path, publicKey, secretKey) &
             @["-A", "un-cli/2.0", "-w", "\n" & STATUS_MARK & "%{http_code}"]
  var output = ""
  try:
    output = execProcess("curl", args = args, options = {poUsePath, poStdErrToStdOut})
  except OSError:
    return (0, "")
  let idx = output.rfind("\n" & STATUS_MARK)
  if idx < 0: return (0, output)
  let status = try: parseInt(output[idx + 1 + STATUS_MARK.len .. ^1].strip()) except ValueError: 0
  return (status, output[0 ..< idx])

proc paasCursorPath(): string =
  let home = getEnv("HOME", "")
  if home == "": "" else: home / ".unsandbox" / "paas_log_cursor"

proc readPaasCursor(): string =
  let p = paasCursorPath()
  if p == "" or not fileExists(p): return ""
  try:
    let ts = parseBiggestInt(readFile(p).strip())
    if ts <= 0: return ""
    var ago = epochTime().int64 - ts
    if ago < 5: ago = 5
    return $ago & "s"
  except CatchableError:
    return ""

proc writePaasCursor() =
  let p = paasCursorPath()
  if p == "": return
  try:
    createDir(parentDir(p))
    writeFile(p, $epochTime().int64 & "\n")
  except CatchableError:
    discard

proc jStr(n: JsonNode, key: string): string =
  if n != nil and n.kind == JObject and n.hasKey(key) and n[key].kind == JString: n[key].getStr() else: ""

proc printPaasError(status: int, body: string) =
  var msg = ""
  try: msg = jStr(parseJson(body), "error")
  except CatchableError: discard
  if msg != "": stderr.writeLine("Error: HTTP " & $status & " — " & msg)
  else: stderr.writeLine("Error: HTTP " & $status)

proc fetchPaasLogs(source: string, lines: int, since, grep: string, jsonOut: bool, level, publicKey, secretKey: string): int =
  let (status, body) = logsFetchRaw(source, lines, since, grep, level, publicKey, secretKey)
  if status == 0:
    stderr.writeLine("Error: Network error fetching logs")
    return 1
  if status != 200:
    printPaasError(status, body)
    return 1
  writePaasCursor()
  if jsonOut:
    echo body
    return 0
  var root: JsonNode
  try: root = parseJson(body)
  except CatchableError:
    stderr.writeLine("Error: Invalid response format")
    return 1
  if root.kind != JObject or not root.hasKey("lines") or root["lines"].kind != JArray:
    stderr.writeLine("Error: Invalid response format")
    return 1
  let isAll = root.hasKey("sources")
  for item in root["lines"]:
    if item.kind == JString:
      echo item.getStr()
    elif item.kind == JObject:
      let src = jStr(item, "source")
      if isAll and src != "": echo "\x1b[36m" & src.alignLeft(12) & "\x1b[0m " & jStr(item, "line")
      else: echo jStr(item, "line")
  return 0

# Follow {PORTAL}/logs/{source}/stream; reconnect (2s doubling to 30s) with
# since= covering the gap and a fresh signature. 401/403 is fatal.
proc streamPaasLogs(source, grep, level, publicKey, secretKey: string): int =
  let path = logsPath(source, true)
  var base = PORTAL_BASE & path
  base.addQuery("grep", grep)
  base.addQuery("level", level)
  stderr.writeLine("Streaming " & source & " logs... (Ctrl+C to stop)")
  var delay = 2
  var lastRecv: int64 = 0
  var reconnect = false
  while true:
    var url = base
    if reconnect and lastRecv > 0:
      let gap = max(epochTime().int64 - lastRecv + 5, 10)
      url.add((if '?' in url: "&" else: "?") & "since=" & $gap & "s")
    let args = @["-s", "-N", "--http1.1", "-H", "Accept: text/event-stream"] &
               portalAuthArgs(path, publicKey, secretKey) &
               @["-A", "un-cli/2.0", "-w", "\n" & STATUS_MARK & "%{http_code}\n", url]
    let connectedAt = epochTime().int64
    var status = 0
    var p: Process
    try:
      p = startProcess("curl", args = args, options = {poUsePath})
    except OSError:
      stderr.writeLine("Error: Stream error: cannot run curl")
      return 1
    let outp = p.outputStream
    var line = ""
    while outp.readLine(line):
      if line.startsWith(STATUS_MARK):
        status = try: parseInt(line[STATUS_MARK.len .. ^1].strip()) except ValueError: 0
        continue
      if not line.startsWith("data:"): continue
      let data = line[5 .. ^1].strip(trailing = false)
      if data == "" or data == ":keepalive": continue
      try:
        let n = parseJson(data)
        if n.kind == JObject and n.hasKey("line"):
          let src = if jStr(n, "source") != "": jStr(n, "source") else: source
          stdout.writeLine("\x1b[36m" & src.alignLeft(12) & "\x1b[0m " & jStr(n, "line"))
        else:
          stdout.writeLine(data)
      except CatchableError:
        stdout.writeLine(data)
      stdout.flushFile()
      lastRecv = epochTime().int64
    discard p.waitForExit()
    p.close()
    if status == 401 or status == 403:
      stderr.writeLine("Error: HTTP " & $status & " (auth failed)")
      return 1
    if status != 0 and status != 200 and status < 500:
      stderr.writeLine("Error: HTTP " & $status)
      return 1
    if status == 0 and not reconnect and lastRecv == 0:
      stderr.writeLine("Error: Stream error: could not connect")
      return 1
    if epochTime().int64 - connectedAt > 30: delay = 2
    sleep(delay * 1000)
    delay = min(delay * 2, 30)
    reconnect = true

proc paasLogsHelp() =
  stderr.write("""Usage: un paas logs [options]

Fetch or stream PaaS production logs.

Sources:
  --all              All sources (default)
  --api              API server logs
  --portal           Portal server logs
  --pool NODE        Pool node logs (e.g., cammy, ai)
  --source SOURCE    Source string (api, portal, pool/cammy, all)

Options:
  --lines N, -n N    Number of lines (default: 100, max: 10000)
  --since TIME       Time window: 1m, 5m, 15m, 1h, 6h, 1d (default: 5m)
  --grep PATTERN     Filter log lines by pattern
  --level LVL, -l    Min log level: debug, info, notice, warning, err, crit
  --follow, -f       Follow logs in real-time (Ctrl+C to stop)
  --json             Output raw JSON response
  -p KEY             Public key
  -k KEY             Secret key
  -h                 Show this help

Examples:
  un paas logs                             # last 100 lines from all sources
  un paas logs --api --lines 500           # last 500 API log lines
  un paas logs --portal --grep error       # portal logs matching 'error'
  un paas logs --pool cammy --since 1h     # cammy pool logs from last hour
  un paas logs -l warning                  # warnings and above from all sources
  un paas logs --follow                    # follow all logs in real-time
  un paas logs --follow -l warning         # follow warnings+ in real-time

Requires a partner API key with log access enabled.
""")

# `un paas logs ...` (args after "logs"); -p/-k/--account come from main's pre-scan.
proc cmdPaasLogs(args: seq[string], publicKey, secretKey: string): int =
  var source = "all"
  var lines = 100
  var since, grep, level = ""
  var follow, jsonOut = false
  var i = 0
  while i < args.len:
    let a = args[i]
    let hasNext = i + 1 < args.len
    if a in ["-p", "-k", "--account"] and hasNext: inc i
    elif a == "--source" and hasNext: source = args[i+1]; inc i
    elif a == "--all": source = "all"
    elif a == "--api": source = "api"
    elif a == "--portal": source = "portal"
    elif a == "--pool" and hasNext: source = "pool/" & args[i+1]; inc i
    elif a in ["--lines", "-n"] and hasNext:
      lines = try: parseInt(args[i+1]) except ValueError: 0
      lines = max(1, min(lines, 10000)); inc i
    elif a == "--since" and hasNext: since = args[i+1]; inc i
    elif a == "--grep" and hasNext: grep = args[i+1]; inc i
    elif a in ["--level", "-l"] and hasNext: level = args[i+1]; inc i
    elif a in ["--follow", "-f"]: follow = true
    elif a == "--json": jsonOut = true
    elif a in ["-h", "--help"]:
      paasLogsHelp()
      return 0
    inc i
  if publicKey == "":
    stderr.writeLine("Error: API credentials required.\n  Set UNSANDBOX_PUBLIC_KEY + UNSANDBOX_SECRET_KEY env vars, or\n  Use -p PUBLIC_KEY -k SECRET_KEY flags, or\n  Create ~/.unsandbox/accounts.csv with: public_key,secret_key")
    return 1
  if follow: return streamPaasLogs(source, grep, level, publicKey, secretKey)
  if since == "":
    since = readPaasCursor()
    if since == "": since = "5m"
  return fetchPaasLogs(source, lines, since, grep, jsonOut, level, publicKey, secretKey)

proc cmdLogs(source: string, lines: int, since, grep: string, follow: bool, publicKey, secretKey: string) =
  ## Library-style entry kept for compatibility: portal /logs/{source}.
  let src = if source != "": source else: "all"
  let rc = if follow: streamPaasLogs(src, grep, "", publicKey, secretKey)
           else: fetchPaasLogs(src, (if lines > 0: min(lines, 10000) else: 100), (if since != "": since else: "5m"), grep, false, "", publicKey, secretKey)
  if rc != 0: quit(rc)

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

proc loadCredentialsFromCsv(csvPath: string, accountIndex: int): tuple[pk: string, sk: string] =
  ## Load public_key,secret_key from CSV at given row index (0-based, skipping comments/blanks).
  result = ("", "")
  if not fileExists(csvPath):
    return
  try:
    let content = readFile(csvPath)
    var dataIndex = 0
    for line in content.splitLines():
      let trimmed = line.strip()
      if trimmed.len == 0 or trimmed.startsWith("#"):
        continue
      if dataIndex == accountIndex:
        let parts = trimmed.split(',')
        if parts.len >= 2:
          result = (parts[0].strip(), parts[1].strip())
        return
      dataIndex.inc
  except:
    discard

proc credentialsCsvPath(argPk, argSk: string, accountIndex: int): string =
  ## The accounts.csv the credentials came from, or "" (flags / env vars).
  ## Mirrors resolveCredentials' tiers.
  if argPk != "" and argSk != "":
    return ""
  var index = accountIndex
  if index < 0:
    if getEnv("UNSANDBOX_PUBLIC_KEY", "") != "" and getEnv("UNSANDBOX_SECRET_KEY", "") != "":
      return ""
    try:
      index = parseInt(getEnv("UNSANDBOX_ACCOUNT", "0"))
    except ValueError:
      index = 0
  for path in [getHomeDir() / ".unsandbox" / "accounts.csv", "accounts.csv"]:
    if loadCredentialsFromCsv(path, index).pk != "":
      return path
  return ""

proc writePrivateFile(path, content: string): bool =
  ## Create/truncate path with mode 0600 and write content.
  let fd = posix.open(path.cstring, posix.O_WRONLY or posix.O_CREAT or posix.O_TRUNC, posix.Mode(0o600))
  if fd < 0:
    return false
  var written = 0
  while written < content.len:
    let n = posix.write(fd, unsafeAddr content[written], content.len - written)
    if n <= 0:
      discard posix.close(fd)
      return false
    written += n
  discard posix.fchmod(fd, posix.Mode(0o600))
  return posix.close(fd) == 0

proc rewriteAccountsCsv(path, publicKey, newSecret: string): bool =
  ## Replace the secret on the line whose first field is publicKey. Every other
  ## line, field, comment and the order stay as they are. Temp file (0600) + rename.
  var content: string
  try:
    content = readFile(path)
  except IOError:
    return false
  var lines = content.split('\n')
  var changed = false
  for i in 0 ..< lines.len:
    let stripped = lines[i].strip()
    if stripped.len == 0 or stripped.startsWith("#"):
      continue
    var fields = lines[i].split(',')
    if fields.len >= 2 and fields[0].strip() == publicKey:
      fields[1] = newSecret
      lines[i] = fields.join(",")
      changed = true
      break
  if not changed:
    return false
  let (dir, name, ext) = splitFile(path)
  let tmp = (if dir.len > 0: dir else: ".") / ("." & name & ext & "." & $getCurrentProcessId() & ".tmp")
  if not writePrivateFile(tmp, lines.join("\n")):
    discard tryRemoveFile(tmp)
    return false
  try:
    moveFile(tmp, path)
  except OSError:
    discard tryRemoveFile(tmp)
    return false
  return true

proc isSafeToken(v: string): bool =
  ## Only [A-Za-z0-9_-] may reach the curl command line (OTP / challenge id).
  if v.len == 0:
    return false
  for c in v:
    if not (c.isAlphaNumeric or c == '-' or c == '_'):
      return false
  return true

proc jsonStringField(json, field: string): string =
  ## String value of "field" in a flat JSON object, tolerating whitespace around ':'.
  var pos = 0
  let key = "\"" & field & "\""
  while true:
    let k = json.find(key, pos)
    if k < 0:
      return ""
    var i = k + key.len
    while i < json.len and json[i] in Whitespace: inc i
    if i < json.len and json[i] == ':':
      inc i
      while i < json.len and json[i] in Whitespace: inc i
      if i >= json.len or json[i] != '"':
        return ""
      inc i
      var value = ""
      while i < json.len and json[i] != '"':
        if json[i] == '\\' and i + 1 < json.len:
          inc i
        value.add(json[i])
        inc i
      return value
    pos = k + 1

proc postRotateSecret(publicKey, secretKey, otp, challenge: string): (int, string) =
  ## POST /keys/self/rotate-secret, body {}, fresh in-process signature per call.
  let path = "/keys/self/rotate-secret"
  let authHeaders = buildAuthHeaders("POST", path, "{}", publicKey, secretKey)
  var sudoHeaders = ""
  if otp != "":
    if not isSafeToken(otp) or not isSafeToken(challenge):
      stderr.writeLine("Error: invalid one-time code or challenge")
      quit(2)
    sudoHeaders = fmt"-H 'X-Sudo-OTP: {otp}' -H 'X-Sudo-Challenge: {challenge}'"
  let cmd = fmt"""curl -s -w '\n%{{http_code}}' -X POST '{API_BASE}{path}' -H 'Content-Type: application/json' {authHeaders} {sudoHeaders} -d '{{}}'"""
  let output = execProcess(cmd).strip(leading = false)
  let nl = output.rfind('\n')
  let statusStr = if nl >= 0: output[nl+1 .. ^1] else: output
  let body = if nl >= 0: output[0 ..< nl] else: ""
  var status = 0
  try:
    status = parseInt(statusStr.strip())
  except ValueError:
    status = 0
  return (status, body)

proc cmdKeyRotate(otpFlag, publicKey, secretKey, credsCsv: string) =
  ## un key --rotate [--otp CODE]: rotate this key's secret (see CLI_SPEC.md)
  let unsandboxDir = getHomeDir() / ".unsandbox"
  let challengeFile = unsandboxDir / ("rotate-" & sha256Hex(publicKey)[0 ..< 16] & ".challenge")
  var status: int
  var body: string
  if otpFlag != "":
    var challenge: string
    try:
      challenge = readFile(challengeFile).strip()
    except IOError:
      stderr.writeLine("Error: no rotation in progress; run: un key --rotate")
      quit(2)
    (status, body) = postRotateSecret(publicKey, secretKey, otpFlag.strip(), challenge)
    if status != 0:  # 0: curl never reached the API; keep the challenge for a retry
      discard tryRemoveFile(challengeFile)
  else:
    (status, body) = postRotateSecret(publicKey, secretKey, "", "")
    if status == 428:
      let challenge = jsonStringField(body, "challenge_id")
      if isatty(stdin):
        stderr.writeLine("One-time code emailed to the key owner.")
        stderr.write("Code: ")
        stderr.flushFile()
        var otp = ""
        try:
          otp = stdin.readLine().strip()
        except EOFError:
          otp = ""
        if otp == "":
          stderr.writeLine("Error: cancelled")
          quit(1)
        (status, body) = postRotateSecret(publicKey, secretKey, otp, challenge)
      else:
        if not dirExists(unsandboxDir):
          try:
            createDir(unsandboxDir)
            setFilePermissions(unsandboxDir, {fpUserRead, fpUserWrite, fpUserExec})
          except OSError:
            stderr.writeLine("Error: cannot create " & unsandboxDir)
            quit(1)
        if not writePrivateFile(challengeFile, challenge):
          stderr.writeLine("Error: cannot write " & challengeFile)
          quit(1)
        echo "One-time code emailed to the key owner. Finish with: un key --rotate --otp CODE"
        return

  if status != 200:
    let errField = jsonStringField(body, "error")
    if status == 403 and errField == "email_required":
      stderr.writeLine("Error: this key has no email; add one or use POST /keys/recover")
      quit(3)
    if status == 401:
      stderr.writeLine("Error: authentication failed")
      quit(3)
    var msg = jsonStringField(body, "message")
    if msg == "": msg = errField
    if msg == "": msg = "HTTP " & $status
    stderr.writeLine("Error: " & msg)
    quit(4)
  let newSecret = jsonStringField(body, "secret")
  if newSecret == "":
    stderr.writeLine("Error: API returned no secret")
    quit(4)

  if credsCsv != "" and rewriteAccountsCsv(credsCsv, publicKey, newSecret):
    echo fmt"Secret rotated. Updated {credsCsv} (public key {publicKey})."
  else:
    echo fmt"New secret (shown once, store it now): {newSecret}"
    echo "Warning: UNSANDBOX_SECRET_KEY / -k still hold the old, now invalid, secret."

proc resolveCredentials(argPk: string, argSk: string, accountIndex: int): tuple[pk: string, sk: string] =
  ## Resolve credentials using 5-tier priority:
  ##   1. Explicit -p/-k flags (argPk/argSk)
  ##   2. --account N -> accounts.csv row N (bypasses env vars)
  ##   3. UNSANDBOX_PUBLIC_KEY / UNSANDBOX_SECRET_KEY env vars
  ##   4. ~/.unsandbox/accounts.csv row 0 (or UNSANDBOX_ACCOUNT env var)
  ##   5. ./accounts.csv row 0
  # Tier 1: explicit key flags
  if argPk != "" and argSk != "":
    return (argPk, argSk)
  # Tier 2: --account N bypasses env vars
  let homeDir = getHomeDir()
  if accountIndex >= 0:
    let homeCsv2 = homeDir / ".unsandbox" / "accounts.csv"
    var creds2 = loadCredentialsFromCsv(homeCsv2, accountIndex)
    if creds2.pk != "":
      return creds2
    creds2 = loadCredentialsFromCsv("accounts.csv", accountIndex)
    if creds2.pk != "":
      return creds2
    stderr.writeLine(RED & fmt"Error: No credentials found for account index {accountIndex} in accounts.csv" & RESET)
    quit(1)
  # Tier 3: env vars
  let envPk = getEnv("UNSANDBOX_PUBLIC_KEY", "")
  let envSk = getEnv("UNSANDBOX_SECRET_KEY", "")
  if envPk != "" and envSk != "":
    return (envPk, envSk)
  # Tier 4: ~/.unsandbox/accounts.csv (default row or UNSANDBOX_ACCOUNT)
  let defaultIndex = parseInt(getEnv("UNSANDBOX_ACCOUNT", "0"))
  let homeCsv = homeDir / ".unsandbox" / "accounts.csv"
  var creds = loadCredentialsFromCsv(homeCsv, defaultIndex)
  if creds.pk != "":
    return creds
  # Tier 5: ./accounts.csv
  creds = loadCredentialsFromCsv("accounts.csv", defaultIndex)
  if creds.pk != "":
    return creds
  # Legacy UNSANDBOX_API_KEY
  let legacyKey = getEnv("UNSANDBOX_API_KEY", "")
  if legacyKey != "":
    return (legacyKey, legacyKey)
  stderr.writeLine(RED & "Error: No credentials found. Set UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY" & RESET)
  quit(1)

# Remove the global --os OS / --os=OS flag from params (any position) and validate it
# before any request is made. Quits non-zero on a missing or invalid value.
proc extractOsFlag(params: seq[string]): seq[string] =
  result = @[]
  var i = 0
  while i < params.len:
    var val: string
    if params[i] == "--os":
      if i + 1 >= params.len:
        stderr.writeLine("Error: --os requires a value: linux or freebsd")
        quit(1)
      inc i
      val = params[i]
    elif params[i].startsWith("--os="):
      val = params[i][5 .. ^1]
    else:
      result.add(params[i])
      inc i
      continue
    case val.toLowerAscii()
    of "linux", "freebsd": targetOs = val.toLowerAscii()
    else:
      stderr.writeLine("Error: invalid --os '" & val & "' (expected linux or freebsd)")
      quit(1)
    inc i

proc main() =
  let cliParams = extractOsFlag(commandLineParams())
  var argPublicKey = ""
  var argSecretKey = ""
  var accountIndex = -1  # -1 means not set

  # Pre-scan for --account, -p, and -k before subcommand dispatch
  let rawArgs = cliParams
  var i = 0
  while i < rawArgs.len:
    if rawArgs[i] == "--account" and i + 1 < rawArgs.len:
      try:
        accountIndex = parseInt(rawArgs[i + 1])
      except ValueError:
        discard
      i.inc
    elif rawArgs[i] == "-p" and i + 1 < rawArgs.len:
      argPublicKey = rawArgs[i + 1]
      i.inc
    elif rawArgs[i] == "-k" and i + 1 < rawArgs.len:
      argSecretKey = rawArgs[i + 1]
      i.inc
    i.inc

  let credsCsv = credentialsCsvPath(argPublicKey, argSecretKey, accountIndex)
  let resolved = resolveCredentials(argPublicKey, argSecretKey, accountIndex)
  var publicKey = resolved.pk
  var secretKey = resolved.sk

  let args = cliParams

  if args.len < 1:
    stderr.writeLine("Usage: un.nim [options] <source_file>")
    stderr.writeLine("       un.nim session [options]")
    stderr.writeLine("       un.nim service [options]")
    stderr.writeLine("       un.nim image [options]")
    stderr.writeLine("       un.nim service env <action> <service_id> [options]")
    stderr.writeLine("       un.nim languages [--json]")
    stderr.writeLine("       un.nim key [options]")
    stderr.writeLine("       un.nim key --rotate [--otp CODE]")
    stderr.writeLine("")
    stderr.writeLine("Global options:")
    stderr.writeLine("  --os OS             Target OS: linux (default) or freebsd (execute, session, service)")
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
    stderr.writeLine("")
    stderr.writeLine("Credential options (global):")
    stderr.writeLine("  -p PUBLIC_KEY       Explicit public key")
    stderr.writeLine("  -k SECRET_KEY       Explicit secret key")
    stderr.writeLine("  --account N         Use row N from accounts.csv (bypasses env vars)")
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

  if args[0] == "paas":
    if args.len < 2:
      stderr.writeLine("Usage: un paas <command> [options]\n\nCommands:\n  logs    Fetch or stream production logs\n\nRun 'un paas <command> -h' for help on a specific command.")
      quit(1)
    if args[1] != "logs":
      stderr.writeLine("Error: Unknown paas command '" & args[1] & "'\nRun 'un paas' for available commands.")
      quit(1)
    quit(cmdPaasLogs(args[2 .. ^1], publicKey, secretKey))

  if args[0] == "logs":
    # Alias for `paas logs`
    quit(cmdPaasLogs(args[1 .. ^1], publicKey, secretKey))

  if args[0] == "health":
    cmdHealth()
    return

  if args[0] == "version":
    cmdVersion()
    return

  if args[0] == "key":
    var extend = false
    var rotate = false
    var otpFlag = ""
    var i = 1
    while i < args.len:
      case args[i]
      of "--extend": extend = true
      of "--rotate": rotate = true
      of "--otp":
        if i + 1 < args.len:
          otpFlag = args[i+1]
          inc i
      of "-k": publicKey = args[i+1]; inc i
      inc i
    if rotate:
      cmdKeyRotate(otpFlag, publicKey, secretKey, credsCsv)
      return
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
  i = 0
  while i < args.len:
    case args[i]
    of "-e": envs.add(args[i+1]); inc i
    of "-a": artifacts = true
    of "-n": network = args[i+1]; inc i
    of "-v": vcpu = parseInt(args[i+1]); inc i
    of "-k": publicKey = args[i+1]; inc i
    of "-p": publicKey = args[i+1]; inc i
    of "--account": inc i  # already handled in pre-scan
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
