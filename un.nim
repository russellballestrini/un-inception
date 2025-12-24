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

import os, strutils, osproc, strformat

const
  API_BASE = "https://api.unsandbox.com"
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

proc execCurl(cmd: string): string =
  result = execProcess(cmd)

proc cmdExecute(sourceFile: string, envs: seq[string], artifacts: bool, network: string, vcpu: int, apiKey: string) =
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

  let cmd = fmt"""curl -s -X POST '{API_BASE}/execute' -H 'Content-Type: application/json' -H 'Authorization: Bearer {apiKey}' -d '{json}'"""
  echo execCurl(cmd)

proc cmdSession(list: bool, kill, shell, network: string, vcpu: int, tmux, screen: bool, apiKey: string) =
  if list:
    let cmd = fmt"""curl -s -X GET '{API_BASE}/sessions' -H 'Authorization: Bearer {apiKey}'"""
    echo execCurl(cmd)
    return

  if kill != "":
    let cmd = fmt"""curl -s -X DELETE '{API_BASE}/sessions/{kill}' -H 'Authorization: Bearer {apiKey}'"""
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
  let cmd = fmt"""curl -s -X POST '{API_BASE}/sessions' -H 'Content-Type: application/json' -H 'Authorization: Bearer {apiKey}' -d '{json}'"""
  echo execCurl(cmd)

proc cmdService(name, ports, bootstrap: string, list: bool, info, logs, tail, sleep, wake, destroy, network: string, vcpu: int, apiKey: string) =
  if list:
    let cmd = fmt"""curl -s -X GET '{API_BASE}/services' -H 'Authorization: Bearer {apiKey}'"""
    echo execCurl(cmd)
    return

  if info != "":
    let cmd = fmt"""curl -s -X GET '{API_BASE}/services/{info}' -H 'Authorization: Bearer {apiKey}'"""
    echo execCurl(cmd)
    return

  if logs != "":
    let cmd = fmt"""curl -s -X GET '{API_BASE}/services/{logs}/logs' -H 'Authorization: Bearer {apiKey}'"""
    stdout.write(execCurl(cmd))
    return

  if tail != "":
    let cmd = fmt"""curl -s -X GET '{API_BASE}/services/{tail}/logs?lines=9000' -H 'Authorization: Bearer {apiKey}'"""
    stdout.write(execCurl(cmd))
    return

  if sleep != "":
    let cmd = fmt"""curl -s -X POST '{API_BASE}/services/{sleep}/sleep' -H 'Authorization: Bearer {apiKey}'"""
    discard execCurl(cmd)
    echo GREEN & "Service sleeping: " & sleep & RESET
    return

  if wake != "":
    let cmd = fmt"""curl -s -X POST '{API_BASE}/services/{wake}/wake' -H 'Authorization: Bearer {apiKey}'"""
    discard execCurl(cmd)
    echo GREEN & "Service waking: " & wake & RESET
    return

  if destroy != "":
    let cmd = fmt"""curl -s -X DELETE '{API_BASE}/services/{destroy}' -H 'Authorization: Bearer {apiKey}'"""
    discard execCurl(cmd)
    echo GREEN & "Service destroyed: " & destroy & RESET
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
    if network != "": json.add(fmt""","network":"{network}"""")
    if vcpu > 0: json.add(fmt""","vcpu":{vcpu}""")
    json.add("}")

    echo YELLOW & "Creating service..." & RESET
    let cmd = fmt"""curl -s -X POST '{API_BASE}/services' -H 'Content-Type: application/json' -H 'Authorization: Bearer {apiKey}' -d '{json}'"""
    echo execCurl(cmd)
    return

  stderr.writeLine(RED & "Error: Specify --name to create a service" & RESET)
  quit(1)

proc main() =
  var apiKey = getEnv("UNSANDBOX_API_KEY", "")
  let args = commandLineParams()

  if args.len < 1:
    stderr.writeLine("Usage: un.nim [options] <source_file>")
    stderr.writeLine("       un.nim session [options]")
    stderr.writeLine("       un.nim service [options]")
    quit(1)

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
      of "-k": apiKey = args[i+1]; inc i
      inc i
    cmdSession(list, kill, shell, network, vcpu, tmux, screen, apiKey)
    return

  if args[0] == "service":
    var name, ports, bootstrap = ""
    var list = false
    var info, logs, tail, sleep, wake, destroy, network = ""
    var vcpu = 0
    var i = 1
    while i < args.len:
      case args[i]
      of "--name": name = args[i+1]; inc i
      of "--ports": ports = args[i+1]; inc i
      of "--bootstrap": bootstrap = args[i+1]; inc i
      of "--list": list = true
      of "--info": info = args[i+1]; inc i
      of "--logs": logs = args[i+1]; inc i
      of "--tail": tail = args[i+1]; inc i
      of "--sleep": sleep = args[i+1]; inc i
      of "--wake": wake = args[i+1]; inc i
      of "--destroy": destroy = args[i+1]; inc i
      of "-n": network = args[i+1]; inc i
      of "-v": vcpu = parseInt(args[i+1]); inc i
      of "-k": apiKey = args[i+1]; inc i
      inc i
    cmdService(name, ports, bootstrap, list, info, logs, tail, sleep, wake, destroy, network, vcpu, apiKey)
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
    of "-k": apiKey = args[i+1]; inc i
    else:
      if not args[i].startsWith("-"):
        sourceFile = args[i]
    inc i

  if sourceFile == "":
    stderr.writeLine(RED & "Error: No source file specified" & RESET)
    quit(1)

  cmdExecute(sourceFile, envs, artifacts, network, vcpu, apiKey)

when isMainModule:
  main()
