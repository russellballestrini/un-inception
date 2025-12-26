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

#!/usr/bin/env pwsh
# un.ps1 - Unsandbox CLI Client (PowerShell Implementation)
#
# Usage:
#   pwsh un.ps1 [options] <source_file>
#   pwsh un.ps1 session [options]
#   pwsh un.ps1 service [options]

$API_BASE = "https://api.unsandbox.com"

$EXT_MAP = @{
    ".ps1" = "powershell"; ".py" = "python"; ".js" = "javascript"
    ".ts" = "typescript"; ".rb" = "ruby"; ".php" = "php"; ".pl" = "perl"
    ".lua" = "lua"; ".sh" = "bash"; ".go" = "go"; ".rs" = "rust"
    ".c" = "c"; ".cpp" = "cpp"; ".java" = "java"; ".kt" = "kotlin"
    ".cs" = "csharp"; ".fs" = "fsharp"; ".hs" = "haskell"; ".ml" = "ocaml"
    ".clj" = "clojure"; ".scm" = "scheme"; ".lisp" = "commonlisp"
    ".erl" = "erlang"; ".ex" = "elixir"; ".jl" = "julia"; ".r" = "r"
    ".cr" = "crystal"; ".d" = "d"; ".nim" = "nim"; ".zig" = "zig"
    ".v" = "v"; ".dart" = "dart"; ".groovy" = "groovy"; ".f90" = "fortran"
    ".cob" = "cobol"; ".pro" = "prolog"; ".forth" = "forth"; ".tcl" = "tcl"
    ".raku" = "raku"; ".m" = "objc"; ".awk" = "awk"
}

function Get-ApiKey {
    $key = $env:UNSANDBOX_API_KEY
    if (-not $key) {
        Write-Error "Error: UNSANDBOX_API_KEY not set"
        exit 1
    }
    return $key
}

function Invoke-Api {
    param($Endpoint, $Method = "GET", $Body = $null)

    $apiKey = Get-ApiKey
    $headers = @{
        "Authorization" = "Bearer $apiKey"
        "Content-Type" = "application/json"
    }

    $uri = "$API_BASE$Endpoint"

    try {
        if ($Body) {
            $response = Invoke-RestMethod -Uri $uri -Method $Method -Headers $headers -Body $Body
        } else {
            $response = Invoke-RestMethod -Uri $uri -Method $Method -Headers $headers
        }
        return $response
    } catch {
        Write-Error "Error: $($_.Exception.Message)"
        exit 1
    }
}

function Invoke-Execute {
    param($SourceFile, $EnvVars = @{}, $Network = $null)

    if (-not (Test-Path $SourceFile)) {
        Write-Error "Error: File not found: $SourceFile"
        exit 1
    }

    $ext = [System.IO.Path]::GetExtension($SourceFile).ToLower()
    $language = $EXT_MAP[$ext]

    if (-not $language) {
        Write-Error "Error: Unknown extension: $ext"
        exit 1
    }

    $code = Get-Content -Raw $SourceFile

    $payload = @{
        language = $language
        code = $code
    }

    if ($EnvVars.Count -gt 0) {
        $payload["env"] = $EnvVars
    }
    if ($Network) {
        $payload["network"] = $Network
    }

    $body = $payload | ConvertTo-Json -Depth 10
    $result = Invoke-Api -Endpoint "/execute" -Method "POST" -Body $body

    if ($result.stdout) {
        Write-Host "`e[34m$($result.stdout)`e[0m" -NoNewline
    }
    if ($result.stderr) {
        Write-Host "`e[31m$($result.stderr)`e[0m" -NoNewline
    }

    exit $result.exit_code
}

function Invoke-Session {
    param($Args)

    if ($Args -contains "--list" -or $Args -contains "-l") {
        $result = Invoke-Api -Endpoint "/sessions"
        $result | ConvertTo-Json -Depth 5
        return
    }

    if ($Args -contains "--kill") {
        $idx = [array]::IndexOf($Args, "--kill")
        $sessionId = $Args[$idx + 1]
        Invoke-Api -Endpoint "/sessions/$sessionId" -Method "DELETE"
        Write-Host "`e[32mSession terminated: $sessionId`e[0m"
        return
    }

    # Create session
    $shell = "bash"
    if ($Args -contains "--shell" -or $Args -contains "-s") {
        $idx = if ($Args -contains "--shell") { [array]::IndexOf($Args, "--shell") } else { [array]::IndexOf($Args, "-s") }
        $shell = $Args[$idx + 1]
    }

    $payload = @{ shell = $shell } | ConvertTo-Json
    $result = Invoke-Api -Endpoint "/sessions" -Method "POST" -Body $payload
    Write-Host "`e[33mSession created (WebSocket required for interactive)`e[0m"
    $result | ConvertTo-Json -Depth 5
}

function Invoke-Service {
    param($Args)

    if ($Args -contains "--list" -or $Args -contains "-l") {
        $result = Invoke-Api -Endpoint "/services"
        $result | ConvertTo-Json -Depth 5
        return
    }

    if ($Args -contains "--info") {
        $idx = [array]::IndexOf($Args, "--info")
        $serviceId = $Args[$idx + 1]
        $result = Invoke-Api -Endpoint "/services/$serviceId"
        $result | ConvertTo-Json -Depth 5
        return
    }

    if ($Args -contains "--logs") {
        $idx = [array]::IndexOf($Args, "--logs")
        $serviceId = $Args[$idx + 1]
        $result = Invoke-Api -Endpoint "/services/$serviceId/logs"
        Write-Host $result.logs
        return
    }

    if ($Args -contains "--sleep") {
        $idx = [array]::IndexOf($Args, "--sleep")
        $serviceId = $Args[$idx + 1]
        Invoke-Api -Endpoint "/services/$serviceId/sleep" -Method "POST" -Body "{}"
        Write-Host "`e[32mService sleeping: $serviceId`e[0m"
        return
    }

    if ($Args -contains "--wake") {
        $idx = [array]::IndexOf($Args, "--wake")
        $serviceId = $Args[$idx + 1]
        Invoke-Api -Endpoint "/services/$serviceId/wake" -Method "POST" -Body "{}"
        Write-Host "`e[32mService waking: $serviceId`e[0m"
        return
    }

    if ($Args -contains "--destroy") {
        $idx = [array]::IndexOf($Args, "--destroy")
        $serviceId = $Args[$idx + 1]
        Invoke-Api -Endpoint "/services/$serviceId" -Method "DELETE"
        Write-Host "`e[32mService destroyed: $serviceId`e[0m"
        return
    }

    # Create service
    if ($Args -contains "--name") {
        $idx = [array]::IndexOf($Args, "--name")
        $name = $Args[$idx + 1]

        $payload = @{ name = $name }

        if ($Args -contains "--ports") {
            $pIdx = [array]::IndexOf($Args, "--ports")
            $ports = $Args[$pIdx + 1] -split "," | ForEach-Object { [int]$_ }
            $payload["ports"] = $ports
        }

        if ($Args -contains "--bootstrap") {
            $bIdx = [array]::IndexOf($Args, "--bootstrap")
            $payload["bootstrap"] = $Args[$bIdx + 1]
        }

        if ($Args -contains "--type") {
            $tIdx = [array]::IndexOf($Args, "--type")
            $payload["service_type"] = $Args[$tIdx + 1]
        }

        $body = $payload | ConvertTo-Json -Depth 5
        $result = Invoke-Api -Endpoint "/services" -Method "POST" -Body $body
        Write-Host "`e[32mService created`e[0m"
        $result | ConvertTo-Json -Depth 5
        return
    }

    Write-Error "Error: Specify --name to create or use --list, --info, etc."
    exit 1
}

# Main
if ($args.Count -eq 0 -or $args[0] -eq "--help" -or $args[0] -eq "-h") {
    Write-Host @"
Usage: pwsh un.ps1 [options] <source_file>
       pwsh un.ps1 session [options]
       pwsh un.ps1 service [options]

Execute options:
  -e KEY=VALUE    Environment variable
  -n MODE         Network mode (zerotrust|semitrusted)

Session options:
  --list, -l      List sessions
  --kill ID       Terminate session
  --shell NAME    Shell/REPL to use

Service options:
  --name NAME     Service name
  --ports PORTS   Comma-separated ports
  --type TYPE     Service type (minecraft, mumble, teamspeak, source, tcp, udp)
  --bootstrap CMD Bootstrap command
  --list, -l      List services
  --info ID       Get service info
  --logs ID       Get logs
  --sleep ID      Freeze service
  --wake ID       Unfreeze service
  --destroy ID    Destroy service
"@
    exit 0
}

if ($args[0] -eq "session") {
    Invoke-Session -Args $args[1..($args.Count-1)]
} elseif ($args[0] -eq "service") {
    Invoke-Service -Args $args[1..($args.Count-1)]
} else {
    # Parse execute args
    $sourceFile = $null
    $envVars = @{}
    $network = $null

    for ($i = 0; $i -lt $args.Count; $i++) {
        switch ($args[$i]) {
            "-e" {
                $kv = $args[$i+1] -split "=", 2
                $envVars[$kv[0]] = $kv[1]
                $i++
            }
            "-n" { $network = $args[$i+1]; $i++ }
            default {
                if (-not $args[$i].StartsWith("-")) {
                    $sourceFile = $args[$i]
                }
            }
        }
    }

    if (-not $sourceFile) {
        Write-Error "Error: No source file specified"
        exit 1
    }

    Invoke-Execute -SourceFile $sourceFile -EnvVars $envVars -Network $network
}
