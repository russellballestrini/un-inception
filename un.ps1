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
$PORTAL_BASE = "https://unsandbox.com"

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

function Get-ApiKeys {
    $publicKey = $env:UNSANDBOX_PUBLIC_KEY
    $secretKey = $env:UNSANDBOX_SECRET_KEY

    # Fallback to old UNSANDBOX_API_KEY for backwards compat
    if (-not $publicKey -and $env:UNSANDBOX_API_KEY) {
        $publicKey = $env:UNSANDBOX_API_KEY
        $secretKey = ""
    }

    if (-not $publicKey) {
        Write-Error "Error: UNSANDBOX_PUBLIC_KEY or UNSANDBOX_API_KEY not set"
        exit 1
    }
    return @($publicKey, $secretKey)
}

function Invoke-Api {
    param($Endpoint, $Method = "GET", $Body = $null, $BaseUrl = $null)

    $publicKey, $secretKey = Get-ApiKeys
    $headers = @{
        "Authorization" = "Bearer $publicKey"
        "Content-Type" = "application/json"
    }

    # Add HMAC signature if secret key exists
    if ($secretKey) {
        $timestamp = [int][double]::Parse((Get-Date -UFormat %s))
        $bodyContent = if ($Body) { $Body } else { "" }
        $sigInput = "${timestamp}:${Method}:${Endpoint}:${bodyContent}"

        $hmac = New-Object System.Security.Cryptography.HMACSHA256
        $hmac.Key = [System.Text.Encoding]::UTF8.GetBytes($secretKey)
        $hash = $hmac.ComputeHash([System.Text.Encoding]::UTF8.GetBytes($sigInput))
        $signature = [System.BitConverter]::ToString($hash).Replace("-", "").ToLower()

        $headers["X-Timestamp"] = $timestamp.ToString()
        $headers["X-Signature"] = $signature
    }

    $base = if ($BaseUrl) { $BaseUrl } else { $API_BASE }
    $uri = "$base$Endpoint"

    try {
        if ($Body) {
            $response = Invoke-RestMethod -Uri $uri -Method $Method -Headers $headers -Body $Body
        } else {
            $response = Invoke-RestMethod -Uri $uri -Method $Method -Headers $headers
        }
        return $response
    } catch {
        $errorMsg = $_.Exception.Message
        if ($errorMsg -match "401" -and $errorMsg -match "timestamp") {
            Write-Host "`e[31mError: Request timestamp expired (must be within 5 minutes of server time)`e[0m" -ForegroundColor Red
            Write-Host "`e[33mYour computer's clock may have drifted.`e[0m" -ForegroundColor Yellow
            Write-Host "Check your system time and sync with NTP if needed:"
            Write-Host "  Linux:   sudo ntpdate -s time.nist.gov"
            Write-Host "  macOS:   sudo sntp -sS time.apple.com"
            Write-Host "  Windows: w32tm /resync"
        } else {
            Write-Error "Error: $errorMsg"
        }
        exit 1
    }
}

function Invoke-ApiText {
    param($Endpoint, $Method, $Body, $BaseUrl = $null)

    $publicKey, $secretKey = Get-ApiKeys
    $headers = @{
        "Authorization" = "Bearer $publicKey"
        "Content-Type" = "text/plain"
    }

    # Add HMAC signature if secret key exists
    if ($secretKey) {
        $timestamp = [int][double]::Parse((Get-Date -UFormat %s))
        $bodyContent = if ($Body) { $Body } else { "" }
        $sigInput = "${timestamp}:${Method}:${Endpoint}:${bodyContent}"

        $hmac = New-Object System.Security.Cryptography.HMACSHA256
        $hmac.Key = [System.Text.Encoding]::UTF8.GetBytes($secretKey)
        $hash = $hmac.ComputeHash([System.Text.Encoding]::UTF8.GetBytes($sigInput))
        $signature = [System.BitConverter]::ToString($hash).Replace("-", "").ToLower()

        $headers["X-Timestamp"] = $timestamp.ToString()
        $headers["X-Signature"] = $signature
    }

    $base = if ($BaseUrl) { $BaseUrl } else { $API_BASE }
    $uri = "$base$Endpoint"

    try {
        $response = Invoke-RestMethod -Uri $uri -Method $Method -Headers $headers -Body $Body
        return @{ Success = $true; Data = $response }
    } catch {
        return @{ Success = $false; Error = $_.Exception.Message }
    }
}

function Read-EnvFile {
    param($Path)

    if (-not (Test-Path $Path)) {
        Write-Error "Error: Env file not found: $Path"
        exit 1
    }
    return Get-Content -Raw $Path
}

function Build-EnvContent {
    param($Envs, $EnvFile)

    $lines = @()

    # Add from -e flags
    foreach ($env in $Envs) {
        $lines += $env
    }

    # Add from --env-file
    if ($EnvFile) {
        $content = Read-EnvFile -Path $EnvFile
        foreach ($line in ($content -split "`n")) {
            $trimmed = $line.Trim()
            if ($trimmed -and -not $trimmed.StartsWith("#")) {
                $lines += $trimmed
            }
        }
    }

    return $lines -join "`n"
}

$MAX_ENV_CONTENT_SIZE = 65536

function Invoke-ServiceEnvStatus {
    param($ServiceId)

    return Invoke-Api -Endpoint "/services/$ServiceId/env"
}

function Invoke-ServiceEnvSet {
    param($ServiceId, $EnvContent)

    if ($EnvContent.Length -gt $MAX_ENV_CONTENT_SIZE) {
        Write-Host "`e[31mError: Env content exceeds maximum size of 64KB`e[0m"
        return $false
    }

    $result = Invoke-ApiText -Endpoint "/services/$ServiceId/env" -Method "PUT" -Body $EnvContent
    return $result.Success
}

function Invoke-ServiceEnvExport {
    param($ServiceId)

    return Invoke-Api -Endpoint "/services/$ServiceId/env/export" -Method "POST" -Body "{}"
}

function Invoke-ServiceEnvDelete {
    param($ServiceId)

    try {
        Invoke-Api -Endpoint "/services/$ServiceId/env" -Method "DELETE"
        return $true
    } catch {
        return $false
    }
}

function Invoke-ServiceEnv {
    param($Action, $Target, $Envs, $EnvFile)

    switch ($Action) {
        "status" {
            if (-not $Target) {
                Write-Error "Error: service env status requires service ID"
                exit 1
            }
            $result = Invoke-ServiceEnvStatus -ServiceId $Target
            if ($result.has_vault) {
                Write-Host "`e[32mVault: configured`e[0m"
                if ($result.env_count) {
                    Write-Host "Variables: $($result.env_count)"
                }
                if ($result.updated_at) {
                    Write-Host "Updated: $($result.updated_at)"
                }
            } else {
                Write-Host "`e[33mVault: not configured`e[0m"
            }
        }
        "set" {
            if (-not $Target) {
                Write-Error "Error: service env set requires service ID"
                exit 1
            }
            if ($Envs.Count -eq 0 -and -not $EnvFile) {
                Write-Error "Error: service env set requires -e or --env-file"
                exit 1
            }
            $envContent = Build-EnvContent -Envs $Envs -EnvFile $EnvFile
            if (Invoke-ServiceEnvSet -ServiceId $Target -EnvContent $envContent) {
                Write-Host "`e[32mVault updated for service $Target`e[0m"
            } else {
                Write-Error "Error: Failed to update vault"
                exit 1
            }
        }
        "export" {
            if (-not $Target) {
                Write-Error "Error: service env export requires service ID"
                exit 1
            }
            $result = Invoke-ServiceEnvExport -ServiceId $Target
            if ($result.content) {
                Write-Host $result.content -NoNewline
            }
        }
        "delete" {
            if (-not $Target) {
                Write-Error "Error: service env delete requires service ID"
                exit 1
            }
            if (Invoke-ServiceEnvDelete -ServiceId $Target) {
                Write-Host "`e[32mVault deleted for service $Target`e[0m"
            } else {
                Write-Error "Error: Failed to delete vault"
                exit 1
            }
        }
        default {
            Write-Error "Error: Unknown env action: $Action"
            Write-Host "Usage: pwsh un.ps1 service env <status|set|export|delete> <service_id>"
            exit 1
        }
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

    # Parse input files
    $inputFiles = @()
    for ($i = 0; $i -lt $Args.Count; $i++) {
        if ($Args[$i] -eq "-f" -and ($i + 1) -lt $Args.Count) {
            $filepath = $Args[$i + 1]
            if (-not (Test-Path $filepath)) {
                Write-Error "Error: Input file not found: $filepath"
                exit 1
            }
            $content = [System.IO.File]::ReadAllBytes($filepath)
            $b64Content = [Convert]::ToBase64String($content)
            $inputFiles += @{
                filename = [System.IO.Path]::GetFileName($filepath)
                content_base64 = $b64Content
            }
            $i++
        }
    }

    $payload = @{ shell = $shell }
    if ($inputFiles.Count -gt 0) {
        $payload["input_files"] = $inputFiles
    }

    $body = $payload | ConvertTo-Json -Depth 10
    $result = Invoke-Api -Endpoint "/sessions" -Method "POST" -Body $body
    Write-Host "`e[33mSession created (WebSocket required for interactive)`e[0m"
    $result | ConvertTo-Json -Depth 5
}

function Invoke-Key {
    param($Args)

    $extend = $Args -contains "--extend"

    try {
        $result = Invoke-Api -Endpoint "/keys/validate" -Method "POST" -BaseUrl $PORTAL_BASE

        # Handle --extend flag
        if ($extend) {
            $publicKey = $result.public_key
            if ($publicKey) {
                $url = "$PORTAL_BASE/keys/extend?pk=$publicKey"
                Write-Host "`e[34mOpening browser to extend key...`e[0m"
                if ($IsWindows) {
                    Start-Process $url
                } elseif ($IsMacOS) {
                    & open $url
                } elseif ($IsLinux) {
                    & xdg-open $url
                } else {
                    Write-Host "`e[33mPlease open manually: $url`e[0m"
                }
                return
            } else {
                Write-Error "Error: Could not retrieve public key"
                exit 1
            }
        }

        # Check if key is expired
        if ($result.expired) {
            Write-Host "`e[31mExpired`e[0m"
            Write-Host "Public Key: $($result.public_key ?? 'N/A')"
            Write-Host "Tier: $($result.tier ?? 'N/A')"
            Write-Host "Expired: $($result.expires_at ?? 'N/A')"
            Write-Host "`e[33mTo renew: Visit $PORTAL_BASE/keys/extend`e[0m"
            exit 1
        }

        # Valid key
        Write-Host "`e[32mValid`e[0m"
        Write-Host "Public Key: $($result.public_key ?? 'N/A')"
        Write-Host "Tier: $($result.tier ?? 'N/A')"
        Write-Host "Status: $($result.status ?? 'N/A')"
        Write-Host "Expires: $($result.expires_at ?? 'N/A')"
        Write-Host "Time Remaining: $($result.time_remaining ?? 'N/A')"
        Write-Host "Rate Limit: $($result.rate_limit ?? 'N/A')"
        Write-Host "Burst: $($result.burst ?? 'N/A')"
        Write-Host "Concurrency: $($result.concurrency ?? 'N/A')"
    } catch {
        Write-Host "`e[31mInvalid`e[0m"
        Write-Host "Reason: $($_.Exception.Message)"
        exit 1
    }
}

function Invoke-Service {
    param($Args)

    # Parse env subcommand and -e/--env-file
    $envAction = $null
    $envTarget = $null
    $envs = @()
    $envFile = $null

    for ($i = 0; $i -lt $Args.Count; $i++) {
        if ($Args[$i] -eq "env" -and ($i + 1) -lt $Args.Count) {
            $next = $Args[$i + 1]
            if (-not $next.StartsWith("-")) {
                $envAction = $next
                $i++
                if (($i + 1) -lt $Args.Count) {
                    $next2 = $Args[$i + 1]
                    if (-not $next2.StartsWith("-")) {
                        $envTarget = $next2
                        $i++
                    }
                }
            }
        } elseif ($Args[$i] -eq "-e" -and ($i + 1) -lt $Args.Count) {
            $envs += $Args[$i + 1]
            $i++
        } elseif ($Args[$i] -eq "--env-file" -and ($i + 1) -lt $Args.Count) {
            $envFile = $Args[$i + 1]
            $i++
        }
    }

    # Handle env subcommand
    if ($envAction) {
        Invoke-ServiceEnv -Action $envAction -Target $envTarget -Envs $envs -EnvFile $envFile
        return
    }

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

    if ($Args -contains "--freeze") {
        $idx = [array]::IndexOf($Args, "--freeze")
        $serviceId = $Args[$idx + 1]
        Invoke-Api -Endpoint "/services/$serviceId/freeze" -Method "POST" -Body "{}"
        Write-Host "`e[32mService frozen: $serviceId`e[0m"
        return
    }

    if ($Args -contains "--unfreeze") {
        $idx = [array]::IndexOf($Args, "--unfreeze")
        $serviceId = $Args[$idx + 1]
        Invoke-Api -Endpoint "/services/$serviceId/unfreeze" -Method "POST" -Body "{}"
        Write-Host "`e[32mService unfreezing: $serviceId`e[0m"
        return
    }

    if ($Args -contains "--destroy") {
        $idx = [array]::IndexOf($Args, "--destroy")
        $serviceId = $Args[$idx + 1]
        Invoke-Api -Endpoint "/services/$serviceId" -Method "DELETE"
        Write-Host "`e[32mService destroyed: $serviceId`e[0m"
        return
    }

    if ($Args -contains "--resize") {
        $idx = [array]::IndexOf($Args, "--resize")
        $serviceId = $Args[$idx + 1]

        # Get vcpu value from --vcpu or -v
        $vcpuValue = 0
        if ($Args -contains "--vcpu") {
            $vIdx = [array]::IndexOf($Args, "--vcpu")
            $vcpuValue = [int]$Args[$vIdx + 1]
        } elseif ($Args -contains "-v") {
            $vIdx = [array]::IndexOf($Args, "-v")
            $vcpuValue = [int]$Args[$vIdx + 1]
        }

        if ($vcpuValue -le 0) {
            Write-Error "Error: --resize requires --vcpu or -v"
            exit 1
        }
        if ($vcpuValue -lt 1 -or $vcpuValue -gt 8) {
            Write-Error "Error: vCPU must be between 1 and 8"
            exit 1
        }

        $payload = @{ vcpu = $vcpuValue } | ConvertTo-Json
        Invoke-Api -Endpoint "/services/$serviceId" -Method "PATCH" -Body $payload
        $ram = $vcpuValue * 2
        Write-Host "`e[32mService resized to $vcpuValue vCPU, $ram GB RAM`e[0m"
        return
    }

    if ($Args -contains "--dump-bootstrap") {
        $idx = [array]::IndexOf($Args, "--dump-bootstrap")
        $serviceId = $Args[$idx + 1]
        Write-Host "Fetching bootstrap script from $serviceId..." -ForegroundColor Yellow

        $payload = @{ command = "cat /tmp/bootstrap.sh" } | ConvertTo-Json
        $result = Invoke-Api -Endpoint "/services/$serviceId/execute" -Method "POST" -Body $payload

        if ($result.stdout -and $result.stdout.Length -gt 0) {
            $bootstrap = $result.stdout
            if ($Args -contains "--dump-file") {
                $dumpIdx = [array]::IndexOf($Args, "--dump-file")
                $dumpFile = $Args[$dumpIdx + 1]
                # Write to file
                $bootstrap | Set-Content -Path $dumpFile -NoNewline
                if ($IsLinux -or $IsMacOS) {
                    & chmod 755 $dumpFile
                }
                Write-Host "Bootstrap saved to $dumpFile"
            } else {
                # Print to stdout
                Write-Host $bootstrap -NoNewline
            }
        } else {
            Write-Error "Error: Failed to fetch bootstrap (service not running or no bootstrap file)"
            exit 1
        }
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

        if ($Args -contains "--bootstrap-file") {
            $bfIdx = [array]::IndexOf($Args, "--bootstrap-file")
            $bootstrapFile = $Args[$bfIdx + 1]
            if (Test-Path $bootstrapFile) {
                $payload["bootstrap_content"] = Get-Content -Raw $bootstrapFile
            } else {
                Write-Error "Error: Bootstrap file not found: $bootstrapFile"
                exit 1
            }
        }

        if ($Args -contains "--type") {
            $tIdx = [array]::IndexOf($Args, "--type")
            $payload["service_type"] = $Args[$tIdx + 1]
        }

        # Parse input files
        $inputFiles = @()
        for ($i = 0; $i -lt $Args.Count; $i++) {
            if ($Args[$i] -eq "-f" -and ($i + 1) -lt $Args.Count) {
                $filepath = $Args[$i + 1]
                if (-not (Test-Path $filepath)) {
                    Write-Error "Error: Input file not found: $filepath"
                    exit 1
                }
                $content = [System.IO.File]::ReadAllBytes($filepath)
                $b64Content = [Convert]::ToBase64String($content)
                $inputFiles += @{
                    filename = [System.IO.Path]::GetFileName($filepath)
                    content_base64 = $b64Content
                }
                $i++
            }
        }
        if ($inputFiles.Count -gt 0) {
            $payload["input_files"] = $inputFiles
        }

        $body = $payload | ConvertTo-Json -Depth 10
        $result = Invoke-Api -Endpoint "/services" -Method "POST" -Body $body
        $serviceId = $result.id
        Write-Host "`e[32mService created: $serviceId`e[0m"
        $result | ConvertTo-Json -Depth 5

        # Auto-set vault if env vars were provided
        if ($envs.Count -gt 0 -or $envFile) {
            $envContent = Build-EnvContent -Envs $envs -EnvFile $envFile
            if ($envContent) {
                if (Invoke-ServiceEnvSet -ServiceId $serviceId -EnvContent $envContent) {
                    Write-Host "`e[32mVault configured with environment variables`e[0m"
                } else {
                    Write-Host "`e[33mWarning: Failed to set vault`e[0m"
                }
            }
        }
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
       pwsh un.ps1 key [options]

Execute options:
  -e KEY=VALUE    Environment variable
  -n MODE         Network mode (zerotrust|semitrusted)

Session options:
  --list, -l      List sessions
  --kill ID       Terminate session
  --shell NAME    Shell/REPL to use
  -f FILE         Input file (can be repeated)

Service options:
  --name NAME          Service name
  --ports PORTS        Comma-separated ports
  --type TYPE          Service type (minecraft, mumble, teamspeak, source, tcp, udp)
  --bootstrap CMD      Bootstrap command
  -f FILE              Input file (can be repeated)
  -e KEY=VALUE         Environment variable for vault (can be repeated)
  --env-file FILE      Load vault variables from file
  --list, -l           List services
  --info ID            Get service info
  --logs ID            Get logs
  --freeze ID          Freeze service
  --unfreeze ID        Unfreeze service
  --destroy ID         Destroy service
  --resize ID          Resize service (requires --vcpu or -v)
  --dump-bootstrap ID  Dump bootstrap script from service
  --dump-file FILE     Save bootstrap to file (with --dump-bootstrap)

Service env commands:
  env status ID        Show vault status
  env set ID           Set vault (-e KEY=VALUE or --env-file FILE)
  env export ID        Export vault contents
  env delete ID        Delete vault

Key options:
  --extend        Open browser to extend key
"@
    exit 0
}

if ($args[0] -eq "session") {
    Invoke-Session -Args $args[1..($args.Count-1)]
} elseif ($args[0] -eq "service") {
    Invoke-Service -Args $args[1..($args.Count-1)]
} elseif ($args[0] -eq "key") {
    Invoke-Key -Args $args[1..($args.Count-1)]
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
                if ($args[$i].StartsWith("-")) {
                    Write-Error "${RED}Unknown option: $($args[$i])${RESET}"
                    exit 1
                } else {
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
