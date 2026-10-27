#!/usr/bin/env pwsh
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

$API_BASE = $(if ($env:UNSANDBOX_API_URL) { $env:UNSANDBOX_API_URL } else { "https://api.unsandbox.com" }).TrimEnd("/")
$PORTAL_BASE = "https://unsandbox.com"
$LANGUAGES_CACHE_TTL = 3600  # 1 hour in seconds

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

$script:AccountIndex = $null
# Target OS from the global --os flag: $null = API default (linux)
$script:TargetOs = $null

# Normalize a target OS name, case-insensitive: "linux" or "freebsd"; throws otherwise
function ConvertTo-TargetOs {
    param($Value)
    $os = "$Value".ToLower()
    if ($os -eq "linux" -or $os -eq "freebsd") { return $os }
    throw "invalid os '$Value' (expected linux or freebsd)"
}

# Add "os" to a request payload when a target OS is given
function Add-TargetOs {
    param($Payload, $TargetOs = $script:TargetOs)
    if ($null -ne $TargetOs) { $Payload["os"] = ConvertTo-TargetOs $TargetOs }
    return $Payload
}

function Load-AccountsCSV {
    param($Path, $Index)
    if (-not (Test-Path $Path)) { return $null }
    try {
        $rows = @()
        Get-Content $Path | Where-Object {
            $_.Trim() -ne "" -and -not $_.TrimStart().StartsWith("#")
        } | ForEach-Object {
            $parts = $_ -split ","
            if ($parts.Count -ge 2) {
                $rows += ,@($parts[0].Trim(), $parts[1].Trim())
            }
        }
        if ($Index -lt $rows.Count) {
            return $rows[$Index]
        }
    } catch {}
    return $null
}

function Get-ApiKeys {
    $homeDir = $env:HOME
    if (-not $homeDir) { $homeDir = $env:USERPROFILE }

    # --account N: load row N from accounts.csv, bypasses env vars
    if ($null -ne $script:AccountIndex) {
        $result = Load-AccountsCSV -Path "$homeDir/.unsandbox/accounts.csv" -Index $script:AccountIndex
        if (-not $result) {
            $result = Load-AccountsCSV -Path "./accounts.csv" -Index $script:AccountIndex
        }
        if ($result) { return $result }
        Write-Error "Error: account $($script:AccountIndex) not found in accounts.csv"
        exit 1
    }

    $publicKey = $env:UNSANDBOX_PUBLIC_KEY
    $secretKey = $env:UNSANDBOX_SECRET_KEY

    # Fallback to old UNSANDBOX_API_KEY for backwards compat
    if (-not $publicKey -and $env:UNSANDBOX_API_KEY) {
        $publicKey = $env:UNSANDBOX_API_KEY
        $secretKey = ""
    }

    if ($publicKey -and $secretKey) {
        return @($publicKey, $secretKey)
    }

    if ($publicKey) {
        return @($publicKey, "")
    }

    # accounts.csv fallback (row 0 or UNSANDBOX_ACCOUNT env var)
    $rowIdx = 0
    if ($env:UNSANDBOX_ACCOUNT) {
        try { $rowIdx = [int]$env:UNSANDBOX_ACCOUNT } catch {}
    }
    $result = Load-AccountsCSV -Path "$homeDir/.unsandbox/accounts.csv" -Index $rowIdx
    if (-not $result) {
        $result = Load-AccountsCSV -Path "./accounts.csv" -Index $rowIdx
    }
    if ($result) { return $result }

    Write-Error "Error: UNSANDBOX_PUBLIC_KEY or UNSANDBOX_API_KEY not set"
    exit 1
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

# Internal API request function that returns status code info for sudo handling
function Invoke-ApiInternal {
    param($Endpoint, $Method = "GET", $Body = $null, $SudoOtp = $null, $SudoChallengeId = $null)

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

    # Add sudo OTP headers if provided
    if ($SudoOtp) {
        $headers["X-Sudo-OTP"] = $SudoOtp
    }
    if ($SudoChallengeId) {
        $headers["X-Sudo-Challenge"] = $SudoChallengeId
    }

    $uri = "$API_BASE$Endpoint"

    try {
        if ($Body) {
            $response = Invoke-RestMethod -Uri $uri -Method $Method -Headers $headers -Body $Body
        } else {
            $response = Invoke-RestMethod -Uri $uri -Method $Method -Headers $headers
        }
        return @{ Success = $true; Response = $response; StatusCode = 200 }
    } catch {
        $statusCode = 0
        $responseBody = ""

        if ($_.Exception.Response) {
            $statusCode = [int]$_.Exception.Response.StatusCode
            $stream = $_.Exception.Response.GetResponseStream()
            $reader = New-Object System.IO.StreamReader($stream)
            $responseBody = $reader.ReadToEnd()
        }

        return @{ Success = $false; StatusCode = $statusCode; ResponseBody = $responseBody; Error = $_.Exception.Message }
    }
}

# Handle 428 sudo OTP challenge - prompts user for OTP and retries the request
function Invoke-SudoChallenge {
    param($ResponseBody, $Endpoint, $Method, $Body)

    # Extract challenge_id from response
    $challengeId = $null
    try {
        $parsed = $ResponseBody | ConvertFrom-Json
        $challengeId = $parsed.challenge_id
    } catch {}

    Write-Host "`e[33mConfirmation required. Check your email for a one-time code.`e[0m" -ForegroundColor Yellow
    $otp = Read-Host "Enter OTP"

    if (-not $otp) {
        Write-Error "Operation cancelled"
        exit 1
    }

    $otp = $otp.Trim()

    # Retry the request with sudo headers
    return Invoke-ApiInternal -Endpoint $Endpoint -Method $Method -Body $Body -SudoOtp $otp -SudoChallengeId $challengeId
}

# Wrapper for destructive operations that may require 428 sudo OTP
function Invoke-ApiWithSudo {
    param($Endpoint, $Method = "GET", $Body = $null)

    $result = Invoke-ApiInternal -Endpoint $Endpoint -Method $Method -Body $Body

    if (-not $result.Success -and $result.StatusCode -eq 428) {
        $retryResult = Invoke-SudoChallenge -ResponseBody $result.ResponseBody -Endpoint $Endpoint -Method $Method -Body $Body
        if (-not $retryResult.Success) {
            Write-Error "Error: $($retryResult.Error)"
            exit 1
        }
        return $retryResult.Response
    }

    if (-not $result.Success) {
        Write-Error "Error: $($result.Error)"
        exit 1
    }

    return $result.Response
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
    param($SourceFile, $EnvVars = @{}, $Network = $null, $Language = $null, $TargetOs = $script:TargetOs)

    if ($Language) {
        # -s LANGUAGE CODE: SourceFile holds the inline code
        $language = $Language
        $code = $SourceFile
    } else {
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
    }

    $payload = Add-TargetOs -Payload @{
        language = $language
        code = $code
    } -TargetOs $TargetOs

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
    param($CmdArgs)

    if ($CmdArgs -contains "--list" -or $CmdArgs -contains "-l") {
        $result = Invoke-Api -Endpoint "/sessions"
        $result | ConvertTo-Json -Depth 5
        return
    }

    if ($CmdArgs -contains "--kill") {
        $idx = [array]::IndexOf($CmdArgs, "--kill")
        $sessionId = $CmdArgs[$idx + 1]
        Invoke-Api -Endpoint "/sessions/$sessionId" -Method "DELETE"
        Write-Host "`e[32mSession terminated: $sessionId`e[0m"
        return
    }

    # Create session
    $shell = "bash"
    if ($CmdArgs -contains "--shell" -or $CmdArgs -contains "-s") {
        $idx = if ($CmdArgs -contains "--shell") { [array]::IndexOf($CmdArgs, "--shell") } else { [array]::IndexOf($CmdArgs, "-s") }
        $shell = $CmdArgs[$idx + 1]
    }

    # Parse input files
    $inputFiles = @()
    for ($i = 0; $i -lt $CmdArgs.Count; $i++) {
        if ($CmdArgs[$i] -eq "-f" -and ($i + 1) -lt $CmdArgs.Count) {
            $filepath = $CmdArgs[$i + 1]
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

    $payload = Add-TargetOs -Payload @{ shell = $shell }
    if ($inputFiles.Count -gt 0) {
        $payload["input_files"] = $inputFiles
    }

    $body = $payload | ConvertTo-Json -Depth 10
    $result = Invoke-Api -Endpoint "/sessions" -Method "POST" -Body $body
    Write-Host "`e[33mSession created (WebSocket required for interactive)`e[0m"
    $result | ConvertTo-Json -Depth 5
}

function Get-LanguagesCachePath {
    $cacheDir = Join-Path $env:HOME ".unsandbox"
    if (-not (Test-Path $cacheDir)) {
        New-Item -ItemType Directory -Path $cacheDir -Force | Out-Null
    }
    return Join-Path $cacheDir "languages.json"
}

function Get-CachedLanguages {
    $cachePath = Get-LanguagesCachePath
    if (-not (Test-Path $cachePath)) {
        return $null
    }

    try {
        $cacheContent = Get-Content -Raw $cachePath | ConvertFrom-Json
        $currentTime = [int][double]::Parse((Get-Date -UFormat %s))

        if ($cacheContent.timestamp -and ($currentTime - $cacheContent.timestamp) -lt $LANGUAGES_CACHE_TTL) {
            return $cacheContent.languages
        }
    } catch {
        # Cache is invalid, return null
    }
    return $null
}

function Save-LanguagesCache {
    param($Languages)

    $cachePath = Get-LanguagesCachePath
    $timestamp = [int][double]::Parse((Get-Date -UFormat %s))

    $cacheData = @{
        languages = $Languages
        timestamp = $timestamp
    }

    try {
        $cacheData | ConvertTo-Json -Compress | Set-Content -Path $cachePath
    } catch {
        # Silently fail if we can't write cache
    }
}

function Invoke-Languages {
    param($CmdArgs)

    $jsonOutput = $CmdArgs -contains "--json"

    # Check cache first
    $languages = Get-CachedLanguages

    if (-not $languages) {
        # Fetch from API
        $result = Invoke-Api -Endpoint "/languages"
        $languages = $result.languages

        # Save to cache
        if ($languages) {
            Save-LanguagesCache -Languages $languages
        }
    }

    if ($jsonOutput) {
        # Output as JSON array
        $languages | ConvertTo-Json -Compress
    } else {
        # Output one language per line
        foreach ($lang in $languages) {
            Write-Output $lang
        }
    }
}

# The accounts.csv the credentials came from, or $null (env vars). Mirrors Get-ApiKeys.
function Get-CredentialsCsvPath {
    $homeDir = $env:HOME
    if (-not $homeDir) { $homeDir = $env:USERPROFILE }
    $idx = $script:AccountIndex
    if ($null -eq $idx) {
        if ($env:UNSANDBOX_PUBLIC_KEY -and $env:UNSANDBOX_SECRET_KEY) { return $null }
        $idx = 0
        if ($env:UNSANDBOX_ACCOUNT) { try { $idx = [int]$env:UNSANDBOX_ACCOUNT } catch {} }
    }
    foreach ($p in @("$homeDir/.unsandbox/accounts.csv", "./accounts.csv")) {
        if (Load-AccountsCSV -Path $p -Index $idx) { return $p }
    }
    return $null
}

function Set-PrivateMode {
    param($Path, [switch]$Directory)
    if ($IsWindows) { return }
    $mode = if ($Directory) { "UserRead, UserWrite, UserExecute" } else { "UserRead, UserWrite" }
    [System.IO.File]::SetUnixFileMode($Path, [System.IO.UnixFileMode]$mode)
}

# Replace the secret (2nd field) on the line whose 1st field is the public key.
# Every other line, field, comment and the order stay as they are. Temp file (0600) + rename.
function Update-AccountsCsv {
    param($Path, $PublicKey, $NewSecret)
    $full = [System.IO.Path]::GetFullPath($Path)
    $lines = [System.IO.File]::ReadAllText($full) -split "`n"
    $changed = $false
    for ($i = 0; $i -lt $lines.Count; $i++) {
        $t = $lines[$i].Trim()
        if (-not $t -or $t.StartsWith("#")) { continue }
        $fields = $lines[$i] -split ","
        if ($fields.Count -ge 2 -and $fields[0].Trim() -eq $PublicKey) {
            $fields[1] = $NewSecret
            $lines[$i] = $fields -join ","
            $changed = $true
            break
        }
    }
    if (-not $changed) { return $false }
    $tmp = Join-Path ([System.IO.Path]::GetDirectoryName($full)) ".accounts.csv.$PID.tmp"
    [System.IO.File]::WriteAllText($tmp, "")
    Set-PrivateMode $tmp
    [System.IO.File]::WriteAllText($tmp, ($lines -join "`n"))
    [System.IO.File]::Move($tmp, $full, $true)
    return $true
}

# un key --rotate [--otp CODE]: rotate this key's secret (see CLI_SPEC.md)
function Invoke-KeyRotate {
    param($Otp)
    $publicKey, $secretKey = Get-ApiKeys
    $path = "/keys/self/rotate-secret"
    $body = "{}"
    $homeDir = $env:HOME
    if (-not $homeDir) { $homeDir = $env:USERPROFILE }
    $dir = "$homeDir/.unsandbox"
    $sha = [System.Security.Cryptography.SHA256]::Create()
    $pkHash = [System.BitConverter]::ToString($sha.ComputeHash([System.Text.Encoding]::UTF8.GetBytes($publicKey))).Replace("-", "").ToLower()
    $challengeFile = "$dir/rotate-$($pkHash.Substring(0, 16)).challenge"

    $post = {
        param($code, $challenge)
        $timestamp = [DateTimeOffset]::UtcNow.ToUnixTimeSeconds()
        $hmac = New-Object System.Security.Cryptography.HMACSHA256
        $hmac.Key = [System.Text.Encoding]::UTF8.GetBytes($secretKey)
        $hash = $hmac.ComputeHash([System.Text.Encoding]::UTF8.GetBytes("${timestamp}:POST:${path}:${body}"))
        $signature = [System.BitConverter]::ToString($hash).Replace("-", "").ToLower()
        $client = New-Object System.Net.Http.HttpClient
        $client.Timeout = [TimeSpan]::FromSeconds(120)
        $req = New-Object System.Net.Http.HttpRequestMessage([System.Net.Http.HttpMethod]::Post, "$API_BASE$path")
        $req.Content = New-Object System.Net.Http.StringContent($body, [System.Text.Encoding]::UTF8, "application/json")
        $req.Content.Headers.ContentType = [System.Net.Http.Headers.MediaTypeHeaderValue]::new("application/json")
        $req.Headers.TryAddWithoutValidation("Authorization", "Bearer $publicKey") | Out-Null
        $req.Headers.Add("X-Timestamp", "$timestamp")
        $req.Headers.Add("X-Signature", $signature)
        if ($code) {
            $req.Headers.Add("X-Sudo-OTP", $code)
            $req.Headers.Add("X-Sudo-Challenge", "$challenge")
        }
        try {
            $resp = $client.SendAsync($req).GetAwaiter().GetResult()
            $text = $resp.Content.ReadAsStringAsync().GetAwaiter().GetResult()
        } catch {
            [Console]::Error.WriteLine("Error: $($_.Exception.Message)")
            exit 4
        }
        $data = $null
        try { $data = $text | ConvertFrom-Json } catch {}
        return @{ Status = [int]$resp.StatusCode; Data = $data }
    }

    if ($Otp) {
        if (-not (Test-Path $challengeFile)) {
            [Console]::Error.WriteLine("Error: no rotation in progress; run: un key --rotate")
            exit 2
        }
        $challenge = ([System.IO.File]::ReadAllText($challengeFile)).Trim()
        $result = & $post $Otp.Trim() $challenge
        Remove-Item -Force -ErrorAction SilentlyContinue $challengeFile
    } else {
        $result = & $post $null $null
        if ($result.Status -eq 428) {
            $challenge = "$($result.Data.challenge_id)"
            if (-not [Console]::IsInputRedirected) {
                [Console]::Error.WriteLine("One-time code emailed to the key owner.")
                [Console]::Error.Write("Code: ")
                $code = [Console]::In.ReadLine()
                if (-not $code -or -not $code.Trim()) {
                    [Console]::Error.WriteLine("Error: cancelled")
                    exit 1
                }
                $result = & $post $code.Trim() $challenge
            } else {
                if (-not (Test-Path $dir)) {
                    New-Item -ItemType Directory -Path $dir -Force | Out-Null
                    Set-PrivateMode $dir -Directory
                }
                [System.IO.File]::WriteAllText($challengeFile, "")
                Set-PrivateMode $challengeFile
                [System.IO.File]::WriteAllText($challengeFile, $challenge)
                Write-Output "One-time code emailed to the key owner. Finish with: un key --rotate --otp CODE"
                exit 0
            }
        }
    }

    if ($result.Status -ne 200) {
        $err = "$($result.Data.error)"
        if ($result.Status -eq 403 -and $err -eq "email_required") {
            [Console]::Error.WriteLine("Error: this key has no email; add one or use POST /keys/recover")
            exit 3
        }
        if ($result.Status -eq 401) {
            [Console]::Error.WriteLine("Error: authentication failed")
            exit 3
        }
        $msg = if ($result.Data.message) { $result.Data.message } elseif ($err) { $err } else { "HTTP $($result.Status)" }
        [Console]::Error.WriteLine("Error: $msg")
        exit 4
    }
    $newSecret = "$($result.Data.secret)"
    if (-not $newSecret) {
        [Console]::Error.WriteLine("Error: API returned no secret")
        exit 4
    }

    $csvPath = Get-CredentialsCsvPath
    if ($csvPath -and (Update-AccountsCsv -Path $csvPath -PublicKey $publicKey -NewSecret $newSecret)) {
        Write-Output "Secret rotated. Updated $csvPath (public key $publicKey)."
    } else {
        Write-Output "New secret (shown once, store it now): $newSecret"
        Write-Output "Warning: UNSANDBOX_SECRET_KEY / -k still hold the old, now invalid, secret."
    }
    exit 0
}

function Invoke-Key {
    param($CmdArgs)

    $extend = $CmdArgs -contains "--extend"
    $publicKey, $secretKey = Get-ApiKeys

    if ($extend) {
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
    }

    # POST /keys/validate, signed in-process; read the status even on 401
    $path = "/keys/validate"
    $client = New-Object System.Net.Http.HttpClient
    $client.Timeout = [TimeSpan]::FromSeconds(30)
    $req = New-Object System.Net.Http.HttpRequestMessage([System.Net.Http.HttpMethod]::Post, "$API_BASE$path")
    $req.Content = New-Object System.Net.Http.StringContent("", [System.Text.Encoding]::UTF8, "application/json")
    $req.Content.Headers.ContentType = [System.Net.Http.Headers.MediaTypeHeaderValue]::new("application/json")
    $req.Headers.TryAddWithoutValidation("Authorization", "Bearer $publicKey") | Out-Null
    if ($secretKey) {
        $timestamp = [DateTimeOffset]::UtcNow.ToUnixTimeSeconds()
        $hmac = New-Object System.Security.Cryptography.HMACSHA256
        $hmac.Key = [System.Text.Encoding]::UTF8.GetBytes($secretKey)
        $hash = $hmac.ComputeHash([System.Text.Encoding]::UTF8.GetBytes("${timestamp}:POST:${path}:"))
        $req.Headers.Add("X-Timestamp", "$timestamp")
        $req.Headers.Add("X-Signature", [System.BitConverter]::ToString($hash).Replace("-", "").ToLower())
    }
    try {
        $resp = $client.SendAsync($req).GetAwaiter().GetResult()
        $text = $resp.Content.ReadAsStringAsync().GetAwaiter().GetResult()
        $code = [int]$resp.StatusCode
    } catch {
        [Console]::Error.WriteLine("Error: request to $API_BASE$path failed: $($_.Exception.Message)")
        exit 1
    }
    $result = $null
    try { $result = $text | ConvertFrom-Json } catch {}
    if ($null -eq $result -or $result -isnot [psobject]) { $result = [pscustomobject]@{} }

    if ($code -eq 200) {
        $state = if ($null -ne $result.status) { "$($result.status)" } elseif ($result.valid -eq $true) { "active" } else { "invalid" }
        $reason = "status $state"
    } elseif ($code -eq 401) {
        $reason = if ($null -ne $result.error) { "$($result.error)" } else { "unauthorized" }
        $state = if ($reason -in @(":expired", "expired")) { "expired" } elseif ($reason -in @(":suspended", "suspended")) { "suspended" } else { "invalid" }
    } else {
        $msg = if ($null -ne $result.error) { " $($result.error)" } elseif ($null -ne $result.message) { " $($result.message)" } else { "" }
        [Console]::Error.WriteLine("Error: HTTP $code$msg")
        exit 4
    }

    switch ($state) {
        "active" { }
        "expired" {
            Write-Host "`e[31mExpired`e[0m"
            Write-Host "Public Key: $publicKey"
            Write-Host "`e[33mTo renew: Visit $PORTAL_BASE/keys/extend`e[0m"
            exit 3
        }
        "suspended" {
            Write-Host "`e[31mSuspended`e[0m"
            Write-Host "Public Key: $publicKey"
            exit 3
        }
        default {
            Write-Host "`e[31mInvalid`e[0m"
            Write-Host "Public Key: $publicKey"
            Write-Host "Reason: $reason"
            exit 3
        }
    }

    $expires = "N/A"
    $remaining = "N/A"
    if ($null -ne $result.valid_through) {
        $secs = [long][math]::Floor([double]$result.valid_through / 1000)
        $expires = [DateTimeOffset]::FromUnixTimeSeconds($secs).UtcDateTime.ToString("yyyy-MM-dd'T'HH:mm:ss'Z'", [System.Globalization.CultureInfo]::InvariantCulture)
        $left = $secs - [DateTimeOffset]::UtcNow.ToUnixTimeSeconds()
        if ($left -le 0) {
            $remaining = "expired"
        } else {
            $remaining = "{0}d {1}h {2}m" -f [math]::Floor($left / 86400), [math]::Floor(($left % 86400) / 3600), [math]::Floor(($left % 3600) / 60)
        }
    }
    Write-Host "`e[32mValid`e[0m"
    Write-Host "Public Key: $publicKey"
    Write-Host "Tier: $(if ($null -ne $result.tier) { $result.tier } else { 'N/A' })"
    Write-Host "Status: $(if ($null -ne $result.status) { $result.status } else { 'N/A' })"
    Write-Host "Expires: $expires"
    Write-Host "Time Remaining: $remaining"
}

function Invoke-Image {
    param($CmdArgs)

    # Parse arguments
    $listMode = $CmdArgs -contains "--list" -or $CmdArgs -contains "-l"
    $infoId = $null
    $deleteId = $null
    $lockId = $null
    $unlockId = $null
    $publishId = $null
    $sourceType = $null
    $visibilityId = $null
    $visibilityMode = $null
    $spawnId = $null
    $cloneId = $null
    $name = $null
    $ports = $null

    for ($i = 0; $i -lt $CmdArgs.Count; $i++) {
        switch ($CmdArgs[$i]) {
            "--info" { $infoId = $CmdArgs[$i + 1]; $i++ }
            "--delete" { $deleteId = $CmdArgs[$i + 1]; $i++ }
            "--lock" { $lockId = $CmdArgs[$i + 1]; $i++ }
            "--unlock" { $unlockId = $CmdArgs[$i + 1]; $i++ }
            "--publish" { $publishId = $CmdArgs[$i + 1]; $i++ }
            "--source-type" { $sourceType = $CmdArgs[$i + 1]; $i++ }
            "--visibility" {
                $visibilityId = $CmdArgs[$i + 1]
                $visibilityMode = $CmdArgs[$i + 2]
                $i += 2
            }
            "--spawn" { $spawnId = $CmdArgs[$i + 1]; $i++ }
            "--clone" { $cloneId = $CmdArgs[$i + 1]; $i++ }
            "--name" { $name = $CmdArgs[$i + 1]; $i++ }
            "--ports" { $ports = $CmdArgs[$i + 1]; $i++ }
        }
    }

    if ($listMode) {
        $result = Invoke-Api -Endpoint "/images"
        $result | ConvertTo-Json -Depth 5
        return
    }

    if ($infoId) {
        $result = Invoke-Api -Endpoint "/images/$infoId"
        $result | ConvertTo-Json -Depth 5
        return
    }

    if ($deleteId) {
        Invoke-ApiWithSudo -Endpoint "/images/$deleteId" -Method "DELETE"
        Write-Host "`e[32mImage deleted: $deleteId`e[0m"
        return
    }

    if ($lockId) {
        Invoke-Api -Endpoint "/images/$lockId/lock" -Method "POST" -Body "{}"
        Write-Host "`e[32mImage locked: $lockId`e[0m"
        return
    }

    if ($unlockId) {
        Invoke-ApiWithSudo -Endpoint "/images/$unlockId/unlock" -Method "POST" -Body "{}"
        Write-Host "`e[32mImage unlocked: $unlockId`e[0m"
        return
    }

    if ($publishId) {
        if (-not $sourceType) {
            Write-Error "Error: --publish requires --source-type (service or snapshot)"
            exit 1
        }
        $payload = @{
            source_type = $sourceType
            source_id = $publishId
        }
        if ($name) { $payload["name"] = $name }
        $body = $payload | ConvertTo-Json
        $result = Invoke-Api -Endpoint "/images/publish" -Method "POST" -Body $body
        Write-Host "`e[32mImage published`e[0m"
        $result | ConvertTo-Json -Depth 5
        return
    }

    if ($visibilityId -and $visibilityMode) {
        $payload = @{ visibility = $visibilityMode } | ConvertTo-Json
        Invoke-Api -Endpoint "/images/$visibilityId/visibility" -Method "POST" -Body $payload
        Write-Host "`e[32mImage visibility set to $visibilityMode`: $visibilityId`e[0m"
        return
    }

    if ($spawnId) {
        $payload = @{}
        if ($name) { $payload["name"] = $name }
        if ($ports) {
            $portList = $ports -split "," | ForEach-Object { [int]$_ }
            $payload["ports"] = $portList
        }
        $body = $payload | ConvertTo-Json
        $result = Invoke-Api -Endpoint "/images/$spawnId/spawn" -Method "POST" -Body $body
        Write-Host "`e[32mService spawned from image`e[0m"
        $result | ConvertTo-Json -Depth 5
        return
    }

    if ($cloneId) {
        $payload = @{}
        if ($name) { $payload["name"] = $name }
        $body = $payload | ConvertTo-Json
        $result = Invoke-Api -Endpoint "/images/$cloneId/clone" -Method "POST" -Body $body
        Write-Host "`e[32mImage cloned`e[0m"
        $result | ConvertTo-Json -Depth 5
        return
    }

    Write-Error "Error: Use --list, --info, --delete, --lock, --unlock, --publish, --visibility, --spawn, or --clone"
    exit 1
}

function Invoke-Snapshot {
    param($CmdArgs)

    # Parse arguments
    $listMode = $CmdArgs -contains "--list" -or $CmdArgs -contains "-l"
    $infoId = $null
    $deleteId = $null
    $lockId = $null
    $unlockId = $null
    $restoreId = $null
    $cloneId = $null
    $cloneType = $null
    $name = $null
    $shell = $null
    $ports = $null

    for ($i = 0; $i -lt $CmdArgs.Count; $i++) {
        switch ($CmdArgs[$i]) {
            "--info" { $infoId = $CmdArgs[$i + 1]; $i++ }
            "--delete" { $deleteId = $CmdArgs[$i + 1]; $i++ }
            "--lock" { $lockId = $CmdArgs[$i + 1]; $i++ }
            "--unlock" { $unlockId = $CmdArgs[$i + 1]; $i++ }
            "--restore" { $restoreId = $CmdArgs[$i + 1]; $i++ }
            "--clone" { $cloneId = $CmdArgs[$i + 1]; $i++ }
            "--type" { $cloneType = $CmdArgs[$i + 1]; $i++ }
            "--name" { $name = $CmdArgs[$i + 1]; $i++ }
            "--shell" { $shell = $CmdArgs[$i + 1]; $i++ }
            "-s" { $shell = $CmdArgs[$i + 1]; $i++ }
            "--ports" { $ports = $CmdArgs[$i + 1]; $i++ }
        }
    }

    if ($listMode) {
        $result = Invoke-Api -Endpoint "/snapshots"
        $result | ConvertTo-Json -Depth 5
        return
    }

    if ($infoId) {
        $result = Invoke-Api -Endpoint "/snapshots/$infoId"
        $result | ConvertTo-Json -Depth 5
        return
    }

    if ($deleteId) {
        Invoke-ApiWithSudo -Endpoint "/snapshots/$deleteId" -Method "DELETE"
        Write-Host "`e[32mSnapshot deleted: $deleteId`e[0m"
        return
    }

    if ($lockId) {
        Invoke-Api -Endpoint "/snapshots/$lockId/lock" -Method "POST" -Body "{}"
        Write-Host "`e[32mSnapshot locked: $lockId`e[0m"
        return
    }

    if ($unlockId) {
        Invoke-ApiWithSudo -Endpoint "/snapshots/$unlockId/unlock" -Method "POST" -Body "{}"
        Write-Host "`e[32mSnapshot unlocked: $unlockId`e[0m"
        return
    }

    if ($restoreId) {
        $result = Invoke-Api -Endpoint "/snapshots/$restoreId/restore" -Method "POST" -Body "{}"
        Write-Host "`e[32mRestored from snapshot`e[0m"
        $result | ConvertTo-Json -Depth 5
        return
    }

    if ($cloneId) {
        if (-not $cloneType) {
            $cloneType = "session"
        }
        $payload = @{ type = $cloneType }
        if ($name) { $payload["name"] = $name }
        if ($shell) { $payload["shell"] = $shell }
        if ($ports) {
            $portList = $ports -split "," | ForEach-Object { [int]$_ }
            $payload["ports"] = $portList
        }
        $body = $payload | ConvertTo-Json
        $result = Invoke-Api -Endpoint "/snapshots/$cloneId/clone" -Method "POST" -Body $body
        Write-Host "`e[32mCloned to $cloneType`e[0m"
        $result | ConvertTo-Json -Depth 5
        return
    }

    Write-Error "Error: Use --list, --info, --delete, --lock, --unlock, --restore, or --clone"
    exit 1
}

function Invoke-Service {
    param($CmdArgs)

    # Parse env subcommand and -e/--env-file
    $envAction = $null
    $envTarget = $null
    $envs = @()
    $envFile = $null

    for ($i = 0; $i -lt $CmdArgs.Count; $i++) {
        if ($CmdArgs[$i] -eq "env" -and ($i + 1) -lt $CmdArgs.Count) {
            $next = $CmdArgs[$i + 1]
            if (-not $next.StartsWith("-")) {
                $envAction = $next
                $i++
                if (($i + 1) -lt $CmdArgs.Count) {
                    $next2 = $CmdArgs[$i + 1]
                    if (-not $next2.StartsWith("-")) {
                        $envTarget = $next2
                        $i++
                    }
                }
            }
        } elseif ($CmdArgs[$i] -eq "-e" -and ($i + 1) -lt $CmdArgs.Count) {
            $envs += $CmdArgs[$i + 1]
            $i++
        } elseif ($CmdArgs[$i] -eq "--env-file" -and ($i + 1) -lt $CmdArgs.Count) {
            $envFile = $CmdArgs[$i + 1]
            $i++
        }
    }

    # Handle env subcommand
    if ($envAction) {
        Invoke-ServiceEnv -Action $envAction -Target $envTarget -Envs $envs -EnvFile $envFile
        return
    }

    if ($CmdArgs -contains "--list" -or $CmdArgs -contains "-l") {
        $result = Invoke-Api -Endpoint "/services"
        $result | ConvertTo-Json -Depth 5
        return
    }

    if ($CmdArgs -contains "--info") {
        $idx = [array]::IndexOf($CmdArgs, "--info")
        $serviceId = $CmdArgs[$idx + 1]
        $result = Invoke-Api -Endpoint "/services/$serviceId"
        $result | ConvertTo-Json -Depth 5
        return
    }

    if ($CmdArgs -contains "--logs") {
        $idx = [array]::IndexOf($CmdArgs, "--logs")
        $serviceId = $CmdArgs[$idx + 1]
        $result = Invoke-Api -Endpoint "/services/$serviceId/logs"
        Write-Host $result.logs
        return
    }

    if ($CmdArgs -contains "--freeze") {
        $idx = [array]::IndexOf($CmdArgs, "--freeze")
        $serviceId = $CmdArgs[$idx + 1]
        Invoke-Api -Endpoint "/services/$serviceId/freeze" -Method "POST" -Body "{}"
        Write-Host "`e[32mService frozen: $serviceId`e[0m"
        return
    }

    if ($CmdArgs -contains "--unfreeze") {
        $idx = [array]::IndexOf($CmdArgs, "--unfreeze")
        $serviceId = $CmdArgs[$idx + 1]
        Invoke-Api -Endpoint "/services/$serviceId/unfreeze" -Method "POST" -Body "{}"
        Write-Host "`e[32mService unfreezing: $serviceId`e[0m"
        return
    }

    if ($CmdArgs -contains "--destroy") {
        $idx = [array]::IndexOf($CmdArgs, "--destroy")
        $serviceId = $CmdArgs[$idx + 1]
        Invoke-ApiWithSudo -Endpoint "/services/$serviceId" -Method "DELETE"
        Write-Host "`e[32mService destroyed: $serviceId`e[0m"
        return
    }

    if ($CmdArgs -contains "--resize") {
        $idx = [array]::IndexOf($CmdArgs, "--resize")
        $serviceId = $CmdArgs[$idx + 1]

        # Get vcpu value from --vcpu or -v
        $vcpuValue = 0
        if ($CmdArgs -contains "--vcpu") {
            $vIdx = [array]::IndexOf($CmdArgs, "--vcpu")
            $vcpuValue = [int]$CmdArgs[$vIdx + 1]
        } elseif ($CmdArgs -contains "-v") {
            $vIdx = [array]::IndexOf($CmdArgs, "-v")
            $vcpuValue = [int]$CmdArgs[$vIdx + 1]
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

    if ($CmdArgs -contains "--set-unfreeze-on-demand") {
        $idx = [array]::IndexOf($CmdArgs, "--set-unfreeze-on-demand")
        $serviceId = $CmdArgs[$idx + 1]
        $enabled = $CmdArgs[$idx + 2]

        if ($enabled -eq "true" -or $enabled -eq "1") {
            $payload = @{ unfreeze_on_demand = $true } | ConvertTo-Json
            Invoke-Api -Endpoint "/services/$serviceId" -Method "PATCH" -Body $payload
            Write-Host "`e[32mUnfreeze-on-demand enabled for service: $serviceId`e[0m"
        } elseif ($enabled -eq "false" -or $enabled -eq "0") {
            $payload = @{ unfreeze_on_demand = $false } | ConvertTo-Json
            Invoke-Api -Endpoint "/services/$serviceId" -Method "PATCH" -Body $payload
            Write-Host "`e[32mUnfreeze-on-demand disabled for service: $serviceId`e[0m"
        } else {
            Write-Error "Error: --set-unfreeze-on-demand requires true/false or 1/0"
            exit 1
        }
        return
    }

    if ($CmdArgs -contains "--dump-bootstrap") {
        $idx = [array]::IndexOf($CmdArgs, "--dump-bootstrap")
        $serviceId = $CmdArgs[$idx + 1]
        Write-Host "Fetching bootstrap script from $serviceId..." -ForegroundColor Yellow

        $payload = @{ command = "cat /tmp/bootstrap.sh" } | ConvertTo-Json
        $result = Invoke-Api -Endpoint "/services/$serviceId/execute" -Method "POST" -Body $payload

        if ($result.stdout -and $result.stdout.Length -gt 0) {
            $bootstrap = $result.stdout
            if ($CmdArgs -contains "--dump-file") {
                $dumpIdx = [array]::IndexOf($CmdArgs, "--dump-file")
                $dumpFile = $CmdArgs[$dumpIdx + 1]
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
    if ($CmdArgs -contains "--name") {
        $idx = [array]::IndexOf($CmdArgs, "--name")
        $name = $CmdArgs[$idx + 1]

        $payload = Add-TargetOs -Payload @{ name = $name }

        if ($CmdArgs -contains "--ports") {
            $pIdx = [array]::IndexOf($CmdArgs, "--ports")
            $ports = $CmdArgs[$pIdx + 1] -split "," | ForEach-Object { [int]$_ }
            $payload["ports"] = $ports
        }

        if ($CmdArgs -contains "--bootstrap") {
            $bIdx = [array]::IndexOf($CmdArgs, "--bootstrap")
            $payload["bootstrap"] = $CmdArgs[$bIdx + 1]
        }

        if ($CmdArgs -contains "--bootstrap-file") {
            $bfIdx = [array]::IndexOf($CmdArgs, "--bootstrap-file")
            $bootstrapFile = $CmdArgs[$bfIdx + 1]
            if (Test-Path $bootstrapFile) {
                $payload["bootstrap_content"] = Get-Content -Raw $bootstrapFile
            } else {
                Write-Error "Error: Bootstrap file not found: $bootstrapFile"
                exit 1
            }
        }

        if ($CmdArgs -contains "--type") {
            $tIdx = [array]::IndexOf($CmdArgs, "--type")
            $payload["service_type"] = $CmdArgs[$tIdx + 1]
        }

        if ($CmdArgs -contains "--unfreeze-on-demand") {
            $payload["unfreeze_on_demand"] = $true
        }

        # Parse input files
        $inputFiles = @()
        for ($i = 0; $i -lt $CmdArgs.Count; $i++) {
            if ($CmdArgs[$i] -eq "-f" -and ($i + 1) -lt $CmdArgs.Count) {
                $filepath = $CmdArgs[$i + 1]
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
# Dot-sourced (. ./un.ps1) as a library: define functions only, skip the CLI
if ($MyInvocation.InvocationName -eq '.') { return }

if ($args.Count -eq 0 -or $args[0] -eq "--help" -or $args[0] -eq "-h") {
    Write-Host @"
Usage: pwsh un.ps1 [options] <source_file>
       pwsh un.ps1 -s <language> '<code>'
       pwsh un.ps1 session [options]
       pwsh un.ps1 service [options]
       pwsh un.ps1 snapshot [options]
       pwsh un.ps1 image [options]
       pwsh un.ps1 languages [--json]
       pwsh un.ps1 key [options]

Global options:
  --os OS             Target OS: linux (default) or freebsd (execute, session, service)
  --account N         Use account N from accounts.csv

Languages options:
  --json              Output as JSON array

Image options:
  --list, -l          List all images
  --info ID           Get image details
  --delete ID         Delete an image
  --lock ID           Lock image to prevent deletion
  --unlock ID         Unlock image
  --publish ID        Publish image from service/snapshot
  --source-type TYPE  Source type: service or snapshot
  --visibility ID MODE  Set visibility: private, unlisted, or public
  --spawn ID          Spawn new service from image
  --clone ID          Clone an image
  --name NAME         Name for spawned service or cloned image
  --ports PORTS       Ports for spawned service

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
  --unfreeze-on-demand Enable unfreeze-on-demand for new service
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
  --set-unfreeze-on-demand ID true|false  Enable/disable unfreeze-on-demand
  --dump-bootstrap ID  Dump bootstrap script from service
  --dump-file FILE     Save bootstrap to file (with --dump-bootstrap)

Service env commands:
  env status ID        Show vault status
  env set ID           Set vault (-e KEY=VALUE or --env-file FILE)
  env export ID        Export vault contents
  env delete ID        Delete vault

Snapshot options:
  --list, -l          List all snapshots
  --info ID           Get snapshot details
  --delete ID         Delete snapshot
  --lock ID           Lock snapshot
  --unlock ID         Unlock snapshot
  --restore ID        Restore from snapshot
  --clone ID          Clone snapshot to session/service
  --type TYPE         Clone type: session or service
  --name NAME         Name for cloned resource
  --shell NAME        Shell for cloned session
  --ports PORTS       Ports for cloned service

Key options:
  --extend        Open browser to extend key
  --rotate        Rotate this key's secret (sudo OTP); updates accounts.csv
  --otp CODE      Finish a rotation started without a terminal
"@
    exit 0
}

# Pre-parse --account N from args, strip from effective arg list
$effectiveArgs = @()
for ($i = 0; $i -lt $args.Count; $i++) {
    if ($args[$i] -eq "--account" -and ($i + 1) -lt $args.Count) {
        try { $script:AccountIndex = [int]$args[$i + 1] } catch {}
        $i++
    } elseif ($args[$i] -eq "--os" -or "$($args[$i])".StartsWith("--os=")) {
        if ($args[$i] -eq "--os") {
            if (($i + 1) -ge $args.Count) {
                [Console]::Error.WriteLine("Error: --os requires a value: linux or freebsd")
                exit 1
            }
            $osValue = "$($args[$i + 1])"
            $i++
        } else {
            $osValue = "$($args[$i])".Substring(5)
        }
        try {
            $script:TargetOs = ConvertTo-TargetOs $osValue
        } catch {
            [Console]::Error.WriteLine("Error: invalid --os '$osValue' (expected linux or freebsd)")
            exit 1
        }
    } else {
        $effectiveArgs += $args[$i]
    }
}

if ($effectiveArgs[0] -eq "session") {
    Invoke-Session -CmdArgs $effectiveArgs[1..($effectiveArgs.Count-1)]
} elseif ($effectiveArgs[0] -eq "service") {
    Invoke-Service -CmdArgs $effectiveArgs[1..($effectiveArgs.Count-1)]
} elseif ($effectiveArgs[0] -eq "snapshot") {
    Invoke-Snapshot -CmdArgs $effectiveArgs[1..($effectiveArgs.Count-1)]
} elseif ($effectiveArgs[0] -eq "image") {
    Invoke-Image -CmdArgs $effectiveArgs[1..($effectiveArgs.Count-1)]
} elseif ($effectiveArgs[0] -eq "languages") {
    Invoke-Languages -CmdArgs $effectiveArgs[1..($effectiveArgs.Count-1)]
} elseif ($effectiveArgs[0] -eq "key" -and $effectiveArgs -contains "--rotate") {
    $otp = $null
    for ($i = 1; $i -lt $effectiveArgs.Count; $i++) {
        if ($effectiveArgs[$i] -eq "--otp" -and ($i + 1) -lt $effectiveArgs.Count) { $otp = $effectiveArgs[$i + 1] }
    }
    Invoke-KeyRotate -Otp $otp
} elseif ($effectiveArgs[0] -eq "key") {
    Invoke-Key -CmdArgs $effectiveArgs[1..($effectiveArgs.Count-1)]
} else {
    # Parse execute args
    $sourceFile = $null
    $envVars = @{}
    $network = $null
    $inlineLanguage = $null

    for ($i = 0; $i -lt $effectiveArgs.Count; $i++) {
        switch ($effectiveArgs[$i]) {
            "-e" {
                $kv = $effectiveArgs[$i+1] -split "=", 2
                $envVars[$kv[0]] = $kv[1]
                $i++
            }
            "-n" { $network = $effectiveArgs[$i+1]; $i++ }
            "-s" { $inlineLanguage = $effectiveArgs[$i+1]; $i++ }
            default {
                if ($effectiveArgs[$i].StartsWith("-")) {
                    Write-Error "${RED}Unknown option: $($effectiveArgs[$i])${RESET}"
                    exit 1
                } else {
                    $sourceFile = $effectiveArgs[$i]
                }
            }
        }
    }

    if (-not $sourceFile) {
        Write-Error "Error: No source file specified"
        exit 1
    }

    Invoke-Execute -SourceFile $sourceFile -EnvVars $envVars -Network $network -Language $inlineLanguage
}
