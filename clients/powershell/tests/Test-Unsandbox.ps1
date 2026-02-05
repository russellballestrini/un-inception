#!/usr/bin/env pwsh
# PUBLIC DOMAIN - NO LICENSE, NO WARRANTY
# Unit and Functional Tests for Unsandbox PowerShell SDK

$ErrorActionPreference = "Stop"

# Source the main script to get access to functions
. "$PSScriptRoot/../sync/src/un.ps1"

function Write-TestResult {
    param($Name, $Passed, $Message = "")
    if ($Passed) {
        Write-Host "  [PASS] $Name" -ForegroundColor Green
    } else {
        Write-Host "  [FAIL] $Name $Message" -ForegroundColor Red
    }
}

function Test-Unit {
    Write-Host ""
    Write-Host "=== PowerShell SDK Unit Tests ===" -ForegroundColor Cyan
    Write-Host ""

    # Test extension map
    Write-Host "Testing extension detection..."
    $tests = @{
        ".py" = "python"
        ".js" = "javascript"
        ".go" = "go"
        ".rs" = "rust"
        ".ps1" = "powershell"
    }

    $passed = 0
    foreach ($ext in $tests.Keys) {
        $expected = $tests[$ext]
        $actual = $EXT_MAP[$ext]
        if ($actual -eq $expected) {
            $passed++
        }
    }
    Write-TestResult "ExtensionMap" ($passed -eq $tests.Count) "($passed/$($tests.Count))"

    # Test HMAC signing
    Write-Host "Testing HMAC signing..."
    $hmac = New-Object System.Security.Cryptography.HMACSHA256
    $hmac.Key = [System.Text.Encoding]::UTF8.GetBytes("key")
    $hash = $hmac.ComputeHash([System.Text.Encoding]::UTF8.GetBytes("message"))
    $signature = [System.BitConverter]::ToString($hash).Replace("-", "").ToLower()
    $expected = "6e9ef29b75fffc5b7abae527d58fdadb2fe42e7219011976917343065f58ed4a"
    Write-TestResult "HmacSign" ($signature -eq $expected)

    # Test env content building
    Write-Host "Testing env content building..."
    $envs = @("KEY1=value1", "KEY2=value2")
    $result = Build-EnvContent -Envs $envs -EnvFile $null
    $hasKey1 = $result -match "KEY1=value1"
    $hasKey2 = $result -match "KEY2=value2"
    Write-TestResult "BuildEnvContent" ($hasKey1 -and $hasKey2)
}

function Test-Functional {
    Write-Host ""
    Write-Host "=== PowerShell SDK Functional Tests ===" -ForegroundColor Cyan
    Write-Host ""

    $publicKey = $env:UNSANDBOX_PUBLIC_KEY
    $secretKey = $env:UNSANDBOX_SECRET_KEY

    if (-not $publicKey -or -not $secretKey) {
        Write-Host "  [SKIP] No API credentials (set UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY)" -ForegroundColor Yellow
        return
    }

    # Test key validation
    Write-Host "Testing key validation..."
    try {
        $result = Invoke-Api -Endpoint "/keys/validate" -Method "POST" -BaseUrl $PORTAL_BASE
        Write-TestResult "ValidateKeys" ($null -ne $result)
    } catch {
        Write-TestResult "ValidateKeys" $false $_.Exception.Message
    }

    # Test languages endpoint
    Write-Host "Testing languages endpoint..."
    try {
        $result = Invoke-Api -Endpoint "/languages"
        $hasLanguages = ($result.languages -and $result.languages.Count -gt 0)
        Write-TestResult "GetLanguages" $hasLanguages "($($result.languages.Count) languages)"
    } catch {
        Write-TestResult "GetLanguages" $false $_.Exception.Message
    }

    # Test execute
    Write-Host "Testing execute endpoint..."
    try {
        $payload = @{
            language = "python"
            code = 'print("hello from powershell test")'
        } | ConvertTo-Json
        $result = Invoke-Api -Endpoint "/execute" -Method "POST" -Body $payload
        $hasOutput = $result.stdout -match "hello"
        Write-TestResult "Execute" $hasOutput
    } catch {
        Write-TestResult "Execute" $false $_.Exception.Message
    }

    # Test session list
    Write-Host "Testing session list..."
    try {
        $result = Invoke-Api -Endpoint "/sessions"
        Write-TestResult "SessionList" ($null -ne $result)
    } catch {
        Write-TestResult "SessionList" $false $_.Exception.Message
    }

    # Test service list
    Write-Host "Testing service list..."
    try {
        $result = Invoke-Api -Endpoint "/services"
        Write-TestResult "ServiceList" ($null -ne $result)
    } catch {
        Write-TestResult "ServiceList" $false $_.Exception.Message
    }

    # Test snapshot list
    Write-Host "Testing snapshot list..."
    try {
        $result = Invoke-Api -Endpoint "/snapshots"
        Write-TestResult "SnapshotList" ($null -ne $result)
    } catch {
        Write-TestResult "SnapshotList" $false $_.Exception.Message
    }

    # Test image list
    Write-Host "Testing image list..."
    try {
        $result = Invoke-Api -Endpoint "/images"
        Write-TestResult "ImageList" ($null -ne $result)
    } catch {
        Write-TestResult "ImageList" $false $_.Exception.Message
    }

    # Test snapshot list
    Write-Host "Testing snapshot list..."
    try {
        $result = Invoke-Api -Endpoint "/snapshots"
        Write-TestResult "SnapshotList" ($null -ne $result)
    } catch {
        Write-TestResult "SnapshotList" $false $_.Exception.Message
    }
}

# Main
Write-Host "Unsandbox PowerShell SDK Tests" -ForegroundColor Cyan
Write-Host "==============================" -ForegroundColor Cyan

Test-Unit
Test-Functional

Write-Host ""
Write-Host "Tests complete." -ForegroundColor Cyan
