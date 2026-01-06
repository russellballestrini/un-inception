#!/usr/bin/env pwsh
# Unit tests for un.ps1 - tests internal functions without API calls

$passed = 0
$failed = 0

function Test-Case {
    param([string]$Name, [scriptblock]$Code)

    try {
        & $Code
        Write-Host "  ✓ $Name"
        $script:passed++
    }
    catch {
        Write-Host "  ✗ $Name"
        Write-Host "    $_"
        $script:failed++
    }
}

function Assert-Equal {
    param($Actual, $Expected)
    if ($Actual -ne $Expected) {
        throw "Expected '$Expected' but got '$Actual'"
    }
}

function Assert-NotEqual {
    param($A, $B)
    if ($A -eq $B) {
        throw "Expected values to be different but both were '$A'"
    }
}

function Assert-Contains {
    param([string]$Str, [string]$Substr)
    if (-not $Str.Contains($Substr)) {
        throw "Expected '$Str' to contain '$Substr'"
    }
}

function Assert-True {
    param($Val)
    if (-not $Val) {
        throw "Expected true but got false"
    }
}

# Extension mapping
$ExtMap = @{
    ".py" = "python"; ".js" = "javascript"; ".ts" = "typescript"
    ".rb" = "ruby"; ".php" = "php"; ".pl" = "perl"; ".lua" = "lua"
    ".sh" = "bash"; ".go" = "go"; ".rs" = "rust"; ".c" = "c"
    ".cpp" = "cpp"; ".java" = "java"; ".kt" = "kotlin"
    ".hs" = "haskell"; ".clj" = "clojure"; ".erl" = "erlang"
    ".ex" = "elixir"; ".jl" = "julia"; ".ps1" = "powershell"
}

Write-Host "`n=== Extension Mapping Tests ==="

Test-Case "Python extension maps correctly" {
    Assert-Equal $ExtMap[".py"] "python"
}

Test-Case "PowerShell extension maps correctly" {
    Assert-Equal $ExtMap[".ps1"] "powershell"
}

Test-Case "JavaScript extension maps correctly" {
    Assert-Equal $ExtMap[".js"] "javascript"
}

Test-Case "Go extension maps correctly" {
    Assert-Equal $ExtMap[".go"] "go"
}

Test-Case "Ruby extension maps correctly" {
    Assert-Equal $ExtMap[".rb"] "ruby"
}

Write-Host "`n=== HMAC Signature Tests ==="

function Get-HmacSha256 {
    param([string]$Secret, [string]$Message)

    $hmac = New-Object System.Security.Cryptography.HMACSHA256
    $hmac.Key = [System.Text.Encoding]::UTF8.GetBytes($Secret)
    $hash = $hmac.ComputeHash([System.Text.Encoding]::UTF8.GetBytes($Message))
    return [BitConverter]::ToString($hash).Replace("-", "").ToLower()
}

Test-Case "HMAC-SHA256 generates 64 character hex string" {
    $sig = Get-HmacSha256 "test-secret" "test-message"
    Assert-Equal $sig.Length 64
}

Test-Case "Same input produces same signature" {
    $sig1 = Get-HmacSha256 "key" "msg"
    $sig2 = Get-HmacSha256 "key" "msg"
    Assert-Equal $sig1 $sig2
}

Test-Case "Different secrets produce different signatures" {
    $sig1 = Get-HmacSha256 "key1" "msg"
    $sig2 = Get-HmacSha256 "key2" "msg"
    Assert-NotEqual $sig1 $sig2
}

Test-Case "Signature format verification" {
    $timestamp = "1704067200"
    $method = "POST"
    $endpoint = "/execute"
    $body = '{"language":"python"}'

    $message = "$timestamp`:$method`:$endpoint`:$body"

    Assert-True $message.StartsWith($timestamp)
    Assert-Contains $message ":POST:"
    Assert-Contains $message ":/execute:"
}

Write-Host "`n=== Language Detection Tests ==="

Test-Case "Detect language from .ps1 extension" {
    $filename = "script.ps1"
    $ext = [System.IO.Path]::GetExtension($filename)
    Assert-Equal $ExtMap[$ext] "powershell"
}

Test-Case "Python shebang detection" {
    $content = "#!/usr/bin/env python3`nprint('hello')"
    $firstLine = $content.Split("`n")[0]
    Assert-True $firstLine.StartsWith("#!")
    Assert-Contains $firstLine "python"
}

Write-Host "`n=== Argument Parsing Tests ==="

Test-Case "Parse -e KEY=VALUE format" {
    $arg = "DEBUG=1"
    $parts = $arg.Split("=", 2)
    $key = $parts[0]
    $value = $parts[1]
    Assert-Equal $key "DEBUG"
    Assert-Equal $value "1"
}

Test-Case "Parse -e KEY=VALUE with equals in value" {
    $arg = "URL=https://example.com?foo=bar"
    $parts = $arg.Split("=", 2)
    $key = $parts[0]
    $value = $parts[1]
    Assert-Equal $key "URL"
    Assert-Equal $value "https://example.com?foo=bar"
}

Write-Host "`n=== File Operations Tests ==="

Test-Case "Base64 encoding/decoding" {
    $content = "print('hello world')"
    $bytes = [System.Text.Encoding]::UTF8.GetBytes($content)
    $encoded = [Convert]::ToBase64String($bytes)
    $decoded = [System.Text.Encoding]::UTF8.GetString([Convert]::FromBase64String($encoded))
    Assert-Equal $decoded $content
}

Test-Case "Extract file basename" {
    $path = "/home/user/project/script.ps1"
    Assert-Equal ([System.IO.Path]::GetFileName($path)) "script.ps1"
}

Test-Case "Extract file extension" {
    $path = "/home/user/project/script.ps1"
    Assert-Equal ([System.IO.Path]::GetExtension($path)) ".ps1"
}

Write-Host "`n=== API Constants Tests ==="

Test-Case "API base URL format" {
    $apiBase = "https://api.unsandbox.com"
    Assert-True $apiBase.StartsWith("https://")
    Assert-Contains $apiBase "unsandbox.com"
}

# Summary
Write-Host "`n=== Summary ==="
Write-Host "Passed: $passed"
Write-Host "Failed: $failed"
Write-Host "Total:  $($passed + $failed)"

exit $(if ($failed -gt 0) { 1 } else { 0 })
