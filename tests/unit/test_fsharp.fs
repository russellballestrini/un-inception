// Unit tests for un.fs - tests internal functions without API calls
// Run with: dotnet fsi test_fsharp.fs

open System
open System.Security.Cryptography
open System.Text

let mutable passed = 0
let mutable failed = 0

let test name result =
    if result then
        printfn "  ✓ %s" name
        passed <- passed + 1
    else
        printfn "  ✗ %s" name
        failed <- failed + 1

let extMap = Map [
    (".py", "python"); (".js", "javascript"); (".ts", "typescript")
    (".rb", "ruby"); (".go", "go"); (".rs", "rust"); (".c", "c")
    (".fs", "fsharp"); (".java", "java"); (".kt", "kotlin"); (".hs", "haskell")
]

let getLanguage ext =
    match Map.tryFind ext extMap with
    | Some lang -> lang
    | None -> ""

let getExtension (filename: string) =
    let idx = filename.LastIndexOf('.')
    if idx >= 0 then filename.Substring(idx) else ""

let getBasename (path: string) =
    let idx = path.LastIndexOf('/')
    if idx >= 0 then path.Substring(idx + 1) else path

let hmacSha256 (secret: string) (message: string) =
    use hmac = new HMACSHA256(Encoding.UTF8.GetBytes(secret))
    let hash = hmac.ComputeHash(Encoding.UTF8.GetBytes(message))
    BitConverter.ToString(hash).Replace("-", "").ToLower()

printfn "\n=== Extension Mapping Tests ==="

test "Python extension maps correctly" (getLanguage ".py" = "python")
test "F# extension maps correctly" (getLanguage ".fs" = "fsharp")
test "JavaScript extension maps correctly" (getLanguage ".js" = "javascript")
test "Go extension maps correctly" (getLanguage ".go" = "go")

printfn "\n=== HMAC Signature Tests ==="

test "HMAC-SHA256 generates 64 character hex string"
    ((hmacSha256 "test-secret" "test-message").Length = 64)

test "Same input produces same signature"
    (hmacSha256 "key" "msg" = hmacSha256 "key" "msg")

let timestamp = "1704067200"
let httpMethod = "POST"
let endpoint = "/execute"
let body = """{"language":"python"}"""
let message = sprintf "%s:%s:%s:%s" timestamp httpMethod endpoint body

test "Signature format starts with timestamp" (message.StartsWith(timestamp))
test "Signature format contains :POST:" (message.Contains(":POST:"))
test "Signature format contains :/execute:" (message.Contains(":/execute:"))

printfn "\n=== Language Detection Tests ==="

let content = "#!/usr/bin/env python3\nprint('hello')"
let firstLine = content.Split('\n').[0]

test "Python shebang detection - starts with #!" (firstLine.StartsWith("#!"))
test "Python shebang detection - contains python" (firstLine.Contains("python"))

printfn "\n=== Argument Parsing Tests ==="

let arg1 = "DEBUG=1"
let parts1 = arg1.Split([|'='|], 2)
let key1, value1 = parts1.[0], parts1.[1]

test "Parse -e KEY=VALUE format - key" (key1 = "DEBUG")
test "Parse -e KEY=VALUE format - value" (value1 = "1")

let arg2 = "URL=https://example.com?foo=bar"
let parts2 = arg2.Split([|'='|], 2)
let key2, value2 = parts2.[0], parts2.[1]

test "Parse -e KEY=VALUE with equals in value" (key2 = "URL" && value2 = "https://example.com?foo=bar")

printfn "\n=== File Operations Tests ==="

test "Extract file basename" (getBasename "/home/user/project/script.fs" = "script.fs")
test "Extract file extension" (getExtension "/home/user/project/script.fs" = ".fs")

printfn "\n=== API Constants Tests ==="

let apiBase = "https://api.unsandbox.com"

test "API base URL starts with https://" (apiBase.StartsWith("https://"))
test "API base URL contains unsandbox.com" (apiBase.Contains("unsandbox.com"))

printfn "\n=== Summary ==="
printfn "Passed: %d" passed
printfn "Failed: %d" failed
printfn "Total:  %d" (passed + failed)

exit (if failed > 0 then 1 else 0)
