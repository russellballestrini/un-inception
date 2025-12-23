// test_un_fs.fs - Comprehensive tests for un.fs CLI implementation
// Compile: fsharpc test_un_fs.fs
// Run: mono test_un_fs.exe
// Note: Requires un.exe to be compiled in parent directory
// For integration tests: Requires UNSANDBOX_API_KEY environment variable

open System
open System.Diagnostics
open System.IO
open System.Reflection

let mutable testsRun = 0
let mutable testsPassed = 0
let mutable testsFailed = 0

let testDetectLanguage (filename: string) (expectedLang: string) =
    testsRun <- testsRun + 1
    try
        // Load un assembly and call detectLanguage via reflection
        let unAssembly = Assembly.LoadFrom("../un.exe")
        let unModule = unAssembly.GetTypes() |> Array.find (fun t -> t.Name.Contains("un"))
        let detectLanguage = unModule.GetMethod("detectLanguage")

        let result = detectLanguage.Invoke(null, [| box filename |]) :?> string

        if result = expectedLang then
            testsPassed <- testsPassed + 1
            printfn "PASS: detectLanguage(\"%s\") = \"%s\"" filename expectedLang
        else
            testsFailed <- testsFailed + 1
            printfn "FAIL: detectLanguage(\"%s\") expected \"%s\", got \"%s\"" filename expectedLang result
    with ex ->
        testsFailed <- testsFailed + 1
        printfn "FAIL: detectLanguage(\"%s\") threw exception: %s" filename ex.Message

let testDetectLanguageError (filename: string) =
    testsRun <- testsRun + 1
    try
        let unAssembly = Assembly.LoadFrom("../un.exe")
        let unModule = unAssembly.GetTypes() |> Array.find (fun t -> t.Name.Contains("un"))
        let detectLanguage = unModule.GetMethod("detectLanguage")

        try
            detectLanguage.Invoke(null, [| box filename |]) |> ignore
            testsFailed <- testsFailed + 1
            printfn "FAIL: detectLanguage(\"%s\") should throw exception" filename
        with
        | :? TargetInvocationException as ex ->
            // Expected to throw exception
            testsPassed <- testsPassed + 1
            printfn "PASS: detectLanguage(\"%s\") correctly throws exception" filename
        | ex ->
            testsFailed <- testsFailed + 1
            printfn "FAIL: detectLanguage(\"%s\") threw wrong exception: %s" filename (ex.GetType().Name)
    with ex ->
        testsFailed <- testsFailed + 1
        printfn "FAIL: detectLanguage(\"%s\") test setup failed: %s" filename ex.Message

let testExtensionDetection () =
    printfn "--- Unit Tests: Extension Detection ---"

    testDetectLanguage "test.java" "java"
    testDetectLanguage "test.kt" "kotlin"
    testDetectLanguage "test.cs" "csharp"
    testDetectLanguage "test.fs" "fsharp"
    testDetectLanguage "test.groovy" "groovy"
    testDetectLanguage "test.dart" "dart"
    testDetectLanguage "test.py" "python"
    testDetectLanguage "test.js" "javascript"
    testDetectLanguage "test.rs" "rust"
    testDetectLanguage "test.go" "go"

    testDetectLanguageError "noextension"
    testDetectLanguageError "test.unknown"

    printfn ""

let testApiCall () =
    printfn "--- Integration Test: API Call ---"
    testsRun <- testsRun + 1

    try
        // Create a simple test file
        let testCode = "console.log('Hello from F# test');"
        let testFile = "test_api_fs.js"
        File.WriteAllText(testFile, testCode)

        try
            // Execute un with the test file
            let psi = ProcessStartInfo()
            psi.FileName <- "mono"
            psi.Arguments <- "../un.exe test_api_fs.js"
            psi.RedirectStandardOutput <- true
            psi.RedirectStandardError <- true
            psi.UseShellExecute <- false

            use p = Process.Start(psi)
            let output = p.StandardOutput.ReadToEnd()
            let error = p.StandardError.ReadToEnd()
            p.WaitForExit()

            if p.ExitCode = 0 && output.Contains("Hello from F# test") then
                testsPassed <- testsPassed + 1
                printfn "PASS: API call succeeded and returned expected output"
            else
                testsFailed <- testsFailed + 1
                printfn "FAIL: API call failed or unexpected output"
                printfn "Exit code: %d" p.ExitCode
                printfn "Output: %s" output
                printfn "Error: %s" error
        finally
            if File.Exists(testFile) then
                File.Delete(testFile)
    with ex ->
        testsFailed <- testsFailed + 1
        printfn "FAIL: API call test threw exception: %s" ex.Message

    printfn ""

let testFibExecution () =
    printfn "--- Functional Test: fib.java Execution ---"
    testsRun <- testsRun + 1

    try
        // Check if fib.java exists
        if not (File.Exists("fib.java")) then
            testsFailed <- testsFailed + 1
            printfn "FAIL: fib.java not found in tests directory"
            printfn ""
        else
            // Execute un with fib.java
            let psi = ProcessStartInfo()
            psi.FileName <- "mono"
            psi.Arguments <- "../un.exe fib.java"
            psi.RedirectStandardOutput <- true
            psi.RedirectStandardError <- true
            psi.UseShellExecute <- false

            use p = Process.Start(psi)
            let output = p.StandardOutput.ReadToEnd()
            let error = p.StandardError.ReadToEnd()
            p.WaitForExit()

            if p.ExitCode = 0 && output.Contains("fib(10) = 55") then
                testsPassed <- testsPassed + 1
                printfn "PASS: fib.java execution succeeded"
                printfn "Output: %s" (output.Trim())
            else
                testsFailed <- testsFailed + 1
                printfn "FAIL: fib.java execution failed or unexpected output"
                printfn "Exit code: %d" p.ExitCode
                printfn "Output: %s" output
                printfn "Error: %s" error

            printfn ""
    with ex ->
        testsFailed <- testsFailed + 1
        printfn "FAIL: fib.java execution test threw exception: %s" ex.Message
        printfn ""

[<EntryPoint>]
let main argv =
    printfn "=== Running un.fs Tests ===\n"

    // Unit Tests - Extension Detection
    testExtensionDetection ()

    // Integration Tests - API Call (skip if no API key)
    let apiKey = Environment.GetEnvironmentVariable("UNSANDBOX_API_KEY")
    if not (String.IsNullOrEmpty(apiKey)) then
        testApiCall ()
        testFibExecution ()
    else
        printfn "SKIP: API integration tests (UNSANDBOX_API_KEY not set)\n"

    // Print summary
    printfn "=== Test Summary ==="
    printfn "Tests run: %d" testsRun
    printfn "Passed: %d" testsPassed
    printfn "Failed: %d" testsFailed

    if testsFailed > 0 then
        1
    else
        printfn "\nAll tests PASSED!"
        0
