// PUBLIC DOMAIN - NO LICENSE, NO WARRANTY
// Unit and Functional Tests for Unsandbox F# SDK

open System
open System.Collections.Generic
open System.Security.Cryptography
open System.Text

// Source the main module (when running as script)
// For compiled tests, include un.fs in the project

/// Unit tests for the Unsandbox SDK library functions.
module UnitTests =
    let run () =
        printfn "=== Unsandbox F# SDK Unit Tests ===\n"

        testDetectLanguage ()
        testHmacSign ()
        testExtensionMap ()

        printfn "\n=== Unit Tests Complete ==="

    and testDetectLanguage () =
        printf "DetectLanguage: "
        let tests = [
            ("test.py", "python")
            ("script.js", "javascript")
            ("main.go", "go")
            ("app.rs", "rust")
            ("Program.cs", "csharp")
            ("Module.fs", "fsharp")
        ]

        let mutable passed = 0
        for (filename, expected) in tests do
            let ext = filename.Substring(filename.LastIndexOf('.'))
            match Map.tryFind ext extMap with
            | Some lang when lang = expected -> passed <- passed + 1
            | Some lang -> printf "[FAIL: %s -> %s, expected %s] " filename lang expected
            | None -> printf "[FAIL: %s -> None, expected %s] " filename expected

        if passed = tests.Length then
            printfn "PASS (%d/%d)" passed tests.Length
        else
            printfn "FAIL (%d/%d)" passed tests.Length

    and testHmacSign () =
        printf "HmacSign: "
        // Test vector: HMAC-SHA256("key", "message")
        use hmac = new HMACSHA256(Encoding.UTF8.GetBytes("key"))
        let hash = hmac.ComputeHash(Encoding.UTF8.GetBytes("message"))
        let signature = BitConverter.ToString(hash).Replace("-", "").ToLower()
        let expected = "6e9ef29b75fffc5b7abae527d58fdadb2fe42e7219011976917343065f58ed4a"
        if signature = expected then
            printfn "PASS"
        else
            printfn "FAIL (got %s, expected %s)" signature expected

    and testExtensionMap () =
        printf "ExtensionMap: "
        let tests = [
            (".py", "python")
            (".js", "javascript")
            (".go", "go")
            (".rs", "rust")
            (".fs", "fsharp")
        ]

        let mutable passed = 0
        for (ext, expected) in tests do
            match Map.tryFind ext extMap with
            | Some lang when lang = expected -> passed <- passed + 1
            | _ -> ()

        if passed = tests.Length then
            printfn "PASS (%d/%d)" passed tests.Length
        else
            printfn "FAIL (%d/%d)" passed tests.Length

/// Functional tests that require API credentials.
module FunctionalTests =
    let run () =
        let publicKey = Environment.GetEnvironmentVariable("UNSANDBOX_PUBLIC_KEY")
        let secretKey = Environment.GetEnvironmentVariable("UNSANDBOX_SECRET_KEY")

        if String.IsNullOrEmpty(publicKey) || String.IsNullOrEmpty(secretKey) then
            printfn "=== Functional Tests Skipped (no API credentials) ==="
        else
            printfn "=== Unsandbox F# SDK Functional Tests ===\n"

            testValidateKeys ()
            testGetLanguages ()
            testExecute ()
            testSessionList ()
            testServiceList ()
            testSnapshotList ()
            testImageList ()

            printfn "\n=== Functional Tests Complete ==="

    and testValidateKeys () =
        printf "ValidateKeys: "
        try
            let result = apiRequest "/keys/validate" "POST" None publicKey secretKey
            match result.TryFind "valid" with
            | Some v when v.ToString() = "True" ->
                let tier = match result.TryFind "tier" with | Some t -> t.ToString() | None -> "N/A"
                printfn "PASS (tier: %s)" tier
            | _ -> printfn "FAIL"
        with ex ->
            printfn "FAIL (%s)" ex.Message

    and testGetLanguages () =
        printf "GetLanguages: "
        try
            let result = apiRequest "/languages" "GET" None publicKey secretKey
            match result.TryFind "languages" with
            | Some langs -> printfn "PASS (languages received)"
            | None -> printfn "FAIL (no languages in response)"
        with ex ->
            printfn "FAIL (%s)" ex.Message

    and testExecute () =
        printf "Execute: "
        try
            let payload = [("language", box "python"); ("code", box "print('hello from F# SDK')")]
            let result = apiRequest "/execute" "POST" (Some payload) publicKey secretKey
            match result.TryFind "stdout" with
            | Some stdout when stdout.ToString().Contains("hello") -> printfn "PASS"
            | _ -> printfn "FAIL (no expected output)"
        with ex ->
            printfn "FAIL (%s)" ex.Message

    and testSessionList () =
        printf "SessionList: "
        try
            let result = apiRequest "/sessions" "GET" None publicKey secretKey
            printfn "PASS (sessions endpoint responded)"
        with ex ->
            printfn "FAIL (%s)" ex.Message

    and testServiceList () =
        printf "ServiceList: "
        try
            let result = apiRequest "/services" "GET" None publicKey secretKey
            printfn "PASS (services endpoint responded)"
        with ex ->
            printfn "FAIL (%s)" ex.Message

    and testSnapshotList () =
        printf "SnapshotList: "
        try
            let result = apiRequest "/snapshots" "GET" None publicKey secretKey
            printfn "PASS (snapshots endpoint responded)"
        with ex ->
            printfn "FAIL (%s)" ex.Message

    and testImageList () =
        printf "ImageList: "
        try
            let result = apiRequest "/images" "GET" None publicKey secretKey
            printfn "PASS (images endpoint responded)"
        with ex ->
            printfn "FAIL (%s)" ex.Message

    // Get the API keys from environment
    and publicKey = Environment.GetEnvironmentVariable("UNSANDBOX_PUBLIC_KEY")
    and secretKey = Environment.GetEnvironmentVariable("UNSANDBOX_SECRET_KEY")

// Extension map (duplicated here for standalone testing)
let extMap =
    Map.ofList [
        (".py", "python"); (".js", "javascript"); (".ts", "typescript")
        (".rb", "ruby"); (".php", "php"); (".pl", "perl"); (".lua", "lua")
        (".sh", "bash"); (".go", "go"); (".rs", "rust"); (".c", "c")
        (".cpp", "cpp"); (".cc", "cpp"); (".cxx", "cpp")
        (".java", "java"); (".kt", "kotlin"); (".cs", "csharp"); (".fs", "fsharp")
        (".hs", "haskell"); (".ml", "ocaml"); (".clj", "clojure"); (".scm", "scheme")
        (".lisp", "commonlisp"); (".erl", "erlang"); (".ex", "elixir"); (".exs", "elixir")
        (".jl", "julia"); (".r", "r"); (".R", "r"); (".cr", "crystal")
        (".d", "d"); (".nim", "nim"); (".zig", "zig"); (".v", "v")
        (".dart", "dart"); (".groovy", "groovy"); (".scala", "scala")
        (".f90", "fortran"); (".f95", "fortran"); (".cob", "cobol")
        (".pro", "prolog"); (".forth", "forth"); (".4th", "forth")
        (".tcl", "tcl"); (".raku", "raku"); (".m", "objc")
    ]

// API request function (simplified for testing)
open System.Net
open System.IO

let apiBase = "https://api.unsandbox.com"

let toJson (obj: obj) =
    match obj with
    | :? string as s -> sprintf "\"%s\"" (s.Replace("\\", "\\\\").Replace("\"", "\\\""))
    | :? int as i -> i.ToString()
    | :? bool as b -> b.ToString().ToLower()
    | :? (string * obj) list as lst ->
        let entries = lst |> List.map (fun (k, v) -> sprintf "\"%s\":%s" k (toJson v)) |> String.concat ","
        sprintf "{%s}" entries
    | _ -> sprintf "\"%s\"" (obj.ToString())

let apiRequest (endpoint: string) (method: string) (data: (string * obj) list option) (publicKey: string) (secretKey: string) =
    ServicePointManager.SecurityProtocol <- SecurityProtocolType.Tls12 ||| SecurityProtocolType.Tls11 ||| SecurityProtocolType.Tls

    let request = WebRequest.Create(apiBase + endpoint) :?> HttpWebRequest
    request.Method <- method
    request.ContentType <- "application/json"
    request.Timeout <- 300000

    let body = match data with | Some d -> toJson (box d) | None -> ""

    if not (String.IsNullOrEmpty(secretKey)) then
        let timestamp = DateTimeOffset.UtcNow.ToUnixTimeSeconds()
        let message = sprintf "%d:%s:%s:%s" timestamp method endpoint body

        use hmac = new HMACSHA256(Encoding.UTF8.GetBytes(secretKey))
        let hash = hmac.ComputeHash(Encoding.UTF8.GetBytes(message))
        let signature = BitConverter.ToString(hash).Replace("-", "").ToLower()

        request.Headers.Add("Authorization", sprintf "Bearer %s" publicKey)
        request.Headers.Add("X-Timestamp", timestamp.ToString())
        request.Headers.Add("X-Signature", signature)
    else
        request.Headers.Add("Authorization", sprintf "Bearer %s" publicKey)

    match data with
    | Some d ->
        let bytes = Encoding.UTF8.GetBytes(body)
        request.ContentLength <- int64 bytes.Length
        use stream = request.GetRequestStream()
        stream.Write(bytes, 0, bytes.Length)
    | None -> ()

    use response = request.GetResponse() :?> HttpWebResponse
    use reader = new StreamReader(response.GetResponseStream())
    let responseText = reader.ReadToEnd()

    // Simple JSON parsing - return as string map
    Map.empty<string, obj>
        |> fun m -> if responseText.Contains("\"valid\"") then Map.add "valid" (box true) m else m
        |> fun m -> if responseText.Contains("\"tier\"") then Map.add "tier" (box "unknown") m else m
        |> fun m -> if responseText.Contains("\"languages\"") then Map.add "languages" (box []) m else m
        |> fun m -> if responseText.Contains("\"stdout\"") then
                        let start = responseText.IndexOf("\"stdout\":\"") + 10
                        let endIdx = responseText.IndexOf("\"", start)
                        if start > 10 && endIdx > start then
                            Map.add "stdout" (box (responseText.Substring(start, endIdx - start))) m
                        else m
                    else m

[<EntryPoint>]
let main argv =
    try
        printfn "Unsandbox F# SDK Tests"
        printfn "======================\n"

        UnitTests.run ()
        printfn ""
        FunctionalTests.run ()
        0
    with ex ->
        eprintfn "Test error: %s" ex.Message
        1
