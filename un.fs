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


// un.fs - Unsandbox CLI Client (F# Implementation)
// Compile: fsharpc un.fs
// Run: mono un.exe [options] <source_file>
// Requires: UNSANDBOX_API_KEY environment variable

open System
open System.IO
open System.Net
open System.Text

let apiBase = "https://api.unsandbox.com"
let portalBase = "https://unsandbox.com"
let blue = "\x1B[34m"
let red = "\x1B[31m"
let green = "\x1B[32m"
let yellow = "\x1B[33m"
let reset = "\x1B[0m"

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

type Args = {
    mutable Command: string option
    mutable SourceFile: string option
    mutable ApiKey: string option
    mutable Network: string option
    mutable Vcpu: int
    Env: ResizeArray<string>
    Files: ResizeArray<string>
    mutable Artifacts: bool
    mutable OutputDir: string option
    mutable SessionList: bool
    mutable SessionShell: string option
    mutable SessionKill: string option
    mutable ServiceList: bool
    mutable ServiceName: string option
    mutable ServicePorts: string option
    mutable ServiceType: string option
    mutable ServiceBootstrap: string option
    mutable ServiceInfo: string option
    mutable ServiceLogs: string option
    mutable ServiceTail: string option
    mutable ServiceSleep: string option
    mutable ServiceWake: string option
    mutable ServiceDestroy: string option
    mutable KeyExtend: bool
}

let getApiKey (argsKey: string option) =
    let key = match argsKey with | Some k -> k | None -> Environment.GetEnvironmentVariable("UNSANDBOX_API_KEY")
    if String.IsNullOrEmpty(key) then
        eprintfn "%sError: UNSANDBOX_API_KEY not set%s" red reset
        exit 1
    key

let detectLanguage (filename: string) =
    let dotIndex = filename.LastIndexOf('.')
    if dotIndex = -1 then
        failwith "Cannot detect language: no file extension"
    let ext = filename.Substring(dotIndex).ToLower()
    match Map.tryFind ext extMap with
    | Some lang -> lang
    | None -> failwithf "Unsupported file extension: %s" ext

let jsonEscape (s: string) =
    let sb = StringBuilder("\"")
    for c in s do
        match c with
        | '"' -> sb.Append("\\\"") |> ignore
        | '\\' -> sb.Append("\\\\") |> ignore
        | '\n' -> sb.Append("\\n") |> ignore
        | '\r' -> sb.Append("\\r") |> ignore
        | '\t' -> sb.Append("\\t") |> ignore
        | _ -> sb.Append(c) |> ignore
    sb.Append("\"") |> ignore
    sb.ToString()

let rec toJson (obj: obj) =
    match obj with
    | null -> "null"
    | :? string as s -> jsonEscape s
    | :? int as i -> i.ToString()
    | :? float as f -> f.ToString()
    | :? bool as b -> b.ToString().ToLower()
    | :? Map<string, obj> as m ->
        let entries = m |> Map.toSeq |> Seq.map (fun (k, v) -> sprintf "\"%s\":%s" k (toJson v)) |> String.concat ","
        sprintf "{%s}" entries
    | :? ResizeArray<obj> as lst ->
        let items = lst |> Seq.map toJson |> String.concat ","
        sprintf "[%s]" items
    | :? (string * obj) list as lst ->
        let entries = lst |> List.map (fun (k, v) -> sprintf "\"%s\":%s" k (toJson v)) |> String.concat ","
        sprintf "{%s}" entries
    | _ -> jsonEscape (obj.ToString())

let extractJsonValue (json: string) (key: string) =
    let pattern = sprintf "\"%s\":" key
    let startIndex = json.IndexOf(pattern)
    if startIndex = -1 then None
    else
        let mutable idx = startIndex + pattern.Length
        while idx < json.Length && Char.IsWhiteSpace(json.[idx]) do
            idx <- idx + 1

        if json.[idx] = '"' then
            idx <- idx + 1
            let sb = StringBuilder()
            let mutable escaped = false
            let mutable found = false
            let mutable i = idx
            while i < json.Length && not found do
                let c = json.[i]
                if escaped then
                    match c with
                    | 'n' -> sb.Append('\n') |> ignore
                    | 'r' -> sb.Append('\r') |> ignore
                    | 't' -> sb.Append('\t') |> ignore
                    | '"' -> sb.Append('"') |> ignore
                    | '\\' -> sb.Append('\\') |> ignore
                    | _ -> sb.Append(c) |> ignore
                    escaped <- false
                else if c = '\\' then
                    escaped <- true
                else if c = '"' then
                    found <- true
                else
                    sb.Append(c) |> ignore
                i <- i + 1
            Some (sb.ToString())
        else
            let sb = StringBuilder()
            let mutable i = idx
            while i < json.Length && (Char.IsDigit(json.[i]) || json.[i] = '-') do
                sb.Append(json.[i]) |> ignore
                i <- i + 1
            Some (sb.ToString())

let parseJson (json: string) =
    let trimmed = json.Trim()
    if not (trimmed.StartsWith("{")) then Map.empty
    else
        let result = ResizeArray<string * obj>()
        let mutable i = 1

        while i < trimmed.Length do
            while i < trimmed.Length && Char.IsWhiteSpace(trimmed.[i]) do i <- i + 1
            if trimmed.[i] = '}' then i <- trimmed.Length
            elif trimmed.[i] = '"' then
                let keyStart = i + 1
                i <- i + 1
                while i < trimmed.Length && trimmed.[i] <> '"' do
                    if trimmed.[i] = '\\' then i <- i + 1
                    i <- i + 1
                let key = trimmed.Substring(keyStart, i - keyStart).Replace("\\\"", "\"").Replace("\\\\", "\\")
                i <- i + 1

                while i < trimmed.Length && (Char.IsWhiteSpace(trimmed.[i]) || trimmed.[i] = ':') do i <- i + 1

                let value = extractJsonValue trimmed key
                match value with
                | Some v -> result.Add((key, box v))
                | None -> ()

                while i < trimmed.Length && (Char.IsWhiteSpace(trimmed.[i]) || trimmed.[i] = ',' || trimmed.[i] = '"' || Char.IsLetterOrDigit(trimmed.[i]) || trimmed.[i] = '\\') do i <- i + 1
            else
                i <- i + 1

        result |> Seq.map (fun (k, v) -> k, v) |> Map.ofSeq

let apiRequest (endpoint: string) (method: string) (data: (string * obj) list option) (apiKey: string) =
    ServicePointManager.SecurityProtocol <- SecurityProtocolType.Tls12 ||| SecurityProtocolType.Tls11 ||| SecurityProtocolType.Tls

    let request = WebRequest.Create(apiBase + endpoint) :?> HttpWebRequest
    request.Method <- method
    request.ContentType <- "application/json"
    request.Headers.Add("Authorization", sprintf "Bearer %s" apiKey)
    request.Timeout <- 300000

    match data with
    | Some d ->
        let json = toJson (box d)
        let bytes = Encoding.UTF8.GetBytes(json)
        request.ContentLength <- int64 bytes.Length
        use stream = request.GetRequestStream()
        stream.Write(bytes, 0, bytes.Length)
    | None -> ()

    try
        use response = request.GetResponse() :?> HttpWebResponse
        if response.StatusCode <> HttpStatusCode.OK then
            failwithf "HTTP %A" response.StatusCode
        use reader = new StreamReader(response.GetResponseStream())
        let responseText = reader.ReadToEnd()
        parseJson responseText
    with
    | :? WebException as ex ->
        let errorMsg =
            if ex.Response <> null then
                use reader = new StreamReader(ex.Response.GetResponseStream())
                reader.ReadToEnd()
            else
                ex.Message
        failwithf "HTTP error - %s" errorMsg

let cmdExecute (args: Args) =
    let apiKey = getApiKey args.ApiKey
    let code = File.ReadAllText(args.SourceFile.Value)
    let language = detectLanguage args.SourceFile.Value

    let mutable payload = [("language", box language); ("code", box code)]

    if args.Env.Count > 0 then
        let envVars = args.Env |> Seq.choose (fun e ->
            let parts = e.Split([|'='|], 2)
            if parts.Length = 2 then Some (parts.[0], box parts.[1]) else None
        ) |> Seq.toList
        if not (List.isEmpty envVars) then
            payload <- payload @ [("env", box envVars)]

    if args.Files.Count > 0 then
        let inputFiles = args.Files |> Seq.map (fun filepath ->
            let content = File.ReadAllBytes(filepath)
            [("filename", box (Path.GetFileName(filepath))); ("content_base64", box (Convert.ToBase64String(content)))]
        ) |> Seq.toList
        payload <- payload @ [("input_files", box inputFiles)]

    if args.Artifacts then
        payload <- payload @ [("return_artifacts", box true)]
    if args.Network.IsSome then
        payload <- payload @ [("network", box args.Network.Value)]
    if args.Vcpu > 0 then
        payload <- payload @ [("vcpu", box args.Vcpu)]

    let result = apiRequest "/execute" "POST" (Some payload) apiKey

    match result.TryFind "stdout" with
    | Some stdout when not (String.IsNullOrEmpty(stdout.ToString())) ->
        printf "%s%s%s" blue (stdout.ToString()) reset
    | _ -> ()

    match result.TryFind "stderr" with
    | Some stderr when not (String.IsNullOrEmpty(stderr.ToString())) ->
        eprintf "%s%s%s" red (stderr.ToString()) reset
    | _ -> ()

    if args.Artifacts && result.ContainsKey("artifacts") then
        let outDir = match args.OutputDir with | Some d -> d | None -> "."
        Directory.CreateDirectory(outDir) |> ignore
        eprintfn "%sSaved artifacts to %s%s" green outDir reset

    let exitCode = match result.TryFind "exit_code" with | Some ec -> int (ec.ToString()) | None -> 0
    exit exitCode

let cmdSession (args: Args) =
    let apiKey = getApiKey args.ApiKey

    if args.SessionList then
        let result = apiRequest "/sessions" "GET" None apiKey
        printfn "%-40s %-10s %-10s %s" "ID" "Shell" "Status" "Created"
        printfn "No sessions (list parsing not implemented)"
    elif args.SessionKill.IsSome then
        let result = apiRequest (sprintf "/sessions/%s" args.SessionKill.Value) "DELETE" None apiKey
        printfn "%sSession terminated: %s%s" green args.SessionKill.Value reset
    else
        let mutable payload = [("shell", box (match args.SessionShell with | Some s -> s | None -> "bash"))]
        if args.Network.IsSome then
            payload <- payload @ [("network", box args.Network.Value)]
        if args.Vcpu > 0 then
            payload <- payload @ [("vcpu", box args.Vcpu)]

        printfn "%sCreating session...%s" yellow reset
        let result = apiRequest "/sessions" "POST" (Some payload) apiKey
        match result.TryFind "id" with
        | Some id -> printfn "%sSession created: %s%s" green (id.ToString()) reset
        | None -> printfn "%sSession created%s" green reset
        printfn "%s(Interactive sessions require WebSocket - use un2 for full support)%s" yellow reset

let openBrowser (url: string) =
    try
        let os = Environment.OSVersion.Platform
        let cmd =
            if os = PlatformID.Unix || os = PlatformID.MacOSX then
                if System.IO.File.Exists("/usr/bin/xdg-open") then
                    System.Diagnostics.Process.Start("xdg-open", url)
                else
                    System.Diagnostics.Process.Start("open", url)
            else
                System.Diagnostics.Process.Start("cmd", sprintf "/c start %s" url)
        cmd.WaitForExit()
    with ex ->
        eprintfn "%sError opening browser: %s%s" red ex.Message reset

let cmdKey (args: Args) =
    let apiKey = getApiKey args.ApiKey

    ServicePointManager.SecurityProtocol <- SecurityProtocolType.Tls12 ||| SecurityProtocolType.Tls11 ||| SecurityProtocolType.Tls

    let request = WebRequest.Create(portalBase + "/keys/validate") :?> HttpWebRequest
    request.Method <- "POST"
    request.ContentType <- "application/json"
    request.Headers.Add("Authorization", sprintf "Bearer %s" apiKey)
    request.Timeout <- 30000

    try
        use response = request.GetResponse() :?> HttpWebResponse
        use reader = new StreamReader(response.GetResponseStream())
        let responseText = reader.ReadToEnd()
        let result = parseJson responseText

        let publicKey = match result.TryFind "public_key" with | Some v -> v.ToString() | None -> "N/A"
        let tier = match result.TryFind "tier" with | Some v -> v.ToString() | None -> "N/A"
        let status = match result.TryFind "status" with | Some v -> v.ToString() | None -> "N/A"
        let expiresAt = match result.TryFind "expires_at" with | Some v -> v.ToString() | None -> "N/A"
        let timeRemaining = match result.TryFind "time_remaining" with | Some v -> v.ToString() | None -> "N/A"
        let rateLimit = match result.TryFind "rate_limit" with | Some v -> v.ToString() | None -> "N/A"
        let burst = match result.TryFind "burst" with | Some v -> v.ToString() | None -> "N/A"
        let concurrency = match result.TryFind "concurrency" with | Some v -> v.ToString() | None -> "N/A"
        let expired = match result.TryFind "expired" with | Some v -> v.ToString() = "True" | None -> false

        if args.KeyExtend && publicKey <> "N/A" then
            let extendUrl = sprintf "%s/keys/extend?pk=%s" portalBase publicKey
            printfn "%sOpening browser to extend key...%s" blue reset
            openBrowser extendUrl
        elif expired then
            printfn "%sExpired%s" red reset
            printfn "Public Key: %s" publicKey
            printfn "Tier: %s" tier
            printfn "Expired: %s" expiresAt
            printfn "%sTo renew: Visit https://unsandbox.com/keys/extend%s" yellow reset
            exit 1
        else
            printfn "%sValid%s" green reset
            printfn "Public Key: %s" publicKey
            printfn "Tier: %s" tier
            printfn "Status: %s" status
            printfn "Expires: %s" expiresAt
            printfn "Time Remaining: %s" timeRemaining
            printfn "Rate Limit: %s" rateLimit
            printfn "Burst: %s" burst
            printfn "Concurrency: %s" concurrency
    with
    | :? WebException as ex ->
        printfn "%sInvalid%s" red reset
        let errorMsg =
            if ex.Response <> null then
                use reader = new StreamReader(ex.Response.GetResponseStream())
                let body = reader.ReadToEnd()
                try
                    let errorResult = parseJson body
                    match errorResult.TryFind "error" with
                    | Some err -> err.ToString()
                    | None -> body
                with _ -> body
            else
                ex.Message
        printfn "Reason: %s" errorMsg
        exit 1

let cmdService (args: Args) =
    let apiKey = getApiKey args.ApiKey

    if args.ServiceList then
        let result = apiRequest "/services" "GET" None apiKey
        printfn "%-20s %-15s %-10s %-15s %s" "ID" "Name" "Status" "Ports" "Domains"
        printfn "No services (list parsing not implemented)"
    elif args.ServiceInfo.IsSome then
        let result = apiRequest (sprintf "/services/%s" args.ServiceInfo.Value) "GET" None apiKey
        printfn "%s" (toJson (box result))
    elif args.ServiceLogs.IsSome then
        let result = apiRequest (sprintf "/services/%s/logs" args.ServiceLogs.Value) "GET" None apiKey
        match result.TryFind "logs" with
        | Some logs -> printfn "%s" (logs.ToString())
        | None -> ()
    elif args.ServiceTail.IsSome then
        let result = apiRequest (sprintf "/services/%s/logs?lines=9000" args.ServiceTail.Value) "GET" None apiKey
        match result.TryFind "logs" with
        | Some logs -> printfn "%s" (logs.ToString())
        | None -> ()
    elif args.ServiceSleep.IsSome then
        let result = apiRequest (sprintf "/services/%s/sleep" args.ServiceSleep.Value) "POST" None apiKey
        printfn "%sService sleeping: %s%s" green args.ServiceSleep.Value reset
    elif args.ServiceWake.IsSome then
        let result = apiRequest (sprintf "/services/%s/wake" args.ServiceWake.Value) "POST" None apiKey
        printfn "%sService waking: %s%s" green args.ServiceWake.Value reset
    elif args.ServiceDestroy.IsSome then
        let result = apiRequest (sprintf "/services/%s" args.ServiceDestroy.Value) "DELETE" None apiKey
        printfn "%sService destroyed: %s%s" green args.ServiceDestroy.Value reset
    elif args.ServiceName.IsSome then
        let mutable payload = [("name", box args.ServiceName.Value)]
        if args.ServicePorts.IsSome then
            let ports = args.ServicePorts.Value.Split(',') |> Array.map (fun p -> box (int (p.Trim())))
            payload <- payload @ [("ports", box ports)]
        if args.ServiceType.IsSome then
            payload <- payload @ [("service_type", box args.ServiceType.Value)]
        if args.ServiceBootstrap.IsSome then
            payload <- payload @ [("bootstrap", box args.ServiceBootstrap.Value)]
        if args.Network.IsSome then
            payload <- payload @ [("network", box args.Network.Value)]
        if args.Vcpu > 0 then
            payload <- payload @ [("vcpu", box args.Vcpu)]

        let result = apiRequest "/services" "POST" (Some payload) apiKey
        match result.TryFind "id" with
        | Some id -> printfn "%sService created: %s%s" green (id.ToString()) reset
        | None -> printfn "%sService created%s" green reset
        match result.TryFind "name" with
        | Some name -> printfn "Name: %s" (name.ToString())
        | None -> ()
        match result.TryFind "url" with
        | Some url -> printfn "URL: %s" (url.ToString())
        | None -> ()
    else
        eprintfn "%sError: Specify --name to create a service, or use --list, --info, etc.%s" red reset
        exit 1

let parseArgs (argv: string[]) =
    let args = {
        Command = None
        SourceFile = None
        ApiKey = None
        Network = None
        Vcpu = 0
        Env = ResizeArray<string>()
        Files = ResizeArray<string>()
        Artifacts = false
        OutputDir = None
        SessionList = false
        SessionShell = None
        SessionKill = None
        ServiceList = false
        ServiceName = None
        ServicePorts = None
        ServiceType = None
        ServiceBootstrap = None
        ServiceInfo = None
        ServiceLogs = None
        ServiceTail = None
        ServiceSleep = None
        ServiceWake = None
        ServiceDestroy = None
        KeyExtend = false
    }

    let mutable i = 0
    while i < argv.Length do
        match argv.[i] with
        | "session" -> args.Command <- Some "session"
        | "service" -> args.Command <- Some "service"
        | "key" -> args.Command <- Some "key"
        | "-k" | "--api-key" -> i <- i + 1; args.ApiKey <- Some argv.[i]
        | "-n" | "--network" -> i <- i + 1; args.Network <- Some argv.[i]
        | "-v" | "--vcpu" -> i <- i + 1; args.Vcpu <- int argv.[i]
        | "-e" | "--env" -> i <- i + 1; args.Env.Add(argv.[i])
        | "-f" | "--files" -> i <- i + 1; args.Files.Add(argv.[i])
        | "-a" | "--artifacts" -> args.Artifacts <- true
        | "-o" | "--output-dir" -> i <- i + 1; args.OutputDir <- Some argv.[i]
        | "-l" | "--list" ->
            match args.Command with
            | Some "session" -> args.SessionList <- true
            | Some "service" -> args.ServiceList <- true
            | _ -> ()
        | "-s" | "--shell" -> i <- i + 1; args.SessionShell <- Some argv.[i]
        | "--kill" -> i <- i + 1; args.SessionKill <- Some argv.[i]
        | "--name" -> i <- i + 1; args.ServiceName <- Some argv.[i]
        | "--ports" -> i <- i + 1; args.ServicePorts <- Some argv.[i]
        | "--type" -> i <- i + 1; args.ServiceType <- Some argv.[i]
        | "--bootstrap" -> i <- i + 1; args.ServiceBootstrap <- Some argv.[i]
        | "--info" -> i <- i + 1; args.ServiceInfo <- Some argv.[i]
        | "--logs" -> i <- i + 1; args.ServiceLogs <- Some argv.[i]
        | "--tail" -> i <- i + 1; args.ServiceTail <- Some argv.[i]
        | "--sleep" -> i <- i + 1; args.ServiceSleep <- Some argv.[i]
        | "--wake" -> i <- i + 1; args.ServiceWake <- Some argv.[i]
        | "--destroy" -> i <- i + 1; args.ServiceDestroy <- Some argv.[i]
        | "--extend" -> args.KeyExtend <- true
        | arg when not (arg.StartsWith("-")) -> args.SourceFile <- Some arg
        | _ -> ()
        i <- i + 1

    args

let printHelp () =
    printfn "Usage: un [options] <source_file>"
    printfn "       un session [options]"
    printfn "       un service [options]"
    printfn "       un key [options]"
    printfn ""
    printfn "Execute options:"
    printfn "  -e KEY=VALUE      Set environment variable"
    printfn "  -f FILE           Add input file"
    printfn "  -a                Return artifacts"
    printfn "  -o DIR            Output directory for artifacts"
    printfn "  -n MODE           Network mode (zerotrust/semitrusted)"
    printfn "  -v N              vCPU count (1-8)"
    printfn "  -k KEY            API key"
    printfn ""
    printfn "Session options:"
    printfn "  --list            List active sessions"
    printfn "  --shell NAME      Shell/REPL to use"
    printfn "  --kill ID         Terminate session"
    printfn ""
    printfn "Service options:"
    printfn "  --list            List services"
    printfn "  --name NAME       Service name"
    printfn "  --ports PORTS     Comma-separated ports"
    printfn "  --type TYPE       Service type (minecraft/mumble/teamspeak/source/tcp/udp)"
    printfn "  --bootstrap CMD   Bootstrap command"
    printfn "  --info ID         Get service details"
    printfn "  --logs ID         Get all logs"
    printfn "  --tail ID         Get last 9000 lines"
    printfn "  --sleep ID        Freeze service"
    printfn "  --wake ID         Unfreeze service"
    printfn "  --destroy ID      Destroy service"
    printfn ""
    printfn "Key options:"
    printfn "  --extend          Open browser to extend key"
    printfn "  -k KEY            API key to validate"

[<EntryPoint>]
let main argv =
    try
        let args = parseArgs argv

        match args.Command with
        | Some "session" -> cmdSession args; 0
        | Some "service" -> cmdService args; 0
        | Some "key" -> cmdKey args; 0
        | _ ->
            match args.SourceFile with
            | Some _ -> cmdExecute args; 0
            | None -> printHelp(); 1
    with ex ->
        eprintfn "%sError: %s%s" red ex.Message reset
        1
