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
open System.Security.Cryptography

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
    mutable SessionSnapshot: string option
    mutable SessionRestore: string option
    mutable SessionFrom: string option
    mutable SessionSnapshotName: string option
    mutable SessionHot: bool
    mutable ServiceList: bool
    mutable LanguagesJson: bool
    mutable ServiceName: string option
    mutable ServicePorts: string option
    mutable ServiceType: string option
    mutable ServiceBootstrap: string option
    mutable ServiceBootstrapFile: string option
    mutable ServiceInfo: string option
    mutable ServiceLogs: string option
    mutable ServiceTail: string option
    mutable ServiceSleep: string option
    mutable ServiceWake: string option
    mutable ServiceDestroy: string option
    mutable ServiceExecute: string option
    mutable ServiceCommand: string option
    mutable ServiceDumpBootstrap: string option
    mutable ServiceDumpFile: string option
    mutable ServiceResize: string option
    mutable ServiceSnapshot: string option
    mutable ServiceRestore: string option
    mutable ServiceFrom: string option
    mutable ServiceSnapshotName: string option
    mutable ServiceHot: bool
    mutable SnapshotList: bool
    mutable SnapshotInfo: string option
    mutable SnapshotDelete: string option
    mutable SnapshotClone: string option
    mutable SnapshotType: string option
    mutable SnapshotName: string option
    mutable SnapshotShell: string option
    mutable SnapshotPorts: string option
    mutable EnvFile: string option
    mutable EnvAction: string option
    mutable EnvTarget: string option
    mutable KeyExtend: bool
    // Image command options
    mutable ImageList: bool
    mutable ImageInfo: string option
    mutable ImageDelete: string option
    mutable ImageLock: string option
    mutable ImageUnlock: string option
    mutable ImagePublish: string option
    mutable ImageSourceType: string option
    mutable ImageVisibility: string option
    mutable ImageVisibilityMode: string option
    mutable ImageSpawn: string option
    mutable ImageClone: string option
    mutable ImageName: string option
    mutable ImagePorts: string option
}

let getApiKeys (argsKey: string option) =
    let publicKey = Environment.GetEnvironmentVariable("UNSANDBOX_PUBLIC_KEY")
    let secretKey = Environment.GetEnvironmentVariable("UNSANDBOX_SECRET_KEY")

    // Fall back to UNSANDBOX_API_KEY for backwards compatibility
    if String.IsNullOrEmpty(publicKey) || String.IsNullOrEmpty(secretKey) then
        let legacyKey = match argsKey with | Some k -> k | None -> Environment.GetEnvironmentVariable("UNSANDBOX_API_KEY")
        if String.IsNullOrEmpty(legacyKey) then
            eprintfn "%sError: UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY not set%s" red reset
            exit 1
        (legacyKey, null)
    else
        (publicKey, secretKey)

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

let apiRequest (endpoint: string) (method: string) (data: (string * obj) list option) (publicKey: string) (secretKey: string) =
    ServicePointManager.SecurityProtocol <- SecurityProtocolType.Tls12 ||| SecurityProtocolType.Tls11 ||| SecurityProtocolType.Tls

    let request = WebRequest.Create(apiBase + endpoint) :?> HttpWebRequest
    request.Method <- method
    request.ContentType <- "application/json"
    request.Timeout <- 300000

    let body = match data with | Some d -> toJson (box d) | None -> ""

    // Add HMAC authentication headers if secretKey is provided
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
        // Legacy API key authentication
        request.Headers.Add("Authorization", sprintf "Bearer %s" publicKey)

    match data with
    | Some d ->
        let bytes = Encoding.UTF8.GetBytes(body)
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

        // Check for clock drift error
        if errorMsg.Contains("timestamp") && (errorMsg.Contains("401") || errorMsg.Contains("expired") || errorMsg.Contains("invalid")) then
            eprintfn "%sError: Request timestamp expired (must be within 5 minutes of server time)%s" red reset
            eprintfn "%sYour computer's clock may have drifted.%s" yellow reset
            eprintfn "Check your system time and sync with NTP if needed:"
            eprintfn "  Linux:   sudo ntpdate -s time.nist.gov"
            eprintfn "  macOS:   sudo sntp -sS time.apple.com"
            eprintfn "  Windows: w32tm /resync%s" reset
            exit 1

        failwithf "HTTP error - %s" errorMsg

let apiRequestPatch (endpoint: string) (data: (string * obj) list) (publicKey: string) (secretKey: string) =
    ServicePointManager.SecurityProtocol <- SecurityProtocolType.Tls12 ||| SecurityProtocolType.Tls11 ||| SecurityProtocolType.Tls

    let request = WebRequest.Create(apiBase + endpoint) :?> HttpWebRequest
    request.Method <- "PATCH"
    request.ContentType <- "application/json"
    request.Timeout <- 300000

    let body = toJson (box data)

    // Add HMAC authentication headers if secretKey is provided
    if not (String.IsNullOrEmpty(secretKey)) then
        let timestamp = DateTimeOffset.UtcNow.ToUnixTimeSeconds()
        let message = sprintf "%d:%s:%s:%s" timestamp "PATCH" endpoint body

        use hmac = new HMACSHA256(Encoding.UTF8.GetBytes(secretKey))
        let hash = hmac.ComputeHash(Encoding.UTF8.GetBytes(message))
        let signature = BitConverter.ToString(hash).Replace("-", "").ToLower()

        request.Headers.Add("Authorization", sprintf "Bearer %s" publicKey)
        request.Headers.Add("X-Timestamp", timestamp.ToString())
        request.Headers.Add("X-Signature", signature)
    else
        request.Headers.Add("Authorization", sprintf "Bearer %s" publicKey)

    let bytes = Encoding.UTF8.GetBytes(body)
    request.ContentLength <- int64 bytes.Length
    use stream = request.GetRequestStream()
    stream.Write(bytes, 0, bytes.Length)

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

        // Check for clock drift error
        if errorMsg.Contains("timestamp") && (errorMsg.Contains("401") || errorMsg.Contains("expired") || errorMsg.Contains("invalid")) then
            eprintfn "%sError: Request timestamp expired (must be within 5 minutes of server time)%s" red reset
            eprintfn "%sYour computer's clock may have drifted.%s" yellow reset
            eprintfn "Check your system time and sync with NTP if needed:"
            eprintfn "  Linux:   sudo ntpdate -s time.nist.gov"
            eprintfn "  macOS:   sudo sntp -sS time.apple.com"
            eprintfn "  Windows: w32tm /resync%s" reset
            exit 1

        failwithf "HTTP error - %s" errorMsg

let apiRequestText (endpoint: string) (method: string) (body: string) (publicKey: string) (secretKey: string) =
    ServicePointManager.SecurityProtocol <- SecurityProtocolType.Tls12 ||| SecurityProtocolType.Tls11 ||| SecurityProtocolType.Tls

    let request = WebRequest.Create(apiBase + endpoint) :?> HttpWebRequest
    request.Method <- method
    request.ContentType <- "text/plain"
    request.Timeout <- 300000

    let bodyContent = if body = null then "" else body

    // Add HMAC authentication headers if secretKey is provided
    if not (String.IsNullOrEmpty(secretKey)) then
        let timestamp = DateTimeOffset.UtcNow.ToUnixTimeSeconds()
        let message = sprintf "%d:%s:%s:%s" timestamp method endpoint bodyContent

        use hmac = new HMACSHA256(Encoding.UTF8.GetBytes(secretKey))
        let hash = hmac.ComputeHash(Encoding.UTF8.GetBytes(message))
        let signature = BitConverter.ToString(hash).Replace("-", "").ToLower()

        request.Headers.Add("Authorization", sprintf "Bearer %s" publicKey)
        request.Headers.Add("X-Timestamp", timestamp.ToString())
        request.Headers.Add("X-Signature", signature)
    else
        request.Headers.Add("Authorization", sprintf "Bearer %s" publicKey)

    if not (String.IsNullOrEmpty(bodyContent)) then
        let bytes = Encoding.UTF8.GetBytes(bodyContent)
        request.ContentLength <- int64 bytes.Length
        use stream = request.GetRequestStream()
        stream.Write(bytes, 0, bytes.Length)

    try
        use response = request.GetResponse() :?> HttpWebResponse
        use reader = new StreamReader(response.GetResponseStream())
        reader.ReadToEnd()
    with
    | :? WebException as ex ->
        let errorMsg =
            if ex.Response <> null then
                use reader = new StreamReader(ex.Response.GetResponseStream())
                reader.ReadToEnd()
            else
                ex.Message
        failwithf "HTTP error - %s" errorMsg

let readEnvFile (path: string) =
    if not (File.Exists(path)) then
        failwithf "Env file not found: %s" path
    File.ReadAllText(path)

let buildEnvContent (envs: ResizeArray<string>) (envFile: string option) =
    let lines = ResizeArray<string>()

    // Add from -e flags
    for env in envs do
        lines.Add(env)

    // Add from --env-file
    match envFile with
    | Some path ->
        let content = readEnvFile path
        for line in content.Split('\n') do
            let trimmed = line.Trim()
            if not (String.IsNullOrEmpty(trimmed)) && not (trimmed.StartsWith("#")) then
                lines.Add(trimmed)
    | None -> ()

    String.Join("\n", lines)

let serviceEnvStatus (serviceId: string) (publicKey: string) (secretKey: string) =
    apiRequest (sprintf "/services/%s/env" serviceId) "GET" None publicKey secretKey

let serviceEnvSet (serviceId: string) (envContent: string) (publicKey: string) (secretKey: string) =
    let maxEnvContentSize = 65536
    if envContent.Length > maxEnvContentSize then
        eprintfn "%sError: Env content exceeds maximum size of 64KB%s" red reset
        false
    else
        try
            apiRequestText (sprintf "/services/%s/env" serviceId) "PUT" envContent publicKey secretKey |> ignore
            true
        with _ ->
            false

let serviceEnvExport (serviceId: string) (publicKey: string) (secretKey: string) =
    apiRequest (sprintf "/services/%s/env/export" serviceId) "POST" None publicKey secretKey

let serviceEnvDelete (serviceId: string) (publicKey: string) (secretKey: string) =
    try
        apiRequest (sprintf "/services/%s/env" serviceId) "DELETE" None publicKey secretKey |> ignore
        true
    with _ ->
        false

let cmdServiceEnv (args: Args) (publicKey: string) (secretKey: string) =
    match args.EnvAction with
    | Some "status" ->
        match args.EnvTarget with
        | Some target ->
            let result = serviceEnvStatus target publicKey secretKey
            match result.TryFind "has_vault" with
            | Some hasVault when hasVault.ToString() = "True" ->
                printfn "%sVault: configured%s" green reset
                match result.TryFind "env_count" with
                | Some count -> printfn "Variables: %s" (count.ToString())
                | None -> ()
                match result.TryFind "updated_at" with
                | Some updated -> printfn "Updated: %s" (updated.ToString())
                | None -> ()
            | _ ->
                printfn "%sVault: not configured%s" yellow reset
        | None ->
            eprintfn "%sError: service env status requires service ID%s" red reset
            exit 1
    | Some "set" ->
        match args.EnvTarget with
        | Some target ->
            if args.Env.Count = 0 && args.EnvFile.IsNone then
                eprintfn "%sError: service env set requires -e or --env-file%s" red reset
                exit 1
            let envContent = buildEnvContent args.Env args.EnvFile
            if serviceEnvSet target envContent publicKey secretKey then
                printfn "%sVault updated for service %s%s" green target reset
            else
                eprintfn "%sError: Failed to update vault%s" red reset
                exit 1
        | None ->
            eprintfn "%sError: service env set requires service ID%s" red reset
            exit 1
    | Some "export" ->
        match args.EnvTarget with
        | Some target ->
            let result = serviceEnvExport target publicKey secretKey
            match result.TryFind "content" with
            | Some content -> printf "%s" (content.ToString())
            | None -> ()
        | None ->
            eprintfn "%sError: service env export requires service ID%s" red reset
            exit 1
    | Some "delete" ->
        match args.EnvTarget with
        | Some target ->
            if serviceEnvDelete target publicKey secretKey then
                printfn "%sVault deleted for service %s%s" green target reset
            else
                eprintfn "%sError: Failed to delete vault%s" red reset
                exit 1
        | None ->
            eprintfn "%sError: service env delete requires service ID%s" red reset
            exit 1
    | Some action ->
        eprintfn "%sError: Unknown env action: %s%s" red action reset
        eprintfn "Usage: un.fs service env <status|set|export|delete> <service_id>"
        exit 1
    | None ->
        eprintfn "%sError: env action required%s" red reset
        exit 1

let cmdExecute (args: Args) =
    let (publicKey, secretKey) = getApiKeys args.ApiKey
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

    let result = apiRequest "/execute" "POST" (Some payload) publicKey secretKey

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
    let (publicKey, secretKey) = getApiKeys args.ApiKey

    if args.SessionSnapshot.IsSome then
        let mutable payload = []
        if args.SessionSnapshotName.IsSome then
            payload <- payload @ [("name", box args.SessionSnapshotName.Value)]
        if args.SessionHot then
            payload <- payload @ [("hot", box true)]
        let result = apiRequest (sprintf "/sessions/%s/snapshot" args.SessionSnapshot.Value) "POST" (Some payload) publicKey secretKey
        printfn "%sSnapshot created%s" green reset
        printfn "%s" (toJson (box result))
    elif args.SessionRestore.IsSome then
        // --restore takes snapshot ID directly, calls /snapshots/:id/restore
        let result = apiRequest (sprintf "/snapshots/%s/restore" args.SessionRestore.Value) "POST" None publicKey secretKey
        printfn "%sSession restored from snapshot%s" green reset
        printfn "%s" (toJson (box result))
    elif args.SessionList then
        let result = apiRequest "/sessions" "GET" None publicKey secretKey
        printfn "%-40s %-10s %-10s %s" "ID" "Shell" "Status" "Created"
        printfn "No sessions (list parsing not implemented)"
    elif args.SessionKill.IsSome then
        let result = apiRequest (sprintf "/sessions/%s" args.SessionKill.Value) "DELETE" None publicKey secretKey
        printfn "%sSession terminated: %s%s" green args.SessionKill.Value reset
    else
        let mutable payload = [("shell", box (match args.SessionShell with | Some s -> s | None -> "bash"))]
        if args.Network.IsSome then
            payload <- payload @ [("network", box args.Network.Value)]
        if args.Vcpu > 0 then
            payload <- payload @ [("vcpu", box args.Vcpu)]

        if args.Files.Count > 0 then
            let inputFiles = args.Files |> Seq.map (fun filepath ->
                let content = File.ReadAllBytes(filepath)
                [("filename", box (Path.GetFileName(filepath))); ("content_base64", box (Convert.ToBase64String(content)))]
            ) |> Seq.toList
            payload <- payload @ [("input_files", box inputFiles)]

        printfn "%sCreating session...%s" yellow reset
        let result = apiRequest "/sessions" "POST" (Some payload) publicKey secretKey
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

        // Check for clock drift error
        if errorMsg.Contains("timestamp") && (errorMsg.Contains("401") || errorMsg.Contains("expired") || errorMsg.Contains("invalid")) then
            eprintfn "%sError: Request timestamp expired (must be within 5 minutes of server time)%s" red reset
            eprintfn "%sYour computer's clock may have drifted.%s" yellow reset
            eprintfn "Check your system time and sync with NTP if needed:"
            eprintfn "  Linux:   sudo ntpdate -s time.nist.gov"
            eprintfn "  macOS:   sudo sntp -sS time.apple.com"
            eprintfn "  Windows: w32tm /resync%s" reset
            exit 1

        printfn "Reason: %s" errorMsg
        exit 1

let cmdLanguages (args: Args) =
    let (publicKey, secretKey) = getApiKeys args.ApiKey

    let result = apiRequest "/languages" "GET" None publicKey secretKey

    // Extract languages array from the response
    match result.TryFind "languages" with
    | Some langs ->
        // Parse the languages - they come as a string representation
        let langStr = langs.ToString()
        // Simple parsing for array of strings like: python, javascript, ...
        let languages =
            if langStr.StartsWith("[") && langStr.EndsWith("]") then
                langStr.Substring(1, langStr.Length - 2).Split(',')
                |> Array.map (fun s -> s.Trim().Trim('"'))
                |> Array.filter (fun s -> not (String.IsNullOrEmpty(s)))
            else
                [| langStr |]

        if args.LanguagesJson then
            // Output as JSON array
            let jsonArray = sprintf "[%s]" (languages |> Array.map (sprintf "\"%s\"") |> String.concat ",")
            printfn "%s" jsonArray
        else
            // Output one language per line
            for lang in languages do
                printfn "%s" lang
    | None ->
        // Fallback: try to extract from raw JSON using regex
        ()

let cmdImage (args: Args) =
    let (publicKey, secretKey) = getApiKeys args.ApiKey

    if args.ImageList then
        let result = apiRequest "/images" "GET" None publicKey secretKey
        printfn "%s" (toJson (box result))
    elif args.ImageInfo.IsSome then
        let result = apiRequest (sprintf "/images/%s" args.ImageInfo.Value) "GET" None publicKey secretKey
        printfn "%s" (toJson (box result))
    elif args.ImageDelete.IsSome then
        let result = apiRequest (sprintf "/images/%s" args.ImageDelete.Value) "DELETE" None publicKey secretKey
        printfn "%sImage deleted: %s%s" green args.ImageDelete.Value reset
    elif args.ImageLock.IsSome then
        let result = apiRequest (sprintf "/images/%s/lock" args.ImageLock.Value) "POST" None publicKey secretKey
        printfn "%sImage locked: %s%s" green args.ImageLock.Value reset
    elif args.ImageUnlock.IsSome then
        let result = apiRequest (sprintf "/images/%s/unlock" args.ImageUnlock.Value) "POST" None publicKey secretKey
        printfn "%sImage unlocked: %s%s" green args.ImageUnlock.Value reset
    elif args.ImagePublish.IsSome then
        if args.ImageSourceType.IsNone then
            eprintfn "%sError: --source-type required (service or snapshot)%s" red reset
            exit 1
        let mutable payload = [("source_type", box args.ImageSourceType.Value); ("source_id", box args.ImagePublish.Value)]
        if args.ImageName.IsSome then
            payload <- payload @ [("name", box args.ImageName.Value)]
        let result = apiRequest "/images/publish" "POST" (Some payload) publicKey secretKey
        printfn "%sImage published%s" green reset
        printfn "%s" (toJson (box result))
    elif args.ImageVisibility.IsSome then
        if args.ImageVisibilityMode.IsNone then
            eprintfn "%sError: --visibility requires MODE (private, unlisted, or public)%s" red reset
            exit 1
        let payload = [("visibility", box args.ImageVisibilityMode.Value)]
        let result = apiRequest (sprintf "/images/%s/visibility" args.ImageVisibility.Value) "POST" (Some payload) publicKey secretKey
        printfn "%sImage visibility set to %s%s" green args.ImageVisibilityMode.Value reset
    elif args.ImageSpawn.IsSome then
        let mutable payload = []
        if args.ImageName.IsSome then
            payload <- payload @ [("name", box args.ImageName.Value)]
        if args.ImagePorts.IsSome then
            let ports = args.ImagePorts.Value.Split(',') |> Array.map (fun p -> box (int (p.Trim())))
            payload <- payload @ [("ports", box ports)]
        let result = apiRequest (sprintf "/images/%s/spawn" args.ImageSpawn.Value) "POST" (Some payload) publicKey secretKey
        printfn "%sService spawned from image%s" green reset
        printfn "%s" (toJson (box result))
    elif args.ImageClone.IsSome then
        let mutable payload = []
        if args.ImageName.IsSome then
            payload <- payload @ [("name", box args.ImageName.Value)]
        let result = apiRequest (sprintf "/images/%s/clone" args.ImageClone.Value) "POST" (Some payload) publicKey secretKey
        printfn "%sImage cloned%s" green reset
        printfn "%s" (toJson (box result))
    else
        eprintfn "%sError: Use --list, --info ID, --delete ID, --lock ID, --unlock ID, --publish ID, --visibility ID MODE, --spawn ID, or --clone ID%s" red reset
        exit 1

let cmdSnapshot (args: Args) =
    let (publicKey, secretKey) = getApiKeys args.ApiKey

    if args.SnapshotList then
        let result = apiRequest "/snapshots" "GET" None publicKey secretKey
        printfn "%s" (toJson (box result))
    elif args.SnapshotInfo.IsSome then
        let result = apiRequest (sprintf "/snapshots/%s" args.SnapshotInfo.Value) "GET" None publicKey secretKey
        printfn "%s" (toJson (box result))
    elif args.SnapshotDelete.IsSome then
        let result = apiRequest (sprintf "/snapshots/%s" args.SnapshotDelete.Value) "DELETE" None publicKey secretKey
        printfn "%sSnapshot deleted: %s%s" green args.SnapshotDelete.Value reset
    elif args.SnapshotClone.IsSome then
        if args.SnapshotType.IsNone then
            eprintfn "%sError: --type required (session or service)%s" red reset
            exit 1
        let mutable payload = [("type", box args.SnapshotType.Value)]
        if args.SnapshotName.IsSome then
            payload <- payload @ [("name", box args.SnapshotName.Value)]
        if args.SnapshotShell.IsSome then
            payload <- payload @ [("shell", box args.SnapshotShell.Value)]
        if args.SnapshotPorts.IsSome then
            let ports = args.SnapshotPorts.Value.Split(',') |> Array.map (fun p -> box (int (p.Trim())))
            payload <- payload @ [("ports", box ports)]
        let result = apiRequest (sprintf "/snapshots/%s/clone" args.SnapshotClone.Value) "POST" (Some payload) publicKey secretKey
        printfn "%sCreated from snapshot%s" green reset
        printfn "%s" (toJson (box result))
    else
        eprintfn "%sError: Use --list, --info ID, --delete ID, or --clone ID --type TYPE%s" red reset
        exit 1

let cmdService (args: Args) =
    let (publicKey, secretKey) = getApiKeys args.ApiKey

    // Handle env subcommand
    if args.EnvAction.IsSome then
        cmdServiceEnv args publicKey secretKey
    elif args.ServiceSnapshot.IsSome then
        let mutable payload = []
        if args.ServiceSnapshotName.IsSome then
            payload <- payload @ [("name", box args.ServiceSnapshotName.Value)]
        if args.ServiceHot then
            payload <- payload @ [("hot", box true)]
        let result = apiRequest (sprintf "/services/%s/snapshot" args.ServiceSnapshot.Value) "POST" (Some payload) publicKey secretKey
        printfn "%sSnapshot created%s" green reset
        printfn "%s" (toJson (box result))
    elif args.ServiceRestore.IsSome then
        // --restore takes snapshot ID directly, calls /snapshots/:id/restore
        let result = apiRequest (sprintf "/snapshots/%s/restore" args.ServiceRestore.Value) "POST" None publicKey secretKey
        printfn "%sService restored from snapshot%s" green reset
        printfn "%s" (toJson (box result))
    elif args.ServiceList then
        let result = apiRequest "/services" "GET" None publicKey secretKey
        printfn "%-20s %-15s %-10s %-15s %s" "ID" "Name" "Status" "Ports" "Domains"
        printfn "No services (list parsing not implemented)"
    elif args.ServiceInfo.IsSome then
        let result = apiRequest (sprintf "/services/%s" args.ServiceInfo.Value) "GET" None publicKey secretKey
        printfn "%s" (toJson (box result))
    elif args.ServiceLogs.IsSome then
        let result = apiRequest (sprintf "/services/%s/logs" args.ServiceLogs.Value) "GET" None publicKey secretKey
        match result.TryFind "logs" with
        | Some logs -> printfn "%s" (logs.ToString())
        | None -> ()
    elif args.ServiceTail.IsSome then
        let result = apiRequest (sprintf "/services/%s/logs?lines=9000" args.ServiceTail.Value) "GET" None publicKey secretKey
        match result.TryFind "logs" with
        | Some logs -> printfn "%s" (logs.ToString())
        | None -> ()
    elif args.ServiceSleep.IsSome then
        let result = apiRequest (sprintf "/services/%s/freeze" args.ServiceSleep.Value) "POST" None publicKey secretKey
        printfn "%sService frozen: %s%s" green args.ServiceSleep.Value reset
    elif args.ServiceWake.IsSome then
        let result = apiRequest (sprintf "/services/%s/unfreeze" args.ServiceWake.Value) "POST" None publicKey secretKey
        printfn "%sService unfreezing: %s%s" green args.ServiceWake.Value reset
    elif args.ServiceDestroy.IsSome then
        let result = apiRequest (sprintf "/services/%s" args.ServiceDestroy.Value) "DELETE" None publicKey secretKey
        printfn "%sService destroyed: %s%s" green args.ServiceDestroy.Value reset
    elif args.ServiceResize.IsSome then
        if args.Vcpu <= 0 then
            eprintfn "%sError: --resize requires --vcpu N (1-8)%s" red reset
            exit 1
        let payload = [("vcpu", box args.Vcpu)]
        let result = apiRequestPatch (sprintf "/services/%s" args.ServiceResize.Value) payload publicKey secretKey
        let ram = args.Vcpu * 2
        printfn "%sService resized to %d vCPU, %d GB RAM%s" green args.Vcpu ram reset
    elif args.ServiceExecute.IsSome then
        let payload = [("command", box args.ServiceCommand.Value)]
        let result = apiRequest (sprintf "/services/%s/execute" args.ServiceExecute.Value) "POST" (Some payload) publicKey secretKey
        match result.TryFind "stdout" with
        | Some stdout when not (String.IsNullOrEmpty(stdout.ToString())) ->
            printf "%s%s%s" blue (stdout.ToString()) reset
        | _ -> ()
        match result.TryFind "stderr" with
        | Some stderr when not (String.IsNullOrEmpty(stderr.ToString())) ->
            eprintf "%s%s%s" red (stderr.ToString()) reset
        | _ -> ()
    elif args.ServiceDumpBootstrap.IsSome then
        eprintfn "Fetching bootstrap script from %s..." args.ServiceDumpBootstrap.Value
        let payload = [("command", box "cat /tmp/bootstrap.sh")]
        let result = apiRequest (sprintf "/services/%s/execute" args.ServiceDumpBootstrap.Value) "POST" (Some payload) publicKey secretKey

        match result.TryFind "stdout" with
        | Some bootstrap when not (String.IsNullOrEmpty(bootstrap.ToString())) ->
            let bootstrapText = bootstrap.ToString()
            if args.ServiceDumpFile.IsSome then
                try
                    File.WriteAllText(args.ServiceDumpFile.Value, bootstrapText)
                    printfn "Bootstrap saved to %s" args.ServiceDumpFile.Value
                with ex ->
                    eprintfn "%sError: Could not write to %s: %s%s" red args.ServiceDumpFile.Value ex.Message reset
                    exit 1
            else
                printf "%s" bootstrapText
        | _ ->
            eprintfn "%sError: Failed to fetch bootstrap (service not running or no bootstrap file)%s" red reset
            exit 1
    elif args.ServiceName.IsSome then
        let mutable payload = [("name", box args.ServiceName.Value)]
        if args.ServicePorts.IsSome then
            let ports = args.ServicePorts.Value.Split(',') |> Array.map (fun p -> box (int (p.Trim())))
            payload <- payload @ [("ports", box ports)]
        if args.ServiceType.IsSome then
            payload <- payload @ [("service_type", box args.ServiceType.Value)]
        if args.ServiceBootstrap.IsSome then
            payload <- payload @ [("bootstrap", box args.ServiceBootstrap.Value)]
        if args.ServiceBootstrapFile.IsSome then
            if File.Exists(args.ServiceBootstrapFile.Value) then
                let content = File.ReadAllText(args.ServiceBootstrapFile.Value)
                payload <- payload @ [("bootstrap_content", box content)]
            else
                eprintfn "%sError: Bootstrap file not found: %s%s" red args.ServiceBootstrapFile.Value reset
                exit 1
        if args.Files.Count > 0 then
            let inputFiles = args.Files |> Seq.map (fun filepath ->
                let content = File.ReadAllBytes(filepath)
                [("filename", box (Path.GetFileName(filepath))); ("content_base64", box (Convert.ToBase64String(content)))]
            ) |> Seq.toList
            payload <- payload @ [("input_files", box inputFiles)]
        if args.Network.IsSome then
            payload <- payload @ [("network", box args.Network.Value)]
        if args.Vcpu > 0 then
            payload <- payload @ [("vcpu", box args.Vcpu)]

        let result = apiRequest "/services" "POST" (Some payload) publicKey secretKey
        let serviceId = match result.TryFind "id" with | Some id -> Some (id.ToString()) | None -> None
        match serviceId with
        | Some id -> printfn "%sService created: %s%s" green id reset
        | None -> printfn "%sService created%s" green reset
        match result.TryFind "name" with
        | Some name -> printfn "Name: %s" (name.ToString())
        | None -> ()
        match result.TryFind "url" with
        | Some url -> printfn "URL: %s" (url.ToString())
        | None -> ()

        // Auto-set vault if env vars were provided
        match serviceId with
        | Some id when args.Env.Count > 0 || args.EnvFile.IsSome ->
            let envContent = buildEnvContent args.Env args.EnvFile
            if not (String.IsNullOrEmpty(envContent)) then
                if serviceEnvSet id envContent publicKey secretKey then
                    printfn "%sVault configured with environment variables%s" green reset
                else
                    eprintfn "%sWarning: Failed to set vault%s" yellow reset
        | _ -> ()
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
        SessionSnapshot = None
        SessionRestore = None
        SessionFrom = None
        SessionSnapshotName = None
        SessionHot = false
        ServiceList = false
        LanguagesJson = false
        ServiceName = None
        ServicePorts = None
        ServiceType = None
        ServiceBootstrap = None
        ServiceBootstrapFile = None
        ServiceInfo = None
        ServiceLogs = None
        ServiceTail = None
        ServiceSleep = None
        ServiceWake = None
        ServiceDestroy = None
        ServiceExecute = None
        ServiceCommand = None
        ServiceDumpBootstrap = None
        ServiceDumpFile = None
        ServiceResize = None
        ServiceSnapshot = None
        ServiceRestore = None
        ServiceFrom = None
        ServiceSnapshotName = None
        ServiceHot = false
        SnapshotList = false
        SnapshotInfo = None
        SnapshotDelete = None
        SnapshotClone = None
        SnapshotType = None
        SnapshotName = None
        SnapshotShell = None
        SnapshotPorts = None
        EnvFile = None
        EnvAction = None
        EnvTarget = None
        KeyExtend = false
        ImageList = false
        ImageInfo = None
        ImageDelete = None
        ImageLock = None
        ImageUnlock = None
        ImagePublish = None
        ImageSourceType = None
        ImageVisibility = None
        ImageVisibilityMode = None
        ImageSpawn = None
        ImageClone = None
        ImageName = None
        ImagePorts = None
    }

    let mutable i = 0
    while i < argv.Length do
        match argv.[i] with
        | "session" -> args.Command <- Some "session"
        | "service" -> args.Command <- Some "service"
        | "snapshot" -> args.Command <- Some "snapshot"
        | "image" -> args.Command <- Some "image"
        | "key" -> args.Command <- Some "key"
        | "languages" -> args.Command <- Some "languages"
        | "--json" when args.Command = Some "languages" -> args.LanguagesJson <- true
        | "env" when args.Command = Some "service" ->
            // Parse: service env <action> <target>
            if i + 1 < argv.Length && not (argv.[i + 1].StartsWith("-")) then
                i <- i + 1
                args.EnvAction <- Some argv.[i]
                if i + 1 < argv.Length && not (argv.[i + 1].StartsWith("-")) then
                    i <- i + 1
                    args.EnvTarget <- Some argv.[i]
        | "-k" | "--api-key" -> i <- i + 1; args.ApiKey <- Some argv.[i]
        | "-n" | "--network" -> i <- i + 1; args.Network <- Some argv.[i]
        | "-v" | "--vcpu" -> i <- i + 1; args.Vcpu <- int argv.[i]
        | "-e" | "--env" -> i <- i + 1; args.Env.Add(argv.[i])
        | "--env-file" -> i <- i + 1; args.EnvFile <- Some argv.[i]
        | "-f" | "--files" -> i <- i + 1; args.Files.Add(argv.[i])
        | "-a" | "--artifacts" -> args.Artifacts <- true
        | "-o" | "--output-dir" -> i <- i + 1; args.OutputDir <- Some argv.[i]
        | "-l" | "--list" ->
            match args.Command with
            | Some "session" -> args.SessionList <- true
            | Some "service" -> args.ServiceList <- true
            | Some "image" -> args.ImageList <- true
            | Some "snapshot" -> args.SnapshotList <- true
            | _ -> ()
        | "-s" | "--shell" ->
            i <- i + 1
            match args.Command with
            | Some "snapshot" -> args.SnapshotShell <- Some argv.[i]
            | _ -> args.SessionShell <- Some argv.[i]
        | "--kill" -> i <- i + 1; args.SessionKill <- Some argv.[i]
        | "--snapshot" ->
            i <- i + 1
            match args.Command with
            | Some "session" -> args.SessionSnapshot <- Some argv.[i]
            | Some "service" -> args.ServiceSnapshot <- Some argv.[i]
            | _ -> ()
        | "--restore" ->
            i <- i + 1
            match args.Command with
            | Some "session" -> args.SessionRestore <- Some argv.[i]
            | Some "service" -> args.ServiceRestore <- Some argv.[i]
            | _ -> ()
        | "--from" ->
            i <- i + 1
            match args.Command with
            | Some "session" -> args.SessionFrom <- Some argv.[i]
            | Some "service" -> args.ServiceFrom <- Some argv.[i]
            | _ -> ()
        | "--snapshot-name" ->
            i <- i + 1
            match args.Command with
            | Some "session" -> args.SessionSnapshotName <- Some argv.[i]
            | Some "service" -> args.ServiceSnapshotName <- Some argv.[i]
            | _ -> ()
        | "--hot" ->
            match args.Command with
            | Some "session" -> args.SessionHot <- true
            | Some "service" -> args.ServiceHot <- true
            | _ -> ()
        | "--info" ->
            i <- i + 1
            match args.Command with
            | Some "snapshot" -> args.SnapshotInfo <- Some argv.[i]
            | _ -> args.ServiceInfo <- Some argv.[i]
        | "--delete" ->
            i <- i + 1
            match args.Command with
            | Some "snapshot" -> args.SnapshotDelete <- Some argv.[i]
            | _ -> ()
        | "--clone" -> i <- i + 1; args.SnapshotClone <- Some argv.[i]
        | "--type" ->
            i <- i + 1
            match args.Command with
            | Some "snapshot" -> args.SnapshotType <- Some argv.[i]
            | _ -> args.ServiceType <- Some argv.[i]
        | "--name" ->
            i <- i + 1
            match args.Command with
            | Some "snapshot" -> args.SnapshotName <- Some argv.[i]
            | Some "image" -> args.ImageName <- Some argv.[i]
            | _ -> args.ServiceName <- Some argv.[i]
        | "--ports" ->
            i <- i + 1
            match args.Command with
            | Some "snapshot" -> args.SnapshotPorts <- Some argv.[i]
            | Some "image" -> args.ImagePorts <- Some argv.[i]
            | _ -> args.ServicePorts <- Some argv.[i]
        | "--bootstrap" -> i <- i + 1; args.ServiceBootstrap <- Some argv.[i]
        | "--bootstrap-file" -> i <- i + 1; args.ServiceBootstrapFile <- Some argv.[i]
        | "--logs" -> i <- i + 1; args.ServiceLogs <- Some argv.[i]
        | "--tail" -> i <- i + 1; args.ServiceTail <- Some argv.[i]
        | "--freeze" -> i <- i + 1; args.ServiceSleep <- Some argv.[i]
        | "--unfreeze" -> i <- i + 1; args.ServiceWake <- Some argv.[i]
        | "--destroy" -> i <- i + 1; args.ServiceDestroy <- Some argv.[i]
        | "--resize" -> i <- i + 1; args.ServiceResize <- Some argv.[i]
        | "--execute" -> i <- i + 1; args.ServiceExecute <- Some argv.[i]
        | "--command" -> i <- i + 1; args.ServiceCommand <- Some argv.[i]
        | "--dump-bootstrap" -> i <- i + 1; args.ServiceDumpBootstrap <- Some argv.[i]
        | "--dump-file" -> i <- i + 1; args.ServiceDumpFile <- Some argv.[i]
        | "--extend" -> args.KeyExtend <- true
        | "--info" ->
            i <- i + 1
            match args.Command with
            | Some "image" -> args.ImageInfo <- Some argv.[i]
            | _ -> args.ServiceInfo <- Some argv.[i]
        | "--delete" ->
            i <- i + 1
            match args.Command with
            | Some "image" -> args.ImageDelete <- Some argv.[i]
            | Some "snapshot" -> args.SnapshotDelete <- Some argv.[i]
            | _ -> ()
        | "--lock" ->
            i <- i + 1
            if args.Command = Some "image" then
                args.ImageLock <- Some argv.[i]
        | "--unlock" ->
            i <- i + 1
            if args.Command = Some "image" then
                args.ImageUnlock <- Some argv.[i]
        | "--publish" ->
            i <- i + 1
            if args.Command = Some "image" then
                args.ImagePublish <- Some argv.[i]
        | "--source-type" ->
            i <- i + 1
            args.ImageSourceType <- Some argv.[i]
        | "--visibility" ->
            i <- i + 1
            if args.Command = Some "image" then
                args.ImageVisibility <- Some argv.[i]
                if i + 1 < argv.Length && not (argv.[i + 1].StartsWith("-")) then
                    i <- i + 1
                    args.ImageVisibilityMode <- Some argv.[i]
        | "--spawn" ->
            i <- i + 1
            if args.Command = Some "image" then
                args.ImageSpawn <- Some argv.[i]
        | "--clone" ->
            i <- i + 1
            match args.Command with
            | Some "image" -> args.ImageClone <- Some argv.[i]
            | Some "snapshot" -> args.SnapshotClone <- Some argv.[i]
            | _ -> ()
        | arg when not (arg.StartsWith("-")) -> args.SourceFile <- Some arg
        | arg ->
            if arg.StartsWith("-") && args.Command = Some "session" then
                eprintfn "Unknown option: %s" arg
                eprintfn "Usage: un.fs session [options]"
                Environment.Exit(1)
        i <- i + 1

    args

let printHelp () =
    printfn "Usage: un [options] <source_file>"
    printfn "       un session [options]"
    printfn "       un service [options]"
    printfn "       un service env <action> <service_id> [options]"
    printfn "       un image [options]"
    printfn "       un key [options]"
    printfn "       un languages [--json]"
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
    printfn "  --freeze ID       Freeze service"
    printfn "  --unfreeze ID     Unfreeze service"
    printfn "  --destroy ID      Destroy service"
    printfn "  --resize ID       Resize service (requires --vcpu N)"
    printfn "  --execute ID      Execute command in service"
    printfn "  --command CMD     Command to execute (with --execute)"
    printfn "  --dump-bootstrap ID   Dump bootstrap script"
    printfn "  --dump-file FILE      File to save bootstrap (with --dump-bootstrap)"
    printfn "  -e KEY=VALUE      Set vault env var (with --name or env set)"
    printfn "  --env-file FILE   Load vault vars from file"
    printfn ""
    printfn "Service env commands:"
    printfn "  env status ID     Check vault status"
    printfn "  env set ID        Set vault (use -e or --env-file)"
    printfn "  env export ID     Export vault contents"
    printfn "  env delete ID     Delete vault"
    printfn ""
    printfn "Image options:"
    printfn "  -l, --list            List all images"
    printfn "  --info ID             Get image details"
    printfn "  --delete ID           Delete an image"
    printfn "  --lock ID             Lock image to prevent deletion"
    printfn "  --unlock ID           Unlock image"
    printfn "  --publish ID          Publish image (requires --source-type)"
    printfn "  --source-type TYPE    Source type: service or snapshot"
    printfn "  --visibility ID MODE  Set visibility: private, unlisted, or public"
    printfn "  --spawn ID            Spawn new service from image"
    printfn "  --clone ID            Clone an image"
    printfn "  --name NAME           Name for spawned service or cloned image"
    printfn "  --ports PORTS         Ports for spawned service"
    printfn ""
    printfn "Key options:"
    printfn "  --extend          Open browser to extend key"
    printfn "  -k KEY            API key to validate"
    printfn ""
    printfn "Languages options:"
    printfn "  --json            Output as JSON array"

[<EntryPoint>]
let main argv =
    try
        let args = parseArgs argv

        match args.Command with
        | Some "session" -> cmdSession args; 0
        | Some "service" -> cmdService args; 0
        | Some "snapshot" -> cmdSnapshot args; 0
        | Some "image" -> cmdImage args; 0
        | Some "key" -> cmdKey args; 0
        | Some "languages" -> cmdLanguages args; 0
        | _ ->
            match args.SourceFile with
            | Some _ -> cmdExecute args; 0
            | None -> printHelp(); 1
    with ex ->
        eprintfn "%sError: %s%s" red ex.Message reset
        1
