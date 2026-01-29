// PUBLIC DOMAIN - NO LICENSE, NO WARRANTY
//
// Un.cs - Unsandbox CLI Client (.NET 10 Synchronous Implementation)
// Build: dotnet build
// Run: dotnet run -- [options] <source_file>
// Requires: UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY environment variables

using System.Security.Cryptography;
using System.Text;
using System.Text.Json;
using System.Text.Json.Serialization;

const string API_BASE = "https://api.unsandbox.com";
const string PORTAL_BASE = "https://unsandbox.com";
const string VERSION = "4.2.39";

// ANSI colors
const string BLUE = "\x1B[34m";
const string RED = "\x1B[31m";
const string GREEN = "\x1B[32m";
const string YELLOW = "\x1B[33m";
const string RESET = "\x1B[0m";

var extMap = new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase)
{
    [".py"] = "python", [".js"] = "javascript", [".ts"] = "typescript",
    [".rb"] = "ruby", [".php"] = "php", [".pl"] = "perl", [".lua"] = "lua",
    [".sh"] = "bash", [".go"] = "go", [".rs"] = "rust", [".c"] = "c",
    [".cpp"] = "cpp", [".cc"] = "cpp", [".cxx"] = "cpp",
    [".java"] = "java", [".kt"] = "kotlin", [".cs"] = "dotnet", [".fs"] = "fsharp",
    [".hs"] = "haskell", [".ml"] = "ocaml", [".clj"] = "clojure", [".scm"] = "scheme",
    [".lisp"] = "commonlisp", [".erl"] = "erlang", [".ex"] = "elixir", [".exs"] = "elixir",
    [".jl"] = "julia", [".r"] = "r", [".R"] = "r", [".cr"] = "crystal",
    [".d"] = "d", [".nim"] = "nim", [".zig"] = "zig", [".v"] = "v",
    [".dart"] = "dart", [".groovy"] = "groovy", [".scala"] = "scala",
    [".f90"] = "fortran", [".f95"] = "fortran", [".cob"] = "cobol",
    [".pro"] = "prolog", [".forth"] = "forth", [".4th"] = "forth",
    [".tcl"] = "tcl", [".raku"] = "raku", [".m"] = "objc"
};

var jsonOptions = new JsonSerializerOptions
{
    PropertyNamingPolicy = JsonNamingPolicy.SnakeCaseLower,
    PropertyNameCaseInsensitive = true,
    DefaultIgnoreCondition = JsonIgnoreCondition.WhenWritingNull
};

using var httpClient = new HttpClient { BaseAddress = new Uri(API_BASE), Timeout = TimeSpan.FromMinutes(5) };

try
{
    var parsedArgs = ParseArgs(args);

    if (parsedArgs.ShowHelp)
    {
        PrintHelp();
        return 0;
    }

    if (parsedArgs.ShowVersion)
    {
        Console.WriteLine($"un {VERSION} (.NET 10 sync)");
        return 0;
    }

    switch (parsedArgs.Command)
    {
        case "session": CmdSession(parsedArgs); break;
        case "service": CmdService(parsedArgs); break;
        case "snapshot": CmdSnapshot(parsedArgs); break;
        case "image": CmdImage(parsedArgs); break;
        case "languages": CmdLanguages(parsedArgs); break;
        case "key": CmdKey(parsedArgs); break;
        default:
            if (parsedArgs.SourceFile != null) CmdExecute(parsedArgs);
            else { PrintHelp(); return 1; }
            break;
    }

    return 0;
}
catch (Exception ex)
{
    Console.Error.WriteLine($"{RED}Error: {ex.Message}{RESET}");
    return 1;
}

void CmdExecute(Args args)
{
    var (publicKey, secretKey) = GetApiKeys(args.ApiKey);
    var code = File.ReadAllText(args.SourceFile!);
    var language = DetectLanguage(args.SourceFile!);

    var payload = new Dictionary<string, object> { ["language"] = language, ["code"] = code };

    if (args.Env.Count > 0)
    {
        var envVars = args.Env
            .Select(e => e.Split('=', 2))
            .Where(p => p.Length == 2)
            .ToDictionary(p => p[0], p => p[1]);
        if (envVars.Count > 0) payload["env"] = envVars;
    }

    if (args.Files.Count > 0)
    {
        var inputFiles = new List<Dictionary<string, string>>();
        foreach (var filepath in args.Files)
        {
            var content = File.ReadAllBytes(filepath);
            inputFiles.Add(new Dictionary<string, string>
            {
                ["filename"] = Path.GetFileName(filepath),
                ["content_base64"] = Convert.ToBase64String(content)
            });
        }
        payload["input_files"] = inputFiles;
    }

    if (args.Artifacts) payload["return_artifacts"] = true;
    if (args.Network != null) payload["network"] = args.Network;
    if (args.Vcpu > 0) payload["vcpu"] = args.Vcpu;

    var result = ApiRequest("/execute", HttpMethod.Post, payload, publicKey, secretKey);

    if (result.TryGetValue("stdout", out var stdout) && stdout is JsonElement stdoutEl)
        Console.Write($"{BLUE}{stdoutEl.GetString()}{RESET}");
    if (result.TryGetValue("stderr", out var stderr) && stderr is JsonElement stderrEl)
        Console.Error.Write($"{RED}{stderrEl.GetString()}{RESET}");

    if (args.Artifacts && result.TryGetValue("artifacts", out var artifacts) && artifacts is JsonElement artifactsEl)
    {
        var outDir = args.OutputDir ?? ".";
        Directory.CreateDirectory(outDir);
        foreach (var artifact in artifactsEl.EnumerateArray())
        {
            var filename = artifact.GetProperty("filename").GetString() ?? "artifact";
            var contentB64 = artifact.GetProperty("content_base64").GetString() ?? "";
            var path = Path.Combine(outDir, filename);
            File.WriteAllBytes(path, Convert.FromBase64String(contentB64));
            Console.Error.WriteLine($"{GREEN}Saved: {path}{RESET}");
        }
    }

    var exitCode = result.TryGetValue("exit_code", out var ec) && ec is JsonElement ecEl ? ecEl.GetInt32() : 0;
    Environment.Exit(exitCode);
}

void CmdSession(Args args)
{
    var (publicKey, secretKey) = GetApiKeys(args.ApiKey);

    if (args.SessionList)
    {
        var result = ApiRequest("/sessions", HttpMethod.Get, null, publicKey, secretKey);
        if (result.TryGetValue("sessions", out var sessions) && sessions is JsonElement sessionsEl)
        {
            var sessionList = sessionsEl.EnumerateArray().ToList();
            if (sessionList.Count == 0) { Console.WriteLine("No active sessions"); return; }
            Console.WriteLine($"{"ID",-40} {"Shell",-10} {"Status",-10} {"Created"}");
            foreach (var s in sessionList)
                Console.WriteLine($"{GetStr(s, "id"),-40} {GetStr(s, "shell"),-10} {GetStr(s, "status"),-10} {GetStr(s, "created_at")}");
        }
        return;
    }

    if (args.SessionKill != null)
    {
        ApiRequest($"/sessions/{args.SessionKill}", HttpMethod.Delete, null, publicKey, secretKey);
        Console.WriteLine($"{GREEN}Session terminated: {args.SessionKill}{RESET}");
        return;
    }

    if (args.SessionFreeze != null)
    {
        ApiRequest($"/sessions/{args.SessionFreeze}/freeze", HttpMethod.Post, null, publicKey, secretKey);
        Console.WriteLine($"{GREEN}Session frozen: {args.SessionFreeze}{RESET}");
        return;
    }

    if (args.SessionUnfreeze != null)
    {
        ApiRequest($"/sessions/{args.SessionUnfreeze}/unfreeze", HttpMethod.Post, null, publicKey, secretKey);
        Console.WriteLine($"{GREEN}Session unfreezing: {args.SessionUnfreeze}{RESET}");
        return;
    }

    if (args.SessionBoost != null)
    {
        ApiRequest($"/sessions/{args.SessionBoost}/boost", HttpMethod.Post, null, publicKey, secretKey);
        Console.WriteLine($"{GREEN}Session boosted: {args.SessionBoost}{RESET}");
        return;
    }

    if (args.SessionUnboost != null)
    {
        ApiRequest($"/sessions/{args.SessionUnboost}/unboost", HttpMethod.Post, null, publicKey, secretKey);
        Console.WriteLine($"{GREEN}Session unboosted: {args.SessionUnboost}{RESET}");
        return;
    }

    if (args.SessionSnapshot != null)
    {
        var payload = new Dictionary<string, object>();
        if (args.SnapshotName != null) payload["name"] = args.SnapshotName;
        if (args.SnapshotHot) payload["hot"] = true;

        var result = ApiRequest($"/sessions/{args.SessionSnapshot}/snapshot", HttpMethod.Post, payload, publicKey, secretKey);
        var id = result.TryGetValue("id", out var idObj) && idObj is JsonElement idEl ? idEl.GetString() : "unknown";
        Console.WriteLine($"{GREEN}Snapshot created: {id}{RESET}");
        return;
    }

    var createPayload = new Dictionary<string, object> { ["shell"] = args.SessionShell ?? "bash" };
    if (args.Network != null) createPayload["network"] = args.Network;
    if (args.Vcpu > 0) createPayload["vcpu"] = args.Vcpu;

    Console.WriteLine($"{YELLOW}Creating session...{RESET}");
    var createResult = ApiRequest("/sessions", HttpMethod.Post, createPayload, publicKey, secretKey);
    var sessionId = createResult.TryGetValue("id", out var sid) && sid is JsonElement sidEl ? sidEl.GetString() : "unknown";
    Console.WriteLine($"{GREEN}Session created: {sessionId}{RESET}");
    Console.WriteLine($"{YELLOW}(Interactive sessions require WebSocket - use un2 for full support){RESET}");
}

void CmdKey(Args args)
{
    var (publicKey, secretKey) = GetApiKeys(args.ApiKey);
    var result = ApiRequest("/keys/validate", HttpMethod.Post, null, publicKey, secretKey);

    if (!result.TryGetValue("valid", out var validObj) || validObj is not JsonElement validEl)
    {
        Console.Error.WriteLine($"{RED}Error: Invalid response from server{RESET}");
        Environment.Exit(1);
    }

    var isValid = validEl.GetBoolean();
    var isExpired = result.TryGetValue("expired", out var expObj) && expObj is JsonElement expEl && expEl.GetBoolean();

    if (isValid && !isExpired)
    {
        Console.WriteLine($"{GREEN}Valid{RESET}");
        if (result.TryGetValue("public_key", out var pk) && pk is JsonElement pkEl) Console.WriteLine($"Public Key: {pkEl.GetString()}");
        if (result.TryGetValue("tier", out var tier) && tier is JsonElement tierEl) Console.WriteLine($"Tier: {tierEl.GetString()}");
        if (result.TryGetValue("expires_at", out var exp) && exp is JsonElement expAtEl) Console.WriteLine($"Expires: {expAtEl.GetString()}");
    }
    else if (isExpired)
    {
        Console.WriteLine($"{RED}Expired{RESET}");
        string? pkStr = null;
        if (result.TryGetValue("public_key", out var pk) && pk is JsonElement pkEl) { pkStr = pkEl.GetString(); Console.WriteLine($"Public Key: {pkStr}"); }
        if (result.TryGetValue("tier", out var tier) && tier is JsonElement tierEl) Console.WriteLine($"Tier: {tierEl.GetString()}");
        if (result.TryGetValue("expired_at", out var expAt) && expAt is JsonElement expAtEl) Console.WriteLine($"Expired: {expAtEl.GetString()}");
        Console.WriteLine($"{YELLOW}To renew: Visit {PORTAL_BASE}/keys/extend{RESET}");

        if (args.KeyExtend && !string.IsNullOrEmpty(pkStr))
        {
            var url = $"{PORTAL_BASE}/keys/extend?pk={pkStr}";
            Console.WriteLine($"{YELLOW}Opening: {url}{RESET}");
            OpenBrowser(url);
        }
    }
    else
    {
        Console.WriteLine($"{RED}Invalid{RESET}");
    }
}

void OpenBrowser(string url)
{
    try
    {
        if (OperatingSystem.IsWindows())
            System.Diagnostics.Process.Start(new System.Diagnostics.ProcessStartInfo(url) { UseShellExecute = true });
        else if (OperatingSystem.IsLinux())
            System.Diagnostics.Process.Start("xdg-open", url);
        else if (OperatingSystem.IsMacOS())
            System.Diagnostics.Process.Start("open", url);
    }
    catch (Exception ex) { Console.Error.WriteLine($"{RED}Failed to open browser: {ex.Message}{RESET}"); }
}

void CmdService(Args args)
{
    var (publicKey, secretKey) = GetApiKeys(args.ApiKey);

    if (!string.IsNullOrEmpty(args.EnvAction))
    {
        CmdServiceEnv(args, publicKey, secretKey);
        return;
    }

    if (args.ServiceList)
    {
        var result = ApiRequest("/services", HttpMethod.Get, null, publicKey, secretKey);
        if (result.TryGetValue("services", out var services) && services is JsonElement servicesEl)
        {
            var serviceList = servicesEl.EnumerateArray().ToList();
            if (serviceList.Count == 0) { Console.WriteLine("No services"); return; }
            Console.WriteLine($"{"ID",-20} {"Name",-15} {"Status",-10} {"Ports",-15} {"Domains"}");
            foreach (var s in serviceList)
            {
                var ports = s.TryGetProperty("ports", out var p) ? string.Join(",", p.EnumerateArray().Select(x => x.GetInt32())) : "";
                var domains = s.TryGetProperty("domains", out var d) ? string.Join(",", d.EnumerateArray().Select(x => x.GetString())) : "";
                Console.WriteLine($"{GetStr(s, "id"),-20} {GetStr(s, "name"),-15} {GetStr(s, "status"),-10} {ports,-15} {domains}");
            }
        }
        return;
    }

    if (args.ServiceInfo != null)
    {
        var result = ApiRequest($"/services/{args.ServiceInfo}", HttpMethod.Get, null, publicKey, secretKey);
        Console.WriteLine(JsonSerializer.Serialize(result, new JsonSerializerOptions { WriteIndented = true }));
        return;
    }

    if (args.ServiceLogs != null)
    {
        var result = ApiRequest($"/services/{args.ServiceLogs}/logs", HttpMethod.Get, null, publicKey, secretKey);
        if (result.TryGetValue("logs", out var logs) && logs is JsonElement logsEl) Console.WriteLine(logsEl.GetString());
        return;
    }

    if (args.ServiceTail != null)
    {
        var result = ApiRequest($"/services/{args.ServiceTail}/logs?lines=9000", HttpMethod.Get, null, publicKey, secretKey);
        if (result.TryGetValue("logs", out var logs) && logs is JsonElement logsEl) Console.WriteLine(logsEl.GetString());
        return;
    }

    if (args.ServiceSleep != null)
    {
        ApiRequest($"/services/{args.ServiceSleep}/freeze", HttpMethod.Post, null, publicKey, secretKey);
        Console.WriteLine($"{GREEN}Service frozen: {args.ServiceSleep}{RESET}");
        return;
    }

    if (args.ServiceWake != null)
    {
        ApiRequest($"/services/{args.ServiceWake}/unfreeze", HttpMethod.Post, null, publicKey, secretKey);
        Console.WriteLine($"{GREEN}Service unfreezing: {args.ServiceWake}{RESET}");
        return;
    }

    if (args.ServiceUnfreezeOnDemand != null)
    {
        var payload = new Dictionary<string, object> { ["unfreeze_on_demand"] = args.ServiceUnfreezeOnDemandEnabled };
        ApiRequest($"/services/{args.ServiceUnfreezeOnDemand}", new HttpMethod("PATCH"), payload, publicKey, secretKey);
        string status = args.ServiceUnfreezeOnDemandEnabled ? "enabled" : "disabled";
        Console.WriteLine($"{GREEN}Unfreeze-on-demand {status} for service: {args.ServiceUnfreezeOnDemand}{RESET}");
        return;
    }

    if (args.ServiceShowFreezePage != null)
    {
        var payload = new Dictionary<string, object> { ["show_freeze_page"] = args.ServiceShowFreezePageEnabled };
        await ApiRequestAsync($"/services/{args.ServiceShowFreezePage}", new HttpMethod("PATCH"), payload, publicKey, secretKey);
        string status = args.ServiceShowFreezePageEnabled ? "enabled" : "disabled";
        Console.WriteLine($"{GREEN}Show-freeze-page {status} for service: {args.ServiceShowFreezePage}{RESET}");
        return;
    }

    if (args.ServiceDestroy != null)
    {
        ApiRequest($"/services/{args.ServiceDestroy}", HttpMethod.Delete, null, publicKey, secretKey);
        Console.WriteLine($"{GREEN}Service destroyed: {args.ServiceDestroy}{RESET}");
        return;
    }

    if (args.ServiceLock != null)
    {
        ApiRequest($"/services/{args.ServiceLock}/lock", HttpMethod.Post, null, publicKey, secretKey);
        Console.WriteLine($"{GREEN}Service locked: {args.ServiceLock}{RESET}");
        return;
    }

    if (args.ServiceUnlock != null)
    {
        ApiRequest($"/services/{args.ServiceUnlock}/unlock", HttpMethod.Post, null, publicKey, secretKey);
        Console.WriteLine($"{GREEN}Service unlocked: {args.ServiceUnlock}{RESET}");
        return;
    }

    if (args.ServiceResize != null)
    {
        var payload = new Dictionary<string, object>();
        if (args.Vcpu > 0) payload["vcpu"] = args.Vcpu;
        ApiRequest($"/services/{args.ServiceResize}/resize", HttpMethod.Post, payload, publicKey, secretKey);
        Console.WriteLine($"{GREEN}Service resized: {args.ServiceResize}{RESET}");
        return;
    }

    if (args.ServiceRedeploy != null)
    {
        ApiRequest($"/services/{args.ServiceRedeploy}/redeploy", HttpMethod.Post, null, publicKey, secretKey);
        Console.WriteLine($"{GREEN}Service redeploying: {args.ServiceRedeploy}{RESET}");
        return;
    }

    if (args.ServiceSnapshot != null)
    {
        var payload = new Dictionary<string, object>();
        if (args.SnapshotName != null) payload["name"] = args.SnapshotName;
        if (args.SnapshotHot) payload["hot"] = true;

        var result = ApiRequest($"/services/{args.ServiceSnapshot}/snapshot", HttpMethod.Post, payload, publicKey, secretKey);
        var id = result.TryGetValue("id", out var idObj) && idObj is JsonElement idEl ? idEl.GetString() : "unknown";
        Console.WriteLine($"{GREEN}Snapshot created: {id}{RESET}");
        return;
    }

    if (args.ServiceExecute != null)
    {
        var payload = new Dictionary<string, object> { ["command"] = args.ServiceCommand ?? "" };
        var result = ApiRequest($"/services/{args.ServiceExecute}/execute", HttpMethod.Post, payload, publicKey, secretKey);
        if (result.TryGetValue("stdout", out var stdout) && stdout is JsonElement stdoutEl) Console.Write($"{BLUE}{stdoutEl.GetString()}{RESET}");
        if (result.TryGetValue("stderr", out var stderr) && stderr is JsonElement stderrEl) Console.Error.Write($"{RED}{stderrEl.GetString()}{RESET}");
        return;
    }

    if (args.ServiceDumpBootstrap != null)
    {
        Console.Error.WriteLine($"Fetching bootstrap script from {args.ServiceDumpBootstrap}...");
        var payload = new Dictionary<string, object> { ["command"] = "cat /tmp/bootstrap.sh" };
        var result = ApiRequest($"/services/{args.ServiceDumpBootstrap}/execute", HttpMethod.Post, payload, publicKey, secretKey);
        var bootstrap = result.TryGetValue("stdout", out var bs) && bs is JsonElement bsEl ? bsEl.GetString() : null;
        if (!string.IsNullOrEmpty(bootstrap))
        {
            if (args.ServiceDumpFile != null)
            {
                File.WriteAllText(args.ServiceDumpFile, bootstrap);
                Console.WriteLine($"Bootstrap saved to {args.ServiceDumpFile}");
            }
            else Console.Write(bootstrap);
        }
        else
        {
            Console.Error.WriteLine($"{RED}Error: Failed to fetch bootstrap (service not running or no bootstrap file){RESET}");
            Environment.Exit(1);
        }
        return;
    }

    if (args.ServiceName != null)
    {
        var payload = new Dictionary<string, object> { ["name"] = args.ServiceName };
        if (args.ServicePorts != null)
            payload["ports"] = args.ServicePorts.Split(',').Select(p => int.Parse(p.Trim())).ToList();
        if (args.ServiceType != null) payload["service_type"] = args.ServiceType;
        if (args.ServiceBootstrap != null) payload["bootstrap"] = args.ServiceBootstrap;
        else if (args.ServiceBootstrapFile != null) payload["bootstrap"] = File.ReadAllText(args.ServiceBootstrapFile);
        if (args.Network != null) payload["network"] = args.Network;
        if (args.Vcpu > 0) payload["vcpu"] = args.Vcpu;
        if (args.ServiceCreateUnfreezeOnDemand) payload["unfreeze_on_demand"] = true;

        var result = ApiRequest("/services", HttpMethod.Post, payload, publicKey, secretKey);
        var serviceId = result.TryGetValue("id", out var id) && id is JsonElement idEl ? idEl.GetString() : null;
        Console.WriteLine($"{GREEN}Service created: {serviceId}{RESET}");
        if (result.TryGetValue("name", out var name) && name is JsonElement nameEl) Console.WriteLine($"Name: {nameEl.GetString()}");
        if (result.TryGetValue("url", out var url) && url is JsonElement urlEl) Console.WriteLine($"URL: {urlEl.GetString()}");

        if (!string.IsNullOrEmpty(serviceId) && (args.Env.Count > 0 || !string.IsNullOrEmpty(args.EnvFile)))
        {
            var envContent = BuildEnvContent(args.Env, args.EnvFile);
            if (!string.IsNullOrEmpty(envContent))
            {
                if (ServiceEnvSet(serviceId, envContent, publicKey, secretKey))
                    Console.WriteLine($"{GREEN}Vault configured with environment variables{RESET}");
                else
                    Console.Error.WriteLine($"{YELLOW}Warning: Failed to set vault{RESET}");
            }
        }
        return;
    }

    Console.Error.WriteLine($"{RED}Error: Specify --name to create a service, or use --list, --info, etc.{RESET}");
    Environment.Exit(1);
}

void CmdServiceEnv(Args args, string publicKey, string secretKey)
{
    var action = args.EnvAction;
    var target = args.EnvTarget;

    if (action == "status")
    {
        if (string.IsNullOrEmpty(target)) { Console.Error.WriteLine($"{RED}Error: service env status requires service ID{RESET}"); Environment.Exit(1); }
        var result = ApiRequest($"/services/{target}/env", HttpMethod.Get, null, publicKey, secretKey);
        if (result.TryGetValue("has_vault", out var hv) && hv is JsonElement hvEl && hvEl.GetBoolean())
        {
            Console.WriteLine($"{GREEN}Vault: configured{RESET}");
            if (result.TryGetValue("env_count", out var ec) && ec is JsonElement ecEl) Console.WriteLine($"Variables: {ecEl.GetInt32()}");
            if (result.TryGetValue("updated_at", out var ua) && ua is JsonElement uaEl) Console.WriteLine($"Updated: {uaEl.GetString()}");
        }
        else Console.WriteLine($"{YELLOW}Vault: not configured{RESET}");
    }
    else if (action == "set")
    {
        if (string.IsNullOrEmpty(target)) { Console.Error.WriteLine($"{RED}Error: service env set requires service ID{RESET}"); Environment.Exit(1); }
        if (args.Env.Count == 0 && string.IsNullOrEmpty(args.EnvFile)) { Console.Error.WriteLine($"{RED}Error: service env set requires -e or --env-file{RESET}"); Environment.Exit(1); }
        var envContent = BuildEnvContent(args.Env, args.EnvFile);
        if (ServiceEnvSet(target, envContent, publicKey, secretKey))
            Console.WriteLine($"{GREEN}Vault updated for service {target}{RESET}");
        else { Console.Error.WriteLine($"{RED}Error: Failed to update vault{RESET}"); Environment.Exit(1); }
    }
    else if (action == "export")
    {
        if (string.IsNullOrEmpty(target)) { Console.Error.WriteLine($"{RED}Error: service env export requires service ID{RESET}"); Environment.Exit(1); }
        var result = ApiRequest($"/services/{target}/env/export", HttpMethod.Post, null, publicKey, secretKey);
        if (result.TryGetValue("content", out var content) && content is JsonElement contentEl) Console.Write(contentEl.GetString());
    }
    else if (action == "delete")
    {
        if (string.IsNullOrEmpty(target)) { Console.Error.WriteLine($"{RED}Error: service env delete requires service ID{RESET}"); Environment.Exit(1); }
        ApiRequest($"/services/{target}/env", HttpMethod.Delete, null, publicKey, secretKey);
        Console.WriteLine($"{GREEN}Vault deleted for service {target}{RESET}");
    }
}

void CmdSnapshot(Args args)
{
    var (publicKey, secretKey) = GetApiKeys(args.ApiKey);

    if (args.SnapshotList)
    {
        var result = ApiRequest("/snapshots", HttpMethod.Get, null, publicKey, secretKey);
        if (result.TryGetValue("snapshots", out var snapshots) && snapshots is JsonElement snapshotsEl)
        {
            var snapshotList = snapshotsEl.EnumerateArray().ToList();
            if (snapshotList.Count == 0) { Console.WriteLine("No snapshots"); return; }
            Console.WriteLine($"{"ID",-40} {"Name",-20} {"Type",-10} {"Status",-10} {"Created"}");
            foreach (var s in snapshotList)
                Console.WriteLine($"{GetStr(s, "id"),-40} {GetStr(s, "name"),-20} {GetStr(s, "source_type"),-10} {GetStr(s, "status"),-10} {GetStr(s, "created_at")}");
        }
        return;
    }

    if (args.SnapshotInfo != null)
    {
        var result = ApiRequest($"/snapshots/{args.SnapshotInfo}", HttpMethod.Get, null, publicKey, secretKey);
        Console.WriteLine(JsonSerializer.Serialize(result, new JsonSerializerOptions { WriteIndented = true }));
        return;
    }

    if (args.SnapshotDelete != null)
    {
        ApiRequest($"/snapshots/{args.SnapshotDelete}", HttpMethod.Delete, null, publicKey, secretKey);
        Console.WriteLine($"{GREEN}Snapshot deleted: {args.SnapshotDelete}{RESET}");
        return;
    }

    if (args.SnapshotLock != null)
    {
        ApiRequest($"/snapshots/{args.SnapshotLock}/lock", HttpMethod.Post, null, publicKey, secretKey);
        Console.WriteLine($"{GREEN}Snapshot locked: {args.SnapshotLock}{RESET}");
        return;
    }

    if (args.SnapshotUnlock != null)
    {
        ApiRequest($"/snapshots/{args.SnapshotUnlock}/unlock", HttpMethod.Post, null, publicKey, secretKey);
        Console.WriteLine($"{GREEN}Snapshot unlocked: {args.SnapshotUnlock}{RESET}");
        return;
    }

    if (args.SnapshotClone != null)
    {
        var payload = new Dictionary<string, object> { ["type"] = args.SnapshotCloneType ?? "session" };
        if (args.ServiceName != null) payload["name"] = args.ServiceName;
        if (args.SessionShell != null) payload["shell"] = args.SessionShell;
        if (args.ServicePorts != null) payload["ports"] = args.ServicePorts.Split(',').Select(p => int.Parse(p.Trim())).ToList();
        if (args.Network != null) payload["network"] = args.Network;

        var result = ApiRequest($"/snapshots/{args.SnapshotClone}/clone", HttpMethod.Post, payload, publicKey, secretKey);
        var id = result.TryGetValue("id", out var idObj) && idObj is JsonElement idEl ? idEl.GetString() : "unknown";
        Console.WriteLine($"{GREEN}Cloned to {args.SnapshotCloneType ?? "session"}: {id}{RESET}");
        return;
    }

    Console.Error.WriteLine($"{RED}Error: Use --list, --info, --delete, --lock, --unlock, or --clone{RESET}");
    Environment.Exit(1);
}

void CmdImage(Args args)
{
    var (publicKey, secretKey) = GetApiKeys(args.ApiKey);

    if (args.ImageList)
    {
        var result = ApiRequest("/images", HttpMethod.Get, null, publicKey, secretKey);
        if (result.TryGetValue("images", out var images) && images is JsonElement imagesEl)
        {
            var imageList = imagesEl.EnumerateArray().ToList();
            if (imageList.Count == 0) { Console.WriteLine("No images"); return; }
            Console.WriteLine($"{"ID",-40} {"Name",-20} {"Visibility",-12} {"Status",-10} {"Created"}");
            foreach (var img in imageList)
                Console.WriteLine($"{GetStr(img, "id"),-40} {GetStr(img, "name"),-20} {GetStr(img, "visibility"),-12} {GetStr(img, "status"),-10} {GetStr(img, "created_at")}");
        }
        return;
    }

    if (args.ImageInfo != null)
    {
        var result = ApiRequest($"/images/{args.ImageInfo}", HttpMethod.Get, null, publicKey, secretKey);
        Console.WriteLine(JsonSerializer.Serialize(result, new JsonSerializerOptions { WriteIndented = true }));
        return;
    }

    if (args.ImageDelete != null)
    {
        ApiRequest($"/images/{args.ImageDelete}", HttpMethod.Delete, null, publicKey, secretKey);
        Console.WriteLine($"{GREEN}Image deleted: {args.ImageDelete}{RESET}");
        return;
    }

    if (args.ImageLock != null)
    {
        ApiRequest($"/images/{args.ImageLock}/lock", HttpMethod.Post, null, publicKey, secretKey);
        Console.WriteLine($"{GREEN}Image locked: {args.ImageLock}{RESET}");
        return;
    }

    if (args.ImageUnlock != null)
    {
        ApiRequest($"/images/{args.ImageUnlock}/unlock", HttpMethod.Post, null, publicKey, secretKey);
        Console.WriteLine($"{GREEN}Image unlocked: {args.ImageUnlock}{RESET}");
        return;
    }

    if (args.ImagePublish != null)
    {
        var payload = new Dictionary<string, object> { ["source_id"] = args.ImagePublish };
        if (args.ImageSourceType != null) payload["source_type"] = args.ImageSourceType;
        if (args.ServiceName != null) payload["name"] = args.ServiceName;

        var result = ApiRequest("/images", HttpMethod.Post, payload, publicKey, secretKey);
        var id = result.TryGetValue("id", out var idObj) && idObj is JsonElement idEl ? idEl.GetString() : "unknown";
        Console.WriteLine($"{GREEN}Image published: {id}{RESET}");
        return;
    }

    if (args.ImageVisibility != null)
    {
        var payload = new Dictionary<string, object> { ["visibility"] = args.ImageVisibilityMode ?? "private" };
        ApiRequest($"/images/{args.ImageVisibility}", new HttpMethod("PATCH"), payload, publicKey, secretKey);
        Console.WriteLine($"{GREEN}Image visibility set to {args.ImageVisibilityMode}: {args.ImageVisibility}{RESET}");
        return;
    }

    if (args.ImageSpawn != null)
    {
        var payload = new Dictionary<string, object>();
        if (args.ServiceName != null) payload["name"] = args.ServiceName;
        if (args.ServicePorts != null) payload["ports"] = args.ServicePorts.Split(',').Select(p => int.Parse(p.Trim())).ToList();
        if (args.Network != null) payload["network"] = args.Network;

        var result = ApiRequest($"/images/{args.ImageSpawn}/spawn", HttpMethod.Post, payload, publicKey, secretKey);
        var id = result.TryGetValue("id", out var idObj) && idObj is JsonElement idEl ? idEl.GetString() : "unknown";
        Console.WriteLine($"{GREEN}Service spawned: {id}{RESET}");
        if (result.TryGetValue("url", out var url) && url is JsonElement urlEl) Console.WriteLine($"URL: {urlEl.GetString()}");
        return;
    }

    if (args.ImageClone != null)
    {
        var payload = new Dictionary<string, object>();
        if (args.ServiceName != null) payload["name"] = args.ServiceName;

        var result = ApiRequest($"/images/{args.ImageClone}/clone", HttpMethod.Post, payload, publicKey, secretKey);
        var id = result.TryGetValue("id", out var idObj) && idObj is JsonElement idEl ? idEl.GetString() : "unknown";
        Console.WriteLine($"{GREEN}Image cloned: {id}{RESET}");
        return;
    }

    Console.Error.WriteLine($"{RED}Error: Use --list, --info, --delete, --lock, --unlock, --publish, --visibility, --spawn, or --clone{RESET}");
    Environment.Exit(1);
}

void CmdLanguages(Args args)
{
    var (publicKey, secretKey) = GetApiKeys(args.ApiKey);

    // Check cache first
    var cacheDir = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.UserProfile), ".unsandbox");
    var cachePath = Path.Combine(cacheDir, "languages.json");
    var cacheMaxAge = TimeSpan.FromHours(1);

    List<string>? languages = null;

    if (File.Exists(cachePath))
    {
        var cacheAge = DateTime.UtcNow - File.GetLastWriteTimeUtc(cachePath);
        if (cacheAge < cacheMaxAge)
        {
            try
            {
                var cacheContent = File.ReadAllText(cachePath);
                languages = JsonSerializer.Deserialize<List<string>>(cacheContent);
            }
            catch { /* Cache corrupted, fetch fresh */ }
        }
    }

    if (languages == null)
    {
        var result = ApiRequest("/languages", HttpMethod.Get, null, publicKey, secretKey);
        if (result.TryGetValue("languages", out var langsObj) && langsObj is JsonElement langsEl)
        {
            languages = langsEl.EnumerateArray().Select(l => l.GetString() ?? "").Where(l => !string.IsNullOrEmpty(l)).ToList();

            // Save to cache
            try
            {
                Directory.CreateDirectory(cacheDir);
                File.WriteAllText(cachePath, JsonSerializer.Serialize(languages));
            }
            catch { /* Cache write failed, continue anyway */ }
        }
        else
        {
            Console.Error.WriteLine($"{RED}Error: Failed to fetch languages{RESET}");
            Environment.Exit(1);
            return;
        }
    }

    if (args.LanguagesJson)
        Console.WriteLine(JsonSerializer.Serialize(languages));
    else
        foreach (var lang in languages)
            Console.WriteLine(lang);
}

Dictionary<string, object> ApiRequest(string endpoint, HttpMethod method, Dictionary<string, object>? data, string publicKey, string secretKey)
{
    var body = data != null ? JsonSerializer.Serialize(data, jsonOptions) : "";

    using var request = new HttpRequestMessage(method, endpoint);
    if (data != null) request.Content = new StringContent(body, Encoding.UTF8, "application/json");

    // HMAC Authentication
    if (!string.IsNullOrEmpty(secretKey))
    {
        var timestamp = DateTimeOffset.UtcNow.ToUnixTimeSeconds();
        var message = $"{timestamp}:{method.Method}:{endpoint}:{body}";
        using var hmac = new HMACSHA256(Encoding.UTF8.GetBytes(secretKey));
        var signature = Convert.ToHexString(hmac.ComputeHash(Encoding.UTF8.GetBytes(message))).ToLowerInvariant();
        request.Headers.Add("Authorization", $"Bearer {publicKey}");
        request.Headers.Add("X-Timestamp", timestamp.ToString());
        request.Headers.Add("X-Signature", signature);
    }
    else
    {
        request.Headers.Add("Authorization", $"Bearer {publicKey}");
    }

    // Synchronous HTTP call
    var response = httpClient.Send(request);
    using var reader = new StreamReader(response.Content.ReadAsStream());
    var responseBody = reader.ReadToEnd();

    if (!response.IsSuccessStatusCode)
    {
        if (responseBody.Contains("timestamp") && ((int)response.StatusCode == 401 || responseBody.ToLower().Contains("expired")))
        {
            Console.Error.WriteLine($"{RED}Error: Request timestamp expired (must be within 5 minutes of server time){RESET}");
            Console.Error.WriteLine($"{YELLOW}Your computer's clock may have drifted.{RESET}");
            Environment.Exit(1);
        }
        throw new Exception($"HTTP {(int)response.StatusCode}: {responseBody}");
    }

    if (string.IsNullOrWhiteSpace(responseBody)) return new Dictionary<string, object>();

    try
    {
        var doc = JsonDocument.Parse(responseBody);
        return doc.RootElement.EnumerateObject().ToDictionary(p => p.Name, p => (object)p.Value.Clone());
    }
    catch
    {
        return new Dictionary<string, object> { ["raw"] = responseBody };
    }
}

bool ServiceEnvSet(string serviceId, string envContent, string publicKey, string secretKey)
{
    if (envContent.Length > 65536) { Console.Error.WriteLine($"{RED}Error: Env content exceeds maximum size of 64KB{RESET}"); return false; }

    try
    {
        using var request = new HttpRequestMessage(HttpMethod.Put, $"/services/{serviceId}/env");
        request.Content = new StringContent(envContent, Encoding.UTF8, "text/plain");

        var timestamp = DateTimeOffset.UtcNow.ToUnixTimeSeconds();
        var message = $"{timestamp}:PUT:/services/{serviceId}/env:{envContent}";
        using var hmac = new HMACSHA256(Encoding.UTF8.GetBytes(secretKey));
        var signature = Convert.ToHexString(hmac.ComputeHash(Encoding.UTF8.GetBytes(message))).ToLowerInvariant();
        request.Headers.Add("Authorization", $"Bearer {publicKey}");
        request.Headers.Add("X-Timestamp", timestamp.ToString());
        request.Headers.Add("X-Signature", signature);

        var response = httpClient.Send(request);
        return response.IsSuccessStatusCode;
    }
    catch { return false; }
}

(string, string) GetApiKeys(string? argsKey)
{
    var publicKey = Environment.GetEnvironmentVariable("UNSANDBOX_PUBLIC_KEY");
    var secretKey = Environment.GetEnvironmentVariable("UNSANDBOX_SECRET_KEY");

    if (string.IsNullOrEmpty(publicKey) || string.IsNullOrEmpty(secretKey))
    {
        var legacyKey = argsKey ?? Environment.GetEnvironmentVariable("UNSANDBOX_API_KEY");
        if (string.IsNullOrEmpty(legacyKey))
        {
            Console.Error.WriteLine($"{RED}Error: UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY not set{RESET}");
            Environment.Exit(1);
        }
        return (legacyKey, "");
    }
    return (publicKey, secretKey);
}

string DetectLanguage(string filename)
{
    var ext = Path.GetExtension(filename).ToLower();
    if (string.IsNullOrEmpty(ext) || !extMap.TryGetValue(ext, out var language))
        throw new Exception($"Unsupported file extension: {ext}");
    return language;
}

string BuildEnvContent(List<string> envs, string? envFile)
{
    var lines = new List<string>(envs);
    if (!string.IsNullOrEmpty(envFile))
    {
        var content = File.ReadAllText(envFile);
        lines.AddRange(content.Split('\n').Select(l => l.Trim()).Where(l => !string.IsNullOrEmpty(l) && !l.StartsWith("#")));
    }
    return string.Join("\n", lines);
}

string GetStr(JsonElement el, string prop) => el.TryGetProperty(prop, out var p) ? p.GetString() ?? "N/A" : "N/A";

Args ParseArgs(string[] args)
{
    var result = new Args();
    for (var i = 0; i < args.Length; i++)
    {
        var arg = args[i];
        if (arg == "-h" || arg == "--help") result.ShowHelp = true;
        else if (arg == "--version") result.ShowVersion = true;
        else if (arg == "session") result.Command = "session";
        else if (arg == "service") result.Command = "service";
        else if (arg == "snapshot") result.Command = "snapshot";
        else if (arg == "image") result.Command = "image";
        else if (arg == "languages") result.Command = "languages";
        else if (arg == "key") result.Command = "key";
        else if (arg == "env" && result.Command == "service")
        {
            if (i + 1 < args.Length && !args[i + 1].StartsWith("-"))
            {
                result.EnvAction = args[++i];
                if (i + 1 < args.Length && !args[i + 1].StartsWith("-")) result.EnvTarget = args[++i];
            }
        }
        else if (arg == "-k" || arg == "--api-key") result.ApiKey = args[++i];
        else if (arg == "-n" || arg == "--network") result.Network = args[++i];
        else if (arg == "-v" || arg == "--vcpu") result.Vcpu = int.Parse(args[++i]);
        else if (arg == "-e" || arg == "--env") result.Env.Add(args[++i]);
        else if (arg == "--env-file") result.EnvFile = args[++i];
        else if (arg == "-f" || arg == "--files") result.Files.Add(args[++i]);
        else if (arg == "-a" || arg == "--artifacts") result.Artifacts = true;
        else if (arg == "-o" || arg == "--output-dir") result.OutputDir = args[++i];
        else if (arg == "-l" || arg == "--list")
        {
            if (result.Command == "session") result.SessionList = true;
            else if (result.Command == "service") result.ServiceList = true;
            else if (result.Command == "snapshot") result.SnapshotList = true;
            else if (result.Command == "image") result.ImageList = true;
        }
        else if (arg == "-s" || arg == "--shell") result.SessionShell = args[++i];
        else if (arg == "--kill") result.SessionKill = args[++i];
        else if (arg == "--name") result.ServiceName = args[++i];
        else if (arg == "--snapshot-name") result.SnapshotName = args[++i];
        else if (arg == "--hot") result.SnapshotHot = true;
        else if (arg == "--ports") result.ServicePorts = args[++i];
        else if (arg == "--type") result.ServiceType = args[++i];
        else if (arg == "--bootstrap") result.ServiceBootstrap = args[++i];
        else if (arg == "--bootstrap-file") result.ServiceBootstrapFile = args[++i];
        else if (arg == "--info")
        {
            var val = args[++i];
            if (result.Command == "service") result.ServiceInfo = val;
            else if (result.Command == "snapshot") result.SnapshotInfo = val;
            else if (result.Command == "image") result.ImageInfo = val;
        }
        else if (arg == "--logs") result.ServiceLogs = args[++i];
        else if (arg == "--tail") result.ServiceTail = args[++i];
        else if (arg == "--freeze")
        {
            var val = args[++i];
            if (result.Command == "session") result.SessionFreeze = val;
            else result.ServiceSleep = val;
        }
        else if (arg == "--unfreeze")
        {
            var val = args[++i];
            if (result.Command == "session") result.SessionUnfreeze = val;
            else result.ServiceWake = val;
        }
        else if (arg == "--boost") result.SessionBoost = args[++i];
        else if (arg == "--unboost") result.SessionUnboost = args[++i];
        else if (arg == "--snapshot")
        {
            var val = args[++i];
            if (result.Command == "session") result.SessionSnapshot = val;
            else if (result.Command == "service") result.ServiceSnapshot = val;
        }
        else if (arg == "--destroy") result.ServiceDestroy = args[++i];
        else if (arg == "--lock")
        {
            var val = args[++i];
            if (result.Command == "service") result.ServiceLock = val;
            else if (result.Command == "snapshot") result.SnapshotLock = val;
            else if (result.Command == "image") result.ImageLock = val;
        }
        else if (arg == "--unlock")
        {
            var val = args[++i];
            if (result.Command == "service") result.ServiceUnlock = val;
            else if (result.Command == "snapshot") result.SnapshotUnlock = val;
            else if (result.Command == "image") result.ImageUnlock = val;
        }
        else if (arg == "--resize") result.ServiceResize = args[++i];
        else if (arg == "--redeploy") result.ServiceRedeploy = args[++i];
        else if (arg == "--execute") result.ServiceExecute = args[++i];
        else if (arg == "--command") result.ServiceCommand = args[++i];
        else if (arg == "--dump-bootstrap") result.ServiceDumpBootstrap = args[++i];
        else if (arg == "--dump-file") result.ServiceDumpFile = args[++i];
        else if (arg == "--unfreeze-on-demand") result.ServiceUnfreezeOnDemand = args[++i];
        else if (arg == "--unfreeze-on-demand-enabled") result.ServiceUnfreezeOnDemandEnabled = args[++i].ToLower() == "true";
        else if (arg == "--show-freeze-page") result.ServiceShowFreezePage = args[++i];
        else if (arg == "--show-freeze-page-enabled") result.ServiceShowFreezePageEnabled = args[++i].ToLower() == "true";
        else if (arg == "--with-unfreeze-on-demand") result.ServiceCreateUnfreezeOnDemand = true;
        else if (arg == "--extend") result.KeyExtend = true;
        else if (arg == "--delete")
        {
            var val = args[++i];
            if (result.Command == "snapshot") result.SnapshotDelete = val;
            else if (result.Command == "image") result.ImageDelete = val;
        }
        else if (arg == "--clone")
        {
            var val = args[++i];
            if (result.Command == "snapshot") result.SnapshotClone = val;
            else if (result.Command == "image") result.ImageClone = val;
        }
        else if (arg == "--clone-type") result.SnapshotCloneType = args[++i];
        else if (arg == "--publish") result.ImagePublish = args[++i];
        else if (arg == "--source-type") result.ImageSourceType = args[++i];
        else if (arg == "--visibility")
        {
            result.ImageVisibility = args[++i];
            if (i + 1 < args.Length && !args[i + 1].StartsWith("-")) result.ImageVisibilityMode = args[++i];
        }
        else if (arg == "--spawn") result.ImageSpawn = args[++i];
        else if (arg == "--json") result.LanguagesJson = true;
        else if (!arg.StartsWith("-")) result.SourceFile = arg;
    }
    return result;
}

void PrintHelp()
{
    Console.WriteLine($@"un {VERSION} (.NET 10 sync) - Unsandbox CLI

Usage: dotnet run -- [options] <source_file>
       dotnet run -- session [options]
       dotnet run -- service [options]
       dotnet run -- service env <action> <service_id> [options]
       dotnet run -- snapshot [options]
       dotnet run -- image [options]
       dotnet run -- languages [options]
       dotnet run -- key [options]

Execute options:
  -e KEY=VALUE      Set environment variable
  -f FILE           Add input file
  -a                Return artifacts
  -o DIR            Output directory for artifacts
  -n MODE           Network mode (zerotrust/semitrusted)
  -v N              vCPU count (1-8)
  -k KEY            API key

Session options:
  --list            List active sessions
  --shell NAME      Shell/REPL to use
  --kill ID         Terminate session
  --freeze ID       Freeze session
  --unfreeze ID     Unfreeze session
  --boost ID        Boost session resources
  --unboost ID      Remove session boost
  --snapshot ID     Create snapshot from session
  --snapshot-name   Name for snapshot
  --hot             Live snapshot (no freeze)

Service options:
  --list            List services
  --name NAME       Create service with name
  --ports PORTS     Comma-separated ports
  --type TYPE       Service type (minecraft/mumble/teamspeak/source/tcp/udp)
  --bootstrap CMD   Bootstrap command
  --bootstrap-file FILE   Bootstrap from file
  --info ID         Get service details
  --logs ID         Get all logs
  --tail ID         Get last 9000 lines
  --freeze ID       Freeze service
  --unfreeze ID     Unfreeze service
  --lock ID         Prevent deletion
  --unlock ID       Allow deletion
  --resize ID       Resize (use with -v)
  --redeploy ID     Re-run bootstrap
  --snapshot ID     Create snapshot from service
  --unfreeze-on-demand ID   Set unfreeze-on-demand for service
  --unfreeze-on-demand-enabled BOOL   Enable/disable (default: true)
  --show-freeze-page ID   Set show-freeze-page for service
  --show-freeze-page-enabled BOOL   Enable/disable (default: true)
  --with-unfreeze-on-demand   Enable unfreeze-on-demand when creating service
  --destroy ID      Destroy service
  --execute ID      Execute command in service
  --command CMD     Command to execute (with --execute)
  --dump-bootstrap ID   Dump bootstrap script
  --dump-file FILE      File to save bootstrap (with --dump-bootstrap)
  -e KEY=VALUE      Set vault env var (with --name or env set)
  --env-file FILE   Load vault vars from file

Service env commands:
  env status ID     Check vault status
  env set ID        Set vault (use -e or --env-file)
  env export ID     Export vault contents
  env delete ID     Delete vault

Snapshot options:
  --list            List all snapshots
  --info ID         Get snapshot details
  --delete ID       Delete snapshot
  --lock ID         Prevent deletion
  --unlock ID       Allow deletion
  --clone ID        Clone snapshot to session/service
  --clone-type TYPE Clone type: session or service
  --name NAME       Name for cloned resource
  --ports PORTS     Ports for cloned service

Image options:
  --list            List all images
  --info ID         Get image details
  --delete ID       Delete image
  --lock ID         Prevent deletion
  --unlock ID       Allow deletion
  --publish ID      Publish from service/snapshot
  --source-type TYPE   Source type: service or snapshot
  --visibility ID MODE   Set visibility (private/unlisted/public)
  --spawn ID        Spawn new service from image
  --clone ID        Clone image

Languages options:
  --json            Output as JSON array

Key options:
  --extend          Open browser to extend expired key

Environment:
  UNSANDBOX_PUBLIC_KEY   Your public API key
  UNSANDBOX_SECRET_KEY   Your secret API key");
}

class Args
{
    public bool ShowHelp, ShowVersion;
    public string? Command, SourceFile, ApiKey, Network, OutputDir;
    public int Vcpu;
    public List<string> Env = new(), Files = new();
    public bool Artifacts, SessionList, ServiceList;
    public string? SessionShell, SessionKill;
    public string? SessionFreeze, SessionUnfreeze, SessionBoost, SessionUnboost, SessionSnapshot;
    public string? ServiceName, ServicePorts, ServiceBootstrap, ServiceBootstrapFile, ServiceType;
    public string? ServiceInfo, ServiceLogs, ServiceTail, ServiceSleep, ServiceWake, ServiceDestroy;
    public string? ServiceLock, ServiceUnlock, ServiceResize, ServiceRedeploy, ServiceSnapshot;
    public string? ServiceExecute, ServiceCommand;
    public string? ServiceDumpBootstrap, ServiceDumpFile;
    public string? ServiceUnfreezeOnDemand;
    public bool ServiceUnfreezeOnDemandEnabled = true;
    public string? ServiceShowFreezePage;
    public bool ServiceShowFreezePageEnabled = true;
    public bool ServiceCreateUnfreezeOnDemand;
    public string? EnvFile, EnvAction, EnvTarget;
    public bool KeyExtend;
    public bool SnapshotList;
    public string? SnapshotInfo, SnapshotDelete, SnapshotLock, SnapshotUnlock, SnapshotClone;
    public string? SnapshotCloneType, SnapshotName;
    public bool SnapshotHot;
    public bool ImageList;
    public string? ImageInfo, ImageDelete, ImageLock, ImageUnlock;
    public string? ImagePublish, ImageSourceType, ImageVisibility, ImageVisibilityMode;
    public string? ImageSpawn, ImageClone;
    public bool LanguagesJson;
}
