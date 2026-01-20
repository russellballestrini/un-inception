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


// Program.cs - Unsandbox CLI Client (.NET 10 Implementation)
// Build: dotnet build
// Run: dotnet run -- [options] <source_file>
// Requires: UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY environment variables

using System.Diagnostics;
using System.Net.Http.Headers;
using System.Security.Cryptography;
using System.Text;
using System.Text.Json;
using System.Text.Json.Nodes;

namespace Unsandbox.Cli;

class Program
{
    private const string API_BASE = "https://api.unsandbox.com";
    private const string PORTAL_BASE = "https://unsandbox.com";
    private const string BLUE = "\x1B[34m";
    private const string RED = "\x1B[31m";
    private const string GREEN = "\x1B[32m";
    private const string YELLOW = "\x1B[33m";
    private const string RESET = "\x1B[0m";

    private static readonly HttpClient httpClient = new()
    {
        Timeout = TimeSpan.FromMinutes(5)
    };

    private static readonly Dictionary<string, string> ExtMap = new()
    {
        {".py", "python"}, {".js", "javascript"}, {".ts", "typescript"},
        {".rb", "ruby"}, {".php", "php"}, {".pl", "perl"}, {".lua", "lua"},
        {".sh", "bash"}, {".go", "go"}, {".rs", "rust"}, {".c", "c"},
        {".cpp", "cpp"}, {".cc", "cpp"}, {".cxx", "cpp"},
        {".java", "java"}, {".kt", "kotlin"}, {".cs", "csharp"}, {".fs", "fsharp"},
        {".hs", "haskell"}, {".ml", "ocaml"}, {".clj", "clojure"}, {".scm", "scheme"},
        {".lisp", "commonlisp"}, {".erl", "erlang"}, {".ex", "elixir"}, {".exs", "elixir"},
        {".jl", "julia"}, {".r", "r"}, {".R", "r"}, {".cr", "crystal"},
        {".d", "d"}, {".nim", "nim"}, {".zig", "zig"}, {".v", "v"},
        {".dart", "dart"}, {".groovy", "groovy"}, {".scala", "scala"},
        {".f90", "fortran"}, {".f95", "fortran"}, {".cob", "cobol"},
        {".pro", "prolog"}, {".forth", "forth"}, {".4th", "forth"},
        {".tcl", "tcl"}, {".raku", "raku"}, {".m", "objc"}
    };

    static async Task<int> Main(string[] args)
    {
        try
        {
            var parsedArgs = ParseArgs(args);

            if (parsedArgs.Command == "session")
            {
                await CmdSession(parsedArgs);
            }
            else if (parsedArgs.Command == "service")
            {
                await CmdService(parsedArgs);
            }
            else if (parsedArgs.Command == "key")
            {
                await CmdKey(parsedArgs);
            }
            else if (parsedArgs.SourceFile != null)
            {
                return await CmdExecute(parsedArgs);
            }
            else
            {
                PrintHelp();
                return 1;
            }
            return 0;
        }
        catch (Exception ex)
        {
            Console.Error.WriteLine($"{RED}Error: {ex.Message}{RESET}");
            return 1;
        }
    }

    static async Task<int> CmdExecute(Args args)
    {
        var (publicKey, secretKey) = GetApiKeys(args.ApiKey);
        string code = await File.ReadAllTextAsync(args.SourceFile!);
        string language = DetectLanguage(args.SourceFile!);

        var payload = new Dictionary<string, object>
        {
            ["language"] = language,
            ["code"] = code
        };

        if (args.Env.Count > 0)
        {
            var envVars = new Dictionary<string, string>();
            foreach (var e in args.Env)
            {
                var parts = e.Split('=', 2);
                if (parts.Length == 2)
                {
                    envVars[parts[0]] = parts[1];
                }
            }
            if (envVars.Count > 0)
            {
                payload["env"] = envVars;
            }
        }

        if (args.Files.Count > 0)
        {
            var inputFiles = new List<Dictionary<string, string>>();
            foreach (var filepath in args.Files)
            {
                var content = await File.ReadAllBytesAsync(filepath);
                inputFiles.Add(new Dictionary<string, string>
                {
                    ["filename"] = Path.GetFileName(filepath),
                    ["content_base64"] = Convert.ToBase64String(content)
                });
            }
            payload["input_files"] = inputFiles;
        }

        if (args.Artifacts)
        {
            payload["return_artifacts"] = true;
        }
        if (args.Network != null)
        {
            payload["network"] = args.Network;
        }
        if (args.Vcpu > 0)
        {
            payload["vcpu"] = args.Vcpu;
        }

        var result = await ApiRequest("/execute", "POST", payload, publicKey, secretKey);

        if (result.TryGetValue("stdout", out var stdout) && !string.IsNullOrEmpty(stdout.ToString()))
        {
            Console.Write($"{BLUE}{stdout}{RESET}");
        }
        if (result.TryGetValue("stderr", out var stderr) && !string.IsNullOrEmpty(stderr.ToString()))
        {
            Console.Error.Write($"{RED}{stderr}{RESET}");
        }

        if (args.Artifacts && result.TryGetValue("artifacts", out var artifactsObj))
        {
            var artifacts = ((JsonArray)artifactsObj!).Deserialize<List<Dictionary<string, string>>>();
            string outDir = args.OutputDir ?? ".";
            Directory.CreateDirectory(outDir);

            if (artifacts != null)
            {
                foreach (var artifact in artifacts)
                {
                    string filename = artifact.ContainsKey("filename") ? artifact["filename"] : "artifact";
                    byte[] content = Convert.FromBase64String(artifact["content_base64"]);
                    string path = Path.Combine(outDir, filename);
                    await File.WriteAllBytesAsync(path, content);
                    Console.Error.WriteLine($"{GREEN}Saved: {path}{RESET}");
                }
            }
        }

        int exitCode = result.ContainsKey("exit_code") ? Convert.ToInt32(result["exit_code"]) : 0;
        return exitCode;
    }

    static async Task CmdSession(Args args)
    {
        var (publicKey, secretKey) = GetApiKeys(args.ApiKey);

        if (args.SessionList)
        {
            var result = await ApiRequest("/sessions", "GET", null, publicKey, secretKey);
            var sessions = result.TryGetValue("sessions", out var sessionsObj)
                ? ((JsonArray)sessionsObj!).Deserialize<List<Dictionary<string, string>>>()
                : null;

            if (sessions == null || sessions.Count == 0)
            {
                Console.WriteLine("No active sessions");
            }
            else
            {
                Console.WriteLine("{0,-40} {1,-10} {2,-10} {3}", "ID", "Shell", "Status", "Created");
                foreach (var s in sessions)
                {
                    Console.WriteLine("{0,-40} {1,-10} {2,-10} {3}",
                        s.GetValueOrDefault("id", "N/A"),
                        s.GetValueOrDefault("shell", "N/A"),
                        s.GetValueOrDefault("status", "N/A"),
                        s.GetValueOrDefault("created_at", "N/A"));
                }
            }
            return;
        }

        if (args.SessionKill != null)
        {
            await ApiRequest($"/sessions/{args.SessionKill}", "DELETE", null, publicKey, secretKey);
            Console.WriteLine($"{GREEN}Session terminated: {args.SessionKill}{RESET}");
            return;
        }

        var payload = new Dictionary<string, object>
        {
            ["shell"] = args.SessionShell ?? "bash"
        };
        if (args.Network != null)
        {
            payload["network"] = args.Network;
        }
        if (args.Vcpu > 0)
        {
            payload["vcpu"] = args.Vcpu;
        }

        Console.WriteLine($"{YELLOW}Creating session...{RESET}");
        var createResult = await ApiRequest("/sessions", "POST", payload, publicKey, secretKey);
        Console.WriteLine($"{GREEN}Session created: {createResult["id"]}{RESET}");
        Console.WriteLine($"{YELLOW}(Interactive sessions require WebSocket - use un2 for full support){RESET}");
    }

    static async Task CmdKey(Args args)
    {
        var (publicKey, secretKey) = GetApiKeys(args.ApiKey);

        var result = await ApiRequest("/keys/validate", "POST", null, publicKey, secretKey);

        if (!result.ContainsKey("valid"))
        {
            Console.Error.WriteLine($"{RED}Error: Invalid response from server{RESET}");
            Environment.Exit(1);
        }

        bool isValid = (bool)result["valid"]!;
        bool isExpired = result.TryGetValue("expired", out var expiredObj) && (bool)expiredObj!;

        if (isValid && !isExpired)
        {
            Console.WriteLine($"{GREEN}Valid{RESET}");
            if (result.TryGetValue("public_key", out var pk))
            {
                Console.WriteLine($"Public Key: {pk}");
            }
            if (result.TryGetValue("tier", out var tier))
            {
                Console.WriteLine($"Tier: {tier}");
            }
            if (result.TryGetValue("expires_at", out var expiresAt))
            {
                Console.WriteLine($"Expires: {expiresAt}");
            }
        }
        else if (isExpired)
        {
            Console.WriteLine($"{RED}Expired{RESET}");
            if (result.TryGetValue("public_key", out var pk))
            {
                Console.WriteLine($"Public Key: {pk}");
            }
            if (result.TryGetValue("tier", out var tier))
            {
                Console.WriteLine($"Tier: {tier}");
            }
            if (result.TryGetValue("expired_at", out var expiredAt))
            {
                Console.WriteLine($"Expired: {expiredAt}");
            }
            Console.WriteLine($"{YELLOW}To renew: Visit {PORTAL_BASE}/keys/extend{RESET}");

            if (args.KeyExtend && result.TryGetValue("public_key", out var pkForExtend))
            {
                string url = $"{PORTAL_BASE}/keys/extend?pk={pkForExtend}";
                Console.WriteLine($"{YELLOW}Opening: {url}{RESET}");
                OpenBrowser(url);
            }
        }
        else
        {
            Console.WriteLine($"{RED}Invalid{RESET}");
        }
    }

    static void OpenBrowser(string url)
    {
        try
        {
            if (OperatingSystem.IsWindows())
            {
                Process.Start(new ProcessStartInfo(url) { UseShellExecute = true });
            }
            else if (OperatingSystem.IsLinux())
            {
                Process.Start("xdg-open", url);
            }
            else if (OperatingSystem.IsMacOS())
            {
                Process.Start("open", url);
            }
        }
        catch (Exception ex)
        {
            Console.Error.WriteLine($"{RED}Failed to open browser: {ex.Message}{RESET}");
        }
    }

    static async Task CmdService(Args args)
    {
        var (publicKey, secretKey) = GetApiKeys(args.ApiKey);

        // Handle env subcommand
        if (!string.IsNullOrEmpty(args.EnvAction))
        {
            await CmdServiceEnv(args, publicKey, secretKey);
            return;
        }

        if (args.ServiceList)
        {
            var result = await ApiRequest("/services", "GET", null, publicKey, secretKey);
            var services = result.TryGetValue("services", out var servicesObj)
                ? ((JsonArray)servicesObj!).Deserialize<List<Dictionary<string, JsonElement>>>()
                : null;

            if (services == null || services.Count == 0)
            {
                Console.WriteLine("No services");
            }
            else
            {
                Console.WriteLine("{0,-20} {1,-15} {2,-10} {3,-15} {4}", "ID", "Name", "Status", "Ports", "Domains");
                foreach (var s in services)
                {
                    var ports = s.ContainsKey("ports") ? s["ports"].Deserialize<List<int>>() : null;
                    var domains = s.ContainsKey("domains") ? s["domains"].Deserialize<List<string>>() : null;
                    string portsStr = ports != null ? string.Join(",", ports) : "";
                    string domainsStr = domains != null ? string.Join(",", domains) : "";
                    Console.WriteLine("{0,-20} {1,-15} {2,-10} {3,-15} {4}",
                        s.GetValueOrDefault("id").ToString(),
                        s.GetValueOrDefault("name").ToString(),
                        s.GetValueOrDefault("status").ToString(),
                        portsStr, domainsStr);
                }
            }
            return;
        }

        if (args.ServiceInfo != null)
        {
            var result = await ApiRequest($"/services/{args.ServiceInfo}", "GET", null, publicKey, secretKey);
            Console.WriteLine(JsonSerializer.Serialize(result, new JsonSerializerOptions { WriteIndented = true }));
            return;
        }

        if (args.ServiceLogs != null)
        {
            var result = await ApiRequest($"/services/{args.ServiceLogs}/logs", "GET", null, publicKey, secretKey);
            Console.WriteLine(result.GetValueOrDefault("logs", ""));
            return;
        }

        if (args.ServiceTail != null)
        {
            var result = await ApiRequest($"/services/{args.ServiceTail}/logs?lines=9000", "GET", null, publicKey, secretKey);
            Console.WriteLine(result.GetValueOrDefault("logs", ""));
            return;
        }

        if (args.ServiceSleep != null)
        {
            await ApiRequest($"/services/{args.ServiceSleep}/freeze", "POST", null, publicKey, secretKey);
            Console.WriteLine($"{GREEN}Service frozen: {args.ServiceSleep}{RESET}");
            return;
        }

        if (args.ServiceWake != null)
        {
            await ApiRequest($"/services/{args.ServiceWake}/unfreeze", "POST", null, publicKey, secretKey);
            Console.WriteLine($"{GREEN}Service unfreezing: {args.ServiceWake}{RESET}");
            return;
        }

        if (args.ServiceDestroy != null)
        {
            await ApiRequest($"/services/{args.ServiceDestroy}", "DELETE", null, publicKey, secretKey);
            Console.WriteLine($"{GREEN}Service destroyed: {args.ServiceDestroy}{RESET}");
            return;
        }

        if (args.ServiceExecute != null)
        {
            var payload = new Dictionary<string, object>
            {
                ["command"] = args.ServiceCommand!
            };
            var result = await ApiRequest($"/services/{args.ServiceExecute}/execute", "POST", payload, publicKey, secretKey);
            if (result.TryGetValue("stdout", out var stdout) && !string.IsNullOrEmpty(stdout.ToString()))
            {
                Console.Write($"{BLUE}{stdout}{RESET}");
            }
            if (result.TryGetValue("stderr", out var stderr) && !string.IsNullOrEmpty(stderr.ToString()))
            {
                Console.Error.Write($"{RED}{stderr}{RESET}");
            }
            return;
        }

        if (args.ServiceDumpBootstrap != null)
        {
            Console.Error.WriteLine($"Fetching bootstrap script from {args.ServiceDumpBootstrap}...");
            var payload = new Dictionary<string, object>
            {
                ["command"] = "cat /tmp/bootstrap.sh"
            };
            var result = await ApiRequest($"/services/{args.ServiceDumpBootstrap}/execute", "POST", payload, publicKey, secretKey);

            var bootstrap = result.TryGetValue("stdout", out var stdout) ? stdout?.ToString() : null;
            if (!string.IsNullOrEmpty(bootstrap))
            {
                if (args.ServiceDumpFile != null)
                {
                    try
                    {
                        await File.WriteAllTextAsync(args.ServiceDumpFile, bootstrap);
                        Console.WriteLine($"Bootstrap saved to {args.ServiceDumpFile}");
                    }
                    catch (Exception e)
                    {
                        Console.Error.WriteLine($"{RED}Error: Could not write to {args.ServiceDumpFile}: {e.Message}{RESET}");
                        Environment.Exit(1);
                    }
                }
                else
                {
                    Console.Write(bootstrap);
                }
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
            var payload = new Dictionary<string, object>
            {
                ["name"] = args.ServiceName
            };
            if (args.ServicePorts != null)
            {
                var ports = args.ServicePorts.Split(',').Select(p => int.Parse(p.Trim())).ToList();
                payload["ports"] = ports;
            }
            if (args.ServiceType != null)
            {
                payload["service_type"] = args.ServiceType;
            }
            if (args.ServiceBootstrap != null)
            {
                payload["bootstrap"] = args.ServiceBootstrap;
            }
            if (args.Network != null)
            {
                payload["network"] = args.Network;
            }
            if (args.Vcpu > 0)
            {
                payload["vcpu"] = args.Vcpu;
            }

            var result = await ApiRequest("/services", "POST", payload, publicKey, secretKey);
            string? serviceId = result.TryGetValue("id", out var idObj) ? idObj?.ToString() : null;
            Console.WriteLine($"{GREEN}Service created: {serviceId}{RESET}");
            Console.WriteLine($"Name: {result["name"]}");
            if (result.TryGetValue("url", out var url))
            {
                Console.WriteLine($"URL: {url}");
            }

            // Auto-set vault if env vars were provided
            if (!string.IsNullOrEmpty(serviceId) && (args.Env.Count > 0 || !string.IsNullOrEmpty(args.EnvFile)))
            {
                string envContent = await BuildEnvContent(args.Env, args.EnvFile);
                if (!string.IsNullOrEmpty(envContent))
                {
                    if (await ServiceEnvSet(serviceId, envContent, publicKey, secretKey))
                    {
                        Console.WriteLine($"{GREEN}Vault configured with environment variables{RESET}");
                    }
                    else
                    {
                        Console.Error.WriteLine($"{YELLOW}Warning: Failed to set vault{RESET}");
                    }
                }
            }
            return;
        }

        Console.Error.WriteLine($"{RED}Error: Specify --name to create a service, or use --list, --info, etc.{RESET}");
        Environment.Exit(1);
    }

    static (string, string) GetApiKeys(string? argsKey)
    {
        string? publicKey = Environment.GetEnvironmentVariable("UNSANDBOX_PUBLIC_KEY");
        string? secretKey = Environment.GetEnvironmentVariable("UNSANDBOX_SECRET_KEY");

        // Fall back to UNSANDBOX_API_KEY for backwards compatibility
        if (string.IsNullOrEmpty(publicKey) || string.IsNullOrEmpty(secretKey))
        {
            string? legacyKey = argsKey ?? Environment.GetEnvironmentVariable("UNSANDBOX_API_KEY");
            if (string.IsNullOrEmpty(legacyKey))
            {
                Console.Error.WriteLine($"{RED}Error: UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY not set{RESET}");
                Environment.Exit(1);
            }
            return (legacyKey, string.Empty);
        }

        return (publicKey, secretKey);
    }

    static string DetectLanguage(string filename)
    {
        int dotIndex = filename.LastIndexOf('.');
        if (dotIndex == -1)
        {
            throw new Exception("Cannot detect language: no file extension");
        }
        string ext = filename[dotIndex..].ToLower();
        if (!ExtMap.TryGetValue(ext, out var language))
        {
            throw new Exception($"Unsupported file extension: {ext}");
        }
        return language;
    }

    static async Task<Dictionary<string, object?>> ApiRequest(
        string endpoint,
        string method,
        Dictionary<string, object>? data,
        string publicKey,
        string secretKey)
    {
        using var request = new HttpRequestMessage(new HttpMethod(method), API_BASE + endpoint);
        request.Headers.Accept.Add(new MediaTypeWithQualityHeaderValue("application/json"));

        string body = "";
        if (data != null)
        {
            body = JsonSerializer.Serialize(data);
            request.Content = new StringContent(body, Encoding.UTF8, "application/json");
        }

        // Add HMAC authentication headers if secretKey is provided
        if (!string.IsNullOrEmpty(secretKey))
        {
            long timestamp = DateTimeOffset.UtcNow.ToUnixTimeSeconds();
            string message = $"{timestamp}:{method}:{endpoint}:{body}";

            using var hmac = new HMACSHA256(Encoding.UTF8.GetBytes(secretKey));
            byte[] hash = hmac.ComputeHash(Encoding.UTF8.GetBytes(message));
            string signature = Convert.ToHexString(hash).ToLower();

            request.Headers.Authorization = new AuthenticationHeaderValue("Bearer", publicKey);
            request.Headers.Add("X-Timestamp", timestamp.ToString());
            request.Headers.Add("X-Signature", signature);
        }
        else
        {
            // Legacy API key authentication
            request.Headers.Authorization = new AuthenticationHeaderValue("Bearer", publicKey);
        }

        try
        {
            using var response = await httpClient.SendAsync(request);
            string responseText = await response.Content.ReadAsStringAsync();

            if (!response.IsSuccessStatusCode)
            {
                // Check for clock drift errors
                if (responseText.Contains("timestamp") &&
                    ((int)response.StatusCode == 401 || responseText.ToLower().Contains("expired") || responseText.ToLower().Contains("invalid")))
                {
                    Console.Error.WriteLine($"{RED}Error: Request timestamp expired (must be within 5 minutes of server time){RESET}");
                    Console.Error.WriteLine($"{YELLOW}Your computer's clock may have drifted.{RESET}");
                    Console.Error.WriteLine("Check your system time and sync with NTP if needed:");
                    Console.Error.WriteLine("  Linux:   sudo ntpdate -s time.nist.gov");
                    Console.Error.WriteLine("  macOS:   sudo sntp -sS time.apple.com");
                    Console.Error.WriteLine("  Windows: w32tm /resync");
                    Environment.Exit(1);
                }

                throw new Exception($"HTTP {(int)response.StatusCode} - {responseText}");
            }

            return JsonSerializer.Deserialize<Dictionary<string, object?>>(responseText)
                ?? new Dictionary<string, object?>();
        }
        catch (HttpRequestException ex)
        {
            throw new Exception($"HTTP error - {ex.Message}");
        }
    }

    static async Task<string> ApiRequestText(
        string endpoint,
        string method,
        string? body,
        string publicKey,
        string secretKey)
    {
        using var request = new HttpRequestMessage(new HttpMethod(method), API_BASE + endpoint);

        body ??= "";
        if (!string.IsNullOrEmpty(body))
        {
            request.Content = new StringContent(body, Encoding.UTF8, "text/plain");
        }

        // Add HMAC authentication headers
        if (!string.IsNullOrEmpty(secretKey))
        {
            long timestamp = DateTimeOffset.UtcNow.ToUnixTimeSeconds();
            string message = $"{timestamp}:{method}:{endpoint}:{body}";

            using var hmac = new HMACSHA256(Encoding.UTF8.GetBytes(secretKey));
            byte[] hash = hmac.ComputeHash(Encoding.UTF8.GetBytes(message));
            string signature = Convert.ToHexString(hash).ToLower();

            request.Headers.Authorization = new AuthenticationHeaderValue("Bearer", publicKey);
            request.Headers.Add("X-Timestamp", timestamp.ToString());
            request.Headers.Add("X-Signature", signature);
        }
        else
        {
            request.Headers.Authorization = new AuthenticationHeaderValue("Bearer", publicKey);
        }

        try
        {
            using var response = await httpClient.SendAsync(request);
            string responseText = await response.Content.ReadAsStringAsync();

            if (!response.IsSuccessStatusCode)
            {
                throw new Exception($"HTTP {(int)response.StatusCode} - {responseText}");
            }

            return responseText;
        }
        catch (HttpRequestException ex)
        {
            throw new Exception($"HTTP error - {ex.Message}");
        }
    }

    static async Task<string> ReadEnvFile(string path)
    {
        if (!File.Exists(path))
        {
            throw new Exception($"Env file not found: {path}");
        }
        return await File.ReadAllTextAsync(path);
    }

    static async Task<string> BuildEnvContent(List<string> envs, string? envFile)
    {
        var lines = new List<string>();

        // Add from -e flags
        lines.AddRange(envs);

        // Add from --env-file
        if (!string.IsNullOrEmpty(envFile))
        {
            string content = await ReadEnvFile(envFile);
            foreach (var line in content.Split('\n'))
            {
                string trimmed = line.Trim();
                if (!string.IsNullOrEmpty(trimmed) && !trimmed.StartsWith("#"))
                {
                    lines.Add(trimmed);
                }
            }
        }

        return string.Join("\n", lines);
    }

    static async Task<Dictionary<string, object?>> ServiceEnvStatus(string serviceId, string publicKey, string secretKey)
    {
        return await ApiRequest($"/services/{serviceId}/env", "GET", null, publicKey, secretKey);
    }

    static async Task<bool> ServiceEnvSet(string serviceId, string envContent, string publicKey, string secretKey)
    {
        const int MAX_ENV_CONTENT_SIZE = 65536;
        if (envContent.Length > MAX_ENV_CONTENT_SIZE)
        {
            Console.Error.WriteLine($"{RED}Error: Env content exceeds maximum size of 64KB{RESET}");
            return false;
        }

        try
        {
            await ApiRequestText($"/services/{serviceId}/env", "PUT", envContent, publicKey, secretKey);
            return true;
        }
        catch
        {
            return false;
        }
    }

    static async Task<Dictionary<string, object?>> ServiceEnvExport(string serviceId, string publicKey, string secretKey)
    {
        return await ApiRequest($"/services/{serviceId}/env/export", "POST", null, publicKey, secretKey);
    }

    static async Task<bool> ServiceEnvDelete(string serviceId, string publicKey, string secretKey)
    {
        try
        {
            await ApiRequest($"/services/{serviceId}/env", "DELETE", null, publicKey, secretKey);
            return true;
        }
        catch
        {
            return false;
        }
    }

    static async Task CmdServiceEnv(Args args, string publicKey, string secretKey)
    {
        string action = args.EnvAction!;
        string? target = args.EnvTarget;

        if (action == "status")
        {
            if (string.IsNullOrEmpty(target))
            {
                Console.Error.WriteLine($"{RED}Error: service env status requires service ID{RESET}");
                Environment.Exit(1);
            }
            var result = await ServiceEnvStatus(target, publicKey, secretKey);
            if (result.TryGetValue("has_vault", out var hasVaultObj) && (bool)hasVaultObj!)
            {
                Console.WriteLine($"{GREEN}Vault: configured{RESET}");
                if (result.TryGetValue("env_count", out var envCount))
                {
                    Console.WriteLine($"Variables: {envCount}");
                }
                if (result.TryGetValue("updated_at", out var updatedAt))
                {
                    Console.WriteLine($"Updated: {updatedAt}");
                }
            }
            else
            {
                Console.WriteLine($"{YELLOW}Vault: not configured{RESET}");
            }
        }
        else if (action == "set")
        {
            if (string.IsNullOrEmpty(target))
            {
                Console.Error.WriteLine($"{RED}Error: service env set requires service ID{RESET}");
                Environment.Exit(1);
            }
            if (args.Env.Count == 0 && string.IsNullOrEmpty(args.EnvFile))
            {
                Console.Error.WriteLine($"{RED}Error: service env set requires -e or --env-file{RESET}");
                Environment.Exit(1);
            }
            string envContent = await BuildEnvContent(args.Env, args.EnvFile);
            if (await ServiceEnvSet(target, envContent, publicKey, secretKey))
            {
                Console.WriteLine($"{GREEN}Vault updated for service {target}{RESET}");
            }
            else
            {
                Console.Error.WriteLine($"{RED}Error: Failed to update vault{RESET}");
                Environment.Exit(1);
            }
        }
        else if (action == "export")
        {
            if (string.IsNullOrEmpty(target))
            {
                Console.Error.WriteLine($"{RED}Error: service env export requires service ID{RESET}");
                Environment.Exit(1);
            }
            var result = await ServiceEnvExport(target, publicKey, secretKey);
            if (result.TryGetValue("content", out var content))
            {
                Console.Write(content);
            }
        }
        else if (action == "delete")
        {
            if (string.IsNullOrEmpty(target))
            {
                Console.Error.WriteLine($"{RED}Error: service env delete requires service ID{RESET}");
                Environment.Exit(1);
            }
            if (await ServiceEnvDelete(target, publicKey, secretKey))
            {
                Console.WriteLine($"{GREEN}Vault deleted for service {target}{RESET}");
            }
            else
            {
                Console.Error.WriteLine($"{RED}Error: Failed to delete vault{RESET}");
                Environment.Exit(1);
            }
        }
        else
        {
            Console.Error.WriteLine($"{RED}Error: Unknown env action: {action}{RESET}");
            Console.Error.WriteLine("Usage: un service env <status|set|export|delete> <service_id>");
            Environment.Exit(1);
        }
    }

    class Args
    {
        public string? Command;
        public string? SourceFile;
        public string? ApiKey;
        public string? Network;
        public int Vcpu;
        public List<string> Env = new();
        public List<string> Files = new();
        public bool Artifacts;
        public string? OutputDir;
        public bool SessionList;
        public string? SessionShell;
        public string? SessionKill;
        public bool ServiceList;
        public string? ServiceName;
        public string? ServicePorts;
        public string? ServiceBootstrap;
        public string? ServiceInfo;
        public string? ServiceLogs;
        public string? ServiceTail;
        public string? ServiceSleep;
        public string? ServiceWake;
        public string? ServiceDestroy;
        public string? ServiceType;
        public string? ServiceExecute;
        public string? ServiceCommand;
        public string? ServiceDumpBootstrap;
        public string? ServiceDumpFile;
        public string? EnvFile;
        public string? EnvAction;
        public string? EnvTarget;
        public bool KeyExtend;
    }

    static Args ParseArgs(string[] args)
    {
        var result = new Args();
        for (int i = 0; i < args.Length; i++)
        {
            string arg = args[i];
            if (arg == "session") result.Command = "session";
            else if (arg == "service") result.Command = "service";
            else if (arg == "key") result.Command = "key";
            else if (arg == "env" && result.Command == "service")
            {
                // Parse: service env <action> <target>
                if (i + 1 < args.Length && !args[i + 1].StartsWith("-"))
                {
                    result.EnvAction = args[++i];
                    if (i + 1 < args.Length && !args[i + 1].StartsWith("-"))
                    {
                        result.EnvTarget = args[++i];
                    }
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
            }
            else if (arg == "-s" || arg == "--shell") result.SessionShell = args[++i];
            else if (arg == "--kill") result.SessionKill = args[++i];
            else if (arg == "--name") result.ServiceName = args[++i];
            else if (arg == "--ports") result.ServicePorts = args[++i];
            else if (arg == "--type") result.ServiceType = args[++i];
            else if (arg == "--bootstrap") result.ServiceBootstrap = args[++i];
            else if (arg == "--info") result.ServiceInfo = args[++i];
            else if (arg == "--logs") result.ServiceLogs = args[++i];
            else if (arg == "--tail") result.ServiceTail = args[++i];
            else if (arg == "--freeze") result.ServiceSleep = args[++i];
            else if (arg == "--unfreeze") result.ServiceWake = args[++i];
            else if (arg == "--destroy") result.ServiceDestroy = args[++i];
            else if (arg == "--execute") result.ServiceExecute = args[++i];
            else if (arg == "--command") result.ServiceCommand = args[++i];
            else if (arg == "--dump-bootstrap") result.ServiceDumpBootstrap = args[++i];
            else if (arg == "--dump-file") result.ServiceDumpFile = args[++i];
            else if (arg == "--extend") result.KeyExtend = true;
            else if (!arg.StartsWith("-")) result.SourceFile = arg;
        }
        return result;
    }

    static void PrintHelp()
    {
        Console.WriteLine(@"Usage: un [options] <source_file>
       un session [options]
       un service [options]
       un service env <action> <service_id> [options]
       un key [options]

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

Service options:
  --list            List services
  --name NAME       Service name
  --ports PORTS     Comma-separated ports
  --type TYPE       Service type (minecraft/mumble/teamspeak/source/tcp/udp)
  --bootstrap CMD   Bootstrap command
  --info ID         Get service details
  --logs ID         Get all logs
  --tail ID         Get last 9000 lines
  --freeze ID       Freeze service
  --unfreeze ID     Unfreeze service
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

Key options:
  --extend          Open browser to extend expired key");
    }
}
