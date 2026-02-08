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


// Un.cs - Unsandbox CLI Client (C# Implementation)
// Compile: csc Un.cs (or mcs Un.cs on Linux)
// Run: Un.exe [options] <source_file>
// Requires: UNSANDBOX_API_KEY environment variable

using System;
using System.Collections.Generic;
using System.IO;
using System.Net;
using System.Text;
using System.Security.Cryptography;

class Un
{
    private const string API_BASE = "https://api.unsandbox.com";
    private const string PORTAL_BASE = "https://unsandbox.com";
    private const string BLUE = "\x1B[34m";
    private const string RED = "\x1B[31m";
    private const string GREEN = "\x1B[32m";
    private const string YELLOW = "\x1B[33m";
    private const string RESET = "\x1B[0m";

    private static readonly Dictionary<string, string> ExtMap = new Dictionary<string, string>
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

    static void Main(string[] args)
    {
        try
        {
            var parsedArgs = ParseArgs(args);

            if (parsedArgs.Command == "session")
            {
                CmdSession(parsedArgs);
            }
            else if (parsedArgs.Command == "service")
            {
                CmdService(parsedArgs);
            }
            else if (parsedArgs.Command == "key")
            {
                CmdKey(parsedArgs);
            }
            else if (parsedArgs.SourceFile != null)
            {
                CmdExecute(parsedArgs);
            }
            else
            {
                PrintHelp();
                Environment.Exit(1);
            }
        }
        catch (Exception ex)
        {
            Console.Error.WriteLine($"{RED}Error: {ex.Message}{RESET}");
            Environment.Exit(1);
        }
    }

    static void CmdExecute(Args args)
    {
        var (publicKey, secretKey) = GetApiKeys(args.ApiKey);
        string code = File.ReadAllText(args.SourceFile);
        string language = DetectLanguage(args.SourceFile);

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
                var parts = e.Split(new[] { '=' }, 2);
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
                var content = File.ReadAllBytes(filepath);
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

        var result = ApiRequest("/execute", "POST", payload, publicKey, secretKey);

        if (result.ContainsKey("stdout") && !string.IsNullOrEmpty((string)result["stdout"]))
        {
            Console.Write($"{BLUE}{result["stdout"]}{RESET}");
        }
        if (result.ContainsKey("stderr") && !string.IsNullOrEmpty((string)result["stderr"]))
        {
            Console.Error.Write($"{RED}{result["stderr"]}{RESET}");
        }

        if (args.Artifacts && result.ContainsKey("artifacts"))
        {
            var artifacts = result["artifacts"] as List<object>;
            string outDir = args.OutputDir ?? ".";
            Directory.CreateDirectory(outDir);
            foreach (Dictionary<string, object> artifact in artifacts)
            {
                string filename = artifact.ContainsKey("filename") ? (string)artifact["filename"] : "artifact";
                byte[] content = Convert.FromBase64String((string)artifact["content_base64"]);
                string path = Path.Combine(outDir, filename);
                File.WriteAllBytes(path, content);
                Console.Error.WriteLine($"{GREEN}Saved: {path}{RESET}");
            }
        }

        int exitCode = result.ContainsKey("exit_code") ? Convert.ToInt32(result["exit_code"]) : 0;
        Environment.Exit(exitCode);
    }

    static void CmdSession(Args args)
    {
        var (publicKey, secretKey) = GetApiKeys(args.ApiKey);

        if (args.SessionList)
        {
            var result = ApiRequest("/sessions", "GET", null, publicKey, secretKey);
            var sessions = result.ContainsKey("sessions") ? result["sessions"] as List<object> : null;
            if (sessions == null || sessions.Count == 0)
            {
                Console.WriteLine("No active sessions");
            }
            else
            {
                Console.WriteLine("{0,-40} {1,-10} {2,-10} {3}", "ID", "Shell", "Status", "Created");
                foreach (Dictionary<string, object> s in sessions)
                {
                    Console.WriteLine("{0,-40} {1,-10} {2,-10} {3}",
                        s.ContainsKey("id") ? s["id"] : "N/A",
                        s.ContainsKey("shell") ? s["shell"] : "N/A",
                        s.ContainsKey("status") ? s["status"] : "N/A",
                        s.ContainsKey("created_at") ? s["created_at"] : "N/A");
                }
            }
            return;
        }

        if (args.SessionKill != null)
        {
            ApiRequest($"/sessions/{args.SessionKill}", "DELETE", null, publicKey, secretKey);
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
        var createResult = ApiRequest("/sessions", "POST", payload, publicKey, secretKey);
        Console.WriteLine($"{GREEN}Session created: {createResult["id"]}{RESET}");
        Console.WriteLine($"{YELLOW}(Interactive sessions require WebSocket - use un2 for full support){RESET}");
    }

    static void CmdKey(Args args)
    {
        var (publicKey, secretKey) = GetApiKeys(args.ApiKey);

        var result = ApiRequest("/keys/validate", "POST", null, publicKey, secretKey);

        if (!result.ContainsKey("valid"))
        {
            Console.Error.WriteLine($"{RED}Error: Invalid response from server{RESET}");
            Environment.Exit(1);
        }

        bool isValid = (bool)result["valid"];
        bool isExpired = result.ContainsKey("expired") && (bool)result["expired"];

        if (isValid && !isExpired)
        {
            Console.WriteLine($"{GREEN}Valid{RESET}");
            if (result.ContainsKey("public_key"))
            {
                Console.WriteLine($"Public Key: {result["public_key"]}");
            }
            if (result.ContainsKey("tier"))
            {
                Console.WriteLine($"Tier: {result["tier"]}");
            }
            if (result.ContainsKey("expires_at"))
            {
                Console.WriteLine($"Expires: {result["expires_at"]}");
            }
        }
        else if (isExpired)
        {
            Console.WriteLine($"{RED}Expired{RESET}");
            if (result.ContainsKey("public_key"))
            {
                Console.WriteLine($"Public Key: {result["public_key"]}");
            }
            if (result.ContainsKey("tier"))
            {
                Console.WriteLine($"Tier: {result["tier"]}");
            }
            if (result.ContainsKey("expired_at"))
            {
                Console.WriteLine($"Expired: {result["expired_at"]}");
            }
            Console.WriteLine($"{YELLOW}To renew: Visit {PORTAL_BASE}/keys/extend{RESET}");

            if (args.KeyExtend && result.ContainsKey("public_key"))
            {
                string publicKey = (string)result["public_key"];
                string url = $"{PORTAL_BASE}/keys/extend?pk={publicKey}";
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
            if (Environment.OSVersion.Platform == PlatformID.Win32NT)
            {
                System.Diagnostics.Process.Start(new System.Diagnostics.ProcessStartInfo(url) { UseShellExecute = true });
            }
            else if (Environment.OSVersion.Platform == PlatformID.Unix)
            {
                System.Diagnostics.Process.Start("xdg-open", url);
            }
            else if (Environment.OSVersion.Platform == PlatformID.MacOSX)
            {
                System.Diagnostics.Process.Start("open", url);
            }
        }
        catch (Exception ex)
        {
            Console.Error.WriteLine($"{RED}Failed to open browser: {ex.Message}{RESET}");
        }
    }

    static void CmdService(Args args)
    {
        var (publicKey, secretKey) = GetApiKeys(args.ApiKey);

        // Handle env subcommand
        if (!string.IsNullOrEmpty(args.EnvAction))
        {
            CmdServiceEnv(args, publicKey, secretKey);
            return;
        }

        if (args.ServiceList)
        {
            var result = ApiRequest("/services", "GET", null, publicKey, secretKey);
            var services = result.ContainsKey("services") ? result["services"] as List<object> : null;
            if (services == null || services.Count == 0)
            {
                Console.WriteLine("No services");
            }
            else
            {
                Console.WriteLine("{0,-20} {1,-15} {2,-10} {3,-15} {4}", "ID", "Name", "Status", "Ports", "Domains");
                foreach (Dictionary<string, object> s in services)
                {
                    var ports = s.ContainsKey("ports") ? s["ports"] as List<object> : null;
                    var domains = s.ContainsKey("domains") ? s["domains"] as List<object> : null;
                    string portsStr = ports != null ? string.Join(",", ports) : "";
                    string domainsStr = domains != null ? string.Join(",", domains) : "";
                    Console.WriteLine("{0,-20} {1,-15} {2,-10} {3,-15} {4}",
                        s.ContainsKey("id") ? s["id"] : "N/A",
                        s.ContainsKey("name") ? s["name"] : "N/A",
                        s.ContainsKey("status") ? s["status"] : "N/A",
                        portsStr, domainsStr);
                }
            }
            return;
        }

        if (args.ServiceInfo != null)
        {
            var result = ApiRequest($"/services/{args.ServiceInfo}", "GET", null, publicKey, secretKey);
            Console.WriteLine(ToJson(result));
            return;
        }

        if (args.ServiceLogs != null)
        {
            var result = ApiRequest($"/services/{args.ServiceLogs}/logs", "GET", null, publicKey, secretKey);
            Console.WriteLine(result.ContainsKey("logs") ? result["logs"] : "");
            return;
        }

        if (args.ServiceTail != null)
        {
            var result = ApiRequest($"/services/{args.ServiceTail}/logs?lines=9000", "GET", null, publicKey, secretKey);
            Console.WriteLine(result.ContainsKey("logs") ? result["logs"] : "");
            return;
        }

        if (args.ServiceSleep != null)
        {
            ApiRequest($"/services/{args.ServiceSleep}/freeze", "POST", null, publicKey, secretKey);
            Console.WriteLine($"{GREEN}Service frozen: {args.ServiceSleep}{RESET}");
            return;
        }

        if (args.ServiceWake != null)
        {
            ApiRequest($"/services/{args.ServiceWake}/unfreeze", "POST", null, publicKey, secretKey);
            Console.WriteLine($"{GREEN}Service unfreezing: {args.ServiceWake}{RESET}");
            return;
        }

        if (args.ServiceUnfreezeOnDemand != null)
        {
            var payload = new Dictionary<string, object>
            {
                ["unfreeze_on_demand"] = args.ServiceUnfreezeOnDemandEnabled
            };
            ApiRequest($"/services/{args.ServiceUnfreezeOnDemand}", "PATCH", payload, publicKey, secretKey);
            string status = args.ServiceUnfreezeOnDemandEnabled ? "enabled" : "disabled";
            Console.WriteLine($"{GREEN}Unfreeze-on-demand {status} for service: {args.ServiceUnfreezeOnDemand}{RESET}");
            return;
        }

        if (args.ServiceShowFreezePage != null)
        {
            var payload = new Dictionary<string, object>
            {
                ["show_freeze_page"] = args.ServiceShowFreezePageEnabled
            };
            ApiRequest($"/services/{args.ServiceShowFreezePage}", "PATCH", payload, publicKey, secretKey);
            string status = args.ServiceShowFreezePageEnabled ? "enabled" : "disabled";
            Console.WriteLine($"{GREEN}Show-freeze-page {status} for service: {args.ServiceShowFreezePage}{RESET}");
            return;
        }

        if (args.ServiceDestroy != null)
        {
            ApiRequestWithSudo($"/services/{args.ServiceDestroy}", "DELETE", null, publicKey, secretKey);
            Console.WriteLine($"{GREEN}Service destroyed: {args.ServiceDestroy}{RESET}");
            return;
        }

        if (args.ServiceExecute != null)
        {
            var payload = new Dictionary<string, object>
            {
                ["command"] = args.ServiceCommand
            };
            var result = ApiRequest($"/services/{args.ServiceExecute}/execute", "POST", payload, publicKey, secretKey);
            if (result.ContainsKey("stdout") && !string.IsNullOrEmpty((string)result["stdout"]))
            {
                Console.Write($"{BLUE}{result["stdout"]}{RESET}");
            }
            if (result.ContainsKey("stderr") && !string.IsNullOrEmpty((string)result["stderr"]))
            {
                Console.Error.Write($"{RED}{result["stderr"]}{RESET}");
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
            var result = ApiRequest($"/services/{args.ServiceDumpBootstrap}/execute", "POST", payload, publicKey, secretKey);

            var bootstrap = result.ContainsKey("stdout") ? (string)result["stdout"] : null;
            if (!string.IsNullOrEmpty(bootstrap))
            {
                if (args.ServiceDumpFile != null)
                {
                    try
                    {
                        File.WriteAllText(args.ServiceDumpFile, bootstrap);
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
                var ports = new List<int>();
                foreach (var p in args.ServicePorts.Split(','))
                {
                    ports.Add(int.Parse(p.Trim()));
                }
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
            if (args.ServiceCreateUnfreezeOnDemand)
            {
                payload["unfreeze_on_demand"] = true;
            }

            var result = ApiRequest("/services", "POST", payload, publicKey, secretKey);
            string serviceId = result.ContainsKey("id") ? (string)result["id"] : null;
            Console.WriteLine($"{GREEN}Service created: {serviceId}{RESET}");
            Console.WriteLine($"Name: {result["name"]}");
            if (result.ContainsKey("url"))
            {
                Console.WriteLine($"URL: {result["url"]}");
            }

            // Auto-set vault if env vars were provided
            if (!string.IsNullOrEmpty(serviceId) && (args.Env.Count > 0 || !string.IsNullOrEmpty(args.EnvFile)))
            {
                string envContent = BuildEnvContent(args.Env, args.EnvFile);
                if (!string.IsNullOrEmpty(envContent))
                {
                    if (ServiceEnvSet(serviceId, envContent, publicKey, secretKey))
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

    static (string, string) GetApiKeys(string argsKey)
    {
        string publicKey = Environment.GetEnvironmentVariable("UNSANDBOX_PUBLIC_KEY");
        string secretKey = Environment.GetEnvironmentVariable("UNSANDBOX_SECRET_KEY");

        // Fall back to UNSANDBOX_API_KEY for backwards compatibility
        if (string.IsNullOrEmpty(publicKey) || string.IsNullOrEmpty(secretKey))
        {
            string legacyKey = argsKey ?? Environment.GetEnvironmentVariable("UNSANDBOX_API_KEY");
            if (string.IsNullOrEmpty(legacyKey))
            {
                Console.Error.WriteLine($"{RED}Error: UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY not set{RESET}");
                Environment.Exit(1);
            }
            return (legacyKey, null);
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
        string ext = filename.Substring(dotIndex).ToLower();
        if (!ExtMap.ContainsKey(ext))
        {
            throw new Exception($"Unsupported file extension: {ext}");
        }
        return ExtMap[ext];
    }

    static Dictionary<string, object> ApiRequest(string endpoint, string method, Dictionary<string, object> data, string publicKey, string secretKey, string sudoOtp = null, string sudoChallengeId = null)
    {
        ServicePointManager.SecurityProtocol = SecurityProtocolType.Tls12 | SecurityProtocolType.Tls11 | SecurityProtocolType.Tls;

        HttpWebRequest request = (HttpWebRequest)WebRequest.Create(API_BASE + endpoint);
        request.Method = method;
        request.ContentType = "application/json";
        request.Timeout = 300000;

        string body = "";
        if (data != null)
        {
            body = ToJson(data);
        }

        // Add HMAC authentication headers if secretKey is provided
        if (!string.IsNullOrEmpty(secretKey))
        {
            long timestamp = DateTimeOffset.UtcNow.ToUnixTimeSeconds();
            string message = $"{timestamp}:{method}:{endpoint}:{body}";

            using (var hmac = new HMACSHA256(Encoding.UTF8.GetBytes(secretKey)))
            {
                byte[] hash = hmac.ComputeHash(Encoding.UTF8.GetBytes(message));
                string signature = BitConverter.ToString(hash).Replace("-", "").ToLower();

                request.Headers.Add("Authorization", $"Bearer {publicKey}");
                request.Headers.Add("X-Timestamp", timestamp.ToString());
                request.Headers.Add("X-Signature", signature);
            }
        }
        else
        {
            // Legacy API key authentication
            request.Headers.Add("Authorization", $"Bearer {publicKey}");
        }

        // Add sudo OTP headers if provided
        if (!string.IsNullOrEmpty(sudoOtp))
        {
            request.Headers.Add("X-Sudo-OTP", sudoOtp);
        }
        if (!string.IsNullOrEmpty(sudoChallengeId))
        {
            request.Headers.Add("X-Sudo-Challenge", sudoChallengeId);
        }

        if (data != null)
        {
            byte[] bytes = Encoding.UTF8.GetBytes(body);
            request.ContentLength = bytes.Length;
            using (Stream stream = request.GetRequestStream())
            {
                stream.Write(bytes, 0, bytes.Length);
            }
        }

        try
        {
            using (HttpWebResponse response = (HttpWebResponse)request.GetResponse())
            {
                using (StreamReader reader = new StreamReader(response.GetResponseStream()))
                {
                    string responseText = reader.ReadToEnd();
                    return ParseJson(responseText);
                }
            }
        }
        catch (WebException ex)
        {
            string error = "";
            int statusCode = 0;
            if (ex.Response != null)
            {
                using (StreamReader reader = new StreamReader(ex.Response.GetResponseStream()))
                {
                    error = reader.ReadToEnd();
                }
                if (ex.Response is HttpWebResponse httpResponse)
                {
                    statusCode = (int)httpResponse.StatusCode;
                }
            }

            // Check for clock drift errors
            if (error.Contains("timestamp") && (statusCode == 401 || error.ToLower().Contains("expired") || error.ToLower().Contains("invalid")))
            {
                Console.Error.WriteLine($"{RED}Error: Request timestamp expired (must be within 5 minutes of server time){RESET}");
                Console.Error.WriteLine($"{YELLOW}Your computer's clock may have drifted.{RESET}");
                Console.Error.WriteLine("Check your system time and sync with NTP if needed:");
                Console.Error.WriteLine("  Linux:   sudo ntpdate -s time.nist.gov");
                Console.Error.WriteLine("  macOS:   sudo sntp -sS time.apple.com");
                Console.Error.WriteLine("  Windows: w32tm /resync");
                Environment.Exit(1);
            }

            throw new HttpException(statusCode, error);
        }
    }

    // Custom exception to preserve HTTP status code
    class HttpException : Exception
    {
        public int StatusCode { get; }
        public string ResponseBody { get; }
        public HttpException(int statusCode, string responseBody) : base($"HTTP {statusCode}: {responseBody}")
        {
            StatusCode = statusCode;
            ResponseBody = responseBody;
        }
    }

    // Handle 428 sudo OTP challenge - prompts user for OTP and retries the request
    static Dictionary<string, object> HandleSudoChallenge(string responseBody, string endpoint, string method, Dictionary<string, object> data, string publicKey, string secretKey)
    {
        var response = ParseJson(responseBody);
        string challengeId = response.ContainsKey("challenge_id") ? (string)response["challenge_id"] : null;

        Console.Error.WriteLine($"{YELLOW}Confirmation required. Check your email for a one-time code.{RESET}");
        Console.Error.Write("Enter OTP: ");

        string otp = Console.ReadLine();
        if (string.IsNullOrEmpty(otp))
        {
            throw new Exception("Operation cancelled");
        }

        otp = otp.Trim();

        // Retry the request with sudo headers
        return ApiRequest(endpoint, method, data, publicKey, secretKey, otp, challengeId);
    }

    // Wrapper for destructive operations that may require 428 sudo OTP
    static Dictionary<string, object> ApiRequestWithSudo(string endpoint, string method, Dictionary<string, object> data, string publicKey, string secretKey)
    {
        try
        {
            return ApiRequest(endpoint, method, data, publicKey, secretKey);
        }
        catch (HttpException ex) when (ex.StatusCode == 428)
        {
            return HandleSudoChallenge(ex.ResponseBody, endpoint, method, data, publicKey, secretKey);
        }
    }

    static string ApiRequestText(string endpoint, string method, string body, string publicKey, string secretKey)
    {
        ServicePointManager.SecurityProtocol = SecurityProtocolType.Tls12 | SecurityProtocolType.Tls11 | SecurityProtocolType.Tls;

        HttpWebRequest request = (HttpWebRequest)WebRequest.Create(API_BASE + endpoint);
        request.Method = method;
        request.ContentType = "text/plain";
        request.Timeout = 300000;

        if (body == null) body = "";

        // Add HMAC authentication headers
        if (!string.IsNullOrEmpty(secretKey))
        {
            long timestamp = DateTimeOffset.UtcNow.ToUnixTimeSeconds();
            string message = $"{timestamp}:{method}:{endpoint}:{body}";

            using (var hmac = new HMACSHA256(Encoding.UTF8.GetBytes(secretKey)))
            {
                byte[] hash = hmac.ComputeHash(Encoding.UTF8.GetBytes(message));
                string signature = BitConverter.ToString(hash).Replace("-", "").ToLower();

                request.Headers.Add("Authorization", $"Bearer {publicKey}");
                request.Headers.Add("X-Timestamp", timestamp.ToString());
                request.Headers.Add("X-Signature", signature);
            }
        }
        else
        {
            request.Headers.Add("Authorization", $"Bearer {publicKey}");
        }

        if (!string.IsNullOrEmpty(body))
        {
            byte[] bytes = Encoding.UTF8.GetBytes(body);
            request.ContentLength = bytes.Length;
            using (Stream stream = request.GetRequestStream())
            {
                stream.Write(bytes, 0, bytes.Length);
            }
        }

        try
        {
            using (HttpWebResponse response = (HttpWebResponse)request.GetResponse())
            {
                using (StreamReader reader = new StreamReader(response.GetResponseStream()))
                {
                    return reader.ReadToEnd();
                }
            }
        }
        catch (WebException ex)
        {
            string error = "";
            if (ex.Response != null)
            {
                using (StreamReader reader = new StreamReader(ex.Response.GetResponseStream()))
                {
                    error = reader.ReadToEnd();
                }
            }
            throw new Exception($"HTTP error - {error}");
        }
    }

    static string ReadEnvFile(string path)
    {
        if (!File.Exists(path))
        {
            throw new Exception($"Env file not found: {path}");
        }
        return File.ReadAllText(path);
    }

    static string BuildEnvContent(List<string> envs, string envFile)
    {
        var lines = new List<string>();

        // Add from -e flags
        foreach (var env in envs)
        {
            lines.Add(env);
        }

        // Add from --env-file
        if (!string.IsNullOrEmpty(envFile))
        {
            string content = ReadEnvFile(envFile);
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

    static Dictionary<string, object> ServiceEnvStatus(string serviceId, string publicKey, string secretKey)
    {
        return ApiRequest($"/services/{serviceId}/env", "GET", null, publicKey, secretKey);
    }

    static bool ServiceEnvSet(string serviceId, string envContent, string publicKey, string secretKey)
    {
        const int MAX_ENV_CONTENT_SIZE = 65536;
        if (envContent.Length > MAX_ENV_CONTENT_SIZE)
        {
            Console.Error.WriteLine($"{RED}Error: Env content exceeds maximum size of 64KB{RESET}");
            return false;
        }

        try
        {
            ApiRequestText($"/services/{serviceId}/env", "PUT", envContent, publicKey, secretKey);
            return true;
        }
        catch
        {
            return false;
        }
    }

    static Dictionary<string, object> ServiceEnvExport(string serviceId, string publicKey, string secretKey)
    {
        return ApiRequest($"/services/{serviceId}/env/export", "POST", null, publicKey, secretKey);
    }

    static bool ServiceEnvDelete(string serviceId, string publicKey, string secretKey)
    {
        try
        {
            ApiRequest($"/services/{serviceId}/env", "DELETE", null, publicKey, secretKey);
            return true;
        }
        catch
        {
            return false;
        }
    }

    static void CmdServiceEnv(Args args, string publicKey, string secretKey)
    {
        string action = args.EnvAction;
        string target = args.EnvTarget;

        if (action == "status")
        {
            if (string.IsNullOrEmpty(target))
            {
                Console.Error.WriteLine($"{RED}Error: service env status requires service ID{RESET}");
                Environment.Exit(1);
            }
            var result = ServiceEnvStatus(target, publicKey, secretKey);
            if (result.ContainsKey("has_vault") && (bool)result["has_vault"])
            {
                Console.WriteLine($"{GREEN}Vault: configured{RESET}");
                if (result.ContainsKey("env_count"))
                {
                    Console.WriteLine($"Variables: {result["env_count"]}");
                }
                if (result.ContainsKey("updated_at"))
                {
                    Console.WriteLine($"Updated: {result["updated_at"]}");
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
            string envContent = BuildEnvContent(args.Env, args.EnvFile);
            if (ServiceEnvSet(target, envContent, publicKey, secretKey))
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
            var result = ServiceEnvExport(target, publicKey, secretKey);
            if (result.ContainsKey("content"))
            {
                Console.Write(result["content"]);
            }
        }
        else if (action == "delete")
        {
            if (string.IsNullOrEmpty(target))
            {
                Console.Error.WriteLine($"{RED}Error: service env delete requires service ID{RESET}");
                Environment.Exit(1);
            }
            if (ServiceEnvDelete(target, publicKey, secretKey))
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
            Console.Error.WriteLine("Usage: Un service env <status|set|export|delete> <service_id>");
            Environment.Exit(1);
        }
    }

    static string ToJson(object obj)
    {
        if (obj == null) return "null";
        if (obj is string s) return JsonEscape(s);
        if (obj is int || obj is long || obj is double || obj is float) return obj.ToString();
        if (obj is bool b) return b.ToString().ToLower();
        if (obj is Dictionary<string, object> dict)
        {
            var sb = new StringBuilder("{");
            bool first = true;
            foreach (var kv in dict)
            {
                if (!first) sb.Append(",");
                first = false;
                sb.Append(JsonEscape(kv.Key)).Append(":").Append(ToJson(kv.Value));
            }
            sb.Append("}");
            return sb.ToString();
        }
        if (obj is Dictionary<string, string> strDict)
        {
            var sb = new StringBuilder("{");
            bool first = true;
            foreach (var kv in strDict)
            {
                if (!first) sb.Append(",");
                first = false;
                sb.Append(JsonEscape(kv.Key)).Append(":").Append(JsonEscape(kv.Value));
            }
            sb.Append("}");
            return sb.ToString();
        }
        if (obj is List<object> list)
        {
            var sb = new StringBuilder("[");
            for (int i = 0; i < list.Count; i++)
            {
                if (i > 0) sb.Append(",");
                sb.Append(ToJson(list[i]));
            }
            sb.Append("]");
            return sb.ToString();
        }
        if (obj is List<int> intList)
        {
            var sb = new StringBuilder("[");
            for (int i = 0; i < intList.Count; i++)
            {
                if (i > 0) sb.Append(",");
                sb.Append(intList[i]);
            }
            sb.Append("]");
            return sb.ToString();
        }
        if (obj is List<Dictionary<string, string>> dictList)
        {
            var sb = new StringBuilder("[");
            for (int i = 0; i < dictList.Count; i++)
            {
                if (i > 0) sb.Append(",");
                sb.Append(ToJson(dictList[i]));
            }
            sb.Append("]");
            return sb.ToString();
        }
        return JsonEscape(obj.ToString());
    }

    static string JsonEscape(string s)
    {
        var sb = new StringBuilder("\"");
        foreach (char c in s)
        {
            switch (c)
            {
                case '"': sb.Append("\\\""); break;
                case '\\': sb.Append("\\\\"); break;
                case '\n': sb.Append("\\n"); break;
                case '\r': sb.Append("\\r"); break;
                case '\t': sb.Append("\\t"); break;
                default: sb.Append(c); break;
            }
        }
        sb.Append("\"");
        return sb.ToString();
    }

    static Dictionary<string, object> ParseJson(string json)
    {
        json = json.Trim();
        if (!json.StartsWith("{")) return new Dictionary<string, object>();

        var result = new Dictionary<string, object>();
        int i = 1;

        while (i < json.Length)
        {
            while (i < json.Length && char.IsWhiteSpace(json[i])) i++;
            if (json[i] == '}') break;

            if (json[i] == '"')
            {
                int keyStart = ++i;
                while (i < json.Length && json[i] != '"')
                {
                    if (json[i] == '\\') i++;
                    i++;
                }
                string key = json.Substring(keyStart, i - keyStart).Replace("\\\"", "\"").Replace("\\\\", "\\");
                i++;

                while (i < json.Length && (char.IsWhiteSpace(json[i]) || json[i] == ':')) i++;

                var valuePair = ParseJsonValue(json, i);
                result[key] = valuePair.Item1;
                i = valuePair.Item2;

                while (i < json.Length && (char.IsWhiteSpace(json[i]) || json[i] == ',')) i++;
            }
            else
            {
                i++;
            }
        }
        return result;
    }

    static Tuple<object, int> ParseJsonValue(string json, int start)
    {
        int i = start;
        while (i < json.Length && char.IsWhiteSpace(json[i])) i++;

        if (json[i] == '"')
        {
            i++;
            var sb = new StringBuilder();
            bool escaped = false;
            while (i < json.Length)
            {
                char c = json[i];
                if (escaped)
                {
                    switch (c)
                    {
                        case 'n': sb.Append('\n'); break;
                        case 'r': sb.Append('\r'); break;
                        case 't': sb.Append('\t'); break;
                        case '"': sb.Append('"'); break;
                        case '\\': sb.Append('\\'); break;
                        default: sb.Append(c); break;
                    }
                    escaped = false;
                }
                else if (c == '\\')
                {
                    escaped = true;
                }
                else if (c == '"')
                {
                    return Tuple.Create((object)sb.ToString(), i + 1);
                }
                else
                {
                    sb.Append(c);
                }
                i++;
            }
        }
        else if (json[i] == '{')
        {
            int depth = 1;
            int objStart = i++;
            while (i < json.Length && depth > 0)
            {
                if (json[i] == '{') depth++;
                else if (json[i] == '}') depth--;
                i++;
            }
            return Tuple.Create((object)ParseJson(json.Substring(objStart, i - objStart)), i);
        }
        else if (json[i] == '[')
        {
            var list = new List<object>();
            i++;
            while (i < json.Length)
            {
                while (i < json.Length && char.IsWhiteSpace(json[i])) i++;
                if (json[i] == ']')
                {
                    i++;
                    break;
                }
                var item = ParseJsonValue(json, i);
                list.Add(item.Item1);
                i = item.Item2;
                while (i < json.Length && (char.IsWhiteSpace(json[i]) || json[i] == ',')) i++;
            }
            return Tuple.Create((object)list, i);
        }
        else if (char.IsDigit(json[i]) || json[i] == '-')
        {
            int numStart = i;
            while (i < json.Length && (char.IsDigit(json[i]) || json[i] == '.' || json[i] == '-')) i++;
            string num = json.Substring(numStart, i - numStart);
            return Tuple.Create((object)(num.Contains(".") ? (object)double.Parse(num) : int.Parse(num)), i);
        }
        else if (json.Substring(i).StartsWith("true"))
        {
            return Tuple.Create((object)true, i + 4);
        }
        else if (json.Substring(i).StartsWith("false"))
        {
            return Tuple.Create((object)false, i + 5);
        }
        else if (json.Substring(i).StartsWith("null"))
        {
            return Tuple.Create((object)null, i + 4);
        }
        return Tuple.Create((object)null, i);
    }

    class Args
    {
        public string Command = null;
        public string SourceFile = null;
        public string ApiKey = null;
        public string Network = null;
        public int Vcpu = 0;
        public List<string> Env = new List<string>();
        public List<string> Files = new List<string>();
        public bool Artifacts = false;
        public string OutputDir = null;
        public bool SessionList = false;
        public string SessionShell = null;
        public string SessionKill = null;
        public bool ServiceList = false;
        public string ServiceName = null;
        public string ServicePorts = null;
        public string ServiceBootstrap = null;
        public string ServiceInfo = null;
        public string ServiceLogs = null;
        public string ServiceTail = null;
        public string ServiceSleep = null;
        public string ServiceWake = null;
        public string ServiceDestroy = null;
        public string ServiceType = null;
        public string ServiceExecute = null;
        public string ServiceCommand = null;
        public string ServiceDumpBootstrap = null;
        public string ServiceDumpFile = null;
        public string ServiceUnfreezeOnDemand = null;
        public bool ServiceUnfreezeOnDemandEnabled = true;
        public string ServiceShowFreezePage = null;
        public bool ServiceShowFreezePageEnabled = true;
        public bool ServiceCreateUnfreezeOnDemand = false;
        public string EnvFile = null;
        public string EnvAction = null;
        public string EnvTarget = null;
        public bool KeyExtend = false;
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
            else if (arg == "--unfreeze-on-demand") result.ServiceUnfreezeOnDemand = args[++i];
            else if (arg == "--unfreeze-on-demand-enabled") result.ServiceUnfreezeOnDemandEnabled = args[++i].ToLower() == "true";
            else if (arg == "--show-freeze-page") result.ServiceShowFreezePage = args[++i];
            else if (arg == "--show-freeze-page-enabled") result.ServiceShowFreezePageEnabled = args[++i].ToLower() == "true";
            else if (arg == "--with-unfreeze-on-demand") result.ServiceCreateUnfreezeOnDemand = true;
            else if (arg == "--extend") result.KeyExtend = true;
            else if (!arg.StartsWith("-")) result.SourceFile = arg;
        }
        return result;
    }

    static void PrintHelp()
    {
        Console.WriteLine(@"Usage: Un [options] <source_file>
       Un session [options]
       Un service [options]
       Un service env <action> <service_id> [options]
       Un key [options]

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

Key options:
  --extend          Open browser to extend expired key");
    }
}

// =============================================================================
// Library API - For embedding in other .NET applications
// =============================================================================

/// <summary>
/// Unsandbox SDK for C# (Mono) - Full library API matching the C reference implementation
/// </summary>
public static class Unsandbox
{
    private const string API_BASE = "https://api.unsandbox.com";
    private const string VERSION = "4.3.2";
    private static string _lastError;

    /// <summary>Extension map for language detection</summary>
    public static readonly Dictionary<string, string> ExtMap = new Dictionary<string, string>
    {
        {".py", "python"}, {".js", "javascript"}, {".ts", "typescript"},
        {".rb", "ruby"}, {".php", "php"}, {".pl", "perl"}, {".lua", "lua"},
        {".sh", "bash"}, {".go", "go"}, {".rs", "rust"}, {".c", "c"},
        {".cpp", "cpp"}, {".cc", "cpp"}, {".cxx", "cpp"},
        {".java", "java"}, {".kt", "kotlin"}, {".cs", "csharp"}, {".fs", "fsharp"},
        {".ps1", "powershell"}, {".hs", "haskell"}, {".ml", "ocaml"},
        {".clj", "clojure"}, {".scm", "scheme"}, {".lisp", "commonlisp"},
        {".erl", "erlang"}, {".ex", "elixir"}, {".exs", "elixir"},
        {".jl", "julia"}, {".r", "r"}, {".R", "r"}, {".cr", "crystal"},
        {".d", "d"}, {".nim", "nim"}, {".zig", "zig"}, {".v", "v"},
        {".dart", "dart"}, {".groovy", "groovy"}, {".scala", "scala"},
        {".f90", "fortran"}, {".f95", "fortran"}, {".cob", "cobol"},
        {".pro", "prolog"}, {".forth", "forth"}, {".4th", "forth"},
        {".tcl", "tcl"}, {".raku", "raku"}, {".m", "objc"}
    };

    // --- Execution Functions (8) ---

    /// <summary>Execute code synchronously</summary>
    public static ExecuteResult Execute(string language, string code, string publicKey = null, string secretKey = null)
    {
        var (pk, sk) = ResolveKeys(publicKey, secretKey);
        var payload = new Dictionary<string, object> { ["language"] = language, ["code"] = code };
        try
        {
            var result = ApiCall("/execute", "POST", payload, pk, sk);
            return new ExecuteResult
            {
                Stdout = GetString(result, "stdout"),
                Stderr = GetString(result, "stderr"),
                ExitCode = GetInt(result, "exit_code"),
                Language = language,
                ExecutionTime = GetDouble(result, "execution_time"),
                Success = true
            };
        }
        catch (Exception ex) { _lastError = ex.Message; return new ExecuteResult { Success = false, ErrorMessage = ex.Message }; }
    }

    /// <summary>Execute code asynchronously, returns job ID</summary>
    public static string ExecuteAsync(string language, string code, string publicKey = null, string secretKey = null)
    {
        var (pk, sk) = ResolveKeys(publicKey, secretKey);
        var payload = new Dictionary<string, object> { ["language"] = language, ["code"] = code, ["async"] = true };
        try
        {
            var result = ApiCall("/execute", "POST", payload, pk, sk);
            return GetString(result, "job_id");
        }
        catch (Exception ex) { _lastError = ex.Message; return null; }
    }

    /// <summary>Wait for async job to complete</summary>
    public static ExecuteResult WaitJob(string jobId, string publicKey = null, string secretKey = null)
    {
        var (pk, sk) = ResolveKeys(publicKey, secretKey);
        try
        {
            var result = ApiCall($"/jobs/{jobId}/wait", "GET", null, pk, sk);
            return new ExecuteResult
            {
                Stdout = GetString(result, "stdout"),
                Stderr = GetString(result, "stderr"),
                ExitCode = GetInt(result, "exit_code"),
                ExecutionTime = GetDouble(result, "execution_time"),
                Success = true
            };
        }
        catch (Exception ex) { _lastError = ex.Message; return null; }
    }

    /// <summary>Get job status</summary>
    public static JobInfo GetJob(string jobId, string publicKey = null, string secretKey = null)
    {
        var (pk, sk) = ResolveKeys(publicKey, secretKey);
        try
        {
            var result = ApiCall($"/jobs/{jobId}", "GET", null, pk, sk);
            return new JobInfo
            {
                Id = GetString(result, "id"),
                Language = GetString(result, "language"),
                Status = GetString(result, "status")
            };
        }
        catch (Exception ex) { _lastError = ex.Message; return null; }
    }

    /// <summary>Cancel a running job</summary>
    public static bool CancelJob(string jobId, string publicKey = null, string secretKey = null)
    {
        var (pk, sk) = ResolveKeys(publicKey, secretKey);
        try { ApiCall($"/jobs/{jobId}/cancel", "POST", null, pk, sk); return true; }
        catch (Exception ex) { _lastError = ex.Message; return false; }
    }

    /// <summary>List all jobs</summary>
    public static List<JobInfo> ListJobs(string publicKey = null, string secretKey = null)
    {
        var (pk, sk) = ResolveKeys(publicKey, secretKey);
        try
        {
            var result = ApiCall("/jobs", "GET", null, pk, sk);
            var jobs = new List<JobInfo>();
            if (result.ContainsKey("jobs") && result["jobs"] is List<object> jobList)
                foreach (Dictionary<string, object> j in jobList)
                    jobs.Add(new JobInfo { Id = j.ContainsKey("id") ? (string)j["id"] : null, Status = j.ContainsKey("status") ? (string)j["status"] : null });
            return jobs;
        }
        catch (Exception ex) { _lastError = ex.Message; return new List<JobInfo>(); }
    }

    /// <summary>Get available programming languages</summary>
    public static List<string> GetLanguages(string publicKey = null, string secretKey = null)
    {
        var (pk, sk) = ResolveKeys(publicKey, secretKey);
        try
        {
            var result = ApiCall("/languages", "GET", null, pk, sk);
            if (result.ContainsKey("languages") && result["languages"] is List<object> langs)
                return langs.ConvertAll(x => x.ToString());
            return new List<string>();
        }
        catch (Exception ex) { _lastError = ex.Message; return new List<string>(); }
    }

    /// <summary>Detect language from filename extension</summary>
    public static string DetectLanguage(string filename)
    {
        int dotIndex = filename.LastIndexOf('.');
        if (dotIndex == -1) return null;
        string ext = filename.Substring(dotIndex).ToLower();
        return ExtMap.ContainsKey(ext) ? ExtMap[ext] : null;
    }

    // --- Session Functions (9) ---

    public static List<SessionInfo> SessionList(string publicKey = null, string secretKey = null)
    {
        var (pk, sk) = ResolveKeys(publicKey, secretKey);
        try
        {
            var result = ApiCall("/sessions", "GET", null, pk, sk);
            var sessions = new List<SessionInfo>();
            if (result.ContainsKey("sessions") && result["sessions"] is List<object> sessionList)
                foreach (Dictionary<string, object> s in sessionList)
                    sessions.Add(new SessionInfo { Id = s.ContainsKey("id") ? (string)s["id"] : null, Status = s.ContainsKey("status") ? (string)s["status"] : null });
            return sessions;
        }
        catch (Exception ex) { _lastError = ex.Message; return new List<SessionInfo>(); }
    }

    public static SessionInfo SessionGet(string sessionId, string publicKey = null, string secretKey = null)
    {
        var (pk, sk) = ResolveKeys(publicKey, secretKey);
        try
        {
            var result = ApiCall($"/sessions/{sessionId}", "GET", null, pk, sk);
            return new SessionInfo { Id = GetString(result, "id"), Status = GetString(result, "status") };
        }
        catch (Exception ex) { _lastError = ex.Message; return null; }
    }

    public static SessionInfo SessionCreate(string networkMode = null, string shell = null, string publicKey = null, string secretKey = null)
    {
        var (pk, sk) = ResolveKeys(publicKey, secretKey);
        var payload = new Dictionary<string, object> { ["shell"] = shell ?? "bash" };
        if (networkMode != null) payload["network"] = networkMode;
        try
        {
            var result = ApiCall("/sessions", "POST", payload, pk, sk);
            return new SessionInfo { Id = GetString(result, "id"), Status = "running" };
        }
        catch (Exception ex) { _lastError = ex.Message; return null; }
    }

    public static bool SessionDestroy(string sessionId, string publicKey = null, string secretKey = null)
    {
        var (pk, sk) = ResolveKeys(publicKey, secretKey);
        try { ApiCall($"/sessions/{sessionId}", "DELETE", null, pk, sk); return true; }
        catch (Exception ex) { _lastError = ex.Message; return false; }
    }

    public static bool SessionFreeze(string sessionId, string publicKey = null, string secretKey = null)
    {
        var (pk, sk) = ResolveKeys(publicKey, secretKey);
        try { ApiCall($"/sessions/{sessionId}/freeze", "POST", null, pk, sk); return true; }
        catch (Exception ex) { _lastError = ex.Message; return false; }
    }

    public static bool SessionUnfreeze(string sessionId, string publicKey = null, string secretKey = null)
    {
        var (pk, sk) = ResolveKeys(publicKey, secretKey);
        try { ApiCall($"/sessions/{sessionId}/unfreeze", "POST", null, pk, sk); return true; }
        catch (Exception ex) { _lastError = ex.Message; return false; }
    }

    public static bool SessionBoost(string sessionId, int vcpu = 2, string publicKey = null, string secretKey = null)
    {
        var (pk, sk) = ResolveKeys(publicKey, secretKey);
        var payload = new Dictionary<string, object> { ["vcpu"] = vcpu };
        try { ApiCall($"/sessions/{sessionId}/boost", "POST", payload, pk, sk); return true; }
        catch (Exception ex) { _lastError = ex.Message; return false; }
    }

    public static bool SessionUnboost(string sessionId, string publicKey = null, string secretKey = null)
    {
        var (pk, sk) = ResolveKeys(publicKey, secretKey);
        try { ApiCall($"/sessions/{sessionId}/unboost", "POST", null, pk, sk); return true; }
        catch (Exception ex) { _lastError = ex.Message; return false; }
    }

    public static ExecuteResult SessionExecute(string sessionId, string command, string publicKey = null, string secretKey = null)
    {
        var (pk, sk) = ResolveKeys(publicKey, secretKey);
        var payload = new Dictionary<string, object> { ["command"] = command };
        try
        {
            var result = ApiCall($"/sessions/{sessionId}/execute", "POST", payload, pk, sk);
            return new ExecuteResult
            {
                Stdout = GetString(result, "stdout"),
                Stderr = GetString(result, "stderr"),
                ExitCode = GetInt(result, "exit_code"),
                Success = true
            };
        }
        catch (Exception ex) { _lastError = ex.Message; return null; }
    }

    // --- Service Functions (17) ---

    public static List<ServiceInfo> ServiceList(string publicKey = null, string secretKey = null)
    {
        var (pk, sk) = ResolveKeys(publicKey, secretKey);
        try
        {
            var result = ApiCall("/services", "GET", null, pk, sk);
            var services = new List<ServiceInfo>();
            if (result.ContainsKey("services") && result["services"] is List<object> serviceList)
                foreach (Dictionary<string, object> s in serviceList)
                    services.Add(new ServiceInfo { Id = s.ContainsKey("id") ? (string)s["id"] : null, Name = s.ContainsKey("name") ? (string)s["name"] : null, Status = s.ContainsKey("status") ? (string)s["status"] : null });
            return services;
        }
        catch (Exception ex) { _lastError = ex.Message; return new List<ServiceInfo>(); }
    }

    public static ServiceInfo ServiceGet(string serviceId, string publicKey = null, string secretKey = null)
    {
        var (pk, sk) = ResolveKeys(publicKey, secretKey);
        try
        {
            var result = ApiCall($"/services/{serviceId}", "GET", null, pk, sk);
            return new ServiceInfo { Id = GetString(result, "id"), Name = GetString(result, "name"), Status = GetString(result, "status") };
        }
        catch (Exception ex) { _lastError = ex.Message; return null; }
    }

    public static string ServiceCreate(string name, string ports = null, string domains = null, string bootstrap = null, string networkMode = null, string publicKey = null, string secretKey = null)
    {
        var (pk, sk) = ResolveKeys(publicKey, secretKey);
        var payload = new Dictionary<string, object> { ["name"] = name };
        if (ports != null)
        {
            var portList = new List<int>();
            foreach (var p in ports.Split(',')) portList.Add(int.Parse(p.Trim()));
            payload["ports"] = portList;
        }
        if (domains != null) payload["domains"] = domains;
        if (bootstrap != null) payload["bootstrap"] = bootstrap;
        if (networkMode != null) payload["network"] = networkMode;
        try
        {
            var result = ApiCall("/services", "POST", payload, pk, sk);
            return GetString(result, "id");
        }
        catch (Exception ex) { _lastError = ex.Message; return null; }
    }

    public static bool ServiceDestroy(string serviceId, string publicKey = null, string secretKey = null)
    {
        var (pk, sk) = ResolveKeys(publicKey, secretKey);
        try { ApiCall($"/services/{serviceId}", "DELETE", null, pk, sk); return true; }
        catch (Exception ex) { _lastError = ex.Message; return false; }
    }

    public static bool ServiceFreeze(string serviceId, string publicKey = null, string secretKey = null)
    {
        var (pk, sk) = ResolveKeys(publicKey, secretKey);
        try { ApiCall($"/services/{serviceId}/freeze", "POST", null, pk, sk); return true; }
        catch (Exception ex) { _lastError = ex.Message; return false; }
    }

    public static bool ServiceUnfreeze(string serviceId, string publicKey = null, string secretKey = null)
    {
        var (pk, sk) = ResolveKeys(publicKey, secretKey);
        try { ApiCall($"/services/{serviceId}/unfreeze", "POST", null, pk, sk); return true; }
        catch (Exception ex) { _lastError = ex.Message; return false; }
    }

    public static bool ServiceLock(string serviceId, string publicKey = null, string secretKey = null)
    {
        var (pk, sk) = ResolveKeys(publicKey, secretKey);
        try { ApiCall($"/services/{serviceId}/lock", "POST", null, pk, sk); return true; }
        catch (Exception ex) { _lastError = ex.Message; return false; }
    }

    public static bool ServiceUnlock(string serviceId, string publicKey = null, string secretKey = null)
    {
        var (pk, sk) = ResolveKeys(publicKey, secretKey);
        try { ApiCall($"/services/{serviceId}/unlock", "POST", null, pk, sk); return true; }
        catch (Exception ex) { _lastError = ex.Message; return false; }
    }

    public static bool ServiceSetUnfreezeOnDemand(string serviceId, bool enabled, string publicKey = null, string secretKey = null)
    {
        var (pk, sk) = ResolveKeys(publicKey, secretKey);
        var payload = new Dictionary<string, object> { ["unfreeze_on_demand"] = enabled };
        try { ApiCall($"/services/{serviceId}", "PATCH", payload, pk, sk); return true; }
        catch (Exception ex) { _lastError = ex.Message; return false; }
    }

    public static bool ServiceRedeploy(string serviceId, string bootstrap = null, string publicKey = null, string secretKey = null)
    {
        var (pk, sk) = ResolveKeys(publicKey, secretKey);
        var payload = bootstrap != null ? new Dictionary<string, object> { ["bootstrap"] = bootstrap } : null;
        try { ApiCall($"/services/{serviceId}/redeploy", "POST", payload, pk, sk); return true; }
        catch (Exception ex) { _lastError = ex.Message; return false; }
    }

    public static string ServiceLogs(string serviceId, bool allLogs = false, string publicKey = null, string secretKey = null)
    {
        var (pk, sk) = ResolveKeys(publicKey, secretKey);
        var endpoint = allLogs ? $"/services/{serviceId}/logs?lines=9000" : $"/services/{serviceId}/logs";
        try
        {
            var result = ApiCall(endpoint, "GET", null, pk, sk);
            return GetString(result, "logs");
        }
        catch (Exception ex) { _lastError = ex.Message; return null; }
    }

    public static ExecuteResult ServiceExecute(string serviceId, string command, int timeoutMs = 30000, string publicKey = null, string secretKey = null)
    {
        var (pk, sk) = ResolveKeys(publicKey, secretKey);
        var payload = new Dictionary<string, object> { ["command"] = command };
        try
        {
            var result = ApiCall($"/services/{serviceId}/execute", "POST", payload, pk, sk);
            return new ExecuteResult
            {
                Stdout = GetString(result, "stdout"),
                Stderr = GetString(result, "stderr"),
                ExitCode = GetInt(result, "exit_code"),
                Success = true
            };
        }
        catch (Exception ex) { _lastError = ex.Message; return null; }
    }

    public static string ServiceEnvGet(string serviceId, string publicKey = null, string secretKey = null)
    {
        var (pk, sk) = ResolveKeys(publicKey, secretKey);
        try
        {
            var result = ApiCall($"/services/{serviceId}/env", "GET", null, pk, sk);
            return GetString(result, "content");
        }
        catch (Exception ex) { _lastError = ex.Message; return null; }
    }

    public static bool ServiceEnvSet(string serviceId, string envContent, string publicKey = null, string secretKey = null)
    {
        var (pk, sk) = ResolveKeys(publicKey, secretKey);
        try { ApiCallText($"/services/{serviceId}/env", "PUT", envContent, pk, sk); return true; }
        catch (Exception ex) { _lastError = ex.Message; return false; }
    }

    public static bool ServiceEnvDelete(string serviceId, string publicKey = null, string secretKey = null)
    {
        var (pk, sk) = ResolveKeys(publicKey, secretKey);
        try { ApiCall($"/services/{serviceId}/env", "DELETE", null, pk, sk); return true; }
        catch (Exception ex) { _lastError = ex.Message; return false; }
    }

    public static string ServiceEnvExport(string serviceId, string publicKey = null, string secretKey = null)
    {
        var (pk, sk) = ResolveKeys(publicKey, secretKey);
        try
        {
            var result = ApiCall($"/services/{serviceId}/env/export", "POST", null, pk, sk);
            return GetString(result, "content");
        }
        catch (Exception ex) { _lastError = ex.Message; return null; }
    }

    public static bool ServiceResize(string serviceId, int vcpu, string publicKey = null, string secretKey = null)
    {
        var (pk, sk) = ResolveKeys(publicKey, secretKey);
        var payload = new Dictionary<string, object> { ["vcpu"] = vcpu };
        try { ApiCall($"/services/{serviceId}/resize", "POST", payload, pk, sk); return true; }
        catch (Exception ex) { _lastError = ex.Message; return false; }
    }

    // --- Snapshot Functions (9) ---

    public static List<SnapshotInfo> SnapshotList(string publicKey = null, string secretKey = null)
    {
        var (pk, sk) = ResolveKeys(publicKey, secretKey);
        try
        {
            var result = ApiCall("/snapshots", "GET", null, pk, sk);
            var snapshots = new List<SnapshotInfo>();
            if (result.ContainsKey("snapshots") && result["snapshots"] is List<object> snapshotList)
                foreach (Dictionary<string, object> s in snapshotList)
                    snapshots.Add(new SnapshotInfo { Id = s.ContainsKey("id") ? (string)s["id"] : null, Name = s.ContainsKey("name") ? (string)s["name"] : null });
            return snapshots;
        }
        catch (Exception ex) { _lastError = ex.Message; return new List<SnapshotInfo>(); }
    }

    public static SnapshotInfo SnapshotGet(string snapshotId, string publicKey = null, string secretKey = null)
    {
        var (pk, sk) = ResolveKeys(publicKey, secretKey);
        try
        {
            var result = ApiCall($"/snapshots/{snapshotId}", "GET", null, pk, sk);
            return new SnapshotInfo { Id = GetString(result, "id"), Name = GetString(result, "name"), Type = GetString(result, "source_type") };
        }
        catch (Exception ex) { _lastError = ex.Message; return null; }
    }

    public static string SnapshotSession(string sessionId, string name = null, bool hot = false, string publicKey = null, string secretKey = null)
    {
        var (pk, sk) = ResolveKeys(publicKey, secretKey);
        var payload = new Dictionary<string, object>();
        if (name != null) payload["name"] = name;
        if (hot) payload["hot"] = true;
        try
        {
            var result = ApiCall($"/sessions/{sessionId}/snapshot", "POST", payload, pk, sk);
            return GetString(result, "id");
        }
        catch (Exception ex) { _lastError = ex.Message; return null; }
    }

    public static string SnapshotService(string serviceId, string name = null, bool hot = false, string publicKey = null, string secretKey = null)
    {
        var (pk, sk) = ResolveKeys(publicKey, secretKey);
        var payload = new Dictionary<string, object>();
        if (name != null) payload["name"] = name;
        if (hot) payload["hot"] = true;
        try
        {
            var result = ApiCall($"/services/{serviceId}/snapshot", "POST", payload, pk, sk);
            return GetString(result, "id");
        }
        catch (Exception ex) { _lastError = ex.Message; return null; }
    }

    public static string SnapshotRestore(string snapshotId, string publicKey = null, string secretKey = null)
    {
        var (pk, sk) = ResolveKeys(publicKey, secretKey);
        try
        {
            var result = ApiCall($"/snapshots/{snapshotId}/restore", "POST", null, pk, sk);
            return GetString(result, "id");
        }
        catch (Exception ex) { _lastError = ex.Message; return null; }
    }

    public static bool SnapshotDelete(string snapshotId, string publicKey = null, string secretKey = null)
    {
        var (pk, sk) = ResolveKeys(publicKey, secretKey);
        try { ApiCall($"/snapshots/{snapshotId}", "DELETE", null, pk, sk); return true; }
        catch (Exception ex) { _lastError = ex.Message; return false; }
    }

    public static bool SnapshotLock(string snapshotId, string publicKey = null, string secretKey = null)
    {
        var (pk, sk) = ResolveKeys(publicKey, secretKey);
        try { ApiCall($"/snapshots/{snapshotId}/lock", "POST", null, pk, sk); return true; }
        catch (Exception ex) { _lastError = ex.Message; return false; }
    }

    public static bool SnapshotUnlock(string snapshotId, string publicKey = null, string secretKey = null)
    {
        var (pk, sk) = ResolveKeys(publicKey, secretKey);
        try { ApiCall($"/snapshots/{snapshotId}/unlock", "POST", null, pk, sk); return true; }
        catch (Exception ex) { _lastError = ex.Message; return false; }
    }

    public static string SnapshotClone(string snapshotId, string cloneType, string name = null, string ports = null, string shell = null, string publicKey = null, string secretKey = null)
    {
        var (pk, sk) = ResolveKeys(publicKey, secretKey);
        var payload = new Dictionary<string, object> { ["type"] = cloneType };
        if (name != null) payload["name"] = name;
        if (ports != null)
        {
            var portList = new List<int>();
            foreach (var p in ports.Split(',')) portList.Add(int.Parse(p.Trim()));
            payload["ports"] = portList;
        }
        if (shell != null) payload["shell"] = shell;
        try
        {
            var result = ApiCall($"/snapshots/{snapshotId}/clone", "POST", payload, pk, sk);
            return GetString(result, "id");
        }
        catch (Exception ex) { _lastError = ex.Message; return null; }
    }

    // --- Image Functions (13) ---

    public static List<ImageInfo> ImageList(string filter = null, string publicKey = null, string secretKey = null)
    {
        var (pk, sk) = ResolveKeys(publicKey, secretKey);
        var endpoint = filter != null ? $"/images?filter={filter}" : "/images";
        try
        {
            var result = ApiCall(endpoint, "GET", null, pk, sk);
            var images = new List<ImageInfo>();
            if (result.ContainsKey("images") && result["images"] is List<object> imageList)
                foreach (Dictionary<string, object> img in imageList)
                    images.Add(new ImageInfo { Id = img.ContainsKey("id") ? (string)img["id"] : null, Name = img.ContainsKey("name") ? (string)img["name"] : null });
            return images;
        }
        catch (Exception ex) { _lastError = ex.Message; return new List<ImageInfo>(); }
    }

    public static ImageInfo ImageGet(string imageId, string publicKey = null, string secretKey = null)
    {
        var (pk, sk) = ResolveKeys(publicKey, secretKey);
        try
        {
            var result = ApiCall($"/images/{imageId}", "GET", null, pk, sk);
            return new ImageInfo { Id = GetString(result, "id"), Name = GetString(result, "name"), Visibility = GetString(result, "visibility") };
        }
        catch (Exception ex) { _lastError = ex.Message; return null; }
    }

    public static string ImagePublish(string sourceType, string sourceId, string name = null, string description = null, string publicKey = null, string secretKey = null)
    {
        var (pk, sk) = ResolveKeys(publicKey, secretKey);
        var payload = new Dictionary<string, object> { ["source_type"] = sourceType, ["source_id"] = sourceId };
        if (name != null) payload["name"] = name;
        if (description != null) payload["description"] = description;
        try
        {
            var result = ApiCall("/images", "POST", payload, pk, sk);
            return GetString(result, "id");
        }
        catch (Exception ex) { _lastError = ex.Message; return null; }
    }

    public static bool ImageDelete(string imageId, string publicKey = null, string secretKey = null)
    {
        var (pk, sk) = ResolveKeys(publicKey, secretKey);
        try { ApiCall($"/images/{imageId}", "DELETE", null, pk, sk); return true; }
        catch (Exception ex) { _lastError = ex.Message; return false; }
    }

    public static bool ImageLock(string imageId, string publicKey = null, string secretKey = null)
    {
        var (pk, sk) = ResolveKeys(publicKey, secretKey);
        try { ApiCall($"/images/{imageId}/lock", "POST", null, pk, sk); return true; }
        catch (Exception ex) { _lastError = ex.Message; return false; }
    }

    public static bool ImageUnlock(string imageId, string publicKey = null, string secretKey = null)
    {
        var (pk, sk) = ResolveKeys(publicKey, secretKey);
        try { ApiCall($"/images/{imageId}/unlock", "POST", null, pk, sk); return true; }
        catch (Exception ex) { _lastError = ex.Message; return false; }
    }

    public static bool ImageSetVisibility(string imageId, string visibility, string publicKey = null, string secretKey = null)
    {
        var (pk, sk) = ResolveKeys(publicKey, secretKey);
        var payload = new Dictionary<string, object> { ["visibility"] = visibility };
        try { ApiCall($"/images/{imageId}", "PATCH", payload, pk, sk); return true; }
        catch (Exception ex) { _lastError = ex.Message; return false; }
    }

    public static bool ImageGrantAccess(string imageId, string trustedApiKey, string publicKey = null, string secretKey = null)
    {
        var (pk, sk) = ResolveKeys(publicKey, secretKey);
        var payload = new Dictionary<string, object> { ["api_key"] = trustedApiKey };
        try { ApiCall($"/images/{imageId}/access", "POST", payload, pk, sk); return true; }
        catch (Exception ex) { _lastError = ex.Message; return false; }
    }

    public static bool ImageRevokeAccess(string imageId, string trustedApiKey, string publicKey = null, string secretKey = null)
    {
        var (pk, sk) = ResolveKeys(publicKey, secretKey);
        var payload = new Dictionary<string, object> { ["api_key"] = trustedApiKey };
        try { ApiCall($"/images/{imageId}/access", "DELETE", payload, pk, sk); return true; }
        catch (Exception ex) { _lastError = ex.Message; return false; }
    }

    public static List<string> ImageListTrusted(string imageId, string publicKey = null, string secretKey = null)
    {
        var (pk, sk) = ResolveKeys(publicKey, secretKey);
        try
        {
            var result = ApiCall($"/images/{imageId}/access", "GET", null, pk, sk);
            if (result.ContainsKey("trusted_keys") && result["trusted_keys"] is List<object> keys)
                return keys.ConvertAll(x => x.ToString());
            return new List<string>();
        }
        catch (Exception ex) { _lastError = ex.Message; return new List<string>(); }
    }

    public static bool ImageTransfer(string imageId, string toApiKey, string publicKey = null, string secretKey = null)
    {
        var (pk, sk) = ResolveKeys(publicKey, secretKey);
        var payload = new Dictionary<string, object> { ["to_api_key"] = toApiKey };
        try { ApiCall($"/images/{imageId}/transfer", "POST", payload, pk, sk); return true; }
        catch (Exception ex) { _lastError = ex.Message; return false; }
    }

    public static string ImageSpawn(string imageId, string name = null, string ports = null, string bootstrap = null, string networkMode = null, string publicKey = null, string secretKey = null)
    {
        var (pk, sk) = ResolveKeys(publicKey, secretKey);
        var payload = new Dictionary<string, object>();
        if (name != null) payload["name"] = name;
        if (ports != null)
        {
            var portList = new List<int>();
            foreach (var p in ports.Split(',')) portList.Add(int.Parse(p.Trim()));
            payload["ports"] = portList;
        }
        if (bootstrap != null) payload["bootstrap"] = bootstrap;
        if (networkMode != null) payload["network"] = networkMode;
        try
        {
            var result = ApiCall($"/images/{imageId}/spawn", "POST", payload, pk, sk);
            return GetString(result, "id");
        }
        catch (Exception ex) { _lastError = ex.Message; return null; }
    }

    public static string ImageClone(string imageId, string name = null, string description = null, string publicKey = null, string secretKey = null)
    {
        var (pk, sk) = ResolveKeys(publicKey, secretKey);
        var payload = new Dictionary<string, object>();
        if (name != null) payload["name"] = name;
        if (description != null) payload["description"] = description;
        try
        {
            var result = ApiCall($"/images/{imageId}/clone", "POST", payload, pk, sk);
            return GetString(result, "id");
        }
        catch (Exception ex) { _lastError = ex.Message; return null; }
    }

    // --- Utilities ---

    public static KeyInfo ValidateKeys(string publicKey = null, string secretKey = null)
    {
        var (pk, sk) = ResolveKeys(publicKey, secretKey);
        try
        {
            var result = ApiCall("/keys/validate", "POST", null, pk, sk);
            return new KeyInfo
            {
                Valid = result.ContainsKey("valid") && result["valid"] is bool v && v,
                Tier = GetString(result, "tier"),
                RateLimitPerMinute = GetInt(result, "rate_limit"),
                ConcurrencyLimit = GetInt(result, "concurrency")
            };
        }
        catch (Exception ex) { _lastError = ex.Message; return null; }
    }

    public static string HmacSign(string secretKey, string message)
    {
        using (var hmac = new HMACSHA256(Encoding.UTF8.GetBytes(secretKey)))
        {
            var hash = hmac.ComputeHash(Encoding.UTF8.GetBytes(message));
            return BitConverter.ToString(hash).Replace("-", "").ToLower();
        }
    }

    public static bool HealthCheck()
    {
        try
        {
            ServicePointManager.SecurityProtocol = SecurityProtocolType.Tls12 | SecurityProtocolType.Tls11 | SecurityProtocolType.Tls;
            var request = (HttpWebRequest)WebRequest.Create(API_BASE + "/health");
            request.Method = "GET";
            request.Timeout = 10000;
            using (var response = (HttpWebResponse)request.GetResponse())
                return response.StatusCode == HttpStatusCode.OK;
        }
        catch { return false; }
    }

    public static string Version() => VERSION;

    public static string LastError() => _lastError;

    /// <summary>Build environment content from list of env vars and optional env file</summary>
    public static string BuildEnvContent(List<string> envs, string envFile)
    {
        var lines = new List<string>(envs);
        if (!string.IsNullOrEmpty(envFile) && File.Exists(envFile))
        {
            var content = File.ReadAllText(envFile);
            foreach (var line in content.Split('\n'))
            {
                var trimmed = line.Trim();
                if (!string.IsNullOrEmpty(trimmed) && !trimmed.StartsWith("#"))
                    lines.Add(trimmed);
            }
        }
        return string.Join("\n", lines);
    }

    // --- Internal Helpers ---

    private static (string, string) ResolveKeys(string publicKey, string secretKey)
    {
        var pk = publicKey ?? Environment.GetEnvironmentVariable("UNSANDBOX_PUBLIC_KEY") ?? "";
        var sk = secretKey ?? Environment.GetEnvironmentVariable("UNSANDBOX_SECRET_KEY") ?? "";
        return (pk, sk);
    }

    private static Dictionary<string, object> ApiCall(string endpoint, string method, Dictionary<string, object> data, string publicKey, string secretKey)
    {
        ServicePointManager.SecurityProtocol = SecurityProtocolType.Tls12 | SecurityProtocolType.Tls11 | SecurityProtocolType.Tls;

        var request = (HttpWebRequest)WebRequest.Create(API_BASE + endpoint);
        request.Method = method;
        request.ContentType = "application/json";
        request.Timeout = 300000;

        string body = data != null ? ToJson(data) : "";

        if (!string.IsNullOrEmpty(secretKey))
        {
            long timestamp = DateTimeOffset.UtcNow.ToUnixTimeSeconds();
            string message = $"{timestamp}:{method}:{endpoint}:{body}";
            using (var hmac = new HMACSHA256(Encoding.UTF8.GetBytes(secretKey)))
            {
                byte[] hash = hmac.ComputeHash(Encoding.UTF8.GetBytes(message));
                string signature = BitConverter.ToString(hash).Replace("-", "").ToLower();
                request.Headers.Add("Authorization", $"Bearer {publicKey}");
                request.Headers.Add("X-Timestamp", timestamp.ToString());
                request.Headers.Add("X-Signature", signature);
            }
        }
        else
        {
            request.Headers.Add("Authorization", $"Bearer {publicKey}");
        }

        if (data != null)
        {
            byte[] bytes = Encoding.UTF8.GetBytes(body);
            request.ContentLength = bytes.Length;
            using (Stream stream = request.GetRequestStream())
                stream.Write(bytes, 0, bytes.Length);
        }

        using (var response = (HttpWebResponse)request.GetResponse())
        using (var reader = new StreamReader(response.GetResponseStream()))
        {
            var responseText = reader.ReadToEnd();
            return ParseJson(responseText);
        }
    }

    private static void ApiCallText(string endpoint, string method, string body, string publicKey, string secretKey)
    {
        ServicePointManager.SecurityProtocol = SecurityProtocolType.Tls12 | SecurityProtocolType.Tls11 | SecurityProtocolType.Tls;

        var request = (HttpWebRequest)WebRequest.Create(API_BASE + endpoint);
        request.Method = method;
        request.ContentType = "text/plain";
        request.Timeout = 300000;

        if (!string.IsNullOrEmpty(secretKey))
        {
            long timestamp = DateTimeOffset.UtcNow.ToUnixTimeSeconds();
            string message = $"{timestamp}:{method}:{endpoint}:{body}";
            using (var hmac = new HMACSHA256(Encoding.UTF8.GetBytes(secretKey)))
            {
                byte[] hash = hmac.ComputeHash(Encoding.UTF8.GetBytes(message));
                string signature = BitConverter.ToString(hash).Replace("-", "").ToLower();
                request.Headers.Add("Authorization", $"Bearer {publicKey}");
                request.Headers.Add("X-Timestamp", timestamp.ToString());
                request.Headers.Add("X-Signature", signature);
            }
        }
        else
        {
            request.Headers.Add("Authorization", $"Bearer {publicKey}");
        }

        byte[] bytes = Encoding.UTF8.GetBytes(body);
        request.ContentLength = bytes.Length;
        using (Stream stream = request.GetRequestStream())
            stream.Write(bytes, 0, bytes.Length);

        using (var response = (HttpWebResponse)request.GetResponse()) { }
    }

    private static string ToJson(Dictionary<string, object> dict)
    {
        var sb = new StringBuilder("{");
        bool first = true;
        foreach (var kv in dict)
        {
            if (!first) sb.Append(",");
            first = false;
            sb.Append($"\"{kv.Key}\":");
            sb.Append(ValueToJson(kv.Value));
        }
        sb.Append("}");
        return sb.ToString();
    }

    private static string ValueToJson(object val)
    {
        if (val == null) return "null";
        if (val is string s) return $"\"{s.Replace("\\", "\\\\").Replace("\"", "\\\"").Replace("\n", "\\n").Replace("\r", "\\r")}\"";
        if (val is bool b) return b.ToString().ToLower();
        if (val is int || val is long || val is double) return val.ToString();
        if (val is List<int> intList)
        {
            var sb = new StringBuilder("[");
            for (int i = 0; i < intList.Count; i++)
            {
                if (i > 0) sb.Append(",");
                sb.Append(intList[i]);
            }
            sb.Append("]");
            return sb.ToString();
        }
        if (val is Dictionary<string, object> dict) return ToJson(dict);
        return $"\"{val}\"";
    }

    private static Dictionary<string, object> ParseJson(string json)
    {
        // Reuse the existing ParseJson from the Un class
        json = json.Trim();
        if (!json.StartsWith("{")) return new Dictionary<string, object>();

        var result = new Dictionary<string, object>();
        int i = 1;

        while (i < json.Length)
        {
            while (i < json.Length && char.IsWhiteSpace(json[i])) i++;
            if (json[i] == '}') break;

            if (json[i] == '"')
            {
                int keyStart = ++i;
                while (i < json.Length && json[i] != '"')
                {
                    if (json[i] == '\\') i++;
                    i++;
                }
                string key = json.Substring(keyStart, i - keyStart).Replace("\\\"", "\"").Replace("\\\\", "\\");
                i++;

                while (i < json.Length && (char.IsWhiteSpace(json[i]) || json[i] == ':')) i++;

                var valuePair = ParseJsonValue(json, i);
                result[key] = valuePair.Item1;
                i = valuePair.Item2;

                while (i < json.Length && (char.IsWhiteSpace(json[i]) || json[i] == ',')) i++;
            }
            else
            {
                i++;
            }
        }
        return result;
    }

    private static Tuple<object, int> ParseJsonValue(string json, int start)
    {
        int i = start;
        while (i < json.Length && char.IsWhiteSpace(json[i])) i++;

        if (json[i] == '"')
        {
            i++;
            var sb = new StringBuilder();
            bool escaped = false;
            while (i < json.Length)
            {
                char c = json[i];
                if (escaped)
                {
                    switch (c)
                    {
                        case 'n': sb.Append('\n'); break;
                        case 'r': sb.Append('\r'); break;
                        case 't': sb.Append('\t'); break;
                        case '"': sb.Append('"'); break;
                        case '\\': sb.Append('\\'); break;
                        default: sb.Append(c); break;
                    }
                    escaped = false;
                }
                else if (c == '\\') escaped = true;
                else if (c == '"') return Tuple.Create((object)sb.ToString(), i + 1);
                else sb.Append(c);
                i++;
            }
        }
        else if (json[i] == '{')
        {
            int depth = 1;
            int objStart = i++;
            while (i < json.Length && depth > 0)
            {
                if (json[i] == '{') depth++;
                else if (json[i] == '}') depth--;
                i++;
            }
            return Tuple.Create((object)ParseJson(json.Substring(objStart, i - objStart)), i);
        }
        else if (json[i] == '[')
        {
            var list = new List<object>();
            i++;
            while (i < json.Length)
            {
                while (i < json.Length && char.IsWhiteSpace(json[i])) i++;
                if (json[i] == ']') { i++; break; }
                var item = ParseJsonValue(json, i);
                list.Add(item.Item1);
                i = item.Item2;
                while (i < json.Length && (char.IsWhiteSpace(json[i]) || json[i] == ',')) i++;
            }
            return Tuple.Create((object)list, i);
        }
        else if (char.IsDigit(json[i]) || json[i] == '-')
        {
            int numStart = i;
            while (i < json.Length && (char.IsDigit(json[i]) || json[i] == '.' || json[i] == '-')) i++;
            string num = json.Substring(numStart, i - numStart);
            return Tuple.Create((object)(num.Contains(".") ? (object)double.Parse(num) : int.Parse(num)), i);
        }
        else if (json.Substring(i).StartsWith("true")) return Tuple.Create((object)true, i + 4);
        else if (json.Substring(i).StartsWith("false")) return Tuple.Create((object)false, i + 5);
        else if (json.Substring(i).StartsWith("null")) return Tuple.Create((object)null, i + 4);

        return Tuple.Create((object)null, i);
    }

    private static string GetString(Dictionary<string, object> result, string key)
        => result.ContainsKey(key) ? result[key]?.ToString() : null;

    private static int GetInt(Dictionary<string, object> result, string key)
        => result.ContainsKey(key) && result[key] is int i ? i : 0;

    private static double GetDouble(Dictionary<string, object> result, string key)
        => result.ContainsKey(key) && result[key] is double d ? d : 0;
}

// --- Data Types ---

public class ExecuteResult
{
    public string Stdout { get; set; }
    public string Stderr { get; set; }
    public int ExitCode { get; set; }
    public string Language { get; set; }
    public double ExecutionTime { get; set; }
    public bool Success { get; set; }
    public string ErrorMessage { get; set; }
}

public class JobInfo
{
    public string Id { get; set; }
    public string Language { get; set; }
    public string Status { get; set; }
    public long CreatedAt { get; set; }
    public long CompletedAt { get; set; }
    public string ErrorMessage { get; set; }
}

public class SessionInfo
{
    public string Id { get; set; }
    public string ContainerName { get; set; }
    public string Status { get; set; }
    public string NetworkMode { get; set; }
    public int Vcpu { get; set; }
    public long CreatedAt { get; set; }
    public long LastActivity { get; set; }
}

public class ServiceInfo
{
    public string Id { get; set; }
    public string Name { get; set; }
    public string Status { get; set; }
    public string ContainerName { get; set; }
    public string NetworkMode { get; set; }
    public string Ports { get; set; }
    public string Domains { get; set; }
    public int Vcpu { get; set; }
    public bool Locked { get; set; }
    public bool UnfreezeOnDemand { get; set; }
    public long CreatedAt { get; set; }
    public long LastActivity { get; set; }
}

public class SnapshotInfo
{
    public string Id { get; set; }
    public string Name { get; set; }
    public string Type { get; set; }
    public string SourceId { get; set; }
    public bool Hot { get; set; }
    public bool Locked { get; set; }
    public long CreatedAt { get; set; }
    public long SizeBytes { get; set; }
}

public class ImageInfo
{
    public string Id { get; set; }
    public string Name { get; set; }
    public string Description { get; set; }
    public string Visibility { get; set; }
    public string SourceType { get; set; }
    public string SourceId { get; set; }
    public string OwnerApiKey { get; set; }
    public bool Locked { get; set; }
    public long CreatedAt { get; set; }
    public long SizeBytes { get; set; }
}

public class KeyInfo
{
    public bool Valid { get; set; }
    public string Tier { get; set; }
    public int RateLimitPerMinute { get; set; }
    public int RateLimitBurst { get; set; }
    public int ConcurrencyLimit { get; set; }
    public string ErrorMessage { get; set; }
}
