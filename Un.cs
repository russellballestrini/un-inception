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
            ApiRequest($"/services/{args.ServiceSleep}/sleep", "POST", null, publicKey, secretKey);
            Console.WriteLine($"{GREEN}Service sleeping: {args.ServiceSleep}{RESET}");
            return;
        }

        if (args.ServiceWake != null)
        {
            ApiRequest($"/services/{args.ServiceWake}/wake", "POST", null, publicKey, secretKey);
            Console.WriteLine($"{GREEN}Service waking: {args.ServiceWake}{RESET}");
            return;
        }

        if (args.ServiceDestroy != null)
        {
            ApiRequest($"/services/{args.ServiceDestroy}", "DELETE", null, publicKey, secretKey);
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

            var result = ApiRequest("/services", "POST", payload, publicKey, secretKey);
            Console.WriteLine($"{GREEN}Service created: {result["id"]}{RESET}");
            Console.WriteLine($"Name: {result["name"]}");
            if (result.ContainsKey("url"))
            {
                Console.WriteLine($"URL: {result["url"]}");
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

    static Dictionary<string, object> ApiRequest(string endpoint, string method, Dictionary<string, object> data, string publicKey, string secretKey)
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

            throw new Exception($"HTTP error - {error}");
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
            else if (arg == "-k" || arg == "--api-key") result.ApiKey = args[++i];
            else if (arg == "-n" || arg == "--network") result.Network = args[++i];
            else if (arg == "-v" || arg == "--vcpu") result.Vcpu = int.Parse(args[++i]);
            else if (arg == "-e" || arg == "--env") result.Env.Add(args[++i]);
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
        Console.WriteLine(@"Usage: Un [options] <source_file>
       Un session [options]
       Un service [options]
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
  --freeze ID        Freeze service
  --unfreeze ID         Unfreeze service
  --destroy ID      Destroy service
  --execute ID      Execute command in service
  --command CMD     Command to execute (with --execute)
  --dump-bootstrap ID   Dump bootstrap script
  --dump-file FILE      File to save bootstrap (with --dump-bootstrap)

Key options:
  --extend          Open browser to extend expired key");
    }
}
