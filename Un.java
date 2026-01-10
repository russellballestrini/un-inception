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


// Un.java - Unsandbox CLI Client (Java Implementation)
// Compile: javac Un.java
// Run: java Un [options] <source_file>
// Requires: UNSANDBOX_API_KEY environment variable

import java.io.*;
import java.net.*;
import java.nio.file.*;
import java.util.*;
import java.util.Base64;
import javax.crypto.Mac;
import javax.crypto.spec.SecretKeySpec;

public class Un {
    private static final String API_BASE = "https://api.unsandbox.com";
    private static final String PORTAL_BASE = "https://unsandbox.com";
    private static final String BLUE = "\033[34m";
    private static final String RED = "\033[31m";
    private static final String GREEN = "\033[32m";
    private static final String YELLOW = "\033[33m";
    private static final String RESET = "\033[0m";

    private static final Map<String, String> EXT_MAP = new HashMap<>() {{
        put(".py", "python"); put(".js", "javascript"); put(".ts", "typescript");
        put(".rb", "ruby"); put(".php", "php"); put(".pl", "perl"); put(".lua", "lua");
        put(".sh", "bash"); put(".go", "go"); put(".rs", "rust"); put(".c", "c");
        put(".cpp", "cpp"); put(".cc", "cpp"); put(".cxx", "cpp");
        put(".java", "java"); put(".kt", "kotlin"); put(".cs", "csharp"); put(".fs", "fsharp");
        put(".hs", "haskell"); put(".ml", "ocaml"); put(".clj", "clojure"); put(".scm", "scheme");
        put(".lisp", "commonlisp"); put(".erl", "erlang"); put(".ex", "elixir"); put(".exs", "elixir");
        put(".jl", "julia"); put(".r", "r"); put(".R", "r"); put(".cr", "crystal");
        put(".d", "d"); put(".nim", "nim"); put(".zig", "zig"); put(".v", "v");
        put(".dart", "dart"); put(".groovy", "groovy"); put(".scala", "scala");
        put(".f90", "fortran"); put(".f95", "fortran"); put(".cob", "cobol");
        put(".pro", "prolog"); put(".forth", "forth"); put(".4th", "forth");
        put(".tcl", "tcl"); put(".raku", "raku"); put(".m", "objc");
    }};

    public static void main(String[] args) {
        try {
            Args parsedArgs = parseArgs(args);

            if (parsedArgs.command.equals("session")) {
                cmdSession(parsedArgs);
            } else if (parsedArgs.command.equals("service")) {
                cmdService(parsedArgs);
            } else if (parsedArgs.command.equals("key")) {
                cmdKey(parsedArgs);
            } else if (parsedArgs.sourceFile != null) {
                cmdExecute(parsedArgs);
            } else {
                printHelp();
                System.exit(1);
            }
        } catch (Exception e) {
            System.err.println(RED + "Error: " + e.getMessage() + RESET);
            System.exit(1);
        }
    }

    private static void cmdExecute(Args args) throws Exception {
        String[] keys = getApiKeys(args.apiKey);
        String publicKey = keys[0];
        String secretKey = keys[1];
        String code = Files.readString(Paths.get(args.sourceFile));
        String language = detectLanguage(args.sourceFile);

        Map<String, Object> payload = new HashMap<>();
        payload.put("language", language);
        payload.put("code", code);

        if (args.env != null && !args.env.isEmpty()) {
            Map<String, String> envVars = new HashMap<>();
            for (String e : args.env) {
                String[] parts = e.split("=", 2);
                if (parts.length == 2) {
                    envVars.put(parts[0], parts[1]);
                }
            }
            if (!envVars.isEmpty()) {
                payload.put("env", envVars);
            }
        }

        if (args.files != null && !args.files.isEmpty()) {
            List<Map<String, String>> inputFiles = new ArrayList<>();
            for (String filepath : args.files) {
                byte[] content = Files.readAllBytes(Paths.get(filepath));
                Map<String, String> fileObj = new HashMap<>();
                fileObj.put("filename", Paths.get(filepath).getFileName().toString());
                fileObj.put("content_base64", Base64.getEncoder().encodeToString(content));
                inputFiles.add(fileObj);
            }
            payload.put("input_files", inputFiles);
        }

        if (args.artifacts) {
            payload.put("return_artifacts", true);
        }
        if (args.network != null) {
            payload.put("network", args.network);
        }
        if (args.vcpu > 0) {
            payload.put("vcpu", args.vcpu);
        }

        Map<String, Object> result = apiRequest("/execute", "POST", payload, publicKey, secretKey);

        String stdout = (String) result.get("stdout");
        String stderr = (String) result.get("stderr");
        if (stdout != null && !stdout.isEmpty()) {
            System.out.print(BLUE + stdout + RESET);
        }
        if (stderr != null && !stderr.isEmpty()) {
            System.err.print(RED + stderr + RESET);
        }

        if (args.artifacts && result.containsKey("artifacts")) {
            @SuppressWarnings("unchecked")
            List<Map<String, String>> artifacts = (List<Map<String, String>>) result.get("artifacts");
            String outDir = args.outputDir != null ? args.outputDir : ".";
            Files.createDirectories(Paths.get(outDir));
            for (Map<String, String> artifact : artifacts) {
                String filename = artifact.getOrDefault("filename", "artifact");
                byte[] content = Base64.getDecoder().decode(artifact.get("content_base64"));
                Path path = Paths.get(outDir, filename);
                Files.write(path, content);
                path.toFile().setExecutable(true);
                System.err.println(GREEN + "Saved: " + path + RESET);
            }
        }

        int exitCode = result.containsKey("exit_code") ? ((Number) result.get("exit_code")).intValue() : 0;
        System.exit(exitCode);
    }

    private static void cmdSession(Args args) throws Exception {
        String[] keys = getApiKeys(args.apiKey);
        String publicKey = keys[0];
        String secretKey = keys[1];

        if (args.sessionList) {
            Map<String, Object> result = apiRequest("/sessions", "GET", null, publicKey, secretKey);
            @SuppressWarnings("unchecked")
            List<Map<String, Object>> sessions = (List<Map<String, Object>>) result.get("sessions");
            if (sessions == null || sessions.isEmpty()) {
                System.out.println("No active sessions");
            } else {
                System.out.printf("%-40s %-10s %-10s %s%n", "ID", "Shell", "Status", "Created");
                for (Map<String, Object> s : sessions) {
                    System.out.printf("%-40s %-10s %-10s %s%n",
                        s.getOrDefault("id", "N/A"),
                        s.getOrDefault("shell", "N/A"),
                        s.getOrDefault("status", "N/A"),
                        s.getOrDefault("created_at", "N/A"));
                }
            }
            return;
        }

        if (args.sessionKill != null) {
            apiRequest("/sessions/" + args.sessionKill, "DELETE", null, publicKey, secretKey);
            System.out.println(GREEN + "Session terminated: " + args.sessionKill + RESET);
            return;
        }

        Map<String, Object> payload = new HashMap<>();
        payload.put("shell", args.sessionShell != null ? args.sessionShell : "bash");
        if (args.network != null) {
            payload.put("network", args.network);
        }
        if (args.vcpu > 0) {
            payload.put("vcpu", args.vcpu);
        }

        // Add input files
        if (args.files != null && !args.files.isEmpty()) {
            List<Map<String, String>> inputFiles = new ArrayList<>();
            for (String filepath : args.files) {
                byte[] content = Files.readAllBytes(Paths.get(filepath));
                Map<String, String> fileObj = new HashMap<>();
                fileObj.put("filename", Paths.get(filepath).getFileName().toString());
                fileObj.put("content_base64", Base64.getEncoder().encodeToString(content));
                inputFiles.add(fileObj);
            }
            payload.put("input_files", inputFiles);
        }

        System.out.println(YELLOW + "Creating session..." + RESET);
        Map<String, Object> result = apiRequest("/sessions", "POST", payload, publicKey, secretKey);
        System.out.println(GREEN + "Session created: " + result.getOrDefault("id", "N/A") + RESET);
        System.out.println(YELLOW + "(Interactive sessions require WebSocket - use un2 for full support)" + RESET);
    }

    private static void cmdService(Args args) throws Exception {
        String[] keys = getApiKeys(args.apiKey);
        String publicKey = keys[0];
        String secretKey = keys[1];

        // Handle service env subcommand
        if (args.envAction != null) {
            cmdServiceEnv(args, publicKey, secretKey);
            return;
        }

        if (args.serviceList) {
            Map<String, Object> result = apiRequest("/services", "GET", null, publicKey, secretKey);
            @SuppressWarnings("unchecked")
            List<Map<String, Object>> services = (List<Map<String, Object>>) result.get("services");
            if (services == null || services.isEmpty()) {
                System.out.println("No services");
            } else {
                System.out.printf("%-20s %-15s %-10s %-15s %s%n", "ID", "Name", "Status", "Ports", "Domains");
                for (Map<String, Object> s : services) {
                    @SuppressWarnings("unchecked")
                    List<Integer> ports = (List<Integer>) s.get("ports");
                    @SuppressWarnings("unchecked")
                    List<String> domains = (List<String>) s.get("domains");
                    String portsStr = ports != null ? String.join(",", ports.stream().map(Object::toString).toArray(String[]::new)) : "";
                    String domainsStr = domains != null ? String.join(",", domains) : "";
                    System.out.printf("%-20s %-15s %-10s %-15s %s%n",
                        s.getOrDefault("id", "N/A"),
                        s.getOrDefault("name", "N/A"),
                        s.getOrDefault("status", "N/A"),
                        portsStr, domainsStr);
                }
            }
            return;
        }

        if (args.serviceInfo != null) {
            Map<String, Object> result = apiRequest("/services/" + args.serviceInfo, "GET", null, publicKey, secretKey);
            System.out.println(toJson(result));
            return;
        }

        if (args.serviceLogs != null) {
            Map<String, Object> result = apiRequest("/services/" + args.serviceLogs + "/logs", "GET", null, publicKey, secretKey);
            System.out.println(result.getOrDefault("logs", ""));
            return;
        }

        if (args.serviceTail != null) {
            Map<String, Object> result = apiRequest("/services/" + args.serviceTail + "/logs?lines=9000", "GET", null, publicKey, secretKey);
            System.out.println(result.getOrDefault("logs", ""));
            return;
        }

        if (args.serviceSleep != null) {
            apiRequest("/services/" + args.serviceSleep + "/sleep", "POST", null, publicKey, secretKey);
            System.out.println(GREEN + "Service sleeping: " + args.serviceSleep + RESET);
            return;
        }

        if (args.serviceWake != null) {
            apiRequest("/services/" + args.serviceWake + "/wake", "POST", null, publicKey, secretKey);
            System.out.println(GREEN + "Service waking: " + args.serviceWake + RESET);
            return;
        }

        if (args.serviceDestroy != null) {
            apiRequest("/services/" + args.serviceDestroy, "DELETE", null, publicKey, secretKey);
            System.out.println(GREEN + "Service destroyed: " + args.serviceDestroy + RESET);
            return;
        }

        if (args.serviceExecute != null) {
            Map<String, Object> payload = new HashMap<>();
            payload.put("command", args.serviceCommand);
            Map<String, Object> result = apiRequest("/services/" + args.serviceExecute + "/execute", "POST", payload, publicKey, secretKey);
            String stdout = (String) result.get("stdout");
            String stderr = (String) result.get("stderr");
            if (stdout != null && !stdout.isEmpty()) {
                System.out.print(BLUE + stdout + RESET);
            }
            if (stderr != null && !stderr.isEmpty()) {
                System.err.print(RED + stderr + RESET);
            }
            return;
        }

        if (args.serviceDumpBootstrap != null) {
            System.err.println("Fetching bootstrap script from " + args.serviceDumpBootstrap + "...");
            Map<String, Object> payload = new HashMap<>();
            payload.put("command", "cat /tmp/bootstrap.sh");
            Map<String, Object> result = apiRequest("/services/" + args.serviceDumpBootstrap + "/execute", "POST", payload, publicKey, secretKey);

            String bootstrap = (String) result.get("stdout");
            if (bootstrap != null && !bootstrap.isEmpty()) {
                if (args.serviceDumpFile != null) {
                    try {
                        java.nio.file.Path path = java.nio.file.Paths.get(args.serviceDumpFile);
                        java.nio.file.Files.write(path, bootstrap.getBytes());
                        path.toFile().setExecutable(true, false);
                        System.out.println("Bootstrap saved to " + args.serviceDumpFile);
                    } catch (Exception e) {
                        System.err.println(RED + "Error: Could not write to " + args.serviceDumpFile + ": " + e.getMessage() + RESET);
                        System.exit(1);
                    }
                } else {
                    System.out.print(bootstrap);
                }
            } else {
                System.err.println(RED + "Error: Failed to fetch bootstrap (service not running or no bootstrap file)" + RESET);
                System.exit(1);
            }
            return;
        }

        if (args.serviceName != null) {
            Map<String, Object> payload = new HashMap<>();
            payload.put("name", args.serviceName);
            if (args.servicePorts != null) {
                List<Integer> ports = new ArrayList<>();
                for (String p : args.servicePorts.split(",")) {
                    ports.add(Integer.parseInt(p.trim()));
                }
                payload.put("ports", ports);
            }
            if (args.serviceType != null) {
                payload.put("service_type", args.serviceType);
            }
            if (args.serviceBootstrap != null) {
                payload.put("bootstrap", args.serviceBootstrap);
            }
            // Add input files
            if (args.files != null && !args.files.isEmpty()) {
                List<Map<String, String>> inputFiles = new ArrayList<>();
                for (String filepath : args.files) {
                    byte[] content = Files.readAllBytes(Paths.get(filepath));
                    Map<String, String> fileObj = new HashMap<>();
                    fileObj.put("filename", Paths.get(filepath).getFileName().toString());
                    fileObj.put("content_base64", Base64.getEncoder().encodeToString(content));
                    inputFiles.add(fileObj);
                }
                payload.put("input_files", inputFiles);
            }
            if (args.network != null) {
                payload.put("network", args.network);
            }
            if (args.vcpu > 0) {
                payload.put("vcpu", args.vcpu);
            }

            Map<String, Object> result = apiRequest("/services", "POST", payload, publicKey, secretKey);
            String serviceId = (String) result.get("id");
            System.out.println(GREEN + "Service created: " + (serviceId != null ? serviceId : "N/A") + RESET);
            System.out.println("Name: " + result.getOrDefault("name", "N/A"));
            if (result.containsKey("url")) {
                System.out.println("URL: " + result.get("url"));
            }

            // Auto-set vault if env vars provided
            if (serviceId != null && (!args.env.isEmpty() || (args.envFile != null && !args.envFile.isEmpty()))) {
                try {
                    String envContent = buildEnvContent(args.env, args.envFile);
                    if (!envContent.isEmpty() && envContent.length() <= 65536) {
                        serviceEnvSet(serviceId, envContent, publicKey, secretKey);
                        System.out.println(GREEN + "Vault configured with environment variables" + RESET);
                    }
                } catch (Exception e) {
                    System.err.println(YELLOW + "Warning: Failed to set vault: " + e.getMessage() + RESET);
                }
            }
            return;
        }

        System.err.println(RED + "Error: Specify --name to create a service, or use --list, --info, etc." + RESET);
        System.exit(1);
    }

    private static void cmdKey(Args args) throws Exception {
        String[] keys = getApiKeys(args.apiKey);
        String publicKey = keys[0];
        String secretKey = keys[1];

        if (args.keyExtend) {
            // First validate to get public_key
            Map<String, Object> result = validateKey(publicKey, secretKey);
            String pubKey = (String) result.get("public_key");
            if (pubKey == null || pubKey.isEmpty()) {
                System.err.println(RED + "Error: Could not retrieve public key" + RESET);
                System.exit(1);
            }

            String extendUrl = PORTAL_BASE + "/keys/extend?pk=" + urlEncode(pubKey);
            System.out.println(YELLOW + "Opening browser to extend key:" + RESET);
            System.out.println(extendUrl);

            // Try to open browser
            try {
                String os = System.getProperty("os.name").toLowerCase();
                if (os.contains("mac")) {
                    Runtime.getRuntime().exec(new String[]{"open", extendUrl});
                } else if (os.contains("nix") || os.contains("nux")) {
                    Runtime.getRuntime().exec(new String[]{"xdg-open", extendUrl});
                } else if (os.contains("win")) {
                    Runtime.getRuntime().exec(new String[]{"rundll32", "url.dll,FileProtocolHandler", extendUrl});
                }
            } catch (Exception e) {
                // Browser opening failed, URL already printed
            }
            return;
        }

        // Default: validate key
        Map<String, Object> result = validateKey(publicKey, secretKey);
        Boolean expired = (Boolean) result.get("expired");
        String pubKey = (String) result.get("public_key");
        String tier = (String) result.get("tier");
        String status = (String) result.get("status");
        String expiresAt = (String) result.get("expires_at");
        String timeRemaining = (String) result.get("time_remaining");
        Object rateLimit = result.get("rate_limit");
        Object burst = result.get("burst");
        Object concurrency = result.get("concurrency");

        if (expired != null && expired) {
            System.out.println(RED + "Expired" + RESET);
            System.out.println("Public Key: " + (pubKey != null ? pubKey : "N/A"));
            System.out.println("Tier: " + (tier != null ? tier : "N/A"));
            System.out.println("Expired: " + (expiresAt != null ? expiresAt : "N/A"));
            System.out.println(YELLOW + "To renew: Visit " + PORTAL_BASE + "/keys/extend" + RESET);
            System.exit(1);
        }

        // Valid key
        System.out.println(GREEN + "Valid" + RESET);
        System.out.println("Public Key: " + (pubKey != null ? pubKey : "N/A"));
        System.out.println("Tier: " + (tier != null ? tier : "N/A"));
        System.out.println("Status: " + (status != null ? status : "N/A"));
        System.out.println("Expires: " + (expiresAt != null ? expiresAt : "N/A"));
        System.out.println("Time Remaining: " + (timeRemaining != null ? timeRemaining : "N/A"));
        System.out.println("Rate Limit: " + (rateLimit != null ? rateLimit : "N/A"));
        System.out.println("Burst: " + (burst != null ? burst : "N/A"));
        System.out.println("Concurrency: " + (concurrency != null ? concurrency : "N/A"));
    }

    private static Map<String, Object> validateKey(String publicKey, String secretKey) throws Exception {
        long timestamp = System.currentTimeMillis() / 1000;
        String method = "POST";
        String path = "/keys/validate";
        String body = "";
        String signatureData = timestamp + ":" + method + ":" + path + ":" + body;
        String signature = hmacSha256(secretKey, signatureData);

        URL url = new URL(PORTAL_BASE + path);
        HttpURLConnection conn = (HttpURLConnection) url.openConnection();
        conn.setRequestMethod(method);
        conn.setRequestProperty("Authorization", "Bearer " + (publicKey != null ? publicKey : secretKey));
        conn.setRequestProperty("X-Timestamp", String.valueOf(timestamp));
        conn.setRequestProperty("X-Signature", signature);
        conn.setRequestProperty("Content-Type", "application/json");
        conn.setConnectTimeout(30000);
        conn.setReadTimeout(30000);

        int status = conn.getResponseCode();
        if (status < 200 || status >= 300) {
            String error = readStream(conn.getErrorStream());
            // Try to parse error JSON
            try {
                Map<String, Object> errorJson = parseJson(error);
                String reason = (String) errorJson.getOrDefault("error", error);
                System.out.println(RED + "Invalid" + RESET);
                System.out.println("Reason: " + reason);
            } catch (Exception e) {
                System.out.println(RED + "Invalid" + RESET);
                System.out.println("Reason: " + error);
            }
            System.exit(1);
        }

        String response = readStream(conn.getInputStream());
        return parseJson(response);
    }

    private static String urlEncode(String s) {
        try {
            return URLEncoder.encode(s, "UTF-8");
        } catch (UnsupportedEncodingException e) {
            return s;
        }
    }

    private static String[] getApiKeys(String argsKey) {
        String publicKey = null;
        String secretKey = null;

        if (argsKey != null) {
            // If API key provided via args, use it as secret key for backwards compat
            secretKey = argsKey;
            publicKey = System.getenv("UNSANDBOX_PUBLIC_KEY");
        } else {
            // Try new-style auth first
            publicKey = System.getenv("UNSANDBOX_PUBLIC_KEY");
            secretKey = System.getenv("UNSANDBOX_SECRET_KEY");

            // Fall back to old-style auth
            if (publicKey == null || secretKey == null) {
                String apiKey = System.getenv("UNSANDBOX_API_KEY");
                if (apiKey != null && !apiKey.isEmpty()) {
                    secretKey = apiKey;
                }
            }
        }

        if (secretKey == null || secretKey.isEmpty()) {
            System.err.println(RED + "Error: UNSANDBOX_SECRET_KEY or UNSANDBOX_API_KEY not set" + RESET);
            System.exit(1);
        }

        return new String[] { publicKey, secretKey };
    }

    private static String hmacSha256(String secretKey, String data) throws Exception {
        Mac mac = Mac.getInstance("HmacSHA256");
        SecretKeySpec keySpec = new SecretKeySpec(secretKey.getBytes("UTF-8"), "HmacSHA256");
        mac.init(keySpec);
        byte[] hash = mac.doFinal(data.getBytes("UTF-8"));
        StringBuilder hex = new StringBuilder();
        for (byte b : hash) {
            hex.append(String.format("%02x", b));
        }
        return hex.toString();
    }

    private static String detectLanguage(String filename) throws Exception {
        int dotIndex = filename.lastIndexOf('.');
        if (dotIndex == -1) {
            throw new Exception("Cannot detect language: no file extension");
        }
        String ext = filename.substring(dotIndex).toLowerCase();
        String lang = EXT_MAP.get(ext);
        if (lang == null) {
            throw new Exception("Unsupported file extension: " + ext);
        }
        return lang;
    }

    private static Map<String, Object> apiRequest(String endpoint, String method, Map<String, Object> data, String publicKey, String secretKey) throws Exception {
        long timestamp = System.currentTimeMillis() / 1000;
        String body = data != null ? toJson(data) : "";
        String signatureData = timestamp + ":" + method + ":" + endpoint + ":" + body;
        String signature = hmacSha256(secretKey, signatureData);

        URL url = new URL(API_BASE + endpoint);
        HttpURLConnection conn = (HttpURLConnection) url.openConnection();
        conn.setRequestMethod(method);
        conn.setRequestProperty("Authorization", "Bearer " + (publicKey != null ? publicKey : secretKey));
        conn.setRequestProperty("X-Timestamp", String.valueOf(timestamp));
        conn.setRequestProperty("X-Signature", signature);
        conn.setRequestProperty("Content-Type", "application/json");
        conn.setConnectTimeout(30000);
        conn.setReadTimeout(300000);

        if (data != null) {
            conn.setDoOutput(true);
            try (OutputStream os = conn.getOutputStream()) {
                os.write(body.getBytes("UTF-8"));
            }
        }

        int status = conn.getResponseCode();
        if (status < 200 || status >= 300) {
            String error = readStream(conn.getErrorStream());

            // Check for clock drift errors
            if (error.contains("timestamp") && (status == 401 || error.toLowerCase().contains("expired") || error.toLowerCase().contains("invalid"))) {
                System.err.println(RED + "Error: Request timestamp expired (must be within 5 minutes of server time)" + RESET);
                System.err.println(YELLOW + "Your computer's clock may have drifted." + RESET);
                System.err.println("Check your system time and sync with NTP if needed:");
                System.err.println("  Linux:   sudo ntpdate -s time.nist.gov");
                System.err.println("  macOS:   sudo sntp -sS time.apple.com");
                System.err.println("  Windows: w32tm /resync");
                System.exit(1);
            }

            throw new Exception("HTTP " + status + " - " + error);
        }

        String response = readStream(conn.getInputStream());
        return parseJson(response);
    }

    private static String apiRequestText(String endpoint, String method, String body, String publicKey, String secretKey) throws Exception {
        long timestamp = System.currentTimeMillis() / 1000;
        String signatureData = timestamp + ":" + method + ":" + endpoint + ":" + body;
        String signature = hmacSha256(secretKey, signatureData);

        URL url = new URL(API_BASE + endpoint);
        HttpURLConnection conn = (HttpURLConnection) url.openConnection();
        conn.setRequestMethod(method);
        conn.setRequestProperty("Authorization", "Bearer " + (publicKey != null ? publicKey : secretKey));
        conn.setRequestProperty("X-Timestamp", String.valueOf(timestamp));
        conn.setRequestProperty("X-Signature", signature);
        conn.setRequestProperty("Content-Type", "text/plain");
        conn.setConnectTimeout(30000);
        conn.setReadTimeout(300000);

        if (body != null && !body.isEmpty()) {
            conn.setDoOutput(true);
            try (OutputStream os = conn.getOutputStream()) {
                os.write(body.getBytes("UTF-8"));
            }
        }

        int status = conn.getResponseCode();
        if (status < 200 || status >= 300) {
            String error = readStream(conn.getErrorStream());
            throw new Exception("HTTP " + status + " - " + error);
        }

        return readStream(conn.getInputStream());
    }

    private static String readEnvFile(String path) throws Exception {
        return new String(Files.readAllBytes(Paths.get(path)), "UTF-8");
    }

    private static String buildEnvContent(List<String> envs, String envFile) throws Exception {
        StringBuilder parts = new StringBuilder();
        if (envFile != null && !envFile.isEmpty()) {
            parts.append(readEnvFile(envFile).trim());
        }
        for (String e : envs) {
            if (e.contains("=")) {
                if (parts.length() > 0) parts.append("\n");
                parts.append(e);
            }
        }
        return parts.toString();
    }

    private static Map<String, Object> serviceEnvStatus(String serviceId, String publicKey, String secretKey) throws Exception {
        return apiRequest("/services/" + serviceId + "/env", "GET", null, publicKey, secretKey);
    }

    private static boolean serviceEnvSet(String serviceId, String envContent, String publicKey, String secretKey) throws Exception {
        apiRequestText("/services/" + serviceId + "/env", "PUT", envContent, publicKey, secretKey);
        return true;
    }

    private static Map<String, Object> serviceEnvExport(String serviceId, String publicKey, String secretKey) throws Exception {
        return apiRequest("/services/" + serviceId + "/env/export", "POST", null, publicKey, secretKey);
    }

    private static boolean serviceEnvDelete(String serviceId, String publicKey, String secretKey) throws Exception {
        apiRequest("/services/" + serviceId + "/env", "DELETE", null, publicKey, secretKey);
        return true;
    }

    private static void cmdServiceEnv(Args args, String publicKey, String secretKey) throws Exception {
        String action = args.envAction;
        String target = args.envTarget;

        if (action == null) {
            System.err.println(RED + "Error: Usage: service env <status|set|export|delete> <service_id>" + RESET);
            System.exit(1);
        }

        if (action.equals("status")) {
            if (target == null) {
                System.err.println(RED + "Error: Usage: service env status <service_id>" + RESET);
                System.exit(1);
            }
            Map<String, Object> result = serviceEnvStatus(target, publicKey, secretKey);
            Boolean hasEnv = (Boolean) result.get("has_env");
            Number size = (Number) result.get("size");
            String updatedAt = (String) result.get("updated_at");
            System.out.println("Service: " + target);
            System.out.println("Has Vault: " + (hasEnv != null && hasEnv ? "Yes" : "No"));
            if (hasEnv != null && hasEnv) {
                System.out.println("Size: " + (size != null ? size.intValue() : 0) + " bytes");
                System.out.println("Updated: " + (updatedAt != null ? updatedAt : "N/A"));
            }
        } else if (action.equals("set")) {
            if (target == null) {
                System.err.println(RED + "Error: Usage: service env set <service_id> [-e KEY=VAL] [--env-file FILE]" + RESET);
                System.exit(1);
            }
            String envContent = buildEnvContent(args.env, args.envFile);
            if (envContent.isEmpty()) {
                System.err.println(RED + "Error: No environment variables specified. Use -e KEY=VAL or --env-file FILE" + RESET);
                System.exit(1);
            }
            if (envContent.length() > 65536) {
                System.err.println(RED + "Error: Environment content exceeds 64KB limit" + RESET);
                System.exit(1);
            }
            serviceEnvSet(target, envContent, publicKey, secretKey);
            System.out.println(GREEN + "Vault updated for service: " + target + RESET);
        } else if (action.equals("export")) {
            if (target == null) {
                System.err.println(RED + "Error: Usage: service env export <service_id>" + RESET);
                System.exit(1);
            }
            Map<String, Object> result = serviceEnvExport(target, publicKey, secretKey);
            String content = (String) result.get("content");
            if (content != null && !content.isEmpty()) {
                System.out.print(content);
                if (!content.endsWith("\n")) {
                    System.out.println();
                }
            } else {
                System.err.println(YELLOW + "Vault is empty" + RESET);
            }
        } else if (action.equals("delete")) {
            if (target == null) {
                System.err.println(RED + "Error: Usage: service env delete <service_id>" + RESET);
                System.exit(1);
            }
            serviceEnvDelete(target, publicKey, secretKey);
            System.out.println(GREEN + "Vault deleted for service: " + target + RESET);
        } else {
            System.err.println(RED + "Error: Unknown env action: " + action + ". Use status, set, export, or delete" + RESET);
            System.exit(1);
        }
    }

    private static String readStream(InputStream is) throws IOException {
        if (is == null) return "";
        StringBuilder sb = new StringBuilder();
        try (BufferedReader br = new BufferedReader(new InputStreamReader(is, "UTF-8"))) {
            String line;
            while ((line = br.readLine()) != null) {
                sb.append(line).append("\n");
            }
        }
        return sb.toString();
    }

    private static String toJson(Map<String, Object> map) {
        StringBuilder sb = new StringBuilder("{");
        boolean first = true;
        for (Map.Entry<String, Object> entry : map.entrySet()) {
            if (!first) sb.append(",");
            first = false;
            sb.append(jsonEscape(entry.getKey())).append(":");
            sb.append(valueToJson(entry.getValue()));
        }
        sb.append("}");
        return sb.toString();
    }

    @SuppressWarnings("unchecked")
    private static String valueToJson(Object value) {
        if (value == null) return "null";
        if (value instanceof String) return jsonEscape((String) value);
        if (value instanceof Number) return value.toString();
        if (value instanceof Boolean) return value.toString();
        if (value instanceof Map) return toJson((Map<String, Object>) value);
        if (value instanceof List) {
            List<?> list = (List<?>) value;
            StringBuilder sb = new StringBuilder("[");
            for (int i = 0; i < list.size(); i++) {
                if (i > 0) sb.append(",");
                sb.append(valueToJson(list.get(i)));
            }
            sb.append("]");
            return sb.toString();
        }
        return jsonEscape(value.toString());
    }

    private static String jsonEscape(String s) {
        StringBuilder sb = new StringBuilder("\"");
        for (char c : s.toCharArray()) {
            switch (c) {
                case '"': sb.append("\\\""); break;
                case '\\': sb.append("\\\\"); break;
                case '\n': sb.append("\\n"); break;
                case '\r': sb.append("\\r"); break;
                case '\t': sb.append("\\t"); break;
                default: sb.append(c);
            }
        }
        sb.append("\"");
        return sb.toString();
    }

    @SuppressWarnings("unchecked")
    private static Map<String, Object> parseJson(String json) {
        json = json.trim();
        if (!json.startsWith("{")) return new HashMap<>();

        Map<String, Object> result = new HashMap<>();
        int i = 1;
        while (i < json.length()) {
            while (i < json.length() && Character.isWhitespace(json.charAt(i))) i++;
            if (json.charAt(i) == '}') break;

            if (json.charAt(i) == '"') {
                int keyStart = ++i;
                while (i < json.length() && json.charAt(i) != '"') {
                    if (json.charAt(i) == '\\') i++;
                    i++;
                }
                String key = json.substring(keyStart, i).replace("\\\"", "\"").replace("\\\\", "\\");
                i++;

                while (i < json.length() && (Character.isWhitespace(json.charAt(i)) || json.charAt(i) == ':')) i++;

                Object value = parseJsonValue(json, i);
                if (value instanceof JsonParseResult) {
                    JsonParseResult jpr = (JsonParseResult) value;
                    result.put(key, jpr.value);
                    i = jpr.endIndex;
                }

                while (i < json.length() && (Character.isWhitespace(json.charAt(i)) || json.charAt(i) == ',')) i++;
            } else {
                i++;
            }
        }
        return result;
    }

    private static Object parseJsonValue(String json, int start) {
        int i = start;
        while (i < json.length() && Character.isWhitespace(json.charAt(i))) i++;

        if (json.charAt(i) == '"') {
            int valueStart = ++i;
            StringBuilder sb = new StringBuilder();
            boolean escaped = false;
            while (i < json.length()) {
                char c = json.charAt(i);
                if (escaped) {
                    switch (c) {
                        case 'n': sb.append('\n'); break;
                        case 'r': sb.append('\r'); break;
                        case 't': sb.append('\t'); break;
                        case '"': sb.append('"'); break;
                        case '\\': sb.append('\\'); break;
                        default: sb.append(c);
                    }
                    escaped = false;
                } else if (c == '\\') {
                    escaped = true;
                } else if (c == '"') {
                    return new JsonParseResult(sb.toString(), i + 1);
                } else {
                    sb.append(c);
                }
                i++;
            }
        } else if (json.charAt(i) == '{') {
            int depth = 1;
            int objStart = i++;
            while (i < json.length() && depth > 0) {
                if (json.charAt(i) == '{') depth++;
                else if (json.charAt(i) == '}') depth--;
                i++;
            }
            return new JsonParseResult(parseJson(json.substring(objStart, i)), i);
        } else if (json.charAt(i) == '[') {
            List<Object> list = new ArrayList<>();
            i++;
            while (i < json.length()) {
                while (i < json.length() && Character.isWhitespace(json.charAt(i))) i++;
                if (json.charAt(i) == ']') {
                    i++;
                    break;
                }
                Object item = parseJsonValue(json, i);
                if (item instanceof JsonParseResult) {
                    JsonParseResult jpr = (JsonParseResult) item;
                    list.add(jpr.value);
                    i = jpr.endIndex;
                }
                while (i < json.length() && (Character.isWhitespace(json.charAt(i)) || json.charAt(i) == ',')) i++;
            }
            return new JsonParseResult(list, i);
        } else if (Character.isDigit(json.charAt(i)) || json.charAt(i) == '-') {
            int numStart = i;
            while (i < json.length() && (Character.isDigit(json.charAt(i)) || json.charAt(i) == '.' || json.charAt(i) == '-')) i++;
            String num = json.substring(numStart, i);
            return new JsonParseResult(num.contains(".") ? Double.parseDouble(num) : Integer.parseInt(num), i);
        } else if (json.startsWith("true", i)) {
            return new JsonParseResult(true, i + 4);
        } else if (json.startsWith("false", i)) {
            return new JsonParseResult(false, i + 5);
        } else if (json.startsWith("null", i)) {
            return new JsonParseResult(null, i + 4);
        }
        return new JsonParseResult(null, i);
    }

    static class JsonParseResult {
        Object value;
        int endIndex;
        JsonParseResult(Object value, int endIndex) {
            this.value = value;
            this.endIndex = endIndex;
        }
    }

    static class Args {
        String command = null;
        String sourceFile = null;
        String apiKey = null;
        String network = null;
        int vcpu = 0;
        List<String> env = new ArrayList<>();
        List<String> files = new ArrayList<>();
        boolean artifacts = false;
        String outputDir = null;

        // Session args
        boolean sessionList = false;
        String sessionShell = null;
        String sessionKill = null;

        // Service args
        boolean serviceList = false;
        String serviceName = null;
        String servicePorts = null;
        String serviceType = null;
        String serviceBootstrap = null;
        String serviceInfo = null;
        String serviceLogs = null;
        String serviceTail = null;
        String serviceSleep = null;
        String serviceWake = null;
        String serviceDestroy = null;
        String serviceExecute = null;
        String serviceCommand = null;
        String serviceDumpBootstrap = null;
        String serviceDumpFile = null;

        // Vault args
        String envFile = null;
        String envAction = null;
        String envTarget = null;

        // Key args
        boolean keyExtend = false;
    }

    private static Args parseArgs(String[] args) {
        Args result = new Args();
        for (int i = 0; i < args.length; i++) {
            String arg = args[i];
            if (arg.equals("session")) {
                result.command = "session";
            } else if (arg.equals("service")) {
                result.command = "service";
            } else if (arg.equals("key")) {
                result.command = "key";
            } else if (arg.equals("-k") || arg.equals("--api-key")) {
                result.apiKey = args[++i];
            } else if (arg.equals("-n") || arg.equals("--network")) {
                result.network = args[++i];
            } else if (arg.equals("-v") || arg.equals("--vcpu")) {
                result.vcpu = Integer.parseInt(args[++i]);
            } else if (arg.equals("-e") || arg.equals("--env")) {
                result.env.add(args[++i]);
            } else if (arg.equals("-f") || arg.equals("--files")) {
                result.files.add(args[++i]);
            } else if (arg.equals("-a") || arg.equals("--artifacts")) {
                result.artifacts = true;
            } else if (arg.equals("-o") || arg.equals("--output-dir")) {
                result.outputDir = args[++i];
            } else if (arg.equals("-l") || arg.equals("--list")) {
                if ("session".equals(result.command)) result.sessionList = true;
                else if ("service".equals(result.command)) result.serviceList = true;
            } else if (arg.equals("-s") || arg.equals("--shell")) {
                result.sessionShell = args[++i];
            } else if (arg.equals("--kill")) {
                result.sessionKill = args[++i];
            } else if (arg.equals("--name")) {
                result.serviceName = args[++i];
            } else if (arg.equals("--ports")) {
                result.servicePorts = args[++i];
            } else if (arg.equals("--type")) {
                result.serviceType = args[++i];
            } else if (arg.equals("--bootstrap")) {
                result.serviceBootstrap = args[++i];
            } else if (arg.equals("--info")) {
                result.serviceInfo = args[++i];
            } else if (arg.equals("--logs")) {
                result.serviceLogs = args[++i];
            } else if (arg.equals("--tail")) {
                result.serviceTail = args[++i];
            } else if (arg.equals("--freeze")) {
                result.serviceSleep = args[++i];
            } else if (arg.equals("--unfreeze")) {
                result.serviceWake = args[++i];
            } else if (arg.equals("--destroy")) {
                result.serviceDestroy = args[++i];
            } else if (arg.equals("--execute")) {
                result.serviceExecute = args[++i];
            } else if (arg.equals("--command")) {
                result.serviceCommand = args[++i];
            } else if (arg.equals("--dump-bootstrap")) {
                result.serviceDumpBootstrap = args[++i];
            } else if (arg.equals("--dump-file")) {
                result.serviceDumpFile = args[++i];
            } else if (arg.equals("--env-file")) {
                result.envFile = args[++i];
            } else if (arg.equals("env") && "service".equals(result.command)) {
                // service env <action> <target>
                if (i + 1 < args.length && !args[i + 1].startsWith("-")) {
                    result.envAction = args[++i];
                    if (i + 1 < args.length && !args[i + 1].startsWith("-")) {
                        result.envTarget = args[++i];
                    }
                }
            } else if (arg.equals("--extend")) {
                result.keyExtend = true;
            } else if (!arg.startsWith("-")) {
                result.sourceFile = arg;
            } else {
                System.err.println("Unknown option: " + arg);
                printHelp();
                System.exit(1);
            }
        }
        return result;
    }

    private static void printHelp() {
        System.out.println("Usage: java Un [options] <source_file>");
        System.out.println("       java Un session [options]");
        System.out.println("       java Un service [options]");
        System.out.println("       java Un key [options]");
        System.out.println();
        System.out.println("Execute options:");
        System.out.println("  -e KEY=VALUE      Set environment variable");
        System.out.println("  -f FILE           Add input file");
        System.out.println("  -a                Return artifacts");
        System.out.println("  -o DIR            Output directory for artifacts");
        System.out.println("  -n MODE           Network mode (zerotrust/semitrusted)");
        System.out.println("  -v N              vCPU count (1-8)");
        System.out.println("  -k KEY            API key");
        System.out.println();
        System.out.println("Session options:");
        System.out.println("  --list            List active sessions");
        System.out.println("  --shell NAME      Shell/REPL to use");
        System.out.println("  --kill ID         Terminate session");
        System.out.println();
        System.out.println("Service options:");
        System.out.println("  --list            List services");
        System.out.println("  --name NAME       Service name");
        System.out.println("  --ports PORTS     Comma-separated ports");
        System.out.println("  --type TYPE       Service type (minecraft/mumble/teamspeak/source/tcp/udp)");
        System.out.println("  --bootstrap CMD   Bootstrap command");
        System.out.println("  --info ID         Get service details");
        System.out.println("  --logs ID         Get all logs");
        System.out.println("  --tail ID         Get last 9000 lines");
        System.out.println("  --freeze ID        Freeze service");
        System.out.println("  --unfreeze ID         Unfreeze service");
        System.out.println("  --destroy ID      Destroy service");
        System.out.println("  --execute ID      Execute command in service");
        System.out.println("  --command CMD     Command to execute (with --execute)");
        System.out.println("  --dump-bootstrap ID   Dump bootstrap script");
        System.out.println("  --dump-file FILE      File to save bootstrap (with --dump-bootstrap)");
        System.out.println();
        System.out.println("Service env (vault) commands:");
        System.out.println("  service env status ID          Check vault status");
        System.out.println("  service env set ID [-e K=V]    Set vault contents");
        System.out.println("  service env export ID          Export vault contents");
        System.out.println("  service env delete ID          Delete vault");
        System.out.println("  --env-file FILE                Read env vars from file");
        System.out.println();
        System.out.println("Key options:");
        System.out.println("  --extend          Open browser to extend key");
    }
}
