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

public class Un {
    private static final String API_BASE = "https://api.unsandbox.com";
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
        String apiKey = getApiKey(args.apiKey);
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

        Map<String, Object> result = apiRequest("/execute", "POST", payload, apiKey);

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
        String apiKey = getApiKey(args.apiKey);

        if (args.sessionList) {
            Map<String, Object> result = apiRequest("/sessions", "GET", null, apiKey);
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
            apiRequest("/sessions/" + args.sessionKill, "DELETE", null, apiKey);
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

        System.out.println(YELLOW + "Creating session..." + RESET);
        Map<String, Object> result = apiRequest("/sessions", "POST", payload, apiKey);
        System.out.println(GREEN + "Session created: " + result.getOrDefault("id", "N/A") + RESET);
        System.out.println(YELLOW + "(Interactive sessions require WebSocket - use un2 for full support)" + RESET);
    }

    private static void cmdService(Args args) throws Exception {
        String apiKey = getApiKey(args.apiKey);

        if (args.serviceList) {
            Map<String, Object> result = apiRequest("/services", "GET", null, apiKey);
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
            Map<String, Object> result = apiRequest("/services/" + args.serviceInfo, "GET", null, apiKey);
            System.out.println(toJson(result));
            return;
        }

        if (args.serviceLogs != null) {
            Map<String, Object> result = apiRequest("/services/" + args.serviceLogs + "/logs", "GET", null, apiKey);
            System.out.println(result.getOrDefault("logs", ""));
            return;
        }

        if (args.serviceTail != null) {
            Map<String, Object> result = apiRequest("/services/" + args.serviceTail + "/logs?lines=9000", "GET", null, apiKey);
            System.out.println(result.getOrDefault("logs", ""));
            return;
        }

        if (args.serviceSleep != null) {
            apiRequest("/services/" + args.serviceSleep + "/sleep", "POST", null, apiKey);
            System.out.println(GREEN + "Service sleeping: " + args.serviceSleep + RESET);
            return;
        }

        if (args.serviceWake != null) {
            apiRequest("/services/" + args.serviceWake + "/wake", "POST", null, apiKey);
            System.out.println(GREEN + "Service waking: " + args.serviceWake + RESET);
            return;
        }

        if (args.serviceDestroy != null) {
            apiRequest("/services/" + args.serviceDestroy, "DELETE", null, apiKey);
            System.out.println(GREEN + "Service destroyed: " + args.serviceDestroy + RESET);
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
            if (args.serviceBootstrap != null) {
                payload.put("bootstrap", args.serviceBootstrap);
            }
            if (args.network != null) {
                payload.put("network", args.network);
            }
            if (args.vcpu > 0) {
                payload.put("vcpu", args.vcpu);
            }

            Map<String, Object> result = apiRequest("/services", "POST", payload, apiKey);
            System.out.println(GREEN + "Service created: " + result.getOrDefault("id", "N/A") + RESET);
            System.out.println("Name: " + result.getOrDefault("name", "N/A"));
            if (result.containsKey("url")) {
                System.out.println("URL: " + result.get("url"));
            }
            return;
        }

        System.err.println(RED + "Error: Specify --name to create a service, or use --list, --info, etc." + RESET);
        System.exit(1);
    }

    private static String getApiKey(String argsKey) {
        String key = argsKey != null ? argsKey : System.getenv("UNSANDBOX_API_KEY");
        if (key == null || key.isEmpty()) {
            System.err.println(RED + "Error: UNSANDBOX_API_KEY not set" + RESET);
            System.exit(1);
        }
        return key;
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

    private static Map<String, Object> apiRequest(String endpoint, String method, Map<String, Object> data, String apiKey) throws Exception {
        URL url = new URL(API_BASE + endpoint);
        HttpURLConnection conn = (HttpURLConnection) url.openConnection();
        conn.setRequestMethod(method);
        conn.setRequestProperty("Authorization", "Bearer " + apiKey);
        conn.setRequestProperty("Content-Type", "application/json");
        conn.setConnectTimeout(30000);
        conn.setReadTimeout(300000);

        if (data != null) {
            conn.setDoOutput(true);
            String json = toJson(data);
            try (OutputStream os = conn.getOutputStream()) {
                os.write(json.getBytes("UTF-8"));
            }
        }

        int status = conn.getResponseCode();
        if (status < 200 || status >= 300) {
            String error = readStream(conn.getErrorStream());
            throw new Exception("HTTP " + status + " - " + error);
        }

        String response = readStream(conn.getInputStream());
        return parseJson(response);
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
        String serviceBootstrap = null;
        String serviceInfo = null;
        String serviceLogs = null;
        String serviceTail = null;
        String serviceSleep = null;
        String serviceWake = null;
        String serviceDestroy = null;
    }

    private static Args parseArgs(String[] args) {
        Args result = new Args();
        for (int i = 0; i < args.length; i++) {
            String arg = args[i];
            if (arg.equals("session")) {
                result.command = "session";
            } else if (arg.equals("service")) {
                result.command = "service";
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
            } else if (arg.equals("--bootstrap")) {
                result.serviceBootstrap = args[++i];
            } else if (arg.equals("--info")) {
                result.serviceInfo = args[++i];
            } else if (arg.equals("--logs")) {
                result.serviceLogs = args[++i];
            } else if (arg.equals("--tail")) {
                result.serviceTail = args[++i];
            } else if (arg.equals("--sleep")) {
                result.serviceSleep = args[++i];
            } else if (arg.equals("--wake")) {
                result.serviceWake = args[++i];
            } else if (arg.equals("--destroy")) {
                result.serviceDestroy = args[++i];
            } else if (!arg.startsWith("-")) {
                result.sourceFile = arg;
            }
        }
        return result;
    }

    private static void printHelp() {
        System.out.println("Usage: java Un [options] <source_file>");
        System.out.println("       java Un session [options]");
        System.out.println("       java Un service [options]");
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
        System.out.println("  --bootstrap CMD   Bootstrap command");
        System.out.println("  --info ID         Get service details");
        System.out.println("  --logs ID         Get all logs");
        System.out.println("  --tail ID         Get last 9000 lines");
        System.out.println("  --sleep ID        Freeze service");
        System.out.println("  --wake ID         Unfreeze service");
        System.out.println("  --destroy ID      Destroy service");
    }
}
