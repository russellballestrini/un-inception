/* PUBLIC DOMAIN - NO LICENSE, NO WARRANTY
 *
 * unsandbox.com Java SDK (Synchronous)
 *
 * Library Usage:
 *     import Un;
 *     import java.util.Map;
 *     import java.util.List;
 *
 *     // Execute code synchronously
 *     Map<String, Object> result = Un.executeCode("python", "print('hello')", publicKey, secretKey);
 *
 *     // Execute asynchronously
 *     String jobId = Un.executeAsync("javascript", "console.log('hello')", publicKey, secretKey);
 *
 *     // Wait for job completion with exponential backoff
 *     Map<String, Object> result = Un.waitForJob(jobId, publicKey, secretKey, 60000);
 *
 *     // List all jobs
 *     List<Map<String, Object>> jobs = Un.listJobs(publicKey, secretKey);
 *
 *     // Get supported languages
 *     List<String> languages = Un.getLanguages(publicKey, secretKey);
 *
 *     // Snapshot operations
 *     String snapshotId = Un.sessionSnapshot(sessionId, publicKey, secretKey, "my-snapshot", false);
 *
 * Authentication Priority (4-tier):
 *     1. Method arguments (publicKey, secretKey)
 *     2. Environment variables (UNSANDBOX_PUBLIC_KEY, UNSANDBOX_SECRET_KEY)
 *     3. Config file (~/.unsandbox/accounts.csv, line 0 by default)
 *     4. Local directory (./accounts.csv, line 0 by default)
 *
 * Request Authentication (HMAC-SHA256):
 *     Authorization: Bearer <public_key>
 *     X-Timestamp: <unix_seconds>
 *     X-Signature: HMAC-SHA256(secret_key, "timestamp:METHOD:path:body")
 *
 * Languages Cache:
 *     - Cached in ~/.unsandbox/languages.json
 *     - TTL: 1 hour
 *     - Updated on successful API calls
 */

import javax.crypto.Mac;
import javax.crypto.spec.SecretKeySpec;
import java.io.*;
import java.net.HttpURLConnection;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;
import java.util.*;

/**
 * Un SDK - Synchronous Java client for the unsandbox.com API.
 *
 * <p>This class provides methods to execute code in secure sandboxed environments,
 * manage jobs, and work with snapshots.
 *
 * <p>Example usage:
 * <pre>{@code
 * Map<String, Object> result = Un.executeCode("python", "print('hello')", null, null);
 * System.out.println(result.get("stdout"));
 * }</pre>
 *
 * @see <a href="https://unsandbox.com">unsandbox.com</a>
 */
public class Un {

    private static final String API_BASE = "https://api.unsandbox.com";
    private static final int[] POLL_DELAYS_MS = {300, 450, 700, 900, 650, 1600, 2000};
    private static final long LANGUAGES_CACHE_TTL_MS = 3600 * 1000; // 1 hour
    private static final int DEFAULT_TIMEOUT_MS = 120000; // 2 minutes

    /**
     * Exception thrown when credentials cannot be found or are invalid.
     */
    public static class CredentialsException extends RuntimeException {
        public CredentialsException(String message) {
            super(message);
        }
    }

    /**
     * Exception thrown when an API request fails.
     */
    public static class ApiException extends RuntimeException {
        private final int statusCode;
        private final String responseBody;

        public ApiException(String message, int statusCode, String responseBody) {
            super(message);
            this.statusCode = statusCode;
            this.responseBody = responseBody;
        }

        public int getStatusCode() {
            return statusCode;
        }

        public String getResponseBody() {
            return responseBody;
        }
    }

    // ========================================================================
    // Credential Resolution
    // ========================================================================

    private static Path getUnsandboxDir() {
        String home = System.getProperty("user.home");
        Path unsandboxDir = Paths.get(home, ".unsandbox");
        try {
            if (!Files.exists(unsandboxDir)) {
                Files.createDirectories(unsandboxDir);
            }
        } catch (IOException e) {
            // Ignore - will fail later if needed
        }
        return unsandboxDir;
    }

    private static String[] loadCredentialsFromCsv(Path csvPath, int accountIndex) {
        if (!Files.exists(csvPath)) {
            return null;
        }

        try (BufferedReader reader = Files.newBufferedReader(csvPath)) {
            String line;
            int lineIndex = 0;
            while ((line = reader.readLine()) != null) {
                line = line.trim();
                if (line.isEmpty() || line.startsWith("#")) {
                    continue;
                }
                if (lineIndex == accountIndex) {
                    String[] parts = line.split(",");
                    if (parts.length >= 2) {
                        return new String[]{parts[0].trim(), parts[1].trim()};
                    }
                }
                lineIndex++;
            }
        } catch (IOException e) {
            // Ignore - will try next source
        }
        return null;
    }

    private static String[] resolveCredentials(String publicKey, String secretKey) {
        // Tier 1: Method arguments
        if (publicKey != null && !publicKey.isEmpty() && secretKey != null && !secretKey.isEmpty()) {
            return new String[]{publicKey, secretKey};
        }

        // Tier 2: Environment variables
        String envPk = System.getenv("UNSANDBOX_PUBLIC_KEY");
        String envSk = System.getenv("UNSANDBOX_SECRET_KEY");
        if (envPk != null && !envPk.isEmpty() && envSk != null && !envSk.isEmpty()) {
            return new String[]{envPk, envSk};
        }

        // Determine account index
        int accountIndex = 0;
        String accountEnv = System.getenv("UNSANDBOX_ACCOUNT");
        if (accountEnv != null && !accountEnv.isEmpty()) {
            try {
                accountIndex = Integer.parseInt(accountEnv);
            } catch (NumberFormatException e) {
                // Use default
            }
        }

        // Tier 3: ~/.unsandbox/accounts.csv
        Path unsandboxDir = getUnsandboxDir();
        String[] creds = loadCredentialsFromCsv(unsandboxDir.resolve("accounts.csv"), accountIndex);
        if (creds != null) {
            return creds;
        }

        // Tier 4: ./accounts.csv
        creds = loadCredentialsFromCsv(Paths.get("accounts.csv"), accountIndex);
        if (creds != null) {
            return creds;
        }

        throw new CredentialsException(
            "No credentials found. Please provide via:\n" +
            "  1. Method arguments (publicKey, secretKey)\n" +
            "  2. Environment variables (UNSANDBOX_PUBLIC_KEY, UNSANDBOX_SECRET_KEY)\n" +
            "  3. ~/.unsandbox/accounts.csv\n" +
            "  4. ./accounts.csv"
        );
    }

    // ========================================================================
    // HMAC-SHA256 Signing
    // ========================================================================

    private static String signRequest(String secretKey, long timestamp, String method, String path, String body) {
        try {
            String bodyStr = (body != null) ? body : "";
            String message = timestamp + ":" + method + ":" + path + ":" + bodyStr;

            Mac mac = Mac.getInstance("HmacSHA256");
            SecretKeySpec secretKeySpec = new SecretKeySpec(
                secretKey.getBytes(StandardCharsets.UTF_8),
                "HmacSHA256"
            );
            mac.init(secretKeySpec);

            byte[] hash = mac.doFinal(message.getBytes(StandardCharsets.UTF_8));

            // Convert to lowercase hex
            StringBuilder hexString = new StringBuilder();
            for (byte b : hash) {
                hexString.append(String.format("%02x", b));
            }
            return hexString.toString();
        } catch (NoSuchAlgorithmException | InvalidKeyException e) {
            throw new RuntimeException("Failed to compute HMAC-SHA256", e);
        }
    }

    // ========================================================================
    // HTTP Request
    // ========================================================================

    private static Map<String, Object> makeRequest(
        String method,
        String path,
        String publicKey,
        String secretKey,
        Map<String, Object> data
    ) throws IOException {
        String url = API_BASE + path;
        long timestamp = System.currentTimeMillis() / 1000;
        String body = (data != null) ? mapToJson(data) : "";

        String signature = signRequest(secretKey, timestamp, method, path, data != null ? body : null);

        HttpURLConnection conn = (HttpURLConnection) new URL(url).openConnection();
        conn.setRequestMethod(method);
        conn.setConnectTimeout(DEFAULT_TIMEOUT_MS);
        conn.setReadTimeout(DEFAULT_TIMEOUT_MS);

        conn.setRequestProperty("Authorization", "Bearer " + publicKey);
        conn.setRequestProperty("X-Timestamp", String.valueOf(timestamp));
        conn.setRequestProperty("X-Signature", signature);
        conn.setRequestProperty("Content-Type", "application/json");

        if ("POST".equals(method) && data != null) {
            conn.setDoOutput(true);
            try (OutputStream os = conn.getOutputStream()) {
                os.write(body.getBytes(StandardCharsets.UTF_8));
            }
        }

        int responseCode = conn.getResponseCode();
        String responseBody;

        InputStream inputStream = (responseCode >= 200 && responseCode < 300)
            ? conn.getInputStream()
            : conn.getErrorStream();

        if (inputStream == null) {
            responseBody = "";
        } else {
            try (BufferedReader reader = new BufferedReader(new InputStreamReader(inputStream, StandardCharsets.UTF_8))) {
                StringBuilder sb = new StringBuilder();
                String line;
                while ((line = reader.readLine()) != null) {
                    sb.append(line);
                }
                responseBody = sb.toString();
            }
        }

        if (responseCode < 200 || responseCode >= 300) {
            throw new ApiException(
                "API request failed with status " + responseCode,
                responseCode,
                responseBody
            );
        }

        return parseJson(responseBody);
    }

    // ========================================================================
    // Simple JSON Serialization/Deserialization
    // ========================================================================

    @SuppressWarnings("unchecked")
    private static Map<String, Object> parseJson(String json) {
        if (json == null || json.trim().isEmpty()) {
            return new HashMap<>();
        }

        json = json.trim();
        if (!json.startsWith("{")) {
            throw new RuntimeException("Invalid JSON: expected object");
        }

        return (Map<String, Object>) parseValue(json, new int[]{0});
    }

    private static Object parseValue(String json, int[] pos) {
        skipWhitespace(json, pos);

        char c = json.charAt(pos[0]);
        if (c == '{') {
            return parseObject(json, pos);
        } else if (c == '[') {
            return parseArray(json, pos);
        } else if (c == '"') {
            return parseString(json, pos);
        } else if (c == 't' || c == 'f') {
            return parseBoolean(json, pos);
        } else if (c == 'n') {
            return parseNull(json, pos);
        } else if (c == '-' || Character.isDigit(c)) {
            return parseNumber(json, pos);
        }
        throw new RuntimeException("Unexpected character at position " + pos[0]);
    }

    private static void skipWhitespace(String json, int[] pos) {
        while (pos[0] < json.length() && Character.isWhitespace(json.charAt(pos[0]))) {
            pos[0]++;
        }
    }

    private static Map<String, Object> parseObject(String json, int[] pos) {
        Map<String, Object> result = new LinkedHashMap<>();
        pos[0]++; // skip '{'
        skipWhitespace(json, pos);

        if (json.charAt(pos[0]) == '}') {
            pos[0]++;
            return result;
        }

        while (true) {
            skipWhitespace(json, pos);
            String key = parseString(json, pos);
            skipWhitespace(json, pos);

            if (json.charAt(pos[0]) != ':') {
                throw new RuntimeException("Expected ':' at position " + pos[0]);
            }
            pos[0]++;

            Object value = parseValue(json, pos);
            result.put(key, value);

            skipWhitespace(json, pos);
            char c = json.charAt(pos[0]);
            if (c == '}') {
                pos[0]++;
                break;
            } else if (c == ',') {
                pos[0]++;
            } else {
                throw new RuntimeException("Expected ',' or '}' at position " + pos[0]);
            }
        }
        return result;
    }

    private static List<Object> parseArray(String json, int[] pos) {
        List<Object> result = new ArrayList<>();
        pos[0]++; // skip '['
        skipWhitespace(json, pos);

        if (json.charAt(pos[0]) == ']') {
            pos[0]++;
            return result;
        }

        while (true) {
            result.add(parseValue(json, pos));
            skipWhitespace(json, pos);
            char c = json.charAt(pos[0]);
            if (c == ']') {
                pos[0]++;
                break;
            } else if (c == ',') {
                pos[0]++;
            } else {
                throw new RuntimeException("Expected ',' or ']' at position " + pos[0]);
            }
        }
        return result;
    }

    private static String parseString(String json, int[] pos) {
        pos[0]++; // skip opening quote
        StringBuilder sb = new StringBuilder();
        while (pos[0] < json.length()) {
            char c = json.charAt(pos[0]);
            if (c == '"') {
                pos[0]++;
                return sb.toString();
            } else if (c == '\\') {
                pos[0]++;
                if (pos[0] < json.length()) {
                    char escaped = json.charAt(pos[0]);
                    switch (escaped) {
                        case '"': sb.append('"'); break;
                        case '\\': sb.append('\\'); break;
                        case '/': sb.append('/'); break;
                        case 'b': sb.append('\b'); break;
                        case 'f': sb.append('\f'); break;
                        case 'n': sb.append('\n'); break;
                        case 'r': sb.append('\r'); break;
                        case 't': sb.append('\t'); break;
                        case 'u':
                            String hex = json.substring(pos[0] + 1, pos[0] + 5);
                            sb.append((char) Integer.parseInt(hex, 16));
                            pos[0] += 4;
                            break;
                        default: sb.append(escaped);
                    }
                }
            } else {
                sb.append(c);
            }
            pos[0]++;
        }
        throw new RuntimeException("Unterminated string");
    }

    private static Object parseNumber(String json, int[] pos) {
        int start = pos[0];
        boolean isDouble = false;

        if (json.charAt(pos[0]) == '-') pos[0]++;
        while (pos[0] < json.length() && Character.isDigit(json.charAt(pos[0]))) pos[0]++;

        if (pos[0] < json.length() && json.charAt(pos[0]) == '.') {
            isDouble = true;
            pos[0]++;
            while (pos[0] < json.length() && Character.isDigit(json.charAt(pos[0]))) pos[0]++;
        }

        if (pos[0] < json.length() && (json.charAt(pos[0]) == 'e' || json.charAt(pos[0]) == 'E')) {
            isDouble = true;
            pos[0]++;
            if (pos[0] < json.length() && (json.charAt(pos[0]) == '+' || json.charAt(pos[0]) == '-')) pos[0]++;
            while (pos[0] < json.length() && Character.isDigit(json.charAt(pos[0]))) pos[0]++;
        }

        String numStr = json.substring(start, pos[0]);
        if (isDouble) {
            return Double.parseDouble(numStr);
        } else {
            long value = Long.parseLong(numStr);
            if (value >= Integer.MIN_VALUE && value <= Integer.MAX_VALUE) {
                return (int) value;
            }
            return value;
        }
    }

    private static Boolean parseBoolean(String json, int[] pos) {
        if (json.startsWith("true", pos[0])) {
            pos[0] += 4;
            return true;
        } else if (json.startsWith("false", pos[0])) {
            pos[0] += 5;
            return false;
        }
        throw new RuntimeException("Invalid boolean at position " + pos[0]);
    }

    private static Object parseNull(String json, int[] pos) {
        if (json.startsWith("null", pos[0])) {
            pos[0] += 4;
            return null;
        }
        throw new RuntimeException("Invalid null at position " + pos[0]);
    }

    private static String mapToJson(Map<String, Object> map) {
        StringBuilder sb = new StringBuilder();
        sb.append("{");
        boolean first = true;
        for (Map.Entry<String, Object> entry : map.entrySet()) {
            if (!first) sb.append(",");
            first = false;
            sb.append("\"").append(escapeJsonString(entry.getKey())).append("\":");
            sb.append(valueToJson(entry.getValue()));
        }
        sb.append("}");
        return sb.toString();
    }

    private static String valueToJson(Object value) {
        if (value == null) {
            return "null";
        } else if (value instanceof String) {
            return "\"" + escapeJsonString((String) value) + "\"";
        } else if (value instanceof Number) {
            return value.toString();
        } else if (value instanceof Boolean) {
            return value.toString();
        } else if (value instanceof Map) {
            @SuppressWarnings("unchecked")
            Map<String, Object> map = (Map<String, Object>) value;
            return mapToJson(map);
        } else if (value instanceof List) {
            StringBuilder sb = new StringBuilder();
            sb.append("[");
            boolean first = true;
            for (Object item : (List<?>) value) {
                if (!first) sb.append(",");
                first = false;
                sb.append(valueToJson(item));
            }
            sb.append("]");
            return sb.toString();
        }
        return "\"" + escapeJsonString(value.toString()) + "\"";
    }

    private static String escapeJsonString(String s) {
        StringBuilder sb = new StringBuilder();
        for (char c : s.toCharArray()) {
            switch (c) {
                case '"': sb.append("\\\""); break;
                case '\\': sb.append("\\\\"); break;
                case '\b': sb.append("\\b"); break;
                case '\f': sb.append("\\f"); break;
                case '\n': sb.append("\\n"); break;
                case '\r': sb.append("\\r"); break;
                case '\t': sb.append("\\t"); break;
                default:
                    if (c < 0x20) {
                        sb.append(String.format("\\u%04x", (int) c));
                    } else {
                        sb.append(c);
                    }
            }
        }
        return sb.toString();
    }

    // ========================================================================
    // Languages Cache
    // ========================================================================

    private static Path getLanguagesCachePath() {
        return getUnsandboxDir().resolve("languages.json");
    }

    @SuppressWarnings("unchecked")
    private static List<String> loadLanguagesCache() {
        Path cachePath = getLanguagesCachePath();
        if (!Files.exists(cachePath)) {
            return null;
        }

        try {
            long mtime = Files.getLastModifiedTime(cachePath).toMillis();
            long ageMs = System.currentTimeMillis() - mtime;
            if (ageMs >= LANGUAGES_CACHE_TTL_MS) {
                return null;
            }

            String content = new String(Files.readAllBytes(cachePath), StandardCharsets.UTF_8);
            Map<String, Object> data = parseJson(content);
            Object languages = data.get("languages");
            if (languages instanceof List) {
                List<String> result = new ArrayList<>();
                for (Object item : (List<?>) languages) {
                    if (item instanceof String) {
                        result.add((String) item);
                    }
                }
                return result;
            }
        } catch (IOException e) {
            // Cache failure is non-fatal
        }
        return null;
    }

    private static void saveLanguagesCache(List<String> languages) {
        try {
            Path cachePath = getLanguagesCachePath();
            Map<String, Object> data = new LinkedHashMap<>();
            data.put("languages", languages);
            data.put("timestamp", System.currentTimeMillis() / 1000);
            Files.write(cachePath, mapToJson(data).getBytes(StandardCharsets.UTF_8));
        } catch (IOException e) {
            // Cache failure is non-fatal
        }
    }

    // ========================================================================
    // Language Detection
    // ========================================================================

    private static final Map<String, String> LANGUAGE_MAP = new HashMap<>();
    static {
        LANGUAGE_MAP.put("py", "python");
        LANGUAGE_MAP.put("js", "javascript");
        LANGUAGE_MAP.put("ts", "typescript");
        LANGUAGE_MAP.put("rb", "ruby");
        LANGUAGE_MAP.put("php", "php");
        LANGUAGE_MAP.put("pl", "perl");
        LANGUAGE_MAP.put("sh", "bash");
        LANGUAGE_MAP.put("r", "r");
        LANGUAGE_MAP.put("lua", "lua");
        LANGUAGE_MAP.put("go", "go");
        LANGUAGE_MAP.put("rs", "rust");
        LANGUAGE_MAP.put("c", "c");
        LANGUAGE_MAP.put("cpp", "cpp");
        LANGUAGE_MAP.put("cc", "cpp");
        LANGUAGE_MAP.put("cxx", "cpp");
        LANGUAGE_MAP.put("java", "java");
        LANGUAGE_MAP.put("kt", "kotlin");
        LANGUAGE_MAP.put("m", "objc");
        LANGUAGE_MAP.put("cs", "csharp");
        LANGUAGE_MAP.put("fs", "fsharp");
        LANGUAGE_MAP.put("hs", "haskell");
        LANGUAGE_MAP.put("ml", "ocaml");
        LANGUAGE_MAP.put("clj", "clojure");
        LANGUAGE_MAP.put("scm", "scheme");
        LANGUAGE_MAP.put("ss", "scheme");
        LANGUAGE_MAP.put("erl", "erlang");
        LANGUAGE_MAP.put("ex", "elixir");
        LANGUAGE_MAP.put("exs", "elixir");
        LANGUAGE_MAP.put("jl", "julia");
        LANGUAGE_MAP.put("d", "d");
        LANGUAGE_MAP.put("nim", "nim");
        LANGUAGE_MAP.put("zig", "zig");
        LANGUAGE_MAP.put("v", "v");
        LANGUAGE_MAP.put("cr", "crystal");
        LANGUAGE_MAP.put("dart", "dart");
        LANGUAGE_MAP.put("groovy", "groovy");
        LANGUAGE_MAP.put("f90", "fortran");
        LANGUAGE_MAP.put("f95", "fortran");
        LANGUAGE_MAP.put("lisp", "commonlisp");
        LANGUAGE_MAP.put("lsp", "commonlisp");
        LANGUAGE_MAP.put("cob", "cobol");
        LANGUAGE_MAP.put("tcl", "tcl");
        LANGUAGE_MAP.put("raku", "raku");
        LANGUAGE_MAP.put("pro", "prolog");
        LANGUAGE_MAP.put("p", "prolog");
        LANGUAGE_MAP.put("4th", "forth");
        LANGUAGE_MAP.put("forth", "forth");
        LANGUAGE_MAP.put("fth", "forth");
    }

    /**
     * Detect programming language from filename extension.
     *
     * @param filename Filename to detect language from (e.g., "script.py")
     * @return Language identifier (e.g., "python") or null if unknown
     */
    public static String detectLanguage(String filename) {
        if (filename == null || !filename.contains(".")) {
            return null;
        }
        String ext = filename.substring(filename.lastIndexOf('.') + 1).toLowerCase();
        return LANGUAGE_MAP.get(ext);
    }

    // ========================================================================
    // Public API Methods
    // ========================================================================

    /**
     * Execute code synchronously (blocks until completion).
     *
     * @param language Programming language (e.g., "python", "javascript", "go")
     * @param code Source code to execute
     * @param publicKey Optional API key (uses credentials resolution if null)
     * @param secretKey Optional API secret (uses credentials resolution if null)
     * @return Response map containing stdout, stderr, exit code, etc.
     * @throws IOException on network errors
     * @throws CredentialsException if credentials cannot be found
     * @throws ApiException if API returns an error
     */
    public static Map<String, Object> executeCode(
        String language,
        String code,
        String publicKey,
        String secretKey
    ) throws IOException {
        String[] creds = resolveCredentials(publicKey, secretKey);
        publicKey = creds[0];
        secretKey = creds[1];

        Map<String, Object> data = new LinkedHashMap<>();
        data.put("language", language);
        data.put("code", code);

        Map<String, Object> response = makeRequest("POST", "/execute", publicKey, secretKey, data);

        // If we got a job_id, poll until completion
        Object jobIdObj = response.get("job_id");
        Object statusObj = response.get("status");
        if (jobIdObj != null && statusObj != null) {
            String status = statusObj.toString();
            if ("pending".equals(status) || "running".equals(status)) {
                return waitForJob(jobIdObj.toString(), publicKey, secretKey, 0);
            }
        }

        return response;
    }

    /**
     * Execute code asynchronously (returns immediately with job ID).
     *
     * @param language Programming language (e.g., "python", "javascript")
     * @param code Source code to execute
     * @param publicKey Optional API key
     * @param secretKey Optional API secret
     * @return Job ID string
     * @throws IOException on network errors
     * @throws CredentialsException if credentials cannot be found
     * @throws ApiException if API returns an error
     */
    public static String executeAsync(
        String language,
        String code,
        String publicKey,
        String secretKey
    ) throws IOException {
        String[] creds = resolveCredentials(publicKey, secretKey);
        publicKey = creds[0];
        secretKey = creds[1];

        Map<String, Object> data = new LinkedHashMap<>();
        data.put("language", language);
        data.put("code", code);

        Map<String, Object> response = makeRequest("POST", "/execute", publicKey, secretKey, data);
        Object jobId = response.get("job_id");
        return jobId != null ? jobId.toString() : null;
    }

    /**
     * Get current status/result of a job (single poll, no waiting).
     *
     * @param jobId Job ID from executeAsync()
     * @param publicKey Optional API key
     * @param secretKey Optional API secret
     * @return Job response map
     * @throws IOException on network errors
     * @throws CredentialsException if credentials cannot be found
     * @throws ApiException if API returns an error
     */
    public static Map<String, Object> getJob(
        String jobId,
        String publicKey,
        String secretKey
    ) throws IOException {
        String[] creds = resolveCredentials(publicKey, secretKey);
        return makeRequest("GET", "/jobs/" + jobId, creds[0], creds[1], null);
    }

    /**
     * Wait for job completion with exponential backoff polling.
     *
     * <p>Polling delays (ms): [300, 450, 700, 900, 650, 1600, 2000, ...]
     *
     * @param jobId Job ID from executeAsync()
     * @param publicKey Optional API key
     * @param secretKey Optional API secret
     * @param timeoutMs Maximum time to wait (0 for no timeout)
     * @return Final job result when status is terminal (completed, failed, timeout, cancelled)
     * @throws IOException on network errors
     * @throws CredentialsException if credentials cannot be found
     * @throws ApiException if API returns an error
     * @throws RuntimeException if timeout exceeded
     */
    public static Map<String, Object> waitForJob(
        String jobId,
        String publicKey,
        String secretKey,
        long timeoutMs
    ) throws IOException {
        String[] creds = resolveCredentials(publicKey, secretKey);
        publicKey = creds[0];
        secretKey = creds[1];

        long startTime = System.currentTimeMillis();
        int pollCount = 0;

        while (true) {
            // Sleep before polling
            int delayIdx = Math.min(pollCount, POLL_DELAYS_MS.length - 1);
            try {
                Thread.sleep(POLL_DELAYS_MS[delayIdx]);
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                throw new RuntimeException("Wait interrupted", e);
            }
            pollCount++;

            // Check timeout
            if (timeoutMs > 0 && System.currentTimeMillis() - startTime > timeoutMs) {
                throw new RuntimeException("Timeout waiting for job " + jobId);
            }

            Map<String, Object> response = getJob(jobId, publicKey, secretKey);
            Object statusObj = response.get("status");
            if (statusObj != null) {
                String status = statusObj.toString();
                if ("completed".equals(status) || "failed".equals(status) ||
                    "timeout".equals(status) || "cancelled".equals(status)) {
                    return response;
                }
            }
            // Still running, continue polling
        }
    }

    /**
     * Cancel a running job.
     *
     * @param jobId Job ID to cancel
     * @param publicKey Optional API key
     * @param secretKey Optional API secret
     * @return Response map with cancellation confirmation
     * @throws IOException on network errors
     * @throws CredentialsException if credentials cannot be found
     * @throws ApiException if API returns an error
     */
    public static Map<String, Object> cancelJob(
        String jobId,
        String publicKey,
        String secretKey
    ) throws IOException {
        String[] creds = resolveCredentials(publicKey, secretKey);
        return makeRequest("DELETE", "/jobs/" + jobId, creds[0], creds[1], null);
    }

    /**
     * List all jobs for the authenticated account.
     *
     * @param publicKey Optional API key
     * @param secretKey Optional API secret
     * @return List of job maps
     * @throws IOException on network errors
     * @throws CredentialsException if credentials cannot be found
     * @throws ApiException if API returns an error
     */
    @SuppressWarnings("unchecked")
    public static List<Map<String, Object>> listJobs(
        String publicKey,
        String secretKey
    ) throws IOException {
        String[] creds = resolveCredentials(publicKey, secretKey);
        Map<String, Object> response = makeRequest("GET", "/jobs", creds[0], creds[1], null);
        Object jobs = response.get("jobs");
        if (jobs instanceof List) {
            List<Map<String, Object>> result = new ArrayList<>();
            for (Object item : (List<?>) jobs) {
                if (item instanceof Map) {
                    result.add((Map<String, Object>) item);
                }
            }
            return result;
        }
        return new ArrayList<>();
    }

    /**
     * Get list of supported programming languages.
     *
     * <p>Results are cached for 1 hour in ~/.unsandbox/languages.json
     *
     * @param publicKey Optional API key
     * @param secretKey Optional API secret
     * @return List of language identifiers (e.g., ["python", "javascript", "go", ...])
     * @throws IOException on network errors
     * @throws CredentialsException if credentials cannot be found
     * @throws ApiException if API returns an error
     */
    public static List<String> getLanguages(
        String publicKey,
        String secretKey
    ) throws IOException {
        // Try cache first
        List<String> cached = loadLanguagesCache();
        if (cached != null) {
            return cached;
        }

        String[] creds = resolveCredentials(publicKey, secretKey);
        Map<String, Object> response = makeRequest("GET", "/languages", creds[0], creds[1], null);

        Object languages = response.get("languages");
        List<String> result = new ArrayList<>();
        if (languages instanceof List) {
            for (Object item : (List<?>) languages) {
                if (item instanceof String) {
                    result.add((String) item);
                }
            }
        }

        // Cache the result
        saveLanguagesCache(result);
        return result;
    }

    /**
     * Create a snapshot of a session.
     *
     * @param sessionId Session ID to snapshot
     * @param publicKey Optional API key
     * @param secretKey Optional API secret
     * @param name Optional snapshot name
     * @param ephemeral If true, snapshot is ephemeral (hot snapshot)
     * @return Snapshot ID
     * @throws IOException on network errors
     * @throws CredentialsException if credentials cannot be found
     * @throws ApiException if API returns an error
     */
    public static String sessionSnapshot(
        String sessionId,
        String publicKey,
        String secretKey,
        String name,
        boolean ephemeral
    ) throws IOException {
        String[] creds = resolveCredentials(publicKey, secretKey);

        Map<String, Object> data = new LinkedHashMap<>();
        data.put("session_id", sessionId);
        data.put("hot", ephemeral);
        if (name != null && !name.isEmpty()) {
            data.put("name", name);
        }

        Map<String, Object> response = makeRequest("POST", "/snapshots", creds[0], creds[1], data);
        Object snapshotId = response.get("snapshot_id");
        return snapshotId != null ? snapshotId.toString() : null;
    }

    /**
     * Create a snapshot of a service.
     *
     * @param serviceId Service ID to snapshot
     * @param publicKey Optional API key
     * @param secretKey Optional API secret
     * @param name Optional snapshot name
     * @return Snapshot ID
     * @throws IOException on network errors
     * @throws CredentialsException if credentials cannot be found
     * @throws ApiException if API returns an error
     */
    public static String serviceSnapshot(
        String serviceId,
        String publicKey,
        String secretKey,
        String name
    ) throws IOException {
        String[] creds = resolveCredentials(publicKey, secretKey);

        Map<String, Object> data = new LinkedHashMap<>();
        data.put("service_id", serviceId);
        data.put("hot", false);
        if (name != null && !name.isEmpty()) {
            data.put("name", name);
        }

        Map<String, Object> response = makeRequest("POST", "/snapshots", creds[0], creds[1], data);
        Object snapshotId = response.get("snapshot_id");
        return snapshotId != null ? snapshotId.toString() : null;
    }

    /**
     * List all snapshots.
     *
     * @param publicKey Optional API key
     * @param secretKey Optional API secret
     * @return List of snapshot maps
     * @throws IOException on network errors
     * @throws CredentialsException if credentials cannot be found
     * @throws ApiException if API returns an error
     */
    @SuppressWarnings("unchecked")
    public static List<Map<String, Object>> listSnapshots(
        String publicKey,
        String secretKey
    ) throws IOException {
        String[] creds = resolveCredentials(publicKey, secretKey);
        Map<String, Object> response = makeRequest("GET", "/snapshots", creds[0], creds[1], null);
        Object snapshots = response.get("snapshots");
        if (snapshots instanceof List) {
            List<Map<String, Object>> result = new ArrayList<>();
            for (Object item : (List<?>) snapshots) {
                if (item instanceof Map) {
                    result.add((Map<String, Object>) item);
                }
            }
            return result;
        }
        return new ArrayList<>();
    }

    /**
     * Restore a snapshot.
     *
     * @param snapshotId Snapshot ID to restore
     * @param publicKey Optional API key
     * @param secretKey Optional API secret
     * @return Response map with restored resource info
     * @throws IOException on network errors
     * @throws CredentialsException if credentials cannot be found
     * @throws ApiException if API returns an error
     */
    public static Map<String, Object> restoreSnapshot(
        String snapshotId,
        String publicKey,
        String secretKey
    ) throws IOException {
        String[] creds = resolveCredentials(publicKey, secretKey);
        return makeRequest("POST", "/snapshots/" + snapshotId + "/restore", creds[0], creds[1], new LinkedHashMap<>());
    }

    /**
     * Delete a snapshot.
     *
     * @param snapshotId Snapshot ID to delete
     * @param publicKey Optional API key
     * @param secretKey Optional API secret
     * @return Response map with deletion confirmation
     * @throws IOException on network errors
     * @throws CredentialsException if credentials cannot be found
     * @throws ApiException if API returns an error
     */
    public static Map<String, Object> deleteSnapshot(
        String snapshotId,
        String publicKey,
        String secretKey
    ) throws IOException {
        String[] creds = resolveCredentials(publicKey, secretKey);
        return makeRequest("DELETE", "/snapshots/" + snapshotId, creds[0], creds[1], null);
    }

    // ========================================================================
    // Session API Methods
    // ========================================================================

    /**
     * List all active sessions for the authenticated account.
     *
     * @param publicKey Optional API key
     * @param secretKey Optional API secret
     * @return List of session maps containing id, container_name, shell, status, remaining_ttl
     * @throws IOException on network errors
     * @throws CredentialsException if credentials cannot be found
     * @throws ApiException if API returns an error
     */
    @SuppressWarnings("unchecked")
    public static List<Map<String, Object>> listSessions(
        String publicKey,
        String secretKey
    ) throws IOException {
        String[] creds = resolveCredentials(publicKey, secretKey);
        Map<String, Object> response = makeRequest("GET", "/sessions", creds[0], creds[1], null);
        Object sessions = response.get("sessions");
        if (sessions instanceof List) {
            List<Map<String, Object>> result = new ArrayList<>();
            for (Object item : (List<?>) sessions) {
                if (item instanceof Map) {
                    result.add((Map<String, Object>) item);
                }
            }
            return result;
        }
        return new ArrayList<>();
    }

    /**
     * Get details of a specific session.
     *
     * @param sessionId Session ID to retrieve
     * @param publicKey Optional API key
     * @param secretKey Optional API secret
     * @return Session details map
     * @throws IOException on network errors
     * @throws CredentialsException if credentials cannot be found
     * @throws ApiException if API returns an error
     */
    public static Map<String, Object> getSession(
        String sessionId,
        String publicKey,
        String secretKey
    ) throws IOException {
        String[] creds = resolveCredentials(publicKey, secretKey);
        return makeRequest("GET", "/sessions/" + sessionId, creds[0], creds[1], null);
    }

    /**
     * Create a new interactive session.
     *
     * @param language Programming language/shell for the session (e.g., "bash", "python3")
     * @param publicKey Optional API key
     * @param secretKey Optional API secret
     * @param opts Optional parameters: network_mode, ttl, shell, multiplexer, vcpu
     * @return Response map containing session_id, container_name
     * @throws IOException on network errors
     * @throws CredentialsException if credentials cannot be found
     * @throws ApiException if API returns an error
     */
    public static Map<String, Object> createSession(
        String language,
        String publicKey,
        String secretKey,
        Map<String, Object> opts
    ) throws IOException {
        String[] creds = resolveCredentials(publicKey, secretKey);

        Map<String, Object> data = new LinkedHashMap<>();
        data.put("network_mode", "zerotrust");
        data.put("ttl", 3600);
        if (language != null && !language.isEmpty()) {
            data.put("shell", language);
        }
        if (opts != null) {
            data.putAll(opts);
        }

        return makeRequest("POST", "/sessions", creds[0], creds[1], data);
    }

    /**
     * Delete (terminate) a session.
     *
     * @param sessionId Session ID to terminate
     * @param publicKey Optional API key
     * @param secretKey Optional API secret
     * @return Response map with termination confirmation
     * @throws IOException on network errors
     * @throws CredentialsException if credentials cannot be found
     * @throws ApiException if API returns an error
     */
    public static Map<String, Object> deleteSession(
        String sessionId,
        String publicKey,
        String secretKey
    ) throws IOException {
        String[] creds = resolveCredentials(publicKey, secretKey);
        return makeRequest("DELETE", "/sessions/" + sessionId, creds[0], creds[1], null);
    }

    /**
     * Freeze a session (pause execution, reduce resource consumption).
     *
     * @param sessionId Session ID to freeze
     * @param publicKey Optional API key
     * @param secretKey Optional API secret
     * @return Response map with freeze confirmation
     * @throws IOException on network errors
     * @throws CredentialsException if credentials cannot be found
     * @throws ApiException if API returns an error
     */
    public static Map<String, Object> freezeSession(
        String sessionId,
        String publicKey,
        String secretKey
    ) throws IOException {
        String[] creds = resolveCredentials(publicKey, secretKey);
        return makeRequest("POST", "/sessions/" + sessionId + "/freeze", creds[0], creds[1], new LinkedHashMap<>());
    }

    /**
     * Unfreeze a session (resume execution).
     *
     * @param sessionId Session ID to unfreeze
     * @param publicKey Optional API key
     * @param secretKey Optional API secret
     * @return Response map with unfreeze confirmation
     * @throws IOException on network errors
     * @throws CredentialsException if credentials cannot be found
     * @throws ApiException if API returns an error
     */
    public static Map<String, Object> unfreezeSession(
        String sessionId,
        String publicKey,
        String secretKey
    ) throws IOException {
        String[] creds = resolveCredentials(publicKey, secretKey);
        return makeRequest("POST", "/sessions/" + sessionId + "/unfreeze", creds[0], creds[1], new LinkedHashMap<>());
    }

    /**
     * Boost a session's resources (increase vCPU and memory).
     *
     * @param sessionId Session ID to boost
     * @param publicKey Optional API key
     * @param secretKey Optional API secret
     * @return Response map with boost confirmation
     * @throws IOException on network errors
     * @throws CredentialsException if credentials cannot be found
     * @throws ApiException if API returns an error
     */
    public static Map<String, Object> boostSession(
        String sessionId,
        String publicKey,
        String secretKey
    ) throws IOException {
        String[] creds = resolveCredentials(publicKey, secretKey);
        Map<String, Object> data = new LinkedHashMap<>();
        data.put("vcpu", 2);
        return makeRequest("POST", "/sessions/" + sessionId + "/boost", creds[0], creds[1], data);
    }

    /**
     * Remove boost from a session (return to base resources).
     *
     * @param sessionId Session ID to unboost
     * @param publicKey Optional API key
     * @param secretKey Optional API secret
     * @return Response map with unboost confirmation
     * @throws IOException on network errors
     * @throws CredentialsException if credentials cannot be found
     * @throws ApiException if API returns an error
     */
    public static Map<String, Object> unboostSession(
        String sessionId,
        String publicKey,
        String secretKey
    ) throws IOException {
        String[] creds = resolveCredentials(publicKey, secretKey);
        return makeRequest("POST", "/sessions/" + sessionId + "/unboost", creds[0], creds[1], new LinkedHashMap<>());
    }

    /**
     * Execute a shell command in an existing session.
     *
     * @param sessionId Session ID to execute command in
     * @param command Shell command to execute
     * @param publicKey Optional API key
     * @param secretKey Optional API secret
     * @return Response map with command output
     * @throws IOException on network errors
     * @throws CredentialsException if credentials cannot be found
     * @throws ApiException if API returns an error
     */
    public static Map<String, Object> shellSession(
        String sessionId,
        String command,
        String publicKey,
        String secretKey
    ) throws IOException {
        String[] creds = resolveCredentials(publicKey, secretKey);
        Map<String, Object> data = new LinkedHashMap<>();
        data.put("command", command);
        return makeRequest("POST", "/sessions/" + sessionId + "/shell", creds[0], creds[1], data);
    }

    // ========================================================================
    // Service API Methods
    // ========================================================================

    /**
     * List all services for the authenticated account.
     *
     * @param publicKey Optional API key
     * @param secretKey Optional API secret
     * @return List of service maps containing id, name, state, ports, disk_used
     * @throws IOException on network errors
     * @throws CredentialsException if credentials cannot be found
     * @throws ApiException if API returns an error
     */
    @SuppressWarnings("unchecked")
    public static List<Map<String, Object>> listServices(
        String publicKey,
        String secretKey
    ) throws IOException {
        String[] creds = resolveCredentials(publicKey, secretKey);
        Map<String, Object> response = makeRequest("GET", "/services", creds[0], creds[1], null);
        Object services = response.get("services");
        if (services instanceof List) {
            List<Map<String, Object>> result = new ArrayList<>();
            for (Object item : (List<?>) services) {
                if (item instanceof Map) {
                    result.add((Map<String, Object>) item);
                }
            }
            return result;
        }
        return new ArrayList<>();
    }

    /**
     * Create a new persistent service.
     *
     * @param name Service name
     * @param ports Comma-separated list of ports to expose (e.g., "80,443")
     * @param bootstrap Bootstrap script or URL to run on service creation
     * @param publicKey Optional API key
     * @param secretKey Optional API secret
     * @return Response map containing service_id
     * @throws IOException on network errors
     * @throws CredentialsException if credentials cannot be found
     * @throws ApiException if API returns an error
     */
    public static Map<String, Object> createService(
        String name,
        String ports,
        String bootstrap,
        String publicKey,
        String secretKey
    ) throws IOException {
        String[] creds = resolveCredentials(publicKey, secretKey);

        Map<String, Object> data = new LinkedHashMap<>();
        data.put("name", name);
        if (ports != null && !ports.isEmpty()) {
            // Parse ports string into list
            List<Integer> portList = new ArrayList<>();
            for (String p : ports.split(",")) {
                try {
                    portList.add(Integer.parseInt(p.trim()));
                } catch (NumberFormatException e) {
                    // Skip invalid port
                }
            }
            data.put("ports", portList);
        }
        if (bootstrap != null && !bootstrap.isEmpty()) {
            if (bootstrap.startsWith("http://") || bootstrap.startsWith("https://")) {
                data.put("bootstrap_url", bootstrap);
            } else {
                data.put("bootstrap", bootstrap);
            }
        }

        return makeRequest("POST", "/services", creds[0], creds[1], data);
    }

    /**
     * Get details of a specific service.
     *
     * @param serviceId Service ID to retrieve
     * @param publicKey Optional API key
     * @param secretKey Optional API secret
     * @return Service details map
     * @throws IOException on network errors
     * @throws CredentialsException if credentials cannot be found
     * @throws ApiException if API returns an error
     */
    public static Map<String, Object> getService(
        String serviceId,
        String publicKey,
        String secretKey
    ) throws IOException {
        String[] creds = resolveCredentials(publicKey, secretKey);
        return makeRequest("GET", "/services/" + serviceId, creds[0], creds[1], null);
    }

    /**
     * Update a service's configuration.
     *
     * @param serviceId Service ID to update
     * @param opts Update options (e.g., vcpu for resizing)
     * @param publicKey Optional API key
     * @param secretKey Optional API secret
     * @return Response map with update confirmation
     * @throws IOException on network errors
     * @throws CredentialsException if credentials cannot be found
     * @throws ApiException if API returns an error
     */
    public static Map<String, Object> updateService(
        String serviceId,
        Map<String, Object> opts,
        String publicKey,
        String secretKey
    ) throws IOException {
        String[] creds = resolveCredentials(publicKey, secretKey);
        return makeRequestWithMethod("PATCH", "/services/" + serviceId, creds[0], creds[1], opts);
    }

    /**
     * Delete (destroy) a service.
     *
     * @param serviceId Service ID to destroy
     * @param publicKey Optional API key
     * @param secretKey Optional API secret
     * @return Response map with deletion confirmation
     * @throws IOException on network errors
     * @throws CredentialsException if credentials cannot be found
     * @throws ApiException if API returns an error
     */
    public static Map<String, Object> deleteService(
        String serviceId,
        String publicKey,
        String secretKey
    ) throws IOException {
        String[] creds = resolveCredentials(publicKey, secretKey);
        return makeRequest("DELETE", "/services/" + serviceId, creds[0], creds[1], null);
    }

    /**
     * Freeze a service (pause execution, reduce resource consumption).
     *
     * @param serviceId Service ID to freeze
     * @param publicKey Optional API key
     * @param secretKey Optional API secret
     * @return Response map with freeze confirmation
     * @throws IOException on network errors
     * @throws CredentialsException if credentials cannot be found
     * @throws ApiException if API returns an error
     */
    public static Map<String, Object> freezeService(
        String serviceId,
        String publicKey,
        String secretKey
    ) throws IOException {
        String[] creds = resolveCredentials(publicKey, secretKey);
        return makeRequest("POST", "/services/" + serviceId + "/freeze", creds[0], creds[1], new LinkedHashMap<>());
    }

    /**
     * Unfreeze a service (resume execution).
     *
     * @param serviceId Service ID to unfreeze
     * @param publicKey Optional API key
     * @param secretKey Optional API secret
     * @return Response map with unfreeze confirmation
     * @throws IOException on network errors
     * @throws CredentialsException if credentials cannot be found
     * @throws ApiException if API returns an error
     */
    public static Map<String, Object> unfreezeService(
        String serviceId,
        String publicKey,
        String secretKey
    ) throws IOException {
        String[] creds = resolveCredentials(publicKey, secretKey);
        return makeRequest("POST", "/services/" + serviceId + "/unfreeze", creds[0], creds[1], new LinkedHashMap<>());
    }

    /**
     * Lock a service (prevent modifications and termination).
     *
     * @param serviceId Service ID to lock
     * @param publicKey Optional API key
     * @param secretKey Optional API secret
     * @return Response map with lock confirmation
     * @throws IOException on network errors
     * @throws CredentialsException if credentials cannot be found
     * @throws ApiException if API returns an error
     */
    public static Map<String, Object> lockService(
        String serviceId,
        String publicKey,
        String secretKey
    ) throws IOException {
        String[] creds = resolveCredentials(publicKey, secretKey);
        return makeRequest("POST", "/services/" + serviceId + "/lock", creds[0], creds[1], new LinkedHashMap<>());
    }

    /**
     * Unlock a service (allow modifications and termination).
     *
     * @param serviceId Service ID to unlock
     * @param publicKey Optional API key
     * @param secretKey Optional API secret
     * @return Response map with unlock confirmation
     * @throws IOException on network errors
     * @throws CredentialsException if credentials cannot be found
     * @throws ApiException if API returns an error
     */
    public static Map<String, Object> unlockService(
        String serviceId,
        String publicKey,
        String secretKey
    ) throws IOException {
        String[] creds = resolveCredentials(publicKey, secretKey);
        return makeRequest("POST", "/services/" + serviceId + "/unlock", creds[0], creds[1], new LinkedHashMap<>());
    }

    /**
     * Get bootstrap logs for a service.
     *
     * @param serviceId Service ID to get logs for
     * @param all If true, return all logs; if false, return recent logs only
     * @param publicKey Optional API key
     * @param secretKey Optional API secret
     * @return Response map containing logs
     * @throws IOException on network errors
     * @throws CredentialsException if credentials cannot be found
     * @throws ApiException if API returns an error
     */
    public static Map<String, Object> getServiceLogs(
        String serviceId,
        boolean all,
        String publicKey,
        String secretKey
    ) throws IOException {
        String[] creds = resolveCredentials(publicKey, secretKey);
        String path = "/services/" + serviceId + "/logs";
        if (all) {
            path += "?all=true";
        }
        return makeRequest("GET", path, creds[0], creds[1], null);
    }

    /**
     * Get environment vault status for a service.
     *
     * @param serviceId Service ID to get env status for
     * @param publicKey Optional API key
     * @param secretKey Optional API secret
     * @return Response map containing has_vault, count, updated_at
     * @throws IOException on network errors
     * @throws CredentialsException if credentials cannot be found
     * @throws ApiException if API returns an error
     */
    public static Map<String, Object> getServiceEnv(
        String serviceId,
        String publicKey,
        String secretKey
    ) throws IOException {
        String[] creds = resolveCredentials(publicKey, secretKey);
        return makeRequest("GET", "/services/" + serviceId + "/env", creds[0], creds[1], null);
    }

    /**
     * Set environment vault for a service.
     *
     * @param serviceId Service ID to set env for
     * @param env Environment variables map (KEY=VALUE pairs)
     * @param publicKey Optional API key
     * @param secretKey Optional API secret
     * @return Response map with set confirmation
     * @throws IOException on network errors
     * @throws CredentialsException if credentials cannot be found
     * @throws ApiException if API returns an error
     */
    public static Map<String, Object> setServiceEnv(
        String serviceId,
        Map<String, String> env,
        String publicKey,
        String secretKey
    ) throws IOException {
        String[] creds = resolveCredentials(publicKey, secretKey);
        Map<String, Object> data = new LinkedHashMap<>();
        data.put("env", env);
        return makeRequest("POST", "/services/" + serviceId + "/env", creds[0], creds[1], data);
    }

    /**
     * Delete environment variables from a service's vault.
     *
     * @param serviceId Service ID to delete env from
     * @param keys List of keys to delete (null to delete entire vault)
     * @param publicKey Optional API key
     * @param secretKey Optional API secret
     * @return Response map with deletion confirmation
     * @throws IOException on network errors
     * @throws CredentialsException if credentials cannot be found
     * @throws ApiException if API returns an error
     */
    public static Map<String, Object> deleteServiceEnv(
        String serviceId,
        List<String> keys,
        String publicKey,
        String secretKey
    ) throws IOException {
        String[] creds = resolveCredentials(publicKey, secretKey);
        String path = "/services/" + serviceId + "/env";
        if (keys != null && !keys.isEmpty()) {
            // URL encode keys parameter
            path += "?keys=" + String.join(",", keys);
        }
        return makeRequest("DELETE", path, creds[0], creds[1], null);
    }

    /**
     * Export environment vault for a service (returns decrypted values).
     *
     * @param serviceId Service ID to export env from
     * @param publicKey Optional API key
     * @param secretKey Optional API secret
     * @return Response map containing exported environment variables
     * @throws IOException on network errors
     * @throws CredentialsException if credentials cannot be found
     * @throws ApiException if API returns an error
     */
    public static Map<String, Object> exportServiceEnv(
        String serviceId,
        String publicKey,
        String secretKey
    ) throws IOException {
        String[] creds = resolveCredentials(publicKey, secretKey);
        return makeRequest("POST", "/services/" + serviceId + "/env/export", creds[0], creds[1], new LinkedHashMap<>());
    }

    /**
     * Redeploy a service (re-run bootstrap script).
     *
     * @param serviceId Service ID to redeploy
     * @param publicKey Optional API key
     * @param secretKey Optional API secret
     * @return Response map with redeploy confirmation
     * @throws IOException on network errors
     * @throws CredentialsException if credentials cannot be found
     * @throws ApiException if API returns an error
     */
    public static Map<String, Object> redeployService(
        String serviceId,
        String publicKey,
        String secretKey
    ) throws IOException {
        String[] creds = resolveCredentials(publicKey, secretKey);
        return makeRequest("POST", "/services/" + serviceId + "/redeploy", creds[0], creds[1], new LinkedHashMap<>());
    }

    /**
     * Execute a command in a service container.
     *
     * @param serviceId Service ID to execute command in
     * @param command Command to execute
     * @param publicKey Optional API key
     * @param secretKey Optional API secret
     * @return Response map containing stdout, stderr, exit_code
     * @throws IOException on network errors
     * @throws CredentialsException if credentials cannot be found
     * @throws ApiException if API returns an error
     */
    public static Map<String, Object> executeInService(
        String serviceId,
        String command,
        String publicKey,
        String secretKey
    ) throws IOException {
        String[] creds = resolveCredentials(publicKey, secretKey);
        Map<String, Object> data = new LinkedHashMap<>();
        data.put("command", command);
        return makeRequest("POST", "/services/" + serviceId + "/execute", creds[0], creds[1], data);
    }

    // ========================================================================
    // Additional Snapshot API Methods
    // ========================================================================

    /**
     * Lock a snapshot (prevent deletion).
     *
     * @param snapshotId Snapshot ID to lock
     * @param publicKey Optional API key
     * @param secretKey Optional API secret
     * @return Response map with lock confirmation
     * @throws IOException on network errors
     * @throws CredentialsException if credentials cannot be found
     * @throws ApiException if API returns an error
     */
    public static Map<String, Object> lockSnapshot(
        String snapshotId,
        String publicKey,
        String secretKey
    ) throws IOException {
        String[] creds = resolveCredentials(publicKey, secretKey);
        return makeRequest("POST", "/snapshots/" + snapshotId + "/lock", creds[0], creds[1], new LinkedHashMap<>());
    }

    /**
     * Unlock a snapshot (allow deletion).
     *
     * @param snapshotId Snapshot ID to unlock
     * @param publicKey Optional API key
     * @param secretKey Optional API secret
     * @return Response map with unlock confirmation
     * @throws IOException on network errors
     * @throws CredentialsException if credentials cannot be found
     * @throws ApiException if API returns an error
     */
    public static Map<String, Object> unlockSnapshot(
        String snapshotId,
        String publicKey,
        String secretKey
    ) throws IOException {
        String[] creds = resolveCredentials(publicKey, secretKey);
        return makeRequest("POST", "/snapshots/" + snapshotId + "/unlock", creds[0], creds[1], new LinkedHashMap<>());
    }

    /**
     * Clone a snapshot to create a new snapshot with a different name.
     *
     * @param snapshotId Snapshot ID to clone
     * @param name Name for the cloned snapshot
     * @param publicKey Optional API key
     * @param secretKey Optional API secret
     * @return Response map containing new snapshot_id
     * @throws IOException on network errors
     * @throws CredentialsException if credentials cannot be found
     * @throws ApiException if API returns an error
     */
    public static Map<String, Object> cloneSnapshot(
        String snapshotId,
        String name,
        String publicKey,
        String secretKey
    ) throws IOException {
        String[] creds = resolveCredentials(publicKey, secretKey);
        Map<String, Object> data = new LinkedHashMap<>();
        if (name != null && !name.isEmpty()) {
            data.put("name", name);
        }
        return makeRequest("POST", "/snapshots/" + snapshotId + "/clone", creds[0], creds[1], data);
    }

    // ========================================================================
    // Key Validation API
    // ========================================================================

    /**
     * Validate API key credentials.
     *
     * @param publicKey API public key to validate
     * @param secretKey API secret key to validate
     * @return Response map with validation result (valid, tier, etc.)
     * @throws IOException on network errors
     * @throws ApiException if API returns an error (including invalid credentials)
     */
    public static Map<String, Object> validateKeys(
        String publicKey,
        String secretKey
    ) throws IOException {
        String[] creds = resolveCredentials(publicKey, secretKey);
        // Note: validateKeys uses POST to /keys/validate
        return makeRequest("POST", "/keys/validate", creds[0], creds[1], new LinkedHashMap<>());
    }

    // ========================================================================
    // HTTP Request Helpers
    // ========================================================================

    /**
     * Make an HTTP request with a specified method (supports PATCH).
     */
    private static Map<String, Object> makeRequestWithMethod(
        String method,
        String path,
        String publicKey,
        String secretKey,
        Map<String, Object> data
    ) throws IOException {
        String url = API_BASE + path;
        long timestamp = System.currentTimeMillis() / 1000;
        String body = (data != null) ? mapToJson(data) : "";

        String signature = signRequest(secretKey, timestamp, method, path, data != null ? body : null);

        HttpURLConnection conn = (HttpURLConnection) new URL(url).openConnection();
        conn.setRequestMethod(method);
        conn.setConnectTimeout(DEFAULT_TIMEOUT_MS);
        conn.setReadTimeout(DEFAULT_TIMEOUT_MS);

        conn.setRequestProperty("Authorization", "Bearer " + publicKey);
        conn.setRequestProperty("X-Timestamp", String.valueOf(timestamp));
        conn.setRequestProperty("X-Signature", signature);
        conn.setRequestProperty("Content-Type", "application/json");

        if (data != null) {
            conn.setDoOutput(true);
            try (OutputStream os = conn.getOutputStream()) {
                os.write(body.getBytes(StandardCharsets.UTF_8));
            }
        }

        int responseCode = conn.getResponseCode();
        String responseBody;

        InputStream inputStream = (responseCode >= 200 && responseCode < 300)
            ? conn.getInputStream()
            : conn.getErrorStream();

        if (inputStream == null) {
            responseBody = "";
        } else {
            try (BufferedReader reader = new BufferedReader(new InputStreamReader(inputStream, StandardCharsets.UTF_8))) {
                StringBuilder sb = new StringBuilder();
                String line;
                while ((line = reader.readLine()) != null) {
                    sb.append(line);
                }
                responseBody = sb.toString();
            }
        }

        if (responseCode < 200 || responseCode >= 300) {
            throw new ApiException(
                "API request failed with status " + responseCode,
                responseCode,
                responseBody
            );
        }

        return parseJson(responseBody);
    }
}
