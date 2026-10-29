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
 * Authentication Priority (5-tier):
 *     1. Method arguments (publicKey, secretKey)
 *     2. --account N flag / accountIndex >= 0 (load row N from accounts.csv)
 *     3. Environment variables (UNSANDBOX_PUBLIC_KEY, UNSANDBOX_SECRET_KEY)
 *     4. Config file (~/.unsandbox/accounts.csv, line 0 by default)
 *     5. Local directory (./accounts.csv, line 0 by default)
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

    private static final String API_BASE = resolveApiBase();

    private static String resolveApiBase() {
        String base = System.getenv("UNSANDBOX_API_URL");
        if (base == null || base.isEmpty()) {
            base = "https://api.unsandbox.com";
        }
        while (base.endsWith("/")) {
            base = base.substring(0, base.length() - 1);
        }
        return base;
    }
    /** Portal base (/logs, /keys/extend): UNSANDBOX_PORTAL_URL, default https://unsandbox.com. */
    private static final String PORTAL_BASE = resolvePortalBase();

    private static String resolvePortalBase() {
        String base = System.getenv("UNSANDBOX_PORTAL_URL");
        if (base == null || base.isEmpty()) {
            base = "https://unsandbox.com";
        }
        while (base.endsWith("/")) {
            base = base.substring(0, base.length() - 1);
        }
        return base;
    }
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

    /**
     * Exception thrown when a 428 sudo challenge is received.
     * This indicates a destructive operation requires OTP confirmation.
     */
    public static class SudoChallengeException extends RuntimeException {
        private final String challengeId;
        private final String responseBody;

        public SudoChallengeException(String challengeId, String responseBody) {
            super("Sudo challenge required");
            this.challengeId = challengeId;
            this.responseBody = responseBody;
        }

        public String getChallengeId() {
            return challengeId;
        }

        public String getResponseBody() {
            return responseBody;
        }
    }

    // ========================================================================
    // Credential Resolution
    // ========================================================================

    private static Path getUnsandboxDir() {
        String home = System.getenv("HOME");
        if (home == null || home.isEmpty()) {
            home = System.getProperty("user.home");
        }
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
        return resolveCredentials(publicKey, secretKey, -1);
    }

    private static String[] resolveCredentials(String publicKey, String secretKey, int accountIndex) {
        // Tier 1: Method arguments
        if (publicKey != null && !publicKey.isEmpty() && secretKey != null && !secretKey.isEmpty()) {
            return new String[]{publicKey, secretKey};
        }

        // Tier 2: Explicit account index (e.g. --account N from CLI)
        if (accountIndex >= 0) {
            Path unsandboxDir = getUnsandboxDir();
            String[] creds = loadCredentialsFromCsv(unsandboxDir.resolve("accounts.csv"), accountIndex);
            if (creds != null) {
                return creds;
            }
            creds = loadCredentialsFromCsv(Paths.get("accounts.csv"), accountIndex);
            if (creds != null) {
                return creds;
            }
            throw new CredentialsException(
                "No credentials found at account index " + accountIndex + " in accounts.csv"
            );
        }

        // Tier 3: Environment variables
        String envPk = System.getenv("UNSANDBOX_PUBLIC_KEY");
        String envSk = System.getenv("UNSANDBOX_SECRET_KEY");
        if (envPk != null && !envPk.isEmpty() && envSk != null && !envSk.isEmpty()) {
            return new String[]{envPk, envSk};
        }

        // Determine account index from env (default 0)
        int csvIndex = 0;
        String accountEnv = System.getenv("UNSANDBOX_ACCOUNT");
        if (accountEnv != null && !accountEnv.isEmpty()) {
            try {
                csvIndex = Integer.parseInt(accountEnv);
            } catch (NumberFormatException e) {
                // Use default
            }
        }

        // Tier 4: ~/.unsandbox/accounts.csv
        Path unsandboxDir = getUnsandboxDir();
        String[] creds = loadCredentialsFromCsv(unsandboxDir.resolve("accounts.csv"), csvIndex);
        if (creds != null) {
            return creds;
        }

        // Tier 5: ./accounts.csv
        creds = loadCredentialsFromCsv(Paths.get("accounts.csv"), csvIndex);
        if (creds != null) {
            return creds;
        }

        throw new CredentialsException(
            "No credentials found. Please provide via:\n" +
            "  1. Method arguments (publicKey, secretKey)\n" +
            "  2. --account N flag (load row N from accounts.csv)\n" +
            "  3. Environment variables (UNSANDBOX_PUBLIC_KEY, UNSANDBOX_SECRET_KEY)\n" +
            "  4. ~/.unsandbox/accounts.csv\n" +
            "  5. ./accounts.csv"
        );
    }

    // ========================================================================
    // HMAC-SHA256 Signing
    // ========================================================================

    private static String signRequest(String secretKey, long timestamp, String method, String path, String body) {
        try {
            String bodyStr = (body != null) ? body : "";
            // Server verifies against the request path without its query string (conn.request_path)
            int q = path.indexOf('?');
            String signPath = (q >= 0) ? path.substring(0, q) : path;
            String message = timestamp + ":" + method + ":" + signPath + ":" + bodyStr;

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
        // A GET carries no body, so it signs none; every other method signs what it sends
        boolean sendsBody = data != null && !"GET".equals(method);
        String body = sendsBody ? mapToJson(data) : "";

        String signature = signRequest(secretKey, timestamp, method, path, sendsBody ? body : null);

        HttpURLConnection conn = (HttpURLConnection) new URL(url).openConnection();
        conn.setRequestMethod(method);
        conn.setConnectTimeout(DEFAULT_TIMEOUT_MS);
        conn.setReadTimeout(DEFAULT_TIMEOUT_MS);

        conn.setRequestProperty("Authorization", "Bearer " + publicKey);
        conn.setRequestProperty("X-Timestamp", String.valueOf(timestamp));
        conn.setRequestProperty("X-Signature", signature);
        conn.setRequestProperty("Content-Type", "application/json");

        // Send every signed body (POST, PUT, DELETE): signed bytes == sent bytes
        if (sendsBody) {
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

        if (responseCode == 428) {
            // Extract challenge_id from response
            String challengeId = null;
            try {
                Map<String, Object> errorJson = parseJson(responseBody);
                Object cid = errorJson.get("challenge_id");
                if (cid != null) {
                    challengeId = cid.toString();
                }
            } catch (Exception e) {
                // Ignore parse errors
            }
            throw new SudoChallengeException(challengeId, responseBody);
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
    // Sudo Challenge Handling
    // ========================================================================

    /**
     * Make an HTTP request with sudo headers for OTP verification.
     */
    private static Map<String, Object> makeRequestWithSudo(
        String method,
        String path,
        String publicKey,
        String secretKey,
        Map<String, Object> data,
        String otp,
        String challengeId
    ) throws IOException {
        String url = API_BASE + path;
        long timestamp = System.currentTimeMillis() / 1000;
        // A GET carries no body, so it signs none; every other method signs what it sends
        boolean sendsBody = data != null && !"GET".equals(method);
        String body = sendsBody ? mapToJson(data) : "";

        String signature = signRequest(secretKey, timestamp, method, path, sendsBody ? body : null);

        HttpURLConnection conn = (HttpURLConnection) new URL(url).openConnection();
        conn.setRequestMethod(method);
        conn.setConnectTimeout(DEFAULT_TIMEOUT_MS);
        conn.setReadTimeout(DEFAULT_TIMEOUT_MS);

        conn.setRequestProperty("Authorization", "Bearer " + publicKey);
        conn.setRequestProperty("X-Timestamp", String.valueOf(timestamp));
        conn.setRequestProperty("X-Signature", signature);
        conn.setRequestProperty("Content-Type", "application/json");
        conn.setRequestProperty("X-Sudo-OTP", otp);
        if (challengeId != null) {
            conn.setRequestProperty("X-Sudo-Challenge", challengeId);
        }

        // Send every signed body (POST, PUT, DELETE): signed bytes == sent bytes
        if (sendsBody) {
            conn.setDoOutput(true);
            try (OutputStream os = conn.getOutputStream()) {
                os.write(body.getBytes(StandardCharsets.UTF_8));
            }
        } else if ("DELETE".equals(method)) {
            conn.setRequestMethod("DELETE");
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

    /**
     * Prompt user for OTP and retry a destructive operation.
     * Called when a 428 Sudo Challenge is received.
     *
     * @param challengeId The challenge ID from the 428 response
     * @param method HTTP method (DELETE or POST)
     * @param path API endpoint path
     * @param publicKey API public key
     * @param secretKey API secret key
     * @param data Request body data (can be null)
     * @return Response map on success
     * @throws IOException on network errors
     */
    private static Map<String, Object> handleSudoChallenge(
        String challengeId,
        String method,
        String path,
        String publicKey,
        String secretKey,
        Map<String, Object> data
    ) throws IOException {
        System.err.println("\033[33mConfirmation required. Check your email for a one-time code.\033[0m");
        System.err.print("Enter OTP: ");
        System.err.flush();

        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        String otp = reader.readLine();

        if (otp == null || otp.trim().isEmpty()) {
            throw new RuntimeException("Operation cancelled - no OTP provided");
        }

        otp = otp.trim();
        return makeRequestWithSudo(method, path, publicKey, secretKey, data, otp, challengeId);
    }

    /**
     * Execute a destructive operation with 428 sudo challenge handling.
     * If the API returns 428, prompts for OTP and retries.
     *
     * @param method HTTP method
     * @param path API endpoint path
     * @param publicKey API public key
     * @param secretKey API secret key
     * @param data Request body data (can be null)
     * @return Response map on success
     * @throws IOException on network errors
     */
    private static Map<String, Object> makeDestructiveRequest(
        String method,
        String path,
        String publicKey,
        String secretKey,
        Map<String, Object> data
    ) throws IOException {
        try {
            return makeRequest(method, path, publicKey, secretKey, data);
        } catch (SudoChallengeException e) {
            return handleSudoChallenge(e.getChallengeId(), method, path, publicKey, secretKey, data);
        }
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

    /**
     * Validate a target OS name case-insensitively.
     *
     * @param targetOs "linux" or "freebsd" in any case, or null when not given
     * @return the lowercase name, or null when targetOs is null
     * @throws IllegalArgumentException for any other value, including ""
     */
    public static String normalizeOs(String targetOs) {
        if (targetOs == null) {
            return null;
        }
        String os = targetOs.toLowerCase(Locale.ROOT);
        if (os.equals("linux") || os.equals("freebsd")) {
            return os;
        }
        throw new IllegalArgumentException("invalid os '" + targetOs + "' (expected linux or freebsd)");
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
        return executeCode(language, code, publicKey, secretKey, null);
    }

    /**
     * Execute code synchronously on a chosen target OS.
     *
     * @param targetOs Target OS: "linux" or "freebsd" (null = omitted, API default linux)
     * @throws IllegalArgumentException if targetOs is not linux or freebsd (no request is made)
     * @see #executeCode(String, String, String, String)
     */
    public static Map<String, Object> executeCode(
        String language,
        String code,
        String publicKey,
        String secretKey,
        String targetOs
    ) throws IOException {
        String os = normalizeOs(targetOs);
        String[] creds = resolveCredentials(publicKey, secretKey);
        publicKey = creds[0];
        secretKey = creds[1];

        Map<String, Object> data = new LinkedHashMap<>();
        data.put("language", language);
        data.put("code", code);
        if (os != null) {
            data.put("os", os);
        }

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
        return executeAsync(language, code, publicKey, secretKey, null);
    }

    /**
     * Execute code asynchronously on a chosen target OS.
     *
     * @param targetOs Target OS: "linux" or "freebsd" (null = omitted, API default linux)
     * @throws IllegalArgumentException if targetOs is not linux or freebsd (no request is made)
     * @see #executeAsync(String, String, String, String)
     */
    public static String executeAsync(
        String language,
        String code,
        String publicKey,
        String secretKey,
        String targetOs
    ) throws IOException {
        String os = normalizeOs(targetOs);
        String[] creds = resolveCredentials(publicKey, secretKey);
        publicKey = creds[0];
        secretKey = creds[1];

        Map<String, Object> data = new LinkedHashMap<>();
        data.put("language", language);
        data.put("code", code);
        if (os != null) {
            data.put("os", os);
        }

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
        return makeDestructiveRequest("DELETE", "/snapshots/" + snapshotId, creds[0], creds[1], null);
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
     * @param opts Optional parameters: network_mode, ttl, shell, multiplexer, vcpu,
     *             os ("linux" or "freebsd"; validated, IllegalArgumentException otherwise)
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
            if (opts.containsKey("os")) {
                Object os = opts.get("os");
                data.put("os", normalizeOs(os == null ? "" : os.toString()));
            }
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
        return createService(name, ports, bootstrap, null, publicKey, secretKey);
    }

    /**
     * Create a new service (long-running container) with optional input files.
     *
     * @param name Service name
     * @param ports Comma-separated list of ports to expose (e.g., "80,443")
     * @param bootstrap Bootstrap script or URL to run on service creation
     * @param inputFiles Optional list of maps with "filename" and "content" (base64-encoded)
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
        List<Map<String, String>> inputFiles,
        String publicKey,
        String secretKey
    ) throws IOException {
        return createService(name, ports, bootstrap, false, inputFiles, null, publicKey, secretKey);
    }

    /**
     * Create a new service (long-running container) with unfreeze-on-demand option.
     *
     * @param name Service name (used for hostname)
     * @param ports Comma-separated list of ports to expose (e.g., "80,443")
     * @param bootstrap Bootstrap script or URL to run on service creation
     * @param unfreezeOnDemand If true, frozen service will auto-wake on HTTP request
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
        boolean unfreezeOnDemand,
        String publicKey,
        String secretKey
    ) throws IOException {
        return createService(name, ports, bootstrap, unfreezeOnDemand, null, publicKey, secretKey);
    }

    /**
     * Create a new service (long-running container) with unfreeze-on-demand option and input files.
     *
     * @param name Service name (used for hostname)
     * @param ports Comma-separated list of ports to expose (e.g., "80,443")
     * @param bootstrap Bootstrap script or URL to run on service creation
     * @param unfreezeOnDemand If true, frozen service will auto-wake on HTTP request
     * @param inputFiles Optional list of maps with "filename" and "content" (base64-encoded)
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
        boolean unfreezeOnDemand,
        List<Map<String, String>> inputFiles,
        String publicKey,
        String secretKey
    ) throws IOException {
        return createService(name, ports, bootstrap, unfreezeOnDemand, inputFiles, null, publicKey, secretKey);
    }

    /**
     * Create a new service on a chosen target OS.
     *
     * @param targetOs Target OS: "linux" or "freebsd" (null = omitted, API default linux)
     * @throws IllegalArgumentException if targetOs is not linux or freebsd (no request is made)
     * @see #createService(String, String, String, boolean, List, String, String)
     */
    public static Map<String, Object> createService(
        String name,
        String ports,
        String bootstrap,
        boolean unfreezeOnDemand,
        List<Map<String, String>> inputFiles,
        String targetOs,
        String publicKey,
        String secretKey
    ) throws IOException {
        String os = normalizeOs(targetOs);
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
        if (unfreezeOnDemand) {
            data.put("unfreeze_on_demand", true);
        }
        if (inputFiles != null && !inputFiles.isEmpty()) {
            data.put("input_files", inputFiles);
        }
        if (os != null) {
            data.put("os", os);
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
        return makeDestructiveRequest("DELETE", "/services/" + serviceId, creds[0], creds[1], null);
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
        return makeDestructiveRequest("POST", "/services/" + serviceId + "/unlock", creds[0], creds[1], new LinkedHashMap<>());
    }

    /**
     * Set unfreeze-on-demand for a service.
     *
     * <p>When enabled, a frozen service will automatically wake up when it receives
     * an HTTP request, without requiring an explicit unfreeze API call.
     *
     * @param serviceId Service ID to configure
     * @param enabled True to enable unfreeze-on-demand, false to disable
     * @param publicKey Optional API key
     * @param secretKey Optional API secret
     * @return Response map with update confirmation
     * @throws IOException on network errors
     * @throws CredentialsException if credentials cannot be found
     * @throws ApiException if API returns an error
     */
    public static Map<String, Object> setUnfreezeOnDemand(
        String serviceId,
        boolean enabled,
        String publicKey,
        String secretKey
    ) throws IOException {
        String[] creds = resolveCredentials(publicKey, secretKey);
        Map<String, Object> data = new LinkedHashMap<>();
        data.put("unfreeze_on_demand", enabled);
        return makeRequestWithMethod("PATCH", "/services/" + serviceId, creds[0], creds[1], data);
    }

    /**
     * Set show-freeze-page for a service.
     *
     * <p>When enabled, visitors to a frozen service will see a branded "frozen" page
     * instead of an error. This improves UX for services that use unfreeze-on-demand.
     *
     * @param serviceId Service ID to configure
     * @param enabled True to show freeze page, false to hide it
     * @param publicKey Optional API key
     * @param secretKey Optional API secret
     * @return Response map with update confirmation
     * @throws IOException on network errors
     * @throws CredentialsException if credentials cannot be found
     * @throws ApiException if API returns an error
     */
    public static Map<String, Object> setShowFreezePage(
        String serviceId,
        boolean enabled,
        String publicKey,
        String secretKey
    ) throws IOException {
        String[] creds = resolveCredentials(publicKey, secretKey);
        Map<String, Object> data = new LinkedHashMap<>();
        data.put("show_freeze_page", enabled);
        return makeRequestWithMethod("PATCH", "/services/" + serviceId, creds[0], creds[1], data);
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
        return makeRequest("PUT", "/services/" + serviceId + "/env", creds[0], creds[1], data);
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
        return redeployService(serviceId, null, publicKey, secretKey);
    }

    /**
     * Redeploy a service (re-run bootstrap script) with optional input files.
     *
     * @param serviceId Service ID to redeploy
     * @param inputFiles Optional list of maps with "filename" and "content" (base64-encoded)
     * @param publicKey Optional API key
     * @param secretKey Optional API secret
     * @return Response map with redeploy confirmation
     * @throws IOException on network errors
     * @throws CredentialsException if credentials cannot be found
     * @throws ApiException if API returns an error
     */
    public static Map<String, Object> redeployService(
        String serviceId,
        List<Map<String, String>> inputFiles,
        String publicKey,
        String secretKey
    ) throws IOException {
        String[] creds = resolveCredentials(publicKey, secretKey);
        Map<String, Object> data = new LinkedHashMap<>();
        if (inputFiles != null && !inputFiles.isEmpty()) {
            data.put("input_files", inputFiles);
        }
        return makeRequest("POST", "/services/" + serviceId + "/redeploy", creds[0], creds[1], data);
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
        return makeDestructiveRequest("POST", "/snapshots/" + snapshotId + "/unlock", creds[0], creds[1], new LinkedHashMap<>());
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
    // Images API Methods (LXD Container Images)
    // ========================================================================

    /**
     * Publish a session or service as a reusable LXD container image.
     *
     * @param sourceType Source type: "session" or "service"
     * @param sourceId ID of the session or service to publish
     * @param name Name for the image
     * @param description Optional description for the image
     * @param publicKey Optional API key
     * @param secretKey Optional API secret
     * @return Response map containing image_id
     * @throws IOException on network errors
     * @throws CredentialsException if credentials cannot be found
     * @throws ApiException if API returns an error
     */
    public static Map<String, Object> imagePublish(
        String sourceType,
        String sourceId,
        String name,
        String description,
        String publicKey,
        String secretKey
    ) throws IOException {
        String[] creds = resolveCredentials(publicKey, secretKey);
        Map<String, Object> data = new LinkedHashMap<>();
        data.put("source_type", sourceType);
        data.put("source_id", sourceId);
        data.put("name", name);
        if (description != null && !description.isEmpty()) {
            data.put("description", description);
        }
        return makeRequest("POST", "/images", creds[0], creds[1], data);
    }

    /**
     * List container images with optional filtering.
     *
     * @param filterType Optional filter: "own", "shared", "public", or null for all
     * @param publicKey Optional API key
     * @param secretKey Optional API secret
     * @return List of image maps containing id, name, description, visibility, etc.
     * @throws IOException on network errors
     * @throws CredentialsException if credentials cannot be found
     * @throws ApiException if API returns an error
     */
    @SuppressWarnings("unchecked")
    public static List<Map<String, Object>> listImages(
        String filterType,
        String publicKey,
        String secretKey
    ) throws IOException {
        String[] creds = resolveCredentials(publicKey, secretKey);
        String path = "/images";
        if (filterType != null && !filterType.isEmpty()) {
            path = "/images/" + filterType;
        }
        Map<String, Object> response = makeRequest("GET", path, creds[0], creds[1], null);
        Object images = response.get("images");
        if (images instanceof List) {
            List<Map<String, Object>> result = new ArrayList<>();
            for (Object item : (List<?>) images) {
                if (item instanceof Map) {
                    result.add((Map<String, Object>) item);
                }
            }
            return result;
        }
        return new ArrayList<>();
    }

    /**
     * Get details of a specific image.
     *
     * @param imageId Image ID to retrieve
     * @param publicKey Optional API key
     * @param secretKey Optional API secret
     * @return Image details map
     * @throws IOException on network errors
     * @throws CredentialsException if credentials cannot be found
     * @throws ApiException if API returns an error
     */
    public static Map<String, Object> getImage(
        String imageId,
        String publicKey,
        String secretKey
    ) throws IOException {
        String[] creds = resolveCredentials(publicKey, secretKey);
        return makeRequest("GET", "/images/" + imageId, creds[0], creds[1], null);
    }

    /**
     * Delete an image.
     *
     * @param imageId Image ID to delete
     * @param publicKey Optional API key
     * @param secretKey Optional API secret
     * @return Response map with deletion confirmation
     * @throws IOException on network errors
     * @throws CredentialsException if credentials cannot be found
     * @throws ApiException if API returns an error
     */
    public static Map<String, Object> deleteImage(
        String imageId,
        String publicKey,
        String secretKey
    ) throws IOException {
        String[] creds = resolveCredentials(publicKey, secretKey);
        return makeDestructiveRequest("DELETE", "/images/" + imageId, creds[0], creds[1], null);
    }

    /**
     * Lock an image (prevent deletion and modifications).
     *
     * @param imageId Image ID to lock
     * @param publicKey Optional API key
     * @param secretKey Optional API secret
     * @return Response map with lock confirmation
     * @throws IOException on network errors
     * @throws CredentialsException if credentials cannot be found
     * @throws ApiException if API returns an error
     */
    public static Map<String, Object> lockImage(
        String imageId,
        String publicKey,
        String secretKey
    ) throws IOException {
        String[] creds = resolveCredentials(publicKey, secretKey);
        return makeRequest("POST", "/images/" + imageId + "/lock", creds[0], creds[1], new LinkedHashMap<>());
    }

    /**
     * Unlock an image (allow deletion and modifications).
     *
     * @param imageId Image ID to unlock
     * @param publicKey Optional API key
     * @param secretKey Optional API secret
     * @return Response map with unlock confirmation
     * @throws IOException on network errors
     * @throws CredentialsException if credentials cannot be found
     * @throws ApiException if API returns an error
     */
    public static Map<String, Object> unlockImage(
        String imageId,
        String publicKey,
        String secretKey
    ) throws IOException {
        String[] creds = resolveCredentials(publicKey, secretKey);
        return makeDestructiveRequest("POST", "/images/" + imageId + "/unlock", creds[0], creds[1], new LinkedHashMap<>());
    }

    /**
     * Set the visibility of an image.
     *
     * @param imageId Image ID to modify
     * @param visibility Visibility level: "private", "shared", or "public"
     * @param publicKey Optional API key
     * @param secretKey Optional API secret
     * @return Response map with visibility change confirmation
     * @throws IOException on network errors
     * @throws CredentialsException if credentials cannot be found
     * @throws ApiException if API returns an error
     */
    public static Map<String, Object> setImageVisibility(
        String imageId,
        String visibility,
        String publicKey,
        String secretKey
    ) throws IOException {
        String[] creds = resolveCredentials(publicKey, secretKey);
        Map<String, Object> data = new LinkedHashMap<>();
        data.put("visibility", visibility);
        return makeRequest("POST", "/images/" + imageId + "/visibility", creds[0], creds[1], data);
    }

    /**
     * Publish an image from a service or snapshot.
     *
     * @param sourceType Source type: "service" or "snapshot"
     * @param sourceId Source ID (service_id or snapshot_id)
     * @param name Name for the new image (optional)
     * @param publicKey Optional API key
     * @param secretKey Optional API secret
     * @return Response map containing new image_id
     * @throws IOException on network errors
     * @throws CredentialsException if credentials cannot be found
     * @throws ApiException if API returns an error
     */
    public static Map<String, Object> publishImage(
        String sourceType,
        String sourceId,
        String name,
        String publicKey,
        String secretKey
    ) throws IOException {
        String[] creds = resolveCredentials(publicKey, secretKey);
        Map<String, Object> data = new LinkedHashMap<>();
        data.put("source_type", sourceType);
        data.put("source_id", sourceId);
        if (name != null && !name.isEmpty()) {
            data.put("name", name);
        }
        return makeRequest("POST", "/images/publish", creds[0], creds[1], data);
    }

    /**
     * Grant access to an image for another API key (for shared images).
     *
     * @param imageId Image ID to grant access to
     * @param trustedApiKey Public API key to grant access to
     * @param publicKey Optional API key
     * @param secretKey Optional API secret
     * @return Response map with grant confirmation
     * @throws IOException on network errors
     * @throws CredentialsException if credentials cannot be found
     * @throws ApiException if API returns an error
     */
    public static Map<String, Object> grantImageAccess(
        String imageId,
        String trustedApiKey,
        String publicKey,
        String secretKey
    ) throws IOException {
        String[] creds = resolveCredentials(publicKey, secretKey);
        Map<String, Object> data = new LinkedHashMap<>();
        data.put("trusted_api_key", trustedApiKey);
        return makeRequest("POST", "/images/" + imageId + "/grant", creds[0], creds[1], data);
    }

    /**
     * Revoke access to an image from another API key.
     *
     * @param imageId Image ID to revoke access from
     * @param trustedApiKey Public API key to revoke access from
     * @param publicKey Optional API key
     * @param secretKey Optional API secret
     * @return Response map with revoke confirmation
     * @throws IOException on network errors
     * @throws CredentialsException if credentials cannot be found
     * @throws ApiException if API returns an error
     */
    public static Map<String, Object> revokeImageAccess(
        String imageId,
        String trustedApiKey,
        String publicKey,
        String secretKey
    ) throws IOException {
        String[] creds = resolveCredentials(publicKey, secretKey);
        Map<String, Object> data = new LinkedHashMap<>();
        data.put("trusted_api_key", trustedApiKey);
        return makeRequest("POST", "/images/" + imageId + "/revoke", creds[0], creds[1], data);
    }

    /**
     * List API keys that have been granted access to an image.
     *
     * @param imageId Image ID to list trusted keys for
     * @param publicKey Optional API key
     * @param secretKey Optional API secret
     * @return List of trusted API key maps
     * @throws IOException on network errors
     * @throws CredentialsException if credentials cannot be found
     * @throws ApiException if API returns an error
     */
    @SuppressWarnings("unchecked")
    public static List<Map<String, Object>> listImageTrusted(
        String imageId,
        String publicKey,
        String secretKey
    ) throws IOException {
        String[] creds = resolveCredentials(publicKey, secretKey);
        Map<String, Object> response = makeRequest("GET", "/images/" + imageId + "/trusted", creds[0], creds[1], null);
        Object trusted = response.get("trusted");
        if (trusted instanceof List) {
            List<Map<String, Object>> result = new ArrayList<>();
            for (Object item : (List<?>) trusted) {
                if (item instanceof Map) {
                    result.add((Map<String, Object>) item);
                }
            }
            return result;
        }
        return new ArrayList<>();
    }

    /**
     * Transfer ownership of an image to another API key.
     *
     * @param imageId Image ID to transfer
     * @param toApiKey Public API key of the new owner
     * @param publicKey Optional API key
     * @param secretKey Optional API secret
     * @return Response map with transfer confirmation
     * @throws IOException on network errors
     * @throws CredentialsException if credentials cannot be found
     * @throws ApiException if API returns an error
     */
    public static Map<String, Object> transferImage(
        String imageId,
        String toApiKey,
        String publicKey,
        String secretKey
    ) throws IOException {
        String[] creds = resolveCredentials(publicKey, secretKey);
        Map<String, Object> data = new LinkedHashMap<>();
        data.put("to_api_key", toApiKey);
        return makeRequest("POST", "/images/" + imageId + "/transfer", creds[0], creds[1], data);
    }

    /**
     * Spawn a new service from an image.
     *
     * @param imageId Image ID to spawn from
     * @param name Name for the new service
     * @param ports Comma-separated list of ports to expose (e.g., "80,443")
     * @param bootstrap Optional bootstrap script or URL
     * @param networkMode Network mode: "zerotrust" or "semitrusted"
     * @param publicKey Optional API key
     * @param secretKey Optional API secret
     * @return Response map containing service_id
     * @throws IOException on network errors
     * @throws CredentialsException if credentials cannot be found
     * @throws ApiException if API returns an error
     */
    public static Map<String, Object> spawnFromImage(
        String imageId,
        String name,
        String ports,
        String bootstrap,
        String networkMode,
        String publicKey,
        String secretKey
    ) throws IOException {
        String[] creds = resolveCredentials(publicKey, secretKey);
        Map<String, Object> data = new LinkedHashMap<>();
        data.put("name", name);
        if (ports != null && !ports.isEmpty()) {
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
        if (networkMode != null && !networkMode.isEmpty()) {
            data.put("network_mode", networkMode);
        }
        return makeRequest("POST", "/images/" + imageId + "/spawn", creds[0], creds[1], data);
    }

    /**
     * Clone an image to create a new image with a different name.
     *
     * @param imageId Image ID to clone
     * @param name Name for the cloned image
     * @param description Optional description for the cloned image
     * @param publicKey Optional API key
     * @param secretKey Optional API secret
     * @return Response map containing new image_id
     * @throws IOException on network errors
     * @throws CredentialsException if credentials cannot be found
     * @throws ApiException if API returns an error
     */
    public static Map<String, Object> cloneImage(
        String imageId,
        String name,
        String description,
        String publicKey,
        String secretKey
    ) throws IOException {
        String[] creds = resolveCredentials(publicKey, secretKey);
        Map<String, Object> data = new LinkedHashMap<>();
        if (name != null && !name.isEmpty()) {
            data.put("name", name);
        }
        if (description != null && !description.isEmpty()) {
            data.put("description", description);
        }
        return makeRequest("POST", "/images/" + imageId + "/clone", creds[0], creds[1], data);
    }

    // ========================================================================
    // PaaS Logs API (2)
    // ========================================================================

    /**
     * Fetch batch logs from portal.
     *
     * @param source Log source: "all", "api", "portal", "pool/cammy", "pool/ai"
     * @param lines Number of lines (1-10000)
     * @param since Time window: "1m", "5m", "1h", "1d"
     * @param grep Optional filter pattern (null for no filter)
     * @param publicKey Optional API key
     * @param secretKey Optional API secret
     * @return Response map containing logs
     * @throws IOException on network errors
     * @throws CredentialsException if credentials cannot be found
     * @throws ApiException if API returns an error
     */
    public static Map<String, Object> logsFetch(
        String source,
        int lines,
        String since,
        String grep,
        String publicKey,
        String secretKey
    ) throws IOException {
        return logsFetch(source, lines, since, grep, null, publicKey, secretKey);
    }

    /**
     * Fetch batch logs from the portal: GET {portal}/logs/{source}.
     *
     * @param level Optional minimum level (debug, info, notice, warning, err, crit)
     */
    public static Map<String, Object> logsFetch(
        String source,
        int lines,
        String since,
        String grep,
        String level,
        String publicKey,
        String secretKey
    ) throws IOException {
        String[] creds = resolveCredentials(publicKey, secretKey);
        String[] res = logsFetchRaw(creds, source, lines, since, grep, level);
        int code = Integer.parseInt(res[0]);
        if (code != 200) {
            throw new ApiException("HTTP " + code, code, res[1]);
        }
        return parseJson(res[1]);
    }

    /** Portal logs path: /logs/{source}[/stream]. */
    private static String logsPath(String source, boolean stream) {
        String src = (source == null || source.isEmpty()) ? "all" : source;
        return "/logs/" + src + (stream ? "/stream" : "");
    }

    /** Query string from key/value pairs, percent-encoded, empty values skipped. */
    private static String logsQuery(String... kv) throws UnsupportedEncodingException {
        StringBuilder q = new StringBuilder();
        for (int i = 0; i + 1 < kv.length; i += 2) {
            if (kv[i + 1] == null || kv[i + 1].isEmpty()) continue;
            q.append(q.length() == 0 ? '?' : '&').append(kv[i]).append('=')
             .append(java.net.URLEncoder.encode(kv[i + 1], "UTF-8").replace("+", "%20"));
        }
        return q.toString();
    }

    /** Signed GET to the portal; HMAC covers the path only (server checks conn.request_path). */
    private static HttpURLConnection portalGet(String[] creds, String path, String query, int readTimeout) throws IOException {
        long timestamp = System.currentTimeMillis() / 1000;
        HttpURLConnection conn = (HttpURLConnection) new URL(PORTAL_BASE + path + query).openConnection();
        conn.setRequestMethod("GET");
        conn.setConnectTimeout(30000);
        conn.setReadTimeout(readTimeout);
        conn.setRequestProperty("Authorization", "Bearer " + creds[0]);
        conn.setRequestProperty("X-Timestamp", String.valueOf(timestamp));
        conn.setRequestProperty("X-Signature", signRequest(creds[1], timestamp, "GET", path, null));
        conn.setRequestProperty("User-Agent", "un-java/2.0");
        return conn;
    }

    private static String readAll(InputStream in) throws IOException {
        if (in == null) return "";
        try (InputStream is = in) {
            return new String(is.readAllBytes(), StandardCharsets.UTF_8);
        }
    }

    /** Returns {status, body}. */
    private static String[] logsFetchRaw(String[] creds, String source, int lines, String since, String grep, String level) throws IOException {
        String q = logsQuery("lines", String.valueOf(lines > 0 ? lines : 100), "since", since, "grep", grep, "level", level);
        HttpURLConnection conn = portalGet(creds, logsPath(source, false), q, DEFAULT_TIMEOUT_MS);
        int code = conn.getResponseCode();
        String body = readAll(code >= 400 ? conn.getErrorStream() : conn.getInputStream());
        return new String[] { String.valueOf(code), body };
    }

    /**
     * Interface for receiving streamed log lines.
     */
    public interface LogCallback {
        /**
         * Called for each log line received.
         *
         * @param source The log source (e.g., "api", "portal")
         * @param line The log line content
         */
        void onLogLine(String source, String line);
    }

    /**
     * Stream logs via SSE. Blocks until interrupted or server closes connection.
     *
     * @param source Log source: "all", "api", "portal", "pool/cammy", "pool/ai"
     * @param grep Optional filter pattern (null for no filter)
     * @param callback Callback for each log line received
     * @param publicKey Optional API key
     * @param secretKey Optional API secret
     * @return true on clean shutdown, false on error
     * @throws IOException on network errors
     * @throws CredentialsException if credentials cannot be found
     */
    public static boolean logsStream(
        String source,
        String grep,
        LogCallback callback,
        String publicKey,
        String secretKey
    ) throws IOException {
        String[] creds = resolveCredentials(publicKey, secretKey);
        return logsStreamOnce(creds, source, grep, null, null, callback) == 200;
    }

    /** One SSE connection; returns HTTP status (0 on a network error). */
    private static int logsStreamOnce(String[] creds, String source, String grep, String level, String since, LogCallback callback) {
        try {
            HttpURLConnection conn = portalGet(creds, logsPath(source, true), logsQuery("grep", grep, "level", level, "since", since), 0);
            conn.setRequestProperty("Accept", "text/event-stream");
            int code = conn.getResponseCode();
            if (code != 200) {
                readAll(conn.getErrorStream());
                return code;
            }
            try (BufferedReader reader = new BufferedReader(
                    new InputStreamReader(conn.getInputStream(), StandardCharsets.UTF_8))) {
                String line;
                while ((line = reader.readLine()) != null) {
                    if (!line.startsWith("data:")) continue;
                    String data = line.substring(5).trim();
                    if (data.isEmpty() || data.equals(":keepalive") || callback == null) continue;
                    String src = "";
                    String text = data;
                    if (data.startsWith("{")) {
                        Map<String, Object> ev = parseJson(data);
                        if (!(ev.get("line") instanceof String)) continue;
                        text = (String) ev.get("line");
                        src = ev.get("source") instanceof String ? (String) ev.get("source") : "";
                    }
                    callback.onLogLine(src, text);
                }
            } catch (IOException e) {
                // dropped connection: caller reconnects
            }
            return 200;
        } catch (IOException e) {
            return 0;
        }
    }

    /**
     * Stream logs until interrupted, reconnecting on drops (2 s backoff doubling
     * to 30 s, since= overlap). Returns the HTTP status that ended the stream
     * (401/403 or another 4xx).
     */
    public static int logsFollow(String[] creds, String source, String grep, String level, LogCallback callback) {
        int delay = 2;
        final long[] lastRecv = {0};
        String since = null;
        LogCallback cb = (src, l) -> { lastRecv[0] = System.currentTimeMillis() / 1000; callback.onLogLine(src, l); };
        while (true) {
            long start = System.currentTimeMillis() / 1000;
            int code = logsStreamOnce(creds, source, grep, level, since, cb);
            if (System.currentTimeMillis() / 1000 - start > 30) delay = 2;
            if (code >= 400 && code < 500) return code;
            try { Thread.sleep(delay * 1000L); } catch (InterruptedException e) { return 0; }
            delay = Math.min(delay * 2, 30);
            if (lastRecv[0] > 0) {
                long gap = System.currentTimeMillis() / 1000 - lastRecv[0] + 5;
                since = Math.max(gap, 10) + "s";
            }
        }
    }

    private static Path logCursorPath() {
        String home = System.getProperty("user.home");
        String env = System.getenv("HOME");
        if (env != null && !env.isEmpty()) home = env;
        return Paths.get(home, ".unsandbox", "paas_log_cursor");
    }

    private static String readLogCursor() {
        try {
            long ts = Long.parseLong(new String(Files.readAllBytes(logCursorPath()), StandardCharsets.UTF_8).trim());
            if (ts <= 0) return null;
            long ago = Math.max(System.currentTimeMillis() / 1000 - ts, 5);
            return ago + "s";
        } catch (Exception e) {
            return null;
        }
    }

    private static void writeLogCursor() {
        try {
            Path p = logCursorPath();
            Files.createDirectories(p.getParent());
            Files.write(p, ((System.currentTimeMillis() / 1000) + "\n").getBytes(StandardCharsets.UTF_8));
        } catch (Exception e) {
            // cursor is best effort
        }
    }

    private static void printPaasLogsHelp() {
        System.err.print(
            "Usage: un paas logs [options]\n\n" +
            "Fetch or stream PaaS production logs.\n\n" +
            "Sources:\n" +
            "  --all              All sources (default)\n" +
            "  --api              API server logs\n" +
            "  --portal           Portal server logs\n" +
            "  --pool NODE        Pool node logs (e.g., cammy, ai)\n" +
            "  --source SOURCE    Source string (api, portal, pool/cammy, all)\n\n" +
            "Options:\n" +
            "  --lines N, -n N    Number of lines (default: 100, max: 10000)\n" +
            "  --since TIME       Time window: 1m, 5m, 15m, 1h, 6h, 1d (default: 5m)\n" +
            "  --grep PATTERN     Filter log lines by pattern\n" +
            "  --level LVL, -l    Min log level: debug, info, notice, warning, err, crit\n" +
            "  --follow, -f       Follow logs in real-time (Ctrl+C to stop)\n" +
            "  --json             Output raw JSON response\n" +
            "  -p KEY             Public key\n" +
            "  -k KEY             Secret key\n" +
            "  -h                 Show this help\n\n" +
            "Requires a partner API key with log access enabled.\n");
    }

    private static int clampLines(String v) {
        int n;
        try { n = Integer.parseInt(v); } catch (NumberFormatException e) { n = 0; }
        return Math.max(1, Math.min(10000, n));
    }

    /** `un paas <command>`: currently `logs`. Returns the exit code. */
    static int cmdPaas(String[] args) throws IOException {
        if (args.length < 2) {
            System.err.print("Usage: un paas <command> [options]\n\nCommands:\n  logs    Fetch or stream production logs\n\nRun 'un paas <command> -h' for help on a specific command.\n");
            return 1;
        }
        if (!args[1].equals("logs")) {
            System.err.println("Error: Unknown paas command '" + args[1] + "'");
            System.err.println("Run 'un paas' for available commands.");
            return 1;
        }
        String source = "all", since = null, grep = null, level = null, pk = null, sk = null;
        int lines = 100, account = -1;
        boolean follow = false, json = false;
        for (int i = 2; i < args.length; i++) {
            String a = args[i];
            boolean more = i + 1 < args.length;
            if (a.equals("-p") && more) pk = args[++i];
            else if (a.equals("-k") && more) sk = args[++i];
            else if (a.equals("--account") && more) { try { account = Integer.parseInt(args[++i]); } catch (NumberFormatException e) { account = -1; } }
            else if (a.equals("--source") && more) source = args[++i];
            else if (a.equals("--all")) source = "all";
            else if (a.equals("--api")) source = "api";
            else if (a.equals("--portal")) source = "portal";
            else if (a.equals("--pool") && more) source = "pool/" + args[++i];
            else if ((a.equals("--lines") || a.equals("-n")) && more) lines = clampLines(args[++i]);
            else if (a.equals("--since") && more) since = args[++i];
            else if (a.equals("--grep") && more) grep = args[++i];
            else if ((a.equals("--level") || a.equals("-l")) && more) level = args[++i];
            else if (a.equals("--follow") || a.equals("-f")) follow = true;
            else if (a.equals("--json")) json = true;
            else if (a.equals("-h") || a.equals("--help")) { printPaasLogsHelp(); return 0; }
        }
        String[] creds;
        try {
            creds = resolveCredentials(pk, sk, account);
        } catch (CredentialsException e) {
            creds = null;
        }
        if (creds == null || creds[0] == null || creds[0].isEmpty()) {
            System.err.print("Error: API credentials required.\n  Set UNSANDBOX_PUBLIC_KEY + UNSANDBOX_SECRET_KEY env vars, or\n  Use -p PUBLIC_KEY -k SECRET_KEY flags, or\n  Create ~/.unsandbox/accounts.csv with: public_key,secret_key\n");
            return 1;
        }
        if (follow) {
            System.err.println("Streaming " + source + " logs... (Ctrl+C to stop)");
            int code = logsFollow(creds, source, grep, level, (src, l) -> {
                if (src.isEmpty()) System.out.println(l);
                else System.out.println(String.format("\033[36m%-12s\033[0m %s", src, l));
                System.out.flush();
            });
            if (code == 401 || code == 403) System.err.println("Error: HTTP " + code + " (auth failed)");
            else if (code != 0) System.err.println("Error: HTTP " + code);
            return 1;
        }
        if (since == null || since.isEmpty()) {
            since = readLogCursor();
            if (since == null) since = "5m";
        }
        String[] res;
        try {
            res = logsFetchRaw(creds, source, lines, since, grep, level);
        } catch (IOException e) {
            System.err.println("Error: Network error fetching logs: " + e.getMessage());
            return 1;
        }
        int code = Integer.parseInt(res[0]);
        if (code != 200) {
            String msg = null;
            try {
                Object err = parseJson(res[1]).get("error");
                if (err instanceof String) msg = (String) err;
            } catch (Exception e) {
                msg = null;
            }
            System.err.println("Error: HTTP " + code + (msg != null ? " \u2014 " + msg : ""));
            return 1;
        }
        writeLogCursor();
        if (json) {
            System.out.println(res[1]);
            return 0;
        }
        Map<String, Object> body;
        try {
            body = parseJson(res[1]);
        } catch (Exception e) {
            body = null;
        }
        if (body == null || !(body.get("lines") instanceof List)) {
            System.err.println("Error: Invalid response format");
            return 1;
        }
        for (Object e : (List<?>) body.get("lines")) {
            if (e instanceof String) {
                System.out.println((String) e);
            } else if (e instanceof Map) {
                Map<?, ?> m = (Map<?, ?>) e;
                System.out.println(String.format("\033[36m%-12s\033[0m %s", String.valueOf(m.get("source")), String.valueOf(m.get("line"))));
            }
        }
        return 0;
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
    // Utility Functions
    // ========================================================================

    /**
     * Get SDK version string.
     *
     * @return Version string (e.g., "4.2.0")
     */
    public static String version() {
        return "4.2.0";
    }

    /**
     * Check API health status.
     *
     * @return true if API is healthy, false otherwise
     */
    public static boolean healthCheck() {
        try {
            HttpURLConnection conn = (HttpURLConnection) new URL(API_BASE + "/health").openConnection();
            conn.setRequestMethod("GET");
            conn.setConnectTimeout(5000);
            conn.setReadTimeout(5000);
            return conn.getResponseCode() == 200;
        } catch (Exception e) {
            return false;
        }
    }

    /**
     * Generate HMAC-SHA256 signature.
     *
     * @param secretKey Secret key for HMAC
     * @param message Message to sign
     * @return Lowercase hex-encoded signature
     */
    public static String hmacSign(String secretKey, String message) {
        try {
            Mac mac = Mac.getInstance("HmacSHA256");
            SecretKeySpec secretKeySpec = new SecretKeySpec(
                secretKey.getBytes(StandardCharsets.UTF_8),
                "HmacSHA256"
            );
            mac.init(secretKeySpec);
            byte[] hash = mac.doFinal(message.getBytes(StandardCharsets.UTF_8));
            StringBuilder hexString = new StringBuilder();
            for (byte b : hash) {
                hexString.append(String.format("%02x", b));
            }
            return hexString.toString();
        } catch (Exception e) {
            return null;
        }
    }

    /**
     * Get details of a specific snapshot.
     *
     * @param snapshotId Snapshot ID to retrieve
     * @param publicKey Optional API key
     * @param secretKey Optional API secret
     * @return Snapshot details map
     * @throws IOException on network errors
     * @throws CredentialsException if credentials cannot be found
     * @throws ApiException if API returns an error
     */
    public static Map<String, Object> getSnapshot(
        String snapshotId,
        String publicKey,
        String secretKey
    ) throws IOException {
        String[] creds = resolveCredentials(publicKey, secretKey);
        return makeRequest("GET", "/snapshots/" + snapshotId, creds[0], creds[1], null);
    }

    /**
     * Resize a service (change vCPU allocation).
     *
     * @param serviceId Service ID to resize
     * @param vcpu New vCPU count (1-8)
     * @param publicKey Optional API key
     * @param secretKey Optional API secret
     * @return Response map with resize confirmation
     * @throws IOException on network errors
     * @throws CredentialsException if credentials cannot be found
     * @throws ApiException if API returns an error
     */
    public static Map<String, Object> resizeService(
        String serviceId,
        int vcpu,
        String publicKey,
        String secretKey
    ) throws IOException {
        String[] creds = resolveCredentials(publicKey, secretKey);
        Map<String, Object> data = new LinkedHashMap<>();
        data.put("vcpu", vcpu);
        return makeRequestWithMethod("PATCH", "/services/" + serviceId, creds[0], creds[1], data);
    }

    // ========================================================================
    // Image Generation API
    // ========================================================================

    /**
     * Generate images from text prompt using AI.
     *
     * @param prompt Text description of the image to generate
     * @param model Model to use (optional, can be null)
     * @param size Image size (e.g., "1024x1024")
     * @param quality "standard" or "hd"
     * @param n Number of images to generate
     * @param publicKey API public key (optional)
     * @param secretKey API secret key (optional)
     * @return Map containing "images" (List of base64/URLs) and "created_at"
     * @throws IOException on network errors
     * @throws CredentialsException if credentials cannot be found
     * @throws ApiException if API returns an error
     */
    public static Map<String, Object> image(
        String prompt,
        String model,
        String size,
        String quality,
        int n,
        String publicKey,
        String secretKey
    ) throws IOException {
        String[] creds = resolveCredentials(publicKey, secretKey);

        if (size == null) size = "1024x1024";
        if (quality == null) quality = "standard";
        if (n <= 0) n = 1;

        Map<String, Object> data = new LinkedHashMap<>();
        data.put("prompt", prompt);
        data.put("size", size);
        data.put("quality", quality);
        data.put("n", n);
        if (model != null && !model.isEmpty()) {
            data.put("model", model);
        }

        return makeRequest("POST", "/image", creds[0], creds[1], data);
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

        // HttpURLConnection refuses PATCH (ProtocolException); java.net.http.HttpClient
        // sends any method, with exactly the body we signed.
        java.net.http.HttpRequest.BodyPublisher publisher = data != null
            ? java.net.http.HttpRequest.BodyPublishers.ofString(body, StandardCharsets.UTF_8)
            : java.net.http.HttpRequest.BodyPublishers.noBody();
        java.net.http.HttpRequest request = java.net.http.HttpRequest.newBuilder(java.net.URI.create(url))
            .timeout(java.time.Duration.ofMillis(DEFAULT_TIMEOUT_MS))
            .header("Authorization", "Bearer " + publicKey)
            .header("X-Timestamp", String.valueOf(timestamp))
            .header("X-Signature", signature)
            .header("Content-Type", "application/json")
            .method(method, publisher)
            .build();
        java.net.http.HttpResponse<String> response;
        try {
            response = java.net.http.HttpClient.newBuilder()
                .connectTimeout(java.time.Duration.ofMillis(DEFAULT_TIMEOUT_MS))
                .build()
                .send(request, java.net.http.HttpResponse.BodyHandlers.ofString(StandardCharsets.UTF_8));
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            throw new IOException("Request interrupted", e);
        }

        int responseCode = response.statusCode();
        String responseBody = response.body();

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
    // CLI Implementation
    // ========================================================================

    /**
     * CLI entry point.
     */
    public static void main(String[] args) {
        try {
            cliMain(args);
        } catch (Exception e) {
            System.err.println("Error: " + e.getMessage());
            System.exit(1);
        }
    }

    /** Subcommands whose own options follow them on the command line. */
    private static final Set<String> SUBCOMMANDS = new HashSet<>(Arrays.asList(
        "session", "service", "snapshot", "image", "languages"));

    /**
     * CLI main implementation.
     */
    public static void cliMain(String[] args) throws Exception {
        if (args.length == 0) {
            printHelp();
            System.exit(0);
        }

        // paas has its own flags (-n, -l, -f mean lines/level/follow there)
        if (args[0].equals("paas")) {
            System.exit(cmdPaas(args));
        }

        // Parse global options
        String publicKey = null;
        String secretKey = null;
        String language = null;
        String networkMode = "zerotrust";
        String targetOs = null;
        int vcpu = 1;
        int accountIndex = -1;
        List<String> envVars = new ArrayList<>();
        List<String> files = new ArrayList<>();
        List<String> positionalArgs = new ArrayList<>();
        boolean showHelp = false;
        boolean rotateKey = false;
        String otpCode = null;

        int i = 0;
        while (i < args.length) {
            String arg = args[i];
            if (arg.equals("-h") || arg.equals("--help")) {
                showHelp = true;
                i++;
            } else if (!positionalArgs.isEmpty() && SUBCOMMANDS.contains(positionalArgs.get(0))) {
                // After a subcommand, its options belong to its handler (CLI_SPEC.md);
                // service also takes -f/--file, which feeds input_files.
                if (positionalArgs.get(0).equals("service") && (arg.equals("-f") || arg.equals("--file"))) {
                    if (i + 1 >= args.length) {
                        System.err.println("Error: -f/--file requires an argument");
                        System.exit(2);
                    }
                    files.add(args[++i]);
                } else {
                    positionalArgs.add(arg);
                }
                i++;
            } else if (arg.equals("--account")) {
                if (i + 1 >= args.length) {
                    System.err.println("Error: --account requires an argument");
                    System.exit(2);
                }
                try {
                    accountIndex = Integer.parseInt(args[++i]);
                } catch (NumberFormatException e) {
                    System.err.println("Error: --account requires an integer argument");
                    System.exit(2);
                }
                i++;
            } else if (arg.equals("-s") || arg.equals("--shell")) {
                if (i + 1 >= args.length) {
                    System.err.println("Error: -s/--shell requires an argument");
                    System.exit(2);
                }
                language = args[++i];
                i++;
            } else if (arg.equals("-p") || arg.equals("--public-key")) {
                if (i + 1 >= args.length) {
                    System.err.println("Error: -p/--public-key requires an argument");
                    System.exit(2);
                }
                publicKey = args[++i];
                i++;
            } else if (arg.equals("-k") || arg.equals("--secret-key")) {
                if (i + 1 >= args.length) {
                    System.err.println("Error: -k/--secret-key requires an argument");
                    System.exit(2);
                }
                secretKey = args[++i];
                i++;
            } else if (arg.equals("-n") || arg.equals("--network")) {
                if (i + 1 >= args.length) {
                    System.err.println("Error: -n/--network requires an argument");
                    System.exit(2);
                }
                networkMode = args[++i];
                i++;
            } else if (arg.equals("--os") || arg.startsWith("--os=")) {
                if (arg.equals("--os")) {
                    if (i + 1 >= args.length) {
                        System.err.println("Error: --os requires a value: linux or freebsd");
                        System.exit(2);
                    }
                    targetOs = args[++i];
                } else {
                    targetOs = arg.substring("--os=".length());
                }
                if (targetOs.isEmpty()) {
                    System.err.println("Error: --os requires a value: linux or freebsd");
                    System.exit(2);
                }
                try {
                    targetOs = normalizeOs(targetOs);
                } catch (IllegalArgumentException e) {
                    System.err.println("Error: invalid --os '" + targetOs + "' (expected linux or freebsd)");
                    System.exit(2);
                }
                i++;
            } else if (arg.equals("-v") || arg.equals("--vcpu")) {
                if (i + 1 >= args.length) {
                    System.err.println("Error: -v/--vcpu requires an argument");
                    System.exit(2);
                }
                vcpu = Integer.parseInt(args[++i]);
                i++;
            } else if (arg.equals("-e") || arg.equals("--env")) {
                if (i + 1 >= args.length) {
                    System.err.println("Error: -e/--env requires an argument");
                    System.exit(2);
                }
                envVars.add(args[++i]);
                i++;
            } else if (arg.equals("-f") || arg.equals("--file")) {
                if (i + 1 >= args.length) {
                    System.err.println("Error: -f/--file requires an argument");
                    System.exit(2);
                }
                files.add(args[++i]);
                i++;
            } else if (arg.equals("--otp") && !positionalArgs.isEmpty() && positionalArgs.get(0).equals("key")) {
                if (i + 1 >= args.length) {
                    System.err.println("Error: --otp requires an argument");
                    System.exit(2);
                }
                otpCode = args[++i];
                i++;
            } else if (arg.equals("--rotate") && !positionalArgs.isEmpty() && positionalArgs.get(0).equals("key")) {
                rotateKey = true;
                i++;
            } else if (arg.startsWith("-")) {
                System.err.println("Error: Unknown option: " + arg);
                System.exit(2);
            } else {
                positionalArgs.add(arg);
                i++;
            }
        }

        if (showHelp || positionalArgs.isEmpty()) {
            printHelp();
            System.exit(0);
        }

        String command = positionalArgs.get(0);

        String cliPublicKey = publicKey;
        String cliSecretKey = secretKey;

        // Pre-resolve credentials so --account N is honoured by all subcommands.
        // Only resolve if explicit keys were not supplied via -p/-k flags.
        if (publicKey == null || publicKey.isEmpty() || secretKey == null || secretKey.isEmpty()) {
            String[] creds = resolveCredentials(publicKey, secretKey, accountIndex);
            publicKey = creds[0];
            secretKey = creds[1];
        }

        // Route to subcommand handlers
        switch (command) {
            case "session":
                handleSession(positionalArgs, publicKey, secretKey, networkMode, vcpu, language, targetOs);
                break;
            case "service":
                handleService(positionalArgs, publicKey, secretKey, networkMode, vcpu, envVars, files, targetOs);
                break;
            case "snapshot":
                handleSnapshot(positionalArgs, publicKey, secretKey);
                break;
            case "image":
                handleImage(positionalArgs, publicKey, secretKey);
                break;
            case "key":
                if (rotateKey) {
                    handleKeyRotate(publicKey, secretKey, otpCode, credentialsCsvPath(cliPublicKey, cliSecretKey, accountIndex));
                } else {
                    handleKey(publicKey, secretKey);
                }
                break;
            case "languages":
                handleLanguages(positionalArgs, publicKey, secretKey);
                break;
            default:
                // Default: execute code
                handleExecute(positionalArgs, publicKey, secretKey, language, networkMode, vcpu, envVars, files, targetOs);
                break;
        }
    }

    private static void printHelp() {
        System.out.println("Un - unsandbox.com CLI (Java Synchronous SDK)");
        System.out.println();
        System.out.println("Usage:");
        System.out.println("  java Un [options] <source_file>        Execute code file");
        System.out.println("  java Un [options] -s LANG 'code'       Execute inline code");
        System.out.println("  java Un session [options]              Interactive session");
        System.out.println("  java Un service [options]              Manage services");
        System.out.println("  java Un snapshot [options]             Manage snapshots");
        System.out.println("  java Un image [options]                Manage images");
        System.out.println("  java Un key                            Check API key");
        System.out.println("  java Un key --rotate [--otp CODE]      Rotate API key secret (emailed OTP)");
        System.out.println("  java Un languages [--json]             List supported languages");
        System.out.println();
        System.out.println("Global Options:");
        System.out.println("  -s, --shell LANG      Language for inline code");
        System.out.println("  -e, --env KEY=VAL     Set environment variable");
        System.out.println("  -f, --file FILE       Add input file to /tmp/");
        System.out.println("  -p, --public-key KEY  API public key");
        System.out.println("  -k, --secret-key KEY  API secret key");
        System.out.println("  --account N           Use row N from accounts.csv (overrides env vars)");
        System.out.println("  -n, --network MODE    Network: zerotrust or semitrusted");
        System.out.println("  --os OS               Target OS: linux (default) or freebsd (execute, session, service)");
        System.out.println("  -v, --vcpu N          vCPU count (1-8)");
        System.out.println("  -h, --help            Show help");
        System.out.println();
        System.out.println("Session Options:");
        System.out.println("  --list, -l            List active sessions");
        System.out.println("  --attach ID           Reconnect to session");
        System.out.println("  --kill ID             Terminate session");
        System.out.println("  --freeze ID           Pause session");
        System.out.println("  --unfreeze ID         Resume session");
        System.out.println("  --boost ID            Add resources");
        System.out.println("  --unboost ID          Remove boost");
        System.out.println("  --snapshot ID         Create snapshot");
        System.out.println("  --tmux                Enable persistence with tmux");
        System.out.println("  --screen              Enable persistence with screen");
        System.out.println("  --shell SHELL         Shell/REPL to use");
        System.out.println();
        System.out.println("Service Options:");
        System.out.println("  --list, -l            List all services");
        System.out.println("  --name NAME           Service name (creates new)");
        System.out.println("  --ports PORTS         Comma-separated ports");
        System.out.println("  --bootstrap CMD       Bootstrap command");
        System.out.println("  --info ID             Get service details");
        System.out.println("  --logs ID             Get all logs");
        System.out.println("  --freeze ID           Pause service");
        System.out.println("  --unfreeze ID         Resume service");
        System.out.println("  --destroy ID          Delete service");
        System.out.println("  --lock ID             Prevent deletion");
        System.out.println("  --unlock ID           Allow deletion");
        System.out.println("  --execute ID CMD      Run command in service");
        System.out.println("  --redeploy ID         Re-run bootstrap");
        System.out.println("  --snapshot ID         Create snapshot");
        System.out.println();
        System.out.println("Service Env Subcommand:");
        System.out.println("  java Un service env status ID   Show vault status");
        System.out.println("  java Un service env set ID      Set from stdin");
        System.out.println("  java Un service env export ID   Export to stdout");
        System.out.println("  java Un service env delete ID   Delete vault");
        System.out.println();
        System.out.println("Snapshot Options:");
        System.out.println("  --list, -l            List all snapshots");
        System.out.println("  --info ID             Get snapshot details");
        System.out.println("  --delete ID           Delete snapshot");
        System.out.println("  --lock ID             Prevent deletion");
        System.out.println("  --unlock ID           Allow deletion");
        System.out.println("  --clone ID            Clone snapshot");
        System.out.println();
        System.out.println("Image Options:");
        System.out.println("  --list, -l            List all images");
        System.out.println("  --info ID             Get image details");
        System.out.println("  --delete ID           Delete an image");
        System.out.println("  --lock ID             Lock image to prevent deletion");
        System.out.println("  --unlock ID           Unlock image");
        System.out.println("  --publish ID          Publish from service/snapshot (requires --source-type)");
        System.out.println("  --source-type TYPE    Source type: service or snapshot");
        System.out.println("  --visibility ID MODE  Set visibility (private/unlisted/public)");
        System.out.println("  --spawn ID            Spawn service from image");
        System.out.println("  --clone ID            Clone an image");
        System.out.println("  --name NAME           Name for spawned service or cloned image");
        System.out.println("  --ports PORTS         Ports for spawned service");
        System.out.println();
        System.out.println("Languages Options:");
        System.out.println("  --json                Output as JSON array (for scripts)");
    }

    private static void handleExecute(
        List<String> args,
        String publicKey,
        String secretKey,
        String language,
        String networkMode,
        int vcpu,
        List<String> envVars,
        List<String> files,
        String targetOs
    ) throws Exception {
        String codeOrFile = args.get(0);
        String code;

        if (language != null) {
            // Inline code mode: -s python 'print(1)'
            code = codeOrFile;
        } else {
            // File mode: script.py
            Path filePath = Paths.get(codeOrFile);
            if (!Files.exists(filePath)) {
                System.err.println("Error: File not found: " + codeOrFile);
                System.exit(1);
            }
            code = new String(Files.readAllBytes(filePath), StandardCharsets.UTF_8);
            language = detectLanguage(codeOrFile);
            if (language == null) {
                System.err.println("Error: Cannot detect language from file extension: " + codeOrFile);
                System.exit(1);
            }
        }

        Map<String, Object> result = executeCode(language, code, publicKey, secretKey, targetOs);

        // Print output
        Object stdout = result.get("stdout");
        if (stdout != null && !stdout.toString().isEmpty()) {
            System.out.print(stdout);
        }

        Object stderr = result.get("stderr");
        if (stderr != null && !stderr.toString().isEmpty()) {
            System.err.print(stderr);
        }

        System.out.println("---");
        Object exitCode = result.get("exit_code");
        System.out.println("Exit code: " + (exitCode != null ? exitCode : "0"));

        Object executionTime = result.get("execution_time_ms");
        if (executionTime != null) {
            System.out.println("Execution time: " + executionTime + "ms");
        }

        // Exit with the code's exit code
        if (exitCode != null && exitCode instanceof Number) {
            int code_exit = ((Number) exitCode).intValue();
            if (code_exit != 0) {
                System.exit(code_exit);
            }
        }
    }

    private static void handleSession(
        List<String> args,
        String publicKey,
        String secretKey,
        String networkMode,
        int vcpu,
        String shell,
        String targetOs
    ) throws Exception {
        // Parse session-specific options
        boolean list = false;
        String attachId = null;
        String killId = null;
        String freezeId = null;
        String unfreezeId = null;
        String boostId = null;
        String unboostId = null;
        String snapshotId = null;
        String snapshotName = null;
        boolean hot = false;
        boolean useTmux = false;
        boolean useScreen = false;

        int i = 1; // Skip "session" command
        while (i < args.size()) {
            String arg = args.get(i);
            if (arg.equals("--list") || arg.equals("-l")) {
                list = true;
                i++;
            } else if (arg.equals("--attach")) {
                if (i + 1 >= args.size()) {
                    System.err.println("Error: --attach requires an ID");
                    System.exit(2);
                }
                attachId = args.get(++i);
                i++;
            } else if (arg.equals("--kill")) {
                if (i + 1 >= args.size()) {
                    System.err.println("Error: --kill requires an ID");
                    System.exit(2);
                }
                killId = args.get(++i);
                i++;
            } else if (arg.equals("--freeze")) {
                if (i + 1 >= args.size()) {
                    System.err.println("Error: --freeze requires an ID");
                    System.exit(2);
                }
                freezeId = args.get(++i);
                i++;
            } else if (arg.equals("--unfreeze")) {
                if (i + 1 >= args.size()) {
                    System.err.println("Error: --unfreeze requires an ID");
                    System.exit(2);
                }
                unfreezeId = args.get(++i);
                i++;
            } else if (arg.equals("--boost")) {
                if (i + 1 >= args.size()) {
                    System.err.println("Error: --boost requires an ID");
                    System.exit(2);
                }
                boostId = args.get(++i);
                i++;
            } else if (arg.equals("--unboost")) {
                if (i + 1 >= args.size()) {
                    System.err.println("Error: --unboost requires an ID");
                    System.exit(2);
                }
                unboostId = args.get(++i);
                i++;
            } else if (arg.equals("--snapshot")) {
                if (i + 1 >= args.size()) {
                    System.err.println("Error: --snapshot requires an ID");
                    System.exit(2);
                }
                snapshotId = args.get(++i);
                i++;
            } else if (arg.equals("--snapshot-name")) {
                if (i + 1 >= args.size()) {
                    System.err.println("Error: --snapshot-name requires a name");
                    System.exit(2);
                }
                snapshotName = args.get(++i);
                i++;
            } else if (arg.equals("--hot")) {
                hot = true;
                i++;
            } else if (arg.equals("--tmux")) {
                useTmux = true;
                i++;
            } else if (arg.equals("--screen")) {
                useScreen = true;
                i++;
            } else if (arg.equals("--shell")) {
                if (i + 1 >= args.size()) {
                    System.err.println("Error: --shell requires a value");
                    System.exit(2);
                }
                shell = args.get(++i);
                i++;
            } else {
                i++;
            }
        }

        if (list) {
            List<Map<String, Object>> sessions = listSessions(publicKey, secretKey);
            printSessionList(sessions);
        } else if (attachId != null) {
            Map<String, Object> session = getSession(attachId, publicKey, secretKey);
            System.out.println("Session: " + attachId);
            printMap(session);
        } else if (killId != null) {
            deleteSession(killId, publicKey, secretKey);
            System.out.println("Session terminated: " + killId);
        } else if (freezeId != null) {
            freezeSession(freezeId, publicKey, secretKey);
            System.out.println("Session frozen: " + freezeId);
        } else if (unfreezeId != null) {
            unfreezeSession(unfreezeId, publicKey, secretKey);
            System.out.println("Session unfrozen: " + unfreezeId);
        } else if (boostId != null) {
            boostSession(boostId, publicKey, secretKey);
            System.out.println("Session boosted: " + boostId);
        } else if (unboostId != null) {
            unboostSession(unboostId, publicKey, secretKey);
            System.out.println("Session unboosted: " + unboostId);
        } else if (snapshotId != null) {
            String snapId = sessionSnapshot(snapshotId, publicKey, secretKey, snapshotName, hot);
            System.out.println("Snapshot created: " + snapId);
        } else {
            // Create new session
            Map<String, Object> opts = new LinkedHashMap<>();
            opts.put("network_mode", networkMode);
            if (vcpu > 1) {
                opts.put("vcpu", vcpu);
            }
            if (useTmux) {
                opts.put("multiplexer", "tmux");
            } else if (useScreen) {
                opts.put("multiplexer", "screen");
            }
            if (targetOs != null) {
                opts.put("os", targetOs);
            }

            Map<String, Object> result = createSession(shell != null ? shell : "bash", publicKey, secretKey, opts);
            System.out.println("Session created:");
            printMap(result);
        }
    }

    private static void handleService(
        List<String> args,
        String publicKey,
        String secretKey,
        String networkMode,
        int vcpu,
        List<String> envVars,
        List<String> files,
        String targetOs
    ) throws Exception {
        // Check for "env" subcommand
        if (args.size() > 1 && args.get(1).equals("env")) {
            handleServiceEnv(args, publicKey, secretKey);
            return;
        }

        // Parse service-specific options
        boolean list = false;
        String name = null;
        String ports = null;
        String bootstrap = null;
        String infoId = null;
        String logsId = null;
        String freezeId = null;
        String unfreezeId = null;
        String destroyId = null;
        String lockId = null;
        String unlockId = null;
        String executeId = null;
        String executeCmd = null;
        String redeployId = null;
        String snapshotId = null;

        int i = 1; // Skip "service" command
        while (i < args.size()) {
            String arg = args.get(i);
            if (arg.equals("--list") || arg.equals("-l")) {
                list = true;
                i++;
            } else if (arg.equals("--name")) {
                if (i + 1 >= args.size()) {
                    System.err.println("Error: --name requires a value");
                    System.exit(2);
                }
                name = args.get(++i);
                i++;
            } else if (arg.equals("--ports")) {
                if (i + 1 >= args.size()) {
                    System.err.println("Error: --ports requires a value");
                    System.exit(2);
                }
                ports = args.get(++i);
                i++;
            } else if (arg.equals("--bootstrap")) {
                if (i + 1 >= args.size()) {
                    System.err.println("Error: --bootstrap requires a value");
                    System.exit(2);
                }
                bootstrap = args.get(++i);
                i++;
            } else if (arg.equals("--info")) {
                if (i + 1 >= args.size()) {
                    System.err.println("Error: --info requires an ID");
                    System.exit(2);
                }
                infoId = args.get(++i);
                i++;
            } else if (arg.equals("--logs")) {
                if (i + 1 >= args.size()) {
                    System.err.println("Error: --logs requires an ID");
                    System.exit(2);
                }
                logsId = args.get(++i);
                i++;
            } else if (arg.equals("--freeze")) {
                if (i + 1 >= args.size()) {
                    System.err.println("Error: --freeze requires an ID");
                    System.exit(2);
                }
                freezeId = args.get(++i);
                i++;
            } else if (arg.equals("--unfreeze")) {
                if (i + 1 >= args.size()) {
                    System.err.println("Error: --unfreeze requires an ID");
                    System.exit(2);
                }
                unfreezeId = args.get(++i);
                i++;
            } else if (arg.equals("--destroy")) {
                if (i + 1 >= args.size()) {
                    System.err.println("Error: --destroy requires an ID");
                    System.exit(2);
                }
                destroyId = args.get(++i);
                i++;
            } else if (arg.equals("--lock")) {
                if (i + 1 >= args.size()) {
                    System.err.println("Error: --lock requires an ID");
                    System.exit(2);
                }
                lockId = args.get(++i);
                i++;
            } else if (arg.equals("--unlock")) {
                if (i + 1 >= args.size()) {
                    System.err.println("Error: --unlock requires an ID");
                    System.exit(2);
                }
                unlockId = args.get(++i);
                i++;
            } else if (arg.equals("--execute")) {
                if (i + 2 >= args.size()) {
                    System.err.println("Error: --execute requires ID and command");
                    System.exit(2);
                }
                executeId = args.get(++i);
                executeCmd = args.get(++i);
                i++;
            } else if (arg.equals("--redeploy")) {
                if (i + 1 >= args.size()) {
                    System.err.println("Error: --redeploy requires an ID");
                    System.exit(2);
                }
                redeployId = args.get(++i);
                i++;
            } else if (arg.equals("--snapshot")) {
                if (i + 1 >= args.size()) {
                    System.err.println("Error: --snapshot requires an ID");
                    System.exit(2);
                }
                snapshotId = args.get(++i);
                i++;
            } else {
                i++;
            }
        }

        if (list) {
            List<Map<String, Object>> services = listServices(publicKey, secretKey);
            printServiceList(services);
        } else if (infoId != null) {
            Map<String, Object> service = getService(infoId, publicKey, secretKey);
            printMap(service);
        } else if (logsId != null) {
            Map<String, Object> logs = getServiceLogs(logsId, true, publicKey, secretKey);
            Object content = logs.get("logs");
            if (content != null) {
                System.out.println(content);
            }
        } else if (freezeId != null) {
            freezeService(freezeId, publicKey, secretKey);
            System.out.println("Service frozen: " + freezeId);
        } else if (unfreezeId != null) {
            unfreezeService(unfreezeId, publicKey, secretKey);
            System.out.println("Service unfrozen: " + unfreezeId);
        } else if (destroyId != null) {
            deleteService(destroyId, publicKey, secretKey);
            System.out.println("Service destroyed: " + destroyId);
        } else if (lockId != null) {
            lockService(lockId, publicKey, secretKey);
            System.out.println("Service locked: " + lockId);
        } else if (unlockId != null) {
            unlockService(unlockId, publicKey, secretKey);
            System.out.println("Service unlocked: " + unlockId);
        } else if (executeId != null && executeCmd != null) {
            Map<String, Object> result = executeInService(executeId, executeCmd, publicKey, secretKey);
            Object stdout = result.get("stdout");
            if (stdout != null) {
                System.out.print(stdout);
            }
            Object stderr = result.get("stderr");
            if (stderr != null && !stderr.toString().isEmpty()) {
                System.err.print(stderr);
            }
        } else if (redeployId != null) {
            // Build input_files from -f args
            List<Map<String, String>> inputFiles = buildInputFiles(files);
            redeployService(redeployId, inputFiles, publicKey, secretKey);
            System.out.println("Service redeployed: " + redeployId);
        } else if (snapshotId != null) {
            String snapId = serviceSnapshot(snapshotId, publicKey, secretKey, null);
            System.out.println("Snapshot created: " + snapId);
        } else if (name != null) {
            // Build input_files from -f args
            List<Map<String, String>> inputFiles = buildInputFiles(files);
            Map<String, Object> result = createService(name, ports, bootstrap, false, inputFiles, targetOs, publicKey, secretKey);
            System.out.println("Service created:");
            printMap(result);
        } else {
            System.err.println("Error: No service action specified. Use --list, --name, --info, etc.");
            System.exit(2);
        }
    }

    /**
     * Build input_files list from -f file paths: read each file, base64-encode, return list of maps.
     */
    private static List<Map<String, String>> buildInputFiles(List<String> filePaths) throws IOException {
        if (filePaths == null || filePaths.isEmpty()) {
            return null;
        }
        List<Map<String, String>> inputFiles = new ArrayList<>();
        for (String fpath : filePaths) {
            Path p = Paths.get(fpath);
            byte[] content = Files.readAllBytes(p);
            String encoded = Base64.getEncoder().encodeToString(content);
            Map<String, String> entry = new LinkedHashMap<>();
            entry.put("filename", p.getFileName().toString());
            entry.put("content", encoded);
            inputFiles.add(entry);
        }
        return inputFiles;
    }

    private static void handleServiceEnv(
        List<String> args,
        String publicKey,
        String secretKey
    ) throws Exception {
        if (args.size() < 4) {
            System.err.println("Error: service env requires action and service ID");
            System.err.println("Usage: java Un service env <status|set|export|delete> <service_id>");
            System.exit(2);
        }

        String action = args.get(2);
        String serviceId = args.get(3);

        switch (action) {
            case "status":
                Map<String, Object> status = getServiceEnv(serviceId, publicKey, secretKey);
                printMap(status);
                break;
            case "set":
                // Read env from stdin
                Map<String, String> env = new LinkedHashMap<>();
                try (BufferedReader reader = new BufferedReader(new InputStreamReader(System.in))) {
                    String line;
                    while ((line = reader.readLine()) != null) {
                        line = line.trim();
                        if (line.isEmpty() || line.startsWith("#")) {
                            continue;
                        }
                        int eqIdx = line.indexOf('=');
                        if (eqIdx > 0) {
                            String key = line.substring(0, eqIdx);
                            String value = line.substring(eqIdx + 1);
                            env.put(key, value);
                        }
                    }
                }
                if (!env.isEmpty()) {
                    setServiceEnv(serviceId, env, publicKey, secretKey);
                    System.out.println("Environment set for service: " + serviceId);
                } else {
                    System.err.println("Error: No environment variables provided");
                    System.exit(1);
                }
                break;
            case "export":
                Map<String, Object> exported = exportServiceEnv(serviceId, publicKey, secretKey);
                Object envData = exported.get("env");
                if (envData instanceof Map) {
                    @SuppressWarnings("unchecked")
                    Map<String, Object> envMap = (Map<String, Object>) envData;
                    for (Map.Entry<String, Object> entry : envMap.entrySet()) {
                        System.out.println(entry.getKey() + "=" + entry.getValue());
                    }
                }
                break;
            case "delete":
                deleteServiceEnv(serviceId, null, publicKey, secretKey);
                System.out.println("Environment deleted for service: " + serviceId);
                break;
            default:
                System.err.println("Error: Unknown env action: " + action);
                System.err.println("Valid actions: status, set, export, delete");
                System.exit(2);
        }
    }

    private static void handleSnapshot(
        List<String> args,
        String publicKey,
        String secretKey
    ) throws Exception {
        // Parse snapshot-specific options
        boolean list = false;
        String infoId = null;
        String deleteId = null;
        String lockId = null;
        String unlockId = null;
        String cloneId = null;
        String cloneName = null;
        String cloneType = null;
        String clonePorts = null;

        int i = 1; // Skip "snapshot" command
        while (i < args.size()) {
            String arg = args.get(i);
            if (arg.equals("--list") || arg.equals("-l")) {
                list = true;
                i++;
            } else if (arg.equals("--info")) {
                if (i + 1 >= args.size()) {
                    System.err.println("Error: --info requires an ID");
                    System.exit(2);
                }
                infoId = args.get(++i);
                i++;
            } else if (arg.equals("--delete")) {
                if (i + 1 >= args.size()) {
                    System.err.println("Error: --delete requires an ID");
                    System.exit(2);
                }
                deleteId = args.get(++i);
                i++;
            } else if (arg.equals("--lock")) {
                if (i + 1 >= args.size()) {
                    System.err.println("Error: --lock requires an ID");
                    System.exit(2);
                }
                lockId = args.get(++i);
                i++;
            } else if (arg.equals("--unlock")) {
                if (i + 1 >= args.size()) {
                    System.err.println("Error: --unlock requires an ID");
                    System.exit(2);
                }
                unlockId = args.get(++i);
                i++;
            } else if (arg.equals("--clone")) {
                if (i + 1 >= args.size()) {
                    System.err.println("Error: --clone requires an ID");
                    System.exit(2);
                }
                cloneId = args.get(++i);
                i++;
            } else if (arg.equals("--name")) {
                if (i + 1 >= args.size()) {
                    System.err.println("Error: --name requires a value");
                    System.exit(2);
                }
                cloneName = args.get(++i);
                i++;
            } else if (arg.equals("--type")) {
                if (i + 1 >= args.size()) {
                    System.err.println("Error: --type requires a value");
                    System.exit(2);
                }
                cloneType = args.get(++i);
                i++;
            } else if (arg.equals("--ports")) {
                if (i + 1 >= args.size()) {
                    System.err.println("Error: --ports requires a value");
                    System.exit(2);
                }
                clonePorts = args.get(++i);
                i++;
            } else {
                i++;
            }
        }

        if (list) {
            List<Map<String, Object>> snapshots = listSnapshots(publicKey, secretKey);
            printSnapshotList(snapshots);
        } else if (infoId != null) {
            // Get snapshot info via restore API (or just list and filter)
            List<Map<String, Object>> snapshots = listSnapshots(publicKey, secretKey);
            for (Map<String, Object> snap : snapshots) {
                Object id = snap.get("id");
                if (id != null && id.toString().equals(infoId)) {
                    printMap(snap);
                    return;
                }
            }
            System.err.println("Error: Snapshot not found: " + infoId);
            System.exit(1);
        } else if (deleteId != null) {
            deleteSnapshot(deleteId, publicKey, secretKey);
            System.out.println("Snapshot deleted: " + deleteId);
        } else if (lockId != null) {
            lockSnapshot(lockId, publicKey, secretKey);
            System.out.println("Snapshot locked: " + lockId);
        } else if (unlockId != null) {
            unlockSnapshot(unlockId, publicKey, secretKey);
            System.out.println("Snapshot unlocked: " + unlockId);
        } else if (cloneId != null) {
            Map<String, Object> result = cloneSnapshot(cloneId, cloneName, publicKey, secretKey);
            System.out.println("Snapshot cloned:");
            printMap(result);
        } else {
            System.err.println("Error: No snapshot action specified. Use --list, --info, --delete, etc.");
            System.exit(2);
        }
    }

    private static void handleImage(
        List<String> args,
        String publicKey,
        String secretKey
    ) throws Exception {
        // Parse image-specific options
        boolean list = false;
        String infoId = null;
        String deleteId = null;
        String lockId = null;
        String unlockId = null;
        String publishId = null;
        String sourceType = null;
        String visibilityId = null;
        String visibilityMode = null;
        String spawnId = null;
        String cloneId = null;
        String name = null;
        String ports = null;

        int i = 1; // Skip "image" command
        while (i < args.size()) {
            String arg = args.get(i);
            if (arg.equals("--list") || arg.equals("-l")) {
                list = true;
                i++;
            } else if (arg.equals("--info")) {
                if (i + 1 >= args.size()) {
                    System.err.println("Error: --info requires an ID");
                    System.exit(2);
                }
                infoId = args.get(++i);
                i++;
            } else if (arg.equals("--delete")) {
                if (i + 1 >= args.size()) {
                    System.err.println("Error: --delete requires an ID");
                    System.exit(2);
                }
                deleteId = args.get(++i);
                i++;
            } else if (arg.equals("--lock")) {
                if (i + 1 >= args.size()) {
                    System.err.println("Error: --lock requires an ID");
                    System.exit(2);
                }
                lockId = args.get(++i);
                i++;
            } else if (arg.equals("--unlock")) {
                if (i + 1 >= args.size()) {
                    System.err.println("Error: --unlock requires an ID");
                    System.exit(2);
                }
                unlockId = args.get(++i);
                i++;
            } else if (arg.equals("--publish")) {
                if (i + 1 >= args.size()) {
                    System.err.println("Error: --publish requires an ID");
                    System.exit(2);
                }
                publishId = args.get(++i);
                i++;
            } else if (arg.equals("--source-type")) {
                if (i + 1 >= args.size()) {
                    System.err.println("Error: --source-type requires a value");
                    System.exit(2);
                }
                sourceType = args.get(++i);
                i++;
            } else if (arg.equals("--visibility")) {
                if (i + 2 >= args.size()) {
                    System.err.println("Error: --visibility requires ID and MODE");
                    System.exit(2);
                }
                visibilityId = args.get(++i);
                visibilityMode = args.get(++i);
                i++;
            } else if (arg.equals("--spawn")) {
                if (i + 1 >= args.size()) {
                    System.err.println("Error: --spawn requires an ID");
                    System.exit(2);
                }
                spawnId = args.get(++i);
                i++;
            } else if (arg.equals("--clone")) {
                if (i + 1 >= args.size()) {
                    System.err.println("Error: --clone requires an ID");
                    System.exit(2);
                }
                cloneId = args.get(++i);
                i++;
            } else if (arg.equals("--name")) {
                if (i + 1 >= args.size()) {
                    System.err.println("Error: --name requires a value");
                    System.exit(2);
                }
                name = args.get(++i);
                i++;
            } else if (arg.equals("--ports")) {
                if (i + 1 >= args.size()) {
                    System.err.println("Error: --ports requires a value");
                    System.exit(2);
                }
                ports = args.get(++i);
                i++;
            } else {
                i++;
            }
        }

        if (list) {
            List<Map<String, Object>> images = listImages(null, publicKey, secretKey);
            printImageList(images);
        } else if (infoId != null) {
            Map<String, Object> image = getImage(infoId, publicKey, secretKey);
            printMap(image);
        } else if (deleteId != null) {
            deleteImage(deleteId, publicKey, secretKey);
            System.out.println("Image deleted: " + deleteId);
        } else if (lockId != null) {
            lockImage(lockId, publicKey, secretKey);
            System.out.println("Image locked: " + lockId);
        } else if (unlockId != null) {
            unlockImage(unlockId, publicKey, secretKey);
            System.out.println("Image unlocked: " + unlockId);
        } else if (publishId != null) {
            if (sourceType == null) {
                System.err.println("Error: --publish requires --source-type (service or snapshot)");
                System.exit(2);
            }
            Map<String, Object> result = publishImage(sourceType, publishId, name, publicKey, secretKey);
            System.out.println("Image published:");
            printMap(result);
        } else if (visibilityId != null) {
            if (visibilityMode == null) {
                System.err.println("Error: --visibility requires a mode (private, unlisted, or public)");
                System.exit(2);
            }
            setImageVisibility(visibilityId, visibilityMode, publicKey, secretKey);
            System.out.println("Image visibility set to " + visibilityMode + ": " + visibilityId);
        } else if (spawnId != null) {
            String svcName = name != null ? name : "spawned-service";
            Map<String, Object> result = spawnFromImage(spawnId, svcName, ports, null, null, publicKey, secretKey);
            System.out.println("Service spawned:");
            printMap(result);
        } else if (cloneId != null) {
            String imgName = name != null ? name : cloneId + "-clone";
            Map<String, Object> result = cloneImage(cloneId, imgName, null, publicKey, secretKey);
            System.out.println("Image cloned:");
            printMap(result);
        } else {
            System.err.println("Error: No image action specified. Use --list, --info, --delete, --publish, etc.");
            System.exit(2);
        }
    }

    private static void printImageList(List<Map<String, Object>> images) {
        System.out.printf("%-40s %-20s %-10s %-20s%n", "ID", "NAME", "VISIBILITY", "CREATED");
        for (Map<String, Object> image : images) {
            String id = getStr(image, "image_id", "id");
            String name = getStr(image, "name", "");
            String visibility = getStr(image, "visibility", "");
            String created = getStr(image, "created_at", "created");
            System.out.printf("%-40s %-20s %-10s %-20s%n", id, name, visibility, created);
        }
    }

    private static void handleKey(String publicKey, String secretKey) throws Exception {
        Object[] resp;
        try {
            resp = signedPost(publicKey, secretKey, "/keys/validate", "", null, null);
        } catch (IOException e) {
            System.err.println("Error: " + e.getMessage());
            System.exit(1);
            return;
        }
        int code = (Integer) resp[0];
        @SuppressWarnings("unchecked")
        Map<String, Object> data = (Map<String, Object>) resp[1];
        String state;
        String reason;
        if (code == 200) {
            Object st = data.get("status");
            state = st != null ? st.toString() : (Boolean.TRUE.equals(data.get("valid")) ? "active" : "invalid");
            reason = "status " + state;
        } else if (code == 401) {
            Object err = data.get("error");
            reason = err != null ? err.toString() : "unauthorized";
            if (reason.equals(":expired") || reason.equals("expired")) {
                state = "expired";
            } else if (reason.equals(":suspended") || reason.equals("suspended")) {
                state = "suspended";
            } else {
                state = "invalid";
            }
        } else {
            Object msg = data.get("error") != null ? data.get("error") : data.get("message");
            System.err.println("Error: HTTP " + code + (msg != null ? " " + msg : ""));
            System.exit(4);
            return;
        }
        if (state.equals("expired")) {
            System.out.println("Expired");
            System.out.println("Public Key: " + publicKey);
            System.out.println("To renew: Visit " + PORTAL_BASE + "/keys/extend");
            System.exit(3);
        } else if (state.equals("suspended")) {
            System.out.println("Suspended");
            System.out.println("Public Key: " + publicKey);
            System.exit(3);
        } else if (!state.equals("active")) {
            System.out.println("Invalid");
            System.out.println("Public Key: " + publicKey);
            System.out.println("Reason: " + reason);
            System.exit(3);
        }
        Object tier = data.get("tier");
        Object status = data.get("status");
        String tierStr = "N/A";
        if (tier instanceof Number) {
            tierStr = String.valueOf(((Number) tier).longValue());
        } else if (tier != null) {
            tierStr = tier.toString();
        }
        String expires = "N/A";
        String remaining = "N/A";
        Object vt = data.get("valid_through");
        if (vt instanceof Number) {
            long secs = ((Number) vt).longValue() / 1000;
            expires = java.time.format.DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss'Z'")
                .withZone(java.time.ZoneOffset.UTC).format(java.time.Instant.ofEpochSecond(secs));
            long left = secs - System.currentTimeMillis() / 1000;
            remaining = left <= 0 ? "expired"
                : (left / 86400) + "d " + (left % 86400 / 3600) + "h " + (left % 3600 / 60) + "m";
        }
        System.out.println("Valid");
        System.out.println("Public Key: " + publicKey);
        System.out.println("Tier: " + tierStr);
        System.out.println("Status: " + (status != null ? status.toString() : "N/A"));
        System.out.println("Expires: " + expires);
        System.out.println("Time Remaining: " + remaining);
    }

    /**
     * The accounts.csv the CLI's credentials came from, or null (flags / env vars).
     * Mirrors resolveCredentials' tiers.
     */
    private static Path credentialsCsvPath(String publicKey, String secretKey, int accountIndex) {
        if (publicKey != null && !publicKey.isEmpty() && secretKey != null && !secretKey.isEmpty()) {
            return null;
        }
        int index = accountIndex;
        if (index < 0) {
            String envPk = System.getenv("UNSANDBOX_PUBLIC_KEY");
            String envSk = System.getenv("UNSANDBOX_SECRET_KEY");
            if (envPk != null && !envPk.isEmpty() && envSk != null && !envSk.isEmpty()) {
                return null;
            }
            index = 0;
            String accountEnv = System.getenv("UNSANDBOX_ACCOUNT");
            if (accountEnv != null && !accountEnv.isEmpty()) {
                try {
                    index = Integer.parseInt(accountEnv);
                } catch (NumberFormatException e) {
                    // Use default
                }
            }
        }
        Path[] candidates = {getUnsandboxDir().resolve("accounts.csv"), Paths.get("accounts.csv")};
        for (Path candidate : candidates) {
            if (loadCredentialsFromCsv(candidate, index) != null) {
                return candidate;
            }
        }
        return null;
    }

    /**
     * Replace the secret on the line whose first field is publicKey. Every other
     * line, field, comment and the order stay as they are. Temp file (0600) + rename.
     */
    private static boolean rewriteAccountsCsv(Path path, String publicKey, String newSecret) throws IOException {
        String content = new String(Files.readAllBytes(path), StandardCharsets.UTF_8);
        String[] lines = content.split("\n", -1);
        boolean changed = false;
        for (int j = 0; j < lines.length; j++) {
            String stripped = lines[j].trim();
            if (stripped.isEmpty() || stripped.startsWith("#")) {
                continue;
            }
            String[] fields = lines[j].split(",", -1);
            if (fields.length >= 2 && fields[0].trim().equals(publicKey)) {
                fields[1] = newSecret;
                lines[j] = String.join(",", fields);
                changed = true;
                break;
            }
        }
        if (!changed) {
            return false;
        }
        Path dir = path.toAbsolutePath().getParent();
        Path tmp = dir.resolve("." + path.getFileName() + "." + ProcessHandle.current().pid() + ".tmp");
        java.util.Set<java.nio.file.attribute.PosixFilePermission> perms =
            java.nio.file.attribute.PosixFilePermissions.fromString("rw-------");
        Files.deleteIfExists(tmp);
        Files.createFile(tmp, java.nio.file.attribute.PosixFilePermissions.asFileAttribute(perms));
        Files.write(tmp, String.join("\n", lines).getBytes(StandardCharsets.UTF_8));
        Files.setPosixFilePermissions(tmp, perms);
        Files.move(tmp, path.toAbsolutePath(), java.nio.file.StandardCopyOption.REPLACE_EXISTING,
            java.nio.file.StandardCopyOption.ATOMIC_MOVE);
        return true;
    }

    /** POST /keys/self/rotate-secret with body {}; returns {status, body}. */
    private static Object[] rotatePost(String publicKey, String secretKey, String otp, String challenge) throws IOException {
        return signedPost(publicKey, secretKey, "/keys/self/rotate-secret", "{}", otp, challenge);
    }

    /** Signed POST; returns {statusCode, parsed JSON map} for any status (non-2xx bodies included). */
    private static Object[] signedPost(String publicKey, String secretKey, String path, String body, String otp, String challenge) throws IOException {
        long timestamp = System.currentTimeMillis() / 1000;
        HttpURLConnection conn = (HttpURLConnection) new URL(API_BASE + path).openConnection();
        conn.setRequestMethod("POST");
        conn.setConnectTimeout(DEFAULT_TIMEOUT_MS);
        conn.setReadTimeout(DEFAULT_TIMEOUT_MS);
        conn.setRequestProperty("Authorization", "Bearer " + publicKey);
        conn.setRequestProperty("X-Timestamp", String.valueOf(timestamp));
        conn.setRequestProperty("X-Signature", signRequest(secretKey, timestamp, "POST", path, body));
        conn.setRequestProperty("Content-Type", "application/json");
        if (otp != null) {
            conn.setRequestProperty("X-Sudo-OTP", otp);
            conn.setRequestProperty("X-Sudo-Challenge", challenge != null ? challenge : "");
        }
        conn.setDoOutput(true);
        try (OutputStream os = conn.getOutputStream()) {
            os.write(body.getBytes(StandardCharsets.UTF_8));
        }
        int code = conn.getResponseCode();
        InputStream in = (code >= 200 && code < 300) ? conn.getInputStream() : conn.getErrorStream();
        String text = "";
        if (in != null) {
            try (BufferedReader reader = new BufferedReader(new InputStreamReader(in, StandardCharsets.UTF_8))) {
                StringBuilder sb = new StringBuilder();
                String line;
                while ((line = reader.readLine()) != null) {
                    sb.append(line);
                }
                text = sb.toString();
            }
        }
        Map<String, Object> json;
        try {
            json = parseJson(text);
        } catch (Exception e) {
            json = new HashMap<>();
        }
        if (json == null) {
            json = new HashMap<>();
        }
        return new Object[]{code, json};
    }

    private static boolean stdinIsTerminal() {
        Console console = System.console();
        if (console == null) {
            return false;
        }
        try {
            // Java 22+: a Console may exist even when stdin is redirected.
            return (Boolean) Console.class.getMethod("isTerminal").invoke(console);
        } catch (Exception e) {
            return true;
        }
    }

    /** un key --rotate [--otp CODE]: rotate this key's secret (see CLI_SPEC.md). */
    @SuppressWarnings("unchecked")
    private static void handleKeyRotate(String publicKey, String secretKey, String otp, Path csvPath) throws Exception {
        Path unsandboxDir = getUnsandboxDir();
        java.security.MessageDigest md = java.security.MessageDigest.getInstance("SHA-256");
        StringBuilder hex = new StringBuilder();
        for (byte b : md.digest(publicKey.getBytes(StandardCharsets.UTF_8))) {
            hex.append(String.format("%02x", b));
        }
        Path challengeFile = unsandboxDir.resolve("rotate-" + hex.substring(0, 16) + ".challenge");

        Object[] resp;
        if (otp != null) {
            String challenge;
            try {
                challenge = new String(Files.readAllBytes(challengeFile), StandardCharsets.UTF_8).trim();
            } catch (IOException e) {
                System.err.println("Error: no rotation in progress; run: un key --rotate");
                System.exit(2);
                return;
            }
            resp = rotatePost(publicKey, secretKey, otp.trim(), challenge);
            Files.deleteIfExists(challengeFile);
        } else {
            resp = rotatePost(publicKey, secretKey, null, null);
            if ((Integer) resp[0] == 428) {
                Object cid = ((Map<String, Object>) resp[1]).get("challenge_id");
                String challenge = cid != null ? cid.toString() : "";
                if (stdinIsTerminal()) {
                    System.err.println("One-time code emailed to the key owner.");
                    System.err.print("Code: ");
                    System.err.flush();
                    String line = new BufferedReader(new InputStreamReader(System.in, StandardCharsets.UTF_8)).readLine();
                    String code = line != null ? line.trim() : "";
                    if (code.isEmpty()) {
                        System.err.println("Error: cancelled");
                        System.exit(1);
                    }
                    resp = rotatePost(publicKey, secretKey, code, challenge);
                } else {
                    java.util.Set<java.nio.file.attribute.PosixFilePermission> dirPerms =
                        java.nio.file.attribute.PosixFilePermissions.fromString("rwx------");
                    java.util.Set<java.nio.file.attribute.PosixFilePermission> filePerms =
                        java.nio.file.attribute.PosixFilePermissions.fromString("rw-------");
                    Files.createDirectories(unsandboxDir);
                    Files.setPosixFilePermissions(unsandboxDir, dirPerms);
                    if (!Files.exists(challengeFile)) {
                        Files.createFile(challengeFile,
                            java.nio.file.attribute.PosixFilePermissions.asFileAttribute(filePerms));
                    }
                    Files.setPosixFilePermissions(challengeFile, filePerms);
                    Files.write(challengeFile, challenge.getBytes(StandardCharsets.UTF_8));
                    System.out.println("One-time code emailed to the key owner. Finish with: un key --rotate --otp CODE");
                    return;
                }
            }
        }

        int status = (Integer) resp[0];
        Map<String, Object> data = (Map<String, Object>) resp[1];
        if (status != 200) {
            if (status == 403 && "email_required".equals(data.get("error"))) {
                System.err.println("Error: this key has no email; add one or use POST /keys/recover");
                System.exit(3);
            }
            if (status == 401) {
                System.err.println("Error: authentication failed");
                System.exit(3);
            }
            Object msg = data.get("message");
            if (msg == null) {
                msg = data.get("error");
            }
            System.err.println("Error: " + (msg != null ? msg.toString() : "HTTP " + status));
            System.exit(4);
        }
        Object secret = data.get("secret");
        if (secret == null || secret.toString().isEmpty()) {
            System.err.println("Error: API returned no secret");
            System.exit(4);
        }
        String newSecret = secret.toString();
        if (csvPath != null && rewriteAccountsCsv(csvPath, publicKey, newSecret)) {
            System.out.println("Secret rotated. Updated " + csvPath + " (public key " + publicKey + ").");
        } else {
            System.out.println("New secret (shown once, store it now): " + newSecret);
            System.out.println("Warning: UNSANDBOX_SECRET_KEY / -k still hold the old, now invalid, secret.");
        }
    }

    private static void handleLanguages(List<String> args, String publicKey, String secretKey) throws Exception {
        boolean jsonOutput = false;

        // Parse options
        int i = 1; // Skip "languages" command
        while (i < args.size()) {
            String arg = args.get(i);
            if (arg.equals("--json")) {
                jsonOutput = true;
            }
            i++;
        }

        List<String> languages = getLanguages(publicKey, secretKey);

        if (jsonOutput) {
            // Output as JSON array
            StringBuilder sb = new StringBuilder();
            sb.append("[");
            for (int j = 0; j < languages.size(); j++) {
                if (j > 0) sb.append(",");
                sb.append("\"").append(escapeJsonString(languages.get(j))).append("\"");
            }
            sb.append("]");
            System.out.println(sb.toString());
        } else {
            // Output one language per line
            for (String lang : languages) {
                System.out.println(lang);
            }
        }
    }

    private static void printSessionList(List<Map<String, Object>> sessions) {
        System.out.printf("%-40s %-20s %-10s %-20s%n", "ID", "NAME", "STATUS", "CREATED");
        for (Map<String, Object> session : sessions) {
            String id = getStr(session, "id", "session_id");
            String name = getStr(session, "name", "container_name");
            String status = getStr(session, "status", "state");
            String created = getStr(session, "created_at", "created");
            System.out.printf("%-40s %-20s %-10s %-20s%n", id, name, status, created);
        }
    }

    private static void printServiceList(List<Map<String, Object>> services) {
        System.out.printf("%-40s %-20s %-10s %-20s%n", "ID", "NAME", "STATUS", "CREATED");
        for (Map<String, Object> service : services) {
            String id = getStr(service, "id", "service_id");
            String name = getStr(service, "name", "");
            String status = getStr(service, "state", "status");
            String created = getStr(service, "created_at", "created");
            System.out.printf("%-40s %-20s %-10s %-20s%n", id, name, status, created);
        }
    }

    private static void printSnapshotList(List<Map<String, Object>> snapshots) {
        System.out.printf("%-40s %-20s %-10s %-20s%n", "ID", "NAME", "TYPE", "CREATED");
        for (Map<String, Object> snapshot : snapshots) {
            String id = getStr(snapshot, "id", "snapshot_id");
            String name = getStr(snapshot, "name", "");
            String type = getStr(snapshot, "type", "source_type");
            String created = getStr(snapshot, "created_at", "created");
            System.out.printf("%-40s %-20s %-10s %-20s%n", id, name, type, created);
        }
    }

    private static String getStr(Map<String, Object> map, String key1, String key2) {
        Object val = map.get(key1);
        if (val != null) {
            return val.toString();
        }
        val = map.get(key2);
        if (val != null) {
            return val.toString();
        }
        return "";
    }

    private static void printMap(Map<String, Object> map) {
        for (Map.Entry<String, Object> entry : map.entrySet()) {
            System.out.println(entry.getKey() + ": " + entry.getValue());
        }
    }
}
