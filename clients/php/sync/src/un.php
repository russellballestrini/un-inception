<?php
/**
 * PUBLIC DOMAIN - NO LICENSE, NO WARRANTY
 *
 * unsandbox.com PHP SDK (Synchronous)
 *
 * Library Usage:
 *     require_once 'un.php';
 *     use Unsandbox\Unsandbox;
 *
 *     $client = new Unsandbox();
 *
 *     // Execute code synchronously
 *     $result = $client->executeCode('python', 'print("hello")');
 *
 *     // Execute asynchronously
 *     $jobId = $client->executeAsync('javascript', 'console.log("hello")');
 *
 *     // Wait for job completion with exponential backoff
 *     $result = $client->waitForJob($jobId);
 *
 *     // List all jobs
 *     $jobs = $client->listJobs();
 *
 *     // Get supported languages (cached for 1 hour)
 *     $languages = $client->getLanguages();
 *
 *     // Snapshot operations
 *     $snapshotId = $client->sessionSnapshot($sessionId);
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

namespace Unsandbox;

/**
 * Exception thrown when credentials cannot be found or are invalid.
 */
class CredentialsException extends \Exception {}

/**
 * Exception thrown when an API request fails.
 */
class ApiException extends \Exception {
    private ?array $response;

    public function __construct(string $message, int $code = 0, ?array $response = null, ?\Throwable $previous = null) {
        parent::__construct($message, $code, $previous);
        $this->response = $response;
    }

    public function getResponse(): ?array {
        return $this->response;
    }
}

/**
 * Unsandbox PHP SDK - Synchronous Client
 *
 * Provides methods to execute code, manage jobs, and handle snapshots
 * using the unsandbox.com API.
 */
class Unsandbox {
    private const API_BASE = 'https://api.unsandbox.com';
    private const LANGUAGES_CACHE_TTL = 3600; // 1 hour
    private const POLL_DELAYS_MS = [300, 450, 700, 900, 650, 1600, 2000];

    /**
     * Language detection mapping (file extension -> language)
     */
    private const LANGUAGE_MAP = [
        'py' => 'python',
        'js' => 'javascript',
        'ts' => 'typescript',
        'rb' => 'ruby',
        'php' => 'php',
        'pl' => 'perl',
        'sh' => 'bash',
        'r' => 'r',
        'R' => 'r',
        'lua' => 'lua',
        'go' => 'go',
        'rs' => 'rust',
        'c' => 'c',
        'cpp' => 'cpp',
        'cc' => 'cpp',
        'cxx' => 'cpp',
        'java' => 'java',
        'kt' => 'kotlin',
        'm' => 'objc',
        'cs' => 'csharp',
        'fs' => 'fsharp',
        'hs' => 'haskell',
        'ml' => 'ocaml',
        'clj' => 'clojure',
        'scm' => 'scheme',
        'ss' => 'scheme',
        'erl' => 'erlang',
        'ex' => 'elixir',
        'exs' => 'elixir',
        'jl' => 'julia',
        'd' => 'd',
        'nim' => 'nim',
        'zig' => 'zig',
        'v' => 'v',
        'cr' => 'crystal',
        'dart' => 'dart',
        'groovy' => 'groovy',
        'f90' => 'fortran',
        'f95' => 'fortran',
        'lisp' => 'commonlisp',
        'lsp' => 'commonlisp',
        'cob' => 'cobol',
        'tcl' => 'tcl',
        'raku' => 'raku',
        'pro' => 'prolog',
        'p' => 'prolog',
        '4th' => 'forth',
        'forth' => 'forth',
        'fth' => 'forth',
    ];

    private ?string $defaultPublicKey = null;
    private ?string $defaultSecretKey = null;
    private int $accountIndex = 0;

    /**
     * Create a new Unsandbox client.
     *
     * @param string|null $publicKey Default public key (optional)
     * @param string|null $secretKey Default secret key (optional)
     * @param int $accountIndex Account index for CSV files (default: 0)
     */
    public function __construct(?string $publicKey = null, ?string $secretKey = null, int $accountIndex = 0) {
        $this->defaultPublicKey = $publicKey;
        $this->defaultSecretKey = $secretKey;
        $this->accountIndex = $accountIndex;
    }

    /**
     * Execute code synchronously (awaits until completion).
     *
     * @param string $language Programming language (e.g., "python", "javascript", "go")
     * @param string $code Source code to execute
     * @param string|null $publicKey Optional API key (uses credentials resolution if not provided)
     * @param string|null $secretKey Optional API secret (uses credentials resolution if not provided)
     * @return array Response array containing stdout, stderr, exit code, etc.
     * @throws CredentialsException Missing credentials
     * @throws ApiException API request failed
     */
    public function executeCode(string $language, string $code, ?string $publicKey = null, ?string $secretKey = null): array {
        [$publicKey, $secretKey] = $this->resolveCredentials($publicKey, $secretKey);

        $response = $this->makeRequest(
            'POST',
            '/execute',
            $publicKey,
            $secretKey,
            ['language' => $language, 'code' => $code]
        );

        // If we got a job_id, poll until completion
        $jobId = $response['job_id'] ?? null;
        $status = $response['status'] ?? null;

        if ($jobId && in_array($status, ['pending', 'running'], true)) {
            return $this->waitForJob($jobId, $publicKey, $secretKey);
        }

        return $response;
    }

    /**
     * Execute code asynchronously (returns immediately with job_id).
     *
     * @param string $language Programming language (e.g., "python", "javascript")
     * @param string $code Source code to execute
     * @param string|null $publicKey Optional API key
     * @param string|null $secretKey Optional API secret
     * @return string Job ID string
     * @throws CredentialsException Missing credentials
     * @throws ApiException API request failed
     */
    public function executeAsync(string $language, string $code, ?string $publicKey = null, ?string $secretKey = null): string {
        [$publicKey, $secretKey] = $this->resolveCredentials($publicKey, $secretKey);

        $response = $this->makeRequest(
            'POST',
            '/execute',
            $publicKey,
            $secretKey,
            ['language' => $language, 'code' => $code]
        );

        return $response['job_id'] ?? '';
    }

    /**
     * Get current status/result of a job (single poll, no waiting).
     *
     * @param string $jobId Job ID from executeAsync()
     * @param string|null $publicKey Optional API key
     * @param string|null $secretKey Optional API secret
     * @return array Job response array
     * @throws CredentialsException Missing credentials
     * @throws ApiException API request failed
     */
    public function getJob(string $jobId, ?string $publicKey = null, ?string $secretKey = null): array {
        [$publicKey, $secretKey] = $this->resolveCredentials($publicKey, $secretKey);
        return $this->makeRequest('GET', "/jobs/{$jobId}", $publicKey, $secretKey);
    }

    /**
     * Wait for job completion with exponential backoff polling.
     *
     * Polling delays (ms): [300, 450, 700, 900, 650, 1600, 2000, ...]
     * Cumulative: 300, 750, 1450, 2350, 3000, 4600, 6600ms+
     *
     * @param string $jobId Job ID from executeAsync()
     * @param string|null $publicKey Optional API key
     * @param string|null $secretKey Optional API secret
     * @param int $timeout Maximum wait time in seconds (default: 3600)
     * @return array Final job result when status is terminal (completed, failed, timeout, cancelled)
     * @throws CredentialsException Missing credentials
     * @throws ApiException API request failed or timeout exceeded
     */
    public function waitForJob(string $jobId, ?string $publicKey = null, ?string $secretKey = null, int $timeout = 3600): array {
        [$publicKey, $secretKey] = $this->resolveCredentials($publicKey, $secretKey);
        $pollCount = 0;
        $startTime = time();

        while (true) {
            // Check timeout
            if ((time() - $startTime) >= $timeout) {
                throw new ApiException("Timeout waiting for job {$jobId}");
            }

            // Sleep before polling
            $delayIdx = min($pollCount, count(self::POLL_DELAYS_MS) - 1);
            usleep(self::POLL_DELAYS_MS[$delayIdx] * 1000);
            $pollCount++;

            $response = $this->getJob($jobId, $publicKey, $secretKey);
            $status = $response['status'] ?? null;

            if (in_array($status, ['completed', 'failed', 'timeout', 'cancelled'], true)) {
                return $response;
            }

            // Still running, continue polling
        }
    }

    /**
     * Cancel a running job.
     *
     * @param string $jobId Job ID to cancel
     * @param string|null $publicKey Optional API key
     * @param string|null $secretKey Optional API secret
     * @return array Response array with cancellation confirmation
     * @throws CredentialsException Missing credentials
     * @throws ApiException API request failed
     */
    public function cancelJob(string $jobId, ?string $publicKey = null, ?string $secretKey = null): array {
        [$publicKey, $secretKey] = $this->resolveCredentials($publicKey, $secretKey);
        return $this->makeRequest('DELETE', "/jobs/{$jobId}", $publicKey, $secretKey);
    }

    /**
     * List all jobs for the authenticated account.
     *
     * @param string|null $publicKey Optional API key
     * @param string|null $secretKey Optional API secret
     * @return array List of job arrays
     * @throws CredentialsException Missing credentials
     * @throws ApiException API request failed
     */
    public function listJobs(?string $publicKey = null, ?string $secretKey = null): array {
        [$publicKey, $secretKey] = $this->resolveCredentials($publicKey, $secretKey);
        $response = $this->makeRequest('GET', '/jobs', $publicKey, $secretKey);
        return $response['jobs'] ?? [];
    }

    /**
     * Get list of supported programming languages.
     *
     * Results are cached for 1 hour in ~/.unsandbox/languages.json
     *
     * @param string|null $publicKey Optional API key
     * @param string|null $secretKey Optional API secret
     * @return array List of language identifiers (e.g., ["python", "javascript", "go", ...])
     * @throws CredentialsException Missing credentials
     * @throws ApiException API request failed
     */
    public function getLanguages(?string $publicKey = null, ?string $secretKey = null): array {
        // Try cache first
        $cached = $this->loadLanguagesCache();
        if ($cached !== null) {
            return $cached;
        }

        [$publicKey, $secretKey] = $this->resolveCredentials($publicKey, $secretKey);
        $response = $this->makeRequest('GET', '/languages', $publicKey, $secretKey);
        $languages = $response['languages'] ?? [];

        // Cache the result
        $this->saveLanguagesCache($languages);
        return $languages;
    }

    /**
     * Detect programming language from filename extension.
     *
     * @param string $filename Filename to detect language from (e.g., "script.py")
     * @return string|null Language identifier (e.g., "python") or null if unknown
     */
    public static function detectLanguage(string $filename): ?string {
        if (empty($filename) || strpos($filename, '.') === false) {
            return null;
        }

        $parts = explode('.', $filename);
        $ext = end($parts);

        return self::LANGUAGE_MAP[$ext] ?? null;
    }

    /**
     * Create a snapshot of a session.
     *
     * @param string $sessionId Session ID to snapshot
     * @param string|null $publicKey Optional API key
     * @param string|null $secretKey Optional API secret
     * @param string|null $name Optional snapshot name
     * @param bool $ephemeral If true, snapshot is ephemeral (hot snapshot)
     * @return string Snapshot ID
     * @throws CredentialsException Missing credentials
     * @throws ApiException API request failed
     */
    public function sessionSnapshot(
        string $sessionId,
        ?string $publicKey = null,
        ?string $secretKey = null,
        ?string $name = null,
        bool $ephemeral = false
    ): string {
        [$publicKey, $secretKey] = $this->resolveCredentials($publicKey, $secretKey);

        $data = ['session_id' => $sessionId, 'hot' => $ephemeral];
        if ($name !== null) {
            $data['name'] = $name;
        }

        $response = $this->makeRequest('POST', '/snapshots', $publicKey, $secretKey, $data);
        return $response['snapshot_id'] ?? '';
    }

    /**
     * Create a snapshot of a service.
     *
     * @param string $serviceId Service ID to snapshot
     * @param string|null $publicKey Optional API key
     * @param string|null $secretKey Optional API secret
     * @param string|null $name Optional snapshot name
     * @return string Snapshot ID
     * @throws CredentialsException Missing credentials
     * @throws ApiException API request failed
     */
    public function serviceSnapshot(
        string $serviceId,
        ?string $publicKey = null,
        ?string $secretKey = null,
        ?string $name = null
    ): string {
        [$publicKey, $secretKey] = $this->resolveCredentials($publicKey, $secretKey);

        $data = ['service_id' => $serviceId];
        if ($name !== null) {
            $data['name'] = $name;
        }

        $response = $this->makeRequest('POST', '/snapshots', $publicKey, $secretKey, $data);
        return $response['snapshot_id'] ?? '';
    }

    /**
     * List all snapshots.
     *
     * @param string|null $publicKey Optional API key
     * @param string|null $secretKey Optional API secret
     * @return array List of snapshot arrays
     * @throws CredentialsException Missing credentials
     * @throws ApiException API request failed
     */
    public function listSnapshots(?string $publicKey = null, ?string $secretKey = null): array {
        [$publicKey, $secretKey] = $this->resolveCredentials($publicKey, $secretKey);
        $response = $this->makeRequest('GET', '/snapshots', $publicKey, $secretKey);
        return $response['snapshots'] ?? [];
    }

    /**
     * Restore a snapshot.
     *
     * @param string $snapshotId Snapshot ID to restore
     * @param string|null $publicKey Optional API key
     * @param string|null $secretKey Optional API secret
     * @return array Response array with restored resource info
     * @throws CredentialsException Missing credentials
     * @throws ApiException API request failed
     */
    public function restoreSnapshot(string $snapshotId, ?string $publicKey = null, ?string $secretKey = null): array {
        [$publicKey, $secretKey] = $this->resolveCredentials($publicKey, $secretKey);
        return $this->makeRequest('POST', "/snapshots/{$snapshotId}/restore", $publicKey, $secretKey, []);
    }

    /**
     * Delete a snapshot.
     *
     * @param string $snapshotId Snapshot ID to delete
     * @param string|null $publicKey Optional API key
     * @param string|null $secretKey Optional API secret
     * @return array Response array with deletion confirmation
     * @throws CredentialsException Missing credentials
     * @throws ApiException API request failed
     */
    public function deleteSnapshot(string $snapshotId, ?string $publicKey = null, ?string $secretKey = null): array {
        [$publicKey, $secretKey] = $this->resolveCredentials($publicKey, $secretKey);
        return $this->makeRequest('DELETE', "/snapshots/{$snapshotId}", $publicKey, $secretKey);
    }

    /**
     * Get path to ~/.unsandbox directory, creating if necessary.
     *
     * @return string Path to unsandbox directory
     */
    private function getUnsandboxDir(): string {
        $home = getenv('HOME') ?: (getenv('USERPROFILE') ?: '');
        if (empty($home)) {
            $home = posix_getpwuid(posix_getuid())['dir'] ?? '/tmp';
        }

        $dir = $home . '/.unsandbox';
        if (!is_dir($dir)) {
            mkdir($dir, 0700, true);
        }

        return $dir;
    }

    /**
     * Load credentials from a CSV file.
     *
     * @param string $csvPath Path to CSV file
     * @param int $accountIndex Account index (0-based)
     * @return array|null [publicKey, secretKey] or null if not found
     */
    private function loadCredentialsFromCsv(string $csvPath, int $accountIndex = 0): ?array {
        if (!file_exists($csvPath)) {
            return null;
        }

        $handle = fopen($csvPath, 'r');
        if ($handle === false) {
            return null;
        }

        $currentIndex = 0;
        while (($line = fgets($handle)) !== false) {
            $line = trim($line);
            if (empty($line) || $line[0] === '#') {
                continue;
            }

            if ($currentIndex === $accountIndex) {
                $parts = explode(',', $line);
                if (count($parts) >= 2) {
                    $publicKey = trim($parts[0]);
                    $secretKey = trim($parts[1]);
                    fclose($handle);
                    return [$publicKey, $secretKey];
                }
            }
            $currentIndex++;
        }

        fclose($handle);
        return null;
    }

    /**
     * Resolve credentials from 4-tier priority system.
     *
     * Priority:
     *     1. Method arguments
     *     2. Environment variables
     *     3. ~/.unsandbox/accounts.csv
     *     4. ./accounts.csv
     *
     * @param string|null $publicKey Public key from method argument
     * @param string|null $secretKey Secret key from method argument
     * @return array [publicKey, secretKey]
     * @throws CredentialsException If no credentials found
     */
    private function resolveCredentials(?string $publicKey, ?string $secretKey): array {
        // Tier 1: Method arguments
        if (!empty($publicKey) && !empty($secretKey)) {
            return [$publicKey, $secretKey];
        }

        // Use default keys if provided to constructor
        if (!empty($this->defaultPublicKey) && !empty($this->defaultSecretKey)) {
            return [$this->defaultPublicKey, $this->defaultSecretKey];
        }

        // Tier 2: Environment variables
        $envPk = getenv('UNSANDBOX_PUBLIC_KEY');
        $envSk = getenv('UNSANDBOX_SECRET_KEY');
        if (!empty($envPk) && !empty($envSk)) {
            return [$envPk, $envSk];
        }

        // Determine account index
        $accountIndex = $this->accountIndex;
        $envAccount = getenv('UNSANDBOX_ACCOUNT');
        if ($envAccount !== false && $envAccount !== '') {
            $accountIndex = (int)$envAccount;
        }

        // Tier 3: ~/.unsandbox/accounts.csv
        $unsandboxDir = $this->getUnsandboxDir();
        $creds = $this->loadCredentialsFromCsv($unsandboxDir . '/accounts.csv', $accountIndex);
        if ($creds !== null) {
            return $creds;
        }

        // Tier 4: ./accounts.csv
        $creds = $this->loadCredentialsFromCsv('./accounts.csv', $accountIndex);
        if ($creds !== null) {
            return $creds;
        }

        throw new CredentialsException(
            "No credentials found. Please provide via:\n" .
            "  1. Method arguments (publicKey, secretKey)\n" .
            "  2. Environment variables (UNSANDBOX_PUBLIC_KEY, UNSANDBOX_SECRET_KEY)\n" .
            "  3. ~/.unsandbox/accounts.csv\n" .
            "  4. ./accounts.csv"
        );
    }

    /**
     * Sign a request using HMAC-SHA256.
     *
     * Message format: "timestamp:METHOD:path:body"
     *
     * @param string $secretKey Secret key for signing
     * @param int $timestamp Unix timestamp
     * @param string $method HTTP method
     * @param string $path API endpoint path
     * @param string|null $body Request body (optional)
     * @return string 64-character hex signature
     */
    private function signRequest(string $secretKey, int $timestamp, string $method, string $path, ?string $body = null): string {
        $bodyStr = $body ?? '';
        $message = "{$timestamp}:{$method}:{$path}:{$bodyStr}";
        return hash_hmac('sha256', $message, $secretKey);
    }

    /**
     * Make an authenticated HTTP request to the API.
     *
     * @param string $method HTTP method (GET, POST, DELETE)
     * @param string $path API endpoint path
     * @param string $publicKey API public key
     * @param string $secretKey API secret key
     * @param array|null $data Request data (optional)
     * @return array Decoded JSON response
     * @throws ApiException On network errors or non-2xx response
     */
    private function makeRequest(string $method, string $path, string $publicKey, string $secretKey, ?array $data = null): array {
        $url = self::API_BASE . $path;
        $timestamp = time();
        $body = $data !== null ? json_encode($data) : '';

        $signature = $this->signRequest($secretKey, $timestamp, $method, $path, $data !== null ? $body : null);

        $headers = [
            'Authorization: Bearer ' . $publicKey,
            'X-Timestamp: ' . $timestamp,
            'X-Signature: ' . $signature,
            'Content-Type: application/json',
        ];

        $ch = curl_init();
        curl_setopt($ch, CURLOPT_URL, $url);
        curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
        curl_setopt($ch, CURLOPT_HTTPHEADER, $headers);
        curl_setopt($ch, CURLOPT_TIMEOUT, 120);

        switch ($method) {
            case 'POST':
                curl_setopt($ch, CURLOPT_POST, true);
                curl_setopt($ch, CURLOPT_POSTFIELDS, $body);
                break;
            case 'DELETE':
                curl_setopt($ch, CURLOPT_CUSTOMREQUEST, 'DELETE');
                break;
            case 'GET':
            default:
                // GET is the default
                break;
        }

        $response = curl_exec($ch);
        $httpCode = curl_getinfo($ch, CURLINFO_HTTP_CODE);
        $error = curl_error($ch);
        curl_close($ch);

        if ($response === false) {
            throw new ApiException("cURL error: {$error}");
        }

        $decoded = json_decode($response, true);
        if ($decoded === null && json_last_error() !== JSON_ERROR_NONE) {
            throw new ApiException("Invalid JSON response: " . json_last_error_msg());
        }

        if ($httpCode >= 400) {
            $errorMessage = $decoded['error'] ?? $decoded['message'] ?? "HTTP {$httpCode}";
            throw new ApiException($errorMessage, $httpCode, $decoded);
        }

        return $decoded;
    }

    /**
     * Get path to languages cache file.
     *
     * @return string Path to cache file
     */
    private function getLanguagesCachePath(): string {
        return $this->getUnsandboxDir() . '/languages.json';
    }

    /**
     * Load languages from cache if valid (< 1 hour old).
     *
     * @return array|null List of languages or null if cache invalid
     */
    private function loadLanguagesCache(): ?array {
        $cachePath = $this->getLanguagesCachePath();
        if (!file_exists($cachePath)) {
            return null;
        }

        $mtime = filemtime($cachePath);
        $ageSeconds = time() - $mtime;
        if ($ageSeconds >= self::LANGUAGES_CACHE_TTL) {
            return null;
        }

        $content = file_get_contents($cachePath);
        if ($content === false) {
            return null;
        }

        $data = json_decode($content, true);
        if ($data === null) {
            return null;
        }

        return $data['languages'] ?? null;
    }

    /**
     * Save languages to cache.
     *
     * @param array $languages List of languages
     */
    private function saveLanguagesCache(array $languages): void {
        $cachePath = $this->getLanguagesCachePath();
        $data = [
            'languages' => $languages,
            'timestamp' => time(),
        ];
        file_put_contents($cachePath, json_encode($data));
    }
}
