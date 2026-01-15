<?php
/**
 * PUBLIC DOMAIN - NO LICENSE, NO WARRANTY
 *
 * unsandbox.com PHP SDK (Asynchronous)
 *
 * Library Usage:
 *     require_once 'UnsandboxAsync.php';
 *     use Unsandbox\UnsandboxAsync;
 *
 *     $client = new UnsandboxAsync();
 *
 *     // Execute code (returns a promise)
 *     $promise = $client->executeCode('python', 'print("hello")');
 *     $result = $promise->wait();
 *
 *     // Execute asynchronously
 *     $promise = $client->executeAsync('javascript', 'console.log("hello")');
 *     $jobId = $promise->wait();
 *
 *     // Wait for job completion with exponential backoff
 *     $promise = $client->waitForJob($jobId);
 *     $result = $promise->wait();
 *
 *     // Run multiple requests concurrently
 *     $promises = [
 *         $client->executeCode('python', 'print(1)'),
 *         $client->executeCode('python', 'print(2)'),
 *         $client->executeCode('python', 'print(3)'),
 *     ];
 *     $results = \GuzzleHttp\Promise\Utils::all($promises)->wait();
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
 *
 * Requirements:
 *     - guzzlehttp/guzzle: ^7.0
 *     - guzzlehttp/promises: ^2.0
 */

namespace Unsandbox;

use GuzzleHttp\Client;
use GuzzleHttp\Promise\PromiseInterface;
use GuzzleHttp\Promise\Utils;
use GuzzleHttp\Promise\Create;
use GuzzleHttp\Exception\RequestException;

/**
 * Exception thrown when credentials cannot be found or are invalid.
 */
class AsyncCredentialsException extends \Exception {}

/**
 * Exception thrown when an API request fails.
 */
class AsyncApiException extends \Exception {
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
 * Unsandbox PHP SDK - Asynchronous Client
 *
 * Provides asynchronous methods to execute code, manage jobs, and handle snapshots
 * using the unsandbox.com API. All methods return Guzzle promises for non-blocking I/O.
 */
class UnsandboxAsync {
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

    private Client $httpClient;
    private ?string $defaultPublicKey = null;
    private ?string $defaultSecretKey = null;
    private int $accountIndex = 0;

    /**
     * Create a new UnsandboxAsync client.
     *
     * @param string|null $publicKey Default public key (optional)
     * @param string|null $secretKey Default secret key (optional)
     * @param int $accountIndex Account index for CSV files (default: 0)
     * @param Client|null $httpClient Custom Guzzle client (optional)
     */
    public function __construct(
        ?string $publicKey = null,
        ?string $secretKey = null,
        int $accountIndex = 0,
        ?Client $httpClient = null
    ) {
        $this->defaultPublicKey = $publicKey;
        $this->defaultSecretKey = $secretKey;
        $this->accountIndex = $accountIndex;
        $this->httpClient = $httpClient ?? new Client([
            'base_uri' => self::API_BASE,
            'timeout' => 120,
        ]);
    }

    /**
     * Execute code asynchronously and wait for completion.
     *
     * Returns a promise that resolves to the execution result.
     *
     * @param string $language Programming language (e.g., "python", "javascript", "go")
     * @param string $code Source code to execute
     * @param string|null $publicKey Optional API key (uses credentials resolution if not provided)
     * @param string|null $secretKey Optional API secret (uses credentials resolution if not provided)
     * @return PromiseInterface Resolves to response array containing stdout, stderr, exit code, etc.
     */
    public function executeCode(string $language, string $code, ?string $publicKey = null, ?string $secretKey = null): PromiseInterface {
        [$publicKey, $secretKey] = $this->resolveCredentials($publicKey, $secretKey);

        return $this->makeRequest(
            'POST',
            '/execute',
            $publicKey,
            $secretKey,
            ['language' => $language, 'code' => $code]
        )->then(function (array $response) use ($publicKey, $secretKey) {
            // If we got a job_id, poll until completion
            $jobId = $response['job_id'] ?? null;
            $status = $response['status'] ?? null;

            if ($jobId && in_array($status, ['pending', 'running'], true)) {
                return $this->waitForJob($jobId, $publicKey, $secretKey);
            }

            return $response;
        });
    }

    /**
     * Execute code asynchronously and return job ID immediately.
     *
     * Returns a promise that resolves to the job ID.
     *
     * @param string $language Programming language (e.g., "python", "javascript")
     * @param string $code Source code to execute
     * @param string|null $publicKey Optional API key
     * @param string|null $secretKey Optional API secret
     * @return PromiseInterface Resolves to job ID string
     */
    public function executeAsync(string $language, string $code, ?string $publicKey = null, ?string $secretKey = null): PromiseInterface {
        [$publicKey, $secretKey] = $this->resolveCredentials($publicKey, $secretKey);

        return $this->makeRequest(
            'POST',
            '/execute',
            $publicKey,
            $secretKey,
            ['language' => $language, 'code' => $code]
        )->then(function (array $response) {
            return $response['job_id'] ?? '';
        });
    }

    /**
     * Get current status/result of a job (single poll, no waiting).
     *
     * @param string $jobId Job ID from executeAsync()
     * @param string|null $publicKey Optional API key
     * @param string|null $secretKey Optional API secret
     * @return PromiseInterface Resolves to job response array
     */
    public function getJob(string $jobId, ?string $publicKey = null, ?string $secretKey = null): PromiseInterface {
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
     * @return PromiseInterface Resolves to final job result when status is terminal
     */
    public function waitForJob(string $jobId, ?string $publicKey = null, ?string $secretKey = null, int $timeout = 3600): PromiseInterface {
        [$publicKey, $secretKey] = $this->resolveCredentials($publicKey, $secretKey);

        return $this->pollJob($jobId, $publicKey, $secretKey, 0, time(), $timeout);
    }

    /**
     * Internal polling helper for waitForJob.
     *
     * @param string $jobId Job ID
     * @param string $publicKey API public key
     * @param string $secretKey API secret key
     * @param int $pollCount Current poll iteration
     * @param int $startTime Timestamp when polling started
     * @param int $timeout Maximum wait time
     * @return PromiseInterface Resolves to final job result
     */
    private function pollJob(string $jobId, string $publicKey, string $secretKey, int $pollCount, int $startTime, int $timeout): PromiseInterface {
        // Check timeout
        if ((time() - $startTime) >= $timeout) {
            return Create::rejectionFor(new AsyncApiException("Timeout waiting for job {$jobId}"));
        }

        // Calculate delay
        $delayIdx = min($pollCount, count(self::POLL_DELAYS_MS) - 1);
        $delayMs = self::POLL_DELAYS_MS[$delayIdx];

        // Sleep synchronously (PHP doesn't have native async sleep)
        usleep($delayMs * 1000);

        return $this->getJob($jobId, $publicKey, $secretKey)->then(
            function (array $response) use ($jobId, $publicKey, $secretKey, $pollCount, $startTime, $timeout) {
                $status = $response['status'] ?? null;

                if (in_array($status, ['completed', 'failed', 'timeout', 'cancelled'], true)) {
                    return $response;
                }

                // Still running, continue polling
                return $this->pollJob($jobId, $publicKey, $secretKey, $pollCount + 1, $startTime, $timeout);
            }
        );
    }

    /**
     * Cancel a running job.
     *
     * @param string $jobId Job ID to cancel
     * @param string|null $publicKey Optional API key
     * @param string|null $secretKey Optional API secret
     * @return PromiseInterface Resolves to response array with cancellation confirmation
     */
    public function cancelJob(string $jobId, ?string $publicKey = null, ?string $secretKey = null): PromiseInterface {
        [$publicKey, $secretKey] = $this->resolveCredentials($publicKey, $secretKey);
        return $this->makeRequest('DELETE', "/jobs/{$jobId}", $publicKey, $secretKey);
    }

    /**
     * List all jobs for the authenticated account.
     *
     * @param string|null $publicKey Optional API key
     * @param string|null $secretKey Optional API secret
     * @return PromiseInterface Resolves to list of job arrays
     */
    public function listJobs(?string $publicKey = null, ?string $secretKey = null): PromiseInterface {
        [$publicKey, $secretKey] = $this->resolveCredentials($publicKey, $secretKey);
        return $this->makeRequest('GET', '/jobs', $publicKey, $secretKey)->then(function (array $response) {
            return $response['jobs'] ?? [];
        });
    }

    /**
     * Get list of supported programming languages.
     *
     * Results are cached for 1 hour in ~/.unsandbox/languages.json
     *
     * @param string|null $publicKey Optional API key
     * @param string|null $secretKey Optional API secret
     * @return PromiseInterface Resolves to list of language identifiers
     */
    public function getLanguages(?string $publicKey = null, ?string $secretKey = null): PromiseInterface {
        // Try cache first (synchronously, cache is local)
        $cached = $this->loadLanguagesCache();
        if ($cached !== null) {
            return Create::promiseFor($cached);
        }

        [$publicKey, $secretKey] = $this->resolveCredentials($publicKey, $secretKey);
        return $this->makeRequest('GET', '/languages', $publicKey, $secretKey)->then(function (array $response) {
            $languages = $response['languages'] ?? [];
            // Cache the result
            $this->saveLanguagesCache($languages);
            return $languages;
        });
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
     * @return PromiseInterface Resolves to snapshot ID
     */
    public function sessionSnapshot(
        string $sessionId,
        ?string $publicKey = null,
        ?string $secretKey = null,
        ?string $name = null,
        bool $ephemeral = false
    ): PromiseInterface {
        [$publicKey, $secretKey] = $this->resolveCredentials($publicKey, $secretKey);

        $data = ['session_id' => $sessionId, 'hot' => $ephemeral];
        if ($name !== null) {
            $data['name'] = $name;
        }

        return $this->makeRequest('POST', '/snapshots', $publicKey, $secretKey, $data)->then(function (array $response) {
            return $response['snapshot_id'] ?? '';
        });
    }

    /**
     * Create a snapshot of a service.
     *
     * @param string $serviceId Service ID to snapshot
     * @param string|null $publicKey Optional API key
     * @param string|null $secretKey Optional API secret
     * @param string|null $name Optional snapshot name
     * @return PromiseInterface Resolves to snapshot ID
     */
    public function serviceSnapshot(
        string $serviceId,
        ?string $publicKey = null,
        ?string $secretKey = null,
        ?string $name = null
    ): PromiseInterface {
        [$publicKey, $secretKey] = $this->resolveCredentials($publicKey, $secretKey);

        $data = ['service_id' => $serviceId];
        if ($name !== null) {
            $data['name'] = $name;
        }

        return $this->makeRequest('POST', '/snapshots', $publicKey, $secretKey, $data)->then(function (array $response) {
            return $response['snapshot_id'] ?? '';
        });
    }

    /**
     * List all snapshots.
     *
     * @param string|null $publicKey Optional API key
     * @param string|null $secretKey Optional API secret
     * @return PromiseInterface Resolves to list of snapshot arrays
     */
    public function listSnapshots(?string $publicKey = null, ?string $secretKey = null): PromiseInterface {
        [$publicKey, $secretKey] = $this->resolveCredentials($publicKey, $secretKey);
        return $this->makeRequest('GET', '/snapshots', $publicKey, $secretKey)->then(function (array $response) {
            return $response['snapshots'] ?? [];
        });
    }

    /**
     * Restore a snapshot.
     *
     * @param string $snapshotId Snapshot ID to restore
     * @param string|null $publicKey Optional API key
     * @param string|null $secretKey Optional API secret
     * @return PromiseInterface Resolves to response array with restored resource info
     */
    public function restoreSnapshot(string $snapshotId, ?string $publicKey = null, ?string $secretKey = null): PromiseInterface {
        [$publicKey, $secretKey] = $this->resolveCredentials($publicKey, $secretKey);
        return $this->makeRequest('POST', "/snapshots/{$snapshotId}/restore", $publicKey, $secretKey, []);
    }

    /**
     * Delete a snapshot.
     *
     * @param string $snapshotId Snapshot ID to delete
     * @param string|null $publicKey Optional API key
     * @param string|null $secretKey Optional API secret
     * @return PromiseInterface Resolves to response array with deletion confirmation
     */
    public function deleteSnapshot(string $snapshotId, ?string $publicKey = null, ?string $secretKey = null): PromiseInterface {
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
            $info = posix_getpwuid(posix_getuid());
            $home = $info['dir'] ?? '/tmp';
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
     * @throws AsyncCredentialsException If no credentials found
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

        throw new AsyncCredentialsException(
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
     * Make an authenticated HTTP request to the API asynchronously.
     *
     * @param string $method HTTP method (GET, POST, DELETE)
     * @param string $path API endpoint path
     * @param string $publicKey API public key
     * @param string $secretKey API secret key
     * @param array|null $data Request data (optional)
     * @return PromiseInterface Resolves to decoded JSON response array
     */
    private function makeRequest(string $method, string $path, string $publicKey, string $secretKey, ?array $data = null): PromiseInterface {
        $timestamp = time();
        $body = $data !== null ? json_encode($data) : '';

        $signature = $this->signRequest($secretKey, $timestamp, $method, $path, $data !== null ? $body : null);

        $headers = [
            'Authorization' => 'Bearer ' . $publicKey,
            'X-Timestamp' => (string)$timestamp,
            'X-Signature' => $signature,
            'Content-Type' => 'application/json',
        ];

        $options = [
            'headers' => $headers,
        ];

        if ($data !== null) {
            $options['body'] = $body;
        }

        return $this->httpClient->requestAsync($method, $path, $options)->then(
            function ($response) {
                $body = (string)$response->getBody();
                $decoded = json_decode($body, true);
                if ($decoded === null && json_last_error() !== JSON_ERROR_NONE) {
                    throw new AsyncApiException("Invalid JSON response: " . json_last_error_msg());
                }
                return $decoded;
            },
            function ($exception) {
                if ($exception instanceof RequestException) {
                    $response = $exception->getResponse();
                    if ($response !== null) {
                        $body = (string)$response->getBody();
                        $decoded = json_decode($body, true);
                        $errorMessage = $decoded['error'] ?? $decoded['message'] ?? "HTTP " . $response->getStatusCode();
                        throw new AsyncApiException($errorMessage, $response->getStatusCode(), $decoded, $exception);
                    }
                }
                throw new AsyncApiException($exception->getMessage(), 0, null, $exception);
            }
        );
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
