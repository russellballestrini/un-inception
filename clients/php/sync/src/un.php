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
     * Lock a snapshot to prevent deletion.
     *
     * @param string $snapshotId Snapshot ID to lock
     * @param string|null $publicKey Optional API key
     * @param string|null $secretKey Optional API secret
     * @return array Response array with lock confirmation
     * @throws CredentialsException Missing credentials
     * @throws ApiException API request failed
     */
    public function lockSnapshot(string $snapshotId, ?string $publicKey = null, ?string $secretKey = null): array {
        [$publicKey, $secretKey] = $this->resolveCredentials($publicKey, $secretKey);
        return $this->makeRequest('POST', "/snapshots/{$snapshotId}/lock", $publicKey, $secretKey, []);
    }

    /**
     * Unlock a snapshot to allow deletion.
     *
     * @param string $snapshotId Snapshot ID to unlock
     * @param string|null $publicKey Optional API key
     * @param string|null $secretKey Optional API secret
     * @return array Response array with unlock confirmation
     * @throws CredentialsException Missing credentials
     * @throws ApiException API request failed
     */
    public function unlockSnapshot(string $snapshotId, ?string $publicKey = null, ?string $secretKey = null): array {
        [$publicKey, $secretKey] = $this->resolveCredentials($publicKey, $secretKey);
        return $this->makeRequest('POST', "/snapshots/{$snapshotId}/unlock", $publicKey, $secretKey, []);
    }

    /**
     * Clone a snapshot to create a new session or service.
     *
     * @param string $snapshotId Snapshot ID to clone
     * @param string|null $name Optional name for the new resource
     * @param string|null $publicKey Optional API key
     * @param string|null $secretKey Optional API secret
     * @param array $opts Optional parameters: 'type' (session|service), 'shell', 'ports'
     * @return array Response array with cloned resource info
     * @throws CredentialsException Missing credentials
     * @throws ApiException API request failed
     */
    public function cloneSnapshot(
        string $snapshotId,
        ?string $name = null,
        ?string $publicKey = null,
        ?string $secretKey = null,
        array $opts = []
    ): array {
        [$publicKey, $secretKey] = $this->resolveCredentials($publicKey, $secretKey);

        $data = [];
        if ($name !== null) {
            $data['name'] = $name;
        }
        if (isset($opts['type'])) {
            $data['type'] = $opts['type'];
        }
        if (isset($opts['shell'])) {
            $data['shell'] = $opts['shell'];
        }
        if (isset($opts['ports'])) {
            $data['ports'] = $opts['ports'];
        }

        return $this->makeRequest('POST', "/snapshots/{$snapshotId}/clone", $publicKey, $secretKey, $data);
    }

    // =========================================================================
    // Images Methods (LXD Container Images)
    // =========================================================================

    /**
     * Publish a new image from a session or service container.
     *
     * @param string $sourceType Source type: "session" or "service"
     * @param string $sourceId Session ID or Service ID to publish from
     * @param string|null $name Optional name for the image
     * @param string|null $description Optional description for the image
     * @param string|null $publicKey Optional API key
     * @param string|null $secretKey Optional API secret
     * @return array Response array with image_id and image details
     * @throws CredentialsException Missing credentials
     * @throws ApiException API request failed
     */
    public function imagePublish(
        string $sourceType,
        string $sourceId,
        ?string $name = null,
        ?string $description = null,
        ?string $publicKey = null,
        ?string $secretKey = null
    ): array {
        [$publicKey, $secretKey] = $this->resolveCredentials($publicKey, $secretKey);

        $data = [
            'source_type' => $sourceType,
            'source_id' => $sourceId,
        ];
        if ($name !== null) {
            $data['name'] = $name;
        }
        if ($description !== null) {
            $data['description'] = $description;
        }

        return $this->makeRequest('POST', '/images', $publicKey, $secretKey, $data);
    }

    /**
     * List all images owned by the authenticated account.
     *
     * @param string|null $filterType Optional filter: "owned", "shared", "public", or null for all
     * @param string|null $publicKey Optional API key
     * @param string|null $secretKey Optional API secret
     * @return array List of image arrays
     * @throws CredentialsException Missing credentials
     * @throws ApiException API request failed
     */
    public function listImages(?string $filterType = null, ?string $publicKey = null, ?string $secretKey = null): array {
        [$publicKey, $secretKey] = $this->resolveCredentials($publicKey, $secretKey);

        $path = $filterType !== null ? "/images/{$filterType}" : '/images';
        $response = $this->makeRequest('GET', $path, $publicKey, $secretKey);
        return $response['images'] ?? [];
    }

    /**
     * Get details of a specific image.
     *
     * @param string $imageId Image ID
     * @param string|null $publicKey Optional API key
     * @param string|null $secretKey Optional API secret
     * @return array Image details
     * @throws CredentialsException Missing credentials
     * @throws ApiException API request failed
     */
    public function getImage(string $imageId, ?string $publicKey = null, ?string $secretKey = null): array {
        [$publicKey, $secretKey] = $this->resolveCredentials($publicKey, $secretKey);
        return $this->makeRequest('GET', "/images/{$imageId}", $publicKey, $secretKey);
    }

    /**
     * Delete an image.
     *
     * @param string $imageId Image ID to delete
     * @param string|null $publicKey Optional API key
     * @param string|null $secretKey Optional API secret
     * @return array Response array with deletion confirmation
     * @throws CredentialsException Missing credentials
     * @throws ApiException API request failed
     */
    public function deleteImage(string $imageId, ?string $publicKey = null, ?string $secretKey = null): array {
        [$publicKey, $secretKey] = $this->resolveCredentials($publicKey, $secretKey);
        return $this->makeRequest('DELETE', "/images/{$imageId}", $publicKey, $secretKey);
    }

    /**
     * Lock an image to prevent deletion.
     *
     * @param string $imageId Image ID to lock
     * @param string|null $publicKey Optional API key
     * @param string|null $secretKey Optional API secret
     * @return array Response array with lock confirmation
     * @throws CredentialsException Missing credentials
     * @throws ApiException API request failed
     */
    public function lockImage(string $imageId, ?string $publicKey = null, ?string $secretKey = null): array {
        [$publicKey, $secretKey] = $this->resolveCredentials($publicKey, $secretKey);
        return $this->makeRequest('POST', "/images/{$imageId}/lock", $publicKey, $secretKey, []);
    }

    /**
     * Unlock an image to allow deletion.
     *
     * @param string $imageId Image ID to unlock
     * @param string|null $publicKey Optional API key
     * @param string|null $secretKey Optional API secret
     * @return array Response array with unlock confirmation
     * @throws CredentialsException Missing credentials
     * @throws ApiException API request failed
     */
    public function unlockImage(string $imageId, ?string $publicKey = null, ?string $secretKey = null): array {
        [$publicKey, $secretKey] = $this->resolveCredentials($publicKey, $secretKey);
        return $this->makeRequest('POST', "/images/{$imageId}/unlock", $publicKey, $secretKey, []);
    }

    /**
     * Set visibility of an image.
     *
     * @param string $imageId Image ID
     * @param string $visibility Visibility level: "private" or "public"
     * @param string|null $publicKey Optional API key
     * @param string|null $secretKey Optional API secret
     * @return array Response array with visibility update confirmation
     * @throws CredentialsException Missing credentials
     * @throws ApiException API request failed
     */
    public function setImageVisibility(
        string $imageId,
        string $visibility,
        ?string $publicKey = null,
        ?string $secretKey = null
    ): array {
        [$publicKey, $secretKey] = $this->resolveCredentials($publicKey, $secretKey);
        return $this->makeRequest('POST', "/images/{$imageId}/visibility", $publicKey, $secretKey, [
            'visibility' => $visibility,
        ]);
    }

    /**
     * Grant access to an image for another API key.
     *
     * @param string $imageId Image ID
     * @param string $trustedApiKey The API key to grant access to
     * @param string|null $publicKey Optional API key
     * @param string|null $secretKey Optional API secret
     * @return array Response array with grant confirmation
     * @throws CredentialsException Missing credentials
     * @throws ApiException API request failed
     */
    public function grantImageAccess(
        string $imageId,
        string $trustedApiKey,
        ?string $publicKey = null,
        ?string $secretKey = null
    ): array {
        [$publicKey, $secretKey] = $this->resolveCredentials($publicKey, $secretKey);
        return $this->makeRequest('POST', "/images/{$imageId}/grant", $publicKey, $secretKey, [
            'trusted_api_key' => $trustedApiKey,
        ]);
    }

    /**
     * Revoke access to an image from another API key.
     *
     * @param string $imageId Image ID
     * @param string $trustedApiKey The API key to revoke access from
     * @param string|null $publicKey Optional API key
     * @param string|null $secretKey Optional API secret
     * @return array Response array with revoke confirmation
     * @throws CredentialsException Missing credentials
     * @throws ApiException API request failed
     */
    public function revokeImageAccess(
        string $imageId,
        string $trustedApiKey,
        ?string $publicKey = null,
        ?string $secretKey = null
    ): array {
        [$publicKey, $secretKey] = $this->resolveCredentials($publicKey, $secretKey);
        return $this->makeRequest('POST', "/images/{$imageId}/revoke", $publicKey, $secretKey, [
            'trusted_api_key' => $trustedApiKey,
        ]);
    }

    /**
     * List API keys that have been granted access to an image.
     *
     * @param string $imageId Image ID
     * @param string|null $publicKey Optional API key
     * @param string|null $secretKey Optional API secret
     * @return array List of trusted API key information
     * @throws CredentialsException Missing credentials
     * @throws ApiException API request failed
     */
    public function listImageTrusted(string $imageId, ?string $publicKey = null, ?string $secretKey = null): array {
        [$publicKey, $secretKey] = $this->resolveCredentials($publicKey, $secretKey);
        $response = $this->makeRequest('GET', "/images/{$imageId}/trusted", $publicKey, $secretKey);
        return $response['trusted'] ?? [];
    }

    /**
     * Transfer ownership of an image to another API key.
     *
     * @param string $imageId Image ID to transfer
     * @param string $toApiKey The API key to transfer ownership to
     * @param string|null $publicKey Optional API key
     * @param string|null $secretKey Optional API secret
     * @return array Response array with transfer confirmation
     * @throws CredentialsException Missing credentials
     * @throws ApiException API request failed
     */
    public function transferImage(
        string $imageId,
        string $toApiKey,
        ?string $publicKey = null,
        ?string $secretKey = null
    ): array {
        [$publicKey, $secretKey] = $this->resolveCredentials($publicKey, $secretKey);
        return $this->makeRequest('POST', "/images/{$imageId}/transfer", $publicKey, $secretKey, [
            'to_api_key' => $toApiKey,
        ]);
    }

    /**
     * Spawn a new service from an image.
     *
     * @param string $imageId Image ID to spawn from
     * @param string|null $name Optional service name
     * @param array|string|null $ports Optional port(s) to expose (array of ints or comma-separated string)
     * @param string|null $bootstrap Optional bootstrap command or URL
     * @param string $networkMode Network mode: "zerotrust" or "semitrusted"
     * @param string|null $publicKey Optional API key
     * @param string|null $secretKey Optional API secret
     * @return array Response array with spawned service info
     * @throws CredentialsException Missing credentials
     * @throws ApiException API request failed
     */
    public function spawnFromImage(
        string $imageId,
        ?string $name = null,
        $ports = null,
        ?string $bootstrap = null,
        string $networkMode = 'zerotrust',
        ?string $publicKey = null,
        ?string $secretKey = null
    ): array {
        [$publicKey, $secretKey] = $this->resolveCredentials($publicKey, $secretKey);

        $data = [
            'network_mode' => $networkMode,
        ];
        if ($name !== null) {
            $data['name'] = $name;
        }
        if ($ports !== null) {
            // Convert ports to array if string
            if (is_string($ports)) {
                $ports = array_map('intval', explode(',', $ports));
            }
            $data['ports'] = $ports;
        }
        if ($bootstrap !== null) {
            $data['bootstrap'] = $bootstrap;
        }

        return $this->makeRequest('POST', "/images/{$imageId}/spawn", $publicKey, $secretKey, $data);
    }

    /**
     * Clone an image to create a new image with a different name/description.
     *
     * @param string $imageId Image ID to clone
     * @param string|null $name Optional name for the cloned image
     * @param string|null $description Optional description for the cloned image
     * @param string|null $publicKey Optional API key
     * @param string|null $secretKey Optional API secret
     * @return array Response array with cloned image info
     * @throws CredentialsException Missing credentials
     * @throws ApiException API request failed
     */
    public function cloneImage(
        string $imageId,
        ?string $name = null,
        ?string $description = null,
        ?string $publicKey = null,
        ?string $secretKey = null
    ): array {
        [$publicKey, $secretKey] = $this->resolveCredentials($publicKey, $secretKey);

        $data = [];
        if ($name !== null) {
            $data['name'] = $name;
        }
        if ($description !== null) {
            $data['description'] = $description;
        }

        return $this->makeRequest('POST', "/images/{$imageId}/clone", $publicKey, $secretKey, $data);
    }

    // =========================================================================
    // Session Methods
    // =========================================================================

    /**
     * List all active sessions for the authenticated account.
     *
     * @param string|null $publicKey Optional API key
     * @param string|null $secretKey Optional API secret
     * @return array List of session arrays
     * @throws CredentialsException Missing credentials
     * @throws ApiException API request failed
     */
    public function listSessions(?string $publicKey = null, ?string $secretKey = null): array {
        [$publicKey, $secretKey] = $this->resolveCredentials($publicKey, $secretKey);
        $response = $this->makeRequest('GET', '/sessions', $publicKey, $secretKey);
        return $response['sessions'] ?? [];
    }

    /**
     * Get details of a specific session.
     *
     * @param string $sessionId Session ID
     * @param string|null $publicKey Optional API key
     * @param string|null $secretKey Optional API secret
     * @return array Session details
     * @throws CredentialsException Missing credentials
     * @throws ApiException API request failed
     */
    public function getSession(string $sessionId, ?string $publicKey = null, ?string $secretKey = null): array {
        [$publicKey, $secretKey] = $this->resolveCredentials($publicKey, $secretKey);
        return $this->makeRequest('GET', "/sessions/{$sessionId}", $publicKey, $secretKey);
    }

    /**
     * Create a new interactive session.
     *
     * @param string $language Programming language or shell (e.g., "bash", "python3")
     * @param string|null $publicKey Optional API key
     * @param string|null $secretKey Optional API secret
     * @param array $opts Optional parameters: 'network_mode', 'ttl', 'shell', 'multiplexer', 'vcpu'
     * @return array Session info including session_id and container_name
     * @throws CredentialsException Missing credentials
     * @throws ApiException API request failed
     */
    public function createSession(
        string $language,
        ?string $publicKey = null,
        ?string $secretKey = null,
        array $opts = []
    ): array {
        [$publicKey, $secretKey] = $this->resolveCredentials($publicKey, $secretKey);

        $data = [
            'network_mode' => $opts['network_mode'] ?? 'zerotrust',
            'ttl' => $opts['ttl'] ?? 3600,
        ];
        if (!empty($language)) {
            $data['shell'] = $language;
        }
        if (isset($opts['shell'])) {
            $data['shell'] = $opts['shell'];
        }
        if (isset($opts['multiplexer'])) {
            $data['multiplexer'] = $opts['multiplexer'];
        }
        if (isset($opts['vcpu']) && $opts['vcpu'] > 1) {
            $data['vcpu'] = $opts['vcpu'];
        }
        if (isset($opts['input_files'])) {
            $data['input_files'] = $opts['input_files'];
        }

        return $this->makeRequest('POST', '/sessions', $publicKey, $secretKey, $data);
    }

    /**
     * Delete (terminate) a session.
     *
     * @param string $sessionId Session ID to delete
     * @param string|null $publicKey Optional API key
     * @param string|null $secretKey Optional API secret
     * @return array Response array with deletion confirmation
     * @throws CredentialsException Missing credentials
     * @throws ApiException API request failed
     */
    public function deleteSession(string $sessionId, ?string $publicKey = null, ?string $secretKey = null): array {
        [$publicKey, $secretKey] = $this->resolveCredentials($publicKey, $secretKey);
        return $this->makeRequest('DELETE', "/sessions/{$sessionId}", $publicKey, $secretKey);
    }

    /**
     * Freeze a session to pause execution and reduce resource usage.
     *
     * @param string $sessionId Session ID to freeze
     * @param string|null $publicKey Optional API key
     * @param string|null $secretKey Optional API secret
     * @return array Response array with freeze confirmation
     * @throws CredentialsException Missing credentials
     * @throws ApiException API request failed
     */
    public function freezeSession(string $sessionId, ?string $publicKey = null, ?string $secretKey = null): array {
        [$publicKey, $secretKey] = $this->resolveCredentials($publicKey, $secretKey);
        return $this->makeRequest('POST', "/sessions/{$sessionId}/freeze", $publicKey, $secretKey, []);
    }

    /**
     * Unfreeze a session to resume execution.
     *
     * @param string $sessionId Session ID to unfreeze
     * @param string|null $publicKey Optional API key
     * @param string|null $secretKey Optional API secret
     * @return array Response array with unfreeze confirmation
     * @throws CredentialsException Missing credentials
     * @throws ApiException API request failed
     */
    public function unfreezeSession(string $sessionId, ?string $publicKey = null, ?string $secretKey = null): array {
        [$publicKey, $secretKey] = $this->resolveCredentials($publicKey, $secretKey);
        return $this->makeRequest('POST', "/sessions/{$sessionId}/unfreeze", $publicKey, $secretKey, []);
    }

    /**
     * Boost a session's resources (increase vCPU, memory is derived: vcpu * 2048MB).
     *
     * @param string $sessionId Session ID to boost
     * @param string|null $publicKey Optional API key
     * @param string|null $secretKey Optional API secret
     * @param int $vcpu Number of vCPUs (default: 2)
     * @return array Response array with boost confirmation
     * @throws CredentialsException Missing credentials
     * @throws ApiException API request failed
     */
    public function boostSession(
        string $sessionId,
        ?string $publicKey = null,
        ?string $secretKey = null,
        int $vcpu = 2
    ): array {
        [$publicKey, $secretKey] = $this->resolveCredentials($publicKey, $secretKey);
        return $this->makeRequest('POST', "/sessions/{$sessionId}/boost", $publicKey, $secretKey, ['vcpu' => $vcpu]);
    }

    /**
     * Remove boost from a session (return to base resources).
     *
     * @param string $sessionId Session ID to unboost
     * @param string|null $publicKey Optional API key
     * @param string|null $secretKey Optional API secret
     * @return array Response array with unboost confirmation
     * @throws CredentialsException Missing credentials
     * @throws ApiException API request failed
     */
    public function unboostSession(string $sessionId, ?string $publicKey = null, ?string $secretKey = null): array {
        [$publicKey, $secretKey] = $this->resolveCredentials($publicKey, $secretKey);
        return $this->makeRequest('POST', "/sessions/{$sessionId}/unboost", $publicKey, $secretKey, []);
    }

    /**
     * Execute a shell command in an active session.
     *
     * Note: This is for one-shot commands. For interactive sessions, use WebSocket connection.
     *
     * @param string $sessionId Session ID
     * @param string $command Command to execute
     * @param string|null $publicKey Optional API key
     * @param string|null $secretKey Optional API secret
     * @return array Response array with command output
     * @throws CredentialsException Missing credentials
     * @throws ApiException API request failed
     */
    public function shellSession(
        string $sessionId,
        string $command,
        ?string $publicKey = null,
        ?string $secretKey = null
    ): array {
        [$publicKey, $secretKey] = $this->resolveCredentials($publicKey, $secretKey);
        return $this->makeRequest('POST', "/sessions/{$sessionId}/shell", $publicKey, $secretKey, ['command' => $command]);
    }

    // =========================================================================
    // Service Methods
    // =========================================================================

    /**
     * List all services for the authenticated account.
     *
     * @param string|null $publicKey Optional API key
     * @param string|null $secretKey Optional API secret
     * @return array List of service arrays
     * @throws CredentialsException Missing credentials
     * @throws ApiException API request failed
     */
    public function listServices(?string $publicKey = null, ?string $secretKey = null): array {
        [$publicKey, $secretKey] = $this->resolveCredentials($publicKey, $secretKey);
        $response = $this->makeRequest('GET', '/services', $publicKey, $secretKey);
        return $response['services'] ?? [];
    }

    /**
     * Create a new persistent service.
     *
     * @param string $name Service name
     * @param array|string $ports Port(s) to expose (array of ints or comma-separated string)
     * @param string $bootstrap Bootstrap command or URL
     * @param string|null $publicKey Optional API key
     * @param string|null $secretKey Optional API secret
     * @param array $opts Optional parameters: 'network_mode', 'vcpu', 'service_type', 'custom_domains', 'bootstrap_content', 'input_files'
     * @return array Service info including service_id
     * @throws CredentialsException Missing credentials
     * @throws ApiException API request failed
     */
    public function createService(
        string $name,
        $ports,
        string $bootstrap,
        ?string $publicKey = null,
        ?string $secretKey = null,
        array $opts = []
    ): array {
        [$publicKey, $secretKey] = $this->resolveCredentials($publicKey, $secretKey);

        // Convert ports to array if string
        if (is_string($ports)) {
            $ports = array_map('intval', explode(',', $ports));
        }

        $data = [
            'name' => $name,
            'ports' => $ports,
            'bootstrap' => $bootstrap,
        ];

        if (isset($opts['network_mode'])) {
            $data['network_mode'] = $opts['network_mode'];
        }
        if (isset($opts['vcpu']) && $opts['vcpu'] > 1) {
            $data['vcpu'] = $opts['vcpu'];
        }
        if (isset($opts['service_type'])) {
            $data['service_type'] = $opts['service_type'];
        }
        if (isset($opts['custom_domains'])) {
            $data['custom_domains'] = $opts['custom_domains'];
        }
        if (isset($opts['bootstrap_content'])) {
            $data['bootstrap_content'] = $opts['bootstrap_content'];
        }
        if (isset($opts['input_files'])) {
            $data['input_files'] = $opts['input_files'];
        }

        return $this->makeRequest('POST', '/services', $publicKey, $secretKey, $data);
    }

    /**
     * Get details of a specific service.
     *
     * @param string $serviceId Service ID
     * @param string|null $publicKey Optional API key
     * @param string|null $secretKey Optional API secret
     * @return array Service details
     * @throws CredentialsException Missing credentials
     * @throws ApiException API request failed
     */
    public function getService(string $serviceId, ?string $publicKey = null, ?string $secretKey = null): array {
        [$publicKey, $secretKey] = $this->resolveCredentials($publicKey, $secretKey);
        return $this->makeRequest('GET', "/services/{$serviceId}", $publicKey, $secretKey);
    }

    /**
     * Update a service (e.g., resize vCPU).
     *
     * @param string $serviceId Service ID
     * @param array $opts Update parameters: 'vcpu', 'name', etc.
     * @param string|null $publicKey Optional API key
     * @param string|null $secretKey Optional API secret
     * @return array Updated service details
     * @throws CredentialsException Missing credentials
     * @throws ApiException API request failed
     */
    public function updateService(
        string $serviceId,
        array $opts,
        ?string $publicKey = null,
        ?string $secretKey = null
    ): array {
        [$publicKey, $secretKey] = $this->resolveCredentials($publicKey, $secretKey);
        return $this->makeRequest('PATCH', "/services/{$serviceId}", $publicKey, $secretKey, $opts);
    }

    /**
     * Delete (destroy) a service.
     *
     * @param string $serviceId Service ID to delete
     * @param string|null $publicKey Optional API key
     * @param string|null $secretKey Optional API secret
     * @return array Response array with deletion confirmation
     * @throws CredentialsException Missing credentials
     * @throws ApiException API request failed
     */
    public function deleteService(string $serviceId, ?string $publicKey = null, ?string $secretKey = null): array {
        [$publicKey, $secretKey] = $this->resolveCredentials($publicKey, $secretKey);
        return $this->makeRequest('DELETE', "/services/{$serviceId}", $publicKey, $secretKey);
    }

    /**
     * Freeze a service to pause execution and reduce resource usage.
     *
     * @param string $serviceId Service ID to freeze
     * @param string|null $publicKey Optional API key
     * @param string|null $secretKey Optional API secret
     * @return array Response array with freeze confirmation
     * @throws CredentialsException Missing credentials
     * @throws ApiException API request failed
     */
    public function freezeService(string $serviceId, ?string $publicKey = null, ?string $secretKey = null): array {
        [$publicKey, $secretKey] = $this->resolveCredentials($publicKey, $secretKey);
        return $this->makeRequest('POST', "/services/{$serviceId}/freeze", $publicKey, $secretKey, []);
    }

    /**
     * Unfreeze a service to resume execution.
     *
     * @param string $serviceId Service ID to unfreeze
     * @param string|null $publicKey Optional API key
     * @param string|null $secretKey Optional API secret
     * @return array Response array with unfreeze confirmation
     * @throws CredentialsException Missing credentials
     * @throws ApiException API request failed
     */
    public function unfreezeService(string $serviceId, ?string $publicKey = null, ?string $secretKey = null): array {
        [$publicKey, $secretKey] = $this->resolveCredentials($publicKey, $secretKey);
        return $this->makeRequest('POST', "/services/{$serviceId}/unfreeze", $publicKey, $secretKey, []);
    }

    /**
     * Lock a service to prevent deletion.
     *
     * @param string $serviceId Service ID to lock
     * @param string|null $publicKey Optional API key
     * @param string|null $secretKey Optional API secret
     * @return array Response array with lock confirmation
     * @throws CredentialsException Missing credentials
     * @throws ApiException API request failed
     */
    public function lockService(string $serviceId, ?string $publicKey = null, ?string $secretKey = null): array {
        [$publicKey, $secretKey] = $this->resolveCredentials($publicKey, $secretKey);
        return $this->makeRequest('POST', "/services/{$serviceId}/lock", $publicKey, $secretKey, []);
    }

    /**
     * Unlock a service to allow deletion.
     *
     * @param string $serviceId Service ID to unlock
     * @param string|null $publicKey Optional API key
     * @param string|null $secretKey Optional API secret
     * @return array Response array with unlock confirmation
     * @throws CredentialsException Missing credentials
     * @throws ApiException API request failed
     */
    public function unlockService(string $serviceId, ?string $publicKey = null, ?string $secretKey = null): array {
        [$publicKey, $secretKey] = $this->resolveCredentials($publicKey, $secretKey);
        return $this->makeRequest('POST', "/services/{$serviceId}/unlock", $publicKey, $secretKey, []);
    }

    /**
     * Get bootstrap logs for a service.
     *
     * @param string $serviceId Service ID
     * @param bool $all If true, get all logs; if false, get last 9000 lines
     * @param string|null $publicKey Optional API key
     * @param string|null $secretKey Optional API secret
     * @return array Response array with log content
     * @throws CredentialsException Missing credentials
     * @throws ApiException API request failed
     */
    public function getServiceLogs(
        string $serviceId,
        bool $all = false,
        ?string $publicKey = null,
        ?string $secretKey = null
    ): array {
        [$publicKey, $secretKey] = $this->resolveCredentials($publicKey, $secretKey);
        $path = "/services/{$serviceId}/logs" . ($all ? '?all=true' : '');
        return $this->makeRequest('GET', $path, $publicKey, $secretKey);
    }

    /**
     * Get environment vault status for a service.
     *
     * @param string $serviceId Service ID
     * @param string|null $publicKey Optional API key
     * @param string|null $secretKey Optional API secret
     * @return array Response array with vault status (has_vault, count, updated_at)
     * @throws CredentialsException Missing credentials
     * @throws ApiException API request failed
     */
    public function getServiceEnv(string $serviceId, ?string $publicKey = null, ?string $secretKey = null): array {
        [$publicKey, $secretKey] = $this->resolveCredentials($publicKey, $secretKey);
        return $this->makeRequest('GET', "/services/{$serviceId}/env", $publicKey, $secretKey);
    }

    /**
     * Set environment vault for a service.
     *
     * @param string $serviceId Service ID
     * @param string $env Environment content in .env format (KEY=VALUE\nKEY2=VALUE2)
     * @param string|null $publicKey Optional API key
     * @param string|null $secretKey Optional API secret
     * @return array Response array with update confirmation
     * @throws CredentialsException Missing credentials
     * @throws ApiException API request failed
     */
    public function setServiceEnv(
        string $serviceId,
        string $env,
        ?string $publicKey = null,
        ?string $secretKey = null
    ): array {
        [$publicKey, $secretKey] = $this->resolveCredentials($publicKey, $secretKey);
        return $this->makeRequestRaw('PUT', "/services/{$serviceId}/env", $publicKey, $secretKey, $env, 'text/plain');
    }

    /**
     * Delete environment vault for a service.
     *
     * @param string $serviceId Service ID
     * @param array|null $keys Optional specific keys to delete; if null, deletes entire vault
     * @param string|null $publicKey Optional API key
     * @param string|null $secretKey Optional API secret
     * @return array Response array with deletion confirmation
     * @throws CredentialsException Missing credentials
     * @throws ApiException API request failed
     */
    public function deleteServiceEnv(
        string $serviceId,
        ?array $keys = null,
        ?string $publicKey = null,
        ?string $secretKey = null
    ): array {
        [$publicKey, $secretKey] = $this->resolveCredentials($publicKey, $secretKey);
        return $this->makeRequest('DELETE', "/services/{$serviceId}/env", $publicKey, $secretKey);
    }

    /**
     * Export environment vault for a service (returns .env format).
     *
     * @param string $serviceId Service ID
     * @param string|null $publicKey Optional API key
     * @param string|null $secretKey Optional API secret
     * @return array Response array with env content
     * @throws CredentialsException Missing credentials
     * @throws ApiException API request failed
     */
    public function exportServiceEnv(string $serviceId, ?string $publicKey = null, ?string $secretKey = null): array {
        [$publicKey, $secretKey] = $this->resolveCredentials($publicKey, $secretKey);
        return $this->makeRequest('POST', "/services/{$serviceId}/env/export", $publicKey, $secretKey, []);
    }

    /**
     * Redeploy a service (re-run bootstrap script).
     *
     * @param string $serviceId Service ID
     * @param string|null $publicKey Optional API key
     * @param string|null $secretKey Optional API secret
     * @return array Response array with redeploy confirmation
     * @throws CredentialsException Missing credentials
     * @throws ApiException API request failed
     */
    public function redeployService(string $serviceId, ?string $publicKey = null, ?string $secretKey = null): array {
        [$publicKey, $secretKey] = $this->resolveCredentials($publicKey, $secretKey);
        return $this->makeRequest('POST', "/services/{$serviceId}/redeploy", $publicKey, $secretKey, []);
    }

    /**
     * Execute a command in a running service container.
     *
     * @param string $serviceId Service ID
     * @param string $command Command to execute
     * @param string|null $publicKey Optional API key
     * @param string|null $secretKey Optional API secret
     * @param int $timeout Timeout in milliseconds (default: 30000)
     * @return array Response array with command output (stdout, stderr, exit_code)
     * @throws CredentialsException Missing credentials
     * @throws ApiException API request failed
     */
    public function executeInService(
        string $serviceId,
        string $command,
        ?string $publicKey = null,
        ?string $secretKey = null,
        int $timeout = 30000
    ): array {
        [$publicKey, $secretKey] = $this->resolveCredentials($publicKey, $secretKey);

        $response = $this->makeRequest('POST', "/services/{$serviceId}/execute", $publicKey, $secretKey, [
            'command' => $command,
            'timeout' => $timeout,
        ]);

        // If we got a job_id, poll until completion
        $jobId = $response['job_id'] ?? null;
        if ($jobId) {
            return $this->waitForJob($jobId, $publicKey, $secretKey);
        }

        return $response;
    }

    // =========================================================================
    // Key Validation
    // =========================================================================

    /**
     * Validate API keys.
     *
     * @param string|null $publicKey Optional API key
     * @param string|null $secretKey Optional API secret
     * @return array Response array with validation result
     * @throws CredentialsException Missing credentials
     * @throws ApiException API request failed
     */
    public function validateKeys(?string $publicKey = null, ?string $secretKey = null): array {
        [$publicKey, $secretKey] = $this->resolveCredentials($publicKey, $secretKey);
        return $this->makeRequest('POST', '/keys/validate', $publicKey, $secretKey, []);
    }

    // =========================================================================
    // Image Generation
    // =========================================================================

    /**
     * Generate images from text prompt using AI.
     *
     * @param string $prompt Text description of the image to generate
     * @param string|null $model Model to use (optional)
     * @param string $size Image size (default: "1024x1024")
     * @param string $quality "standard" or "hd" (default: "standard")
     * @param int $n Number of images to generate (default: 1)
     * @param string|null $publicKey API public key
     * @param string|null $secretKey API secret key
     * @return array Result with 'images' array and 'created_at'
     * @throws CredentialsException Missing credentials
     * @throws ApiException API request failed
     */
    public function image(
        string $prompt,
        ?string $model = null,
        string $size = "1024x1024",
        string $quality = "standard",
        int $n = 1,
        ?string $publicKey = null,
        ?string $secretKey = null
    ): array {
        [$publicKey, $secretKey] = $this->resolveCredentials($publicKey, $secretKey);

        $payload = [
            'prompt' => $prompt,
            'size' => $size,
            'quality' => $quality,
            'n' => $n,
        ];
        if ($model !== null) {
            $payload['model'] = $model;
        }

        return $this->makeRequest('POST', '/image', $publicKey, $secretKey, $payload);
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
            case 'PUT':
                curl_setopt($ch, CURLOPT_CUSTOMREQUEST, 'PUT');
                curl_setopt($ch, CURLOPT_POSTFIELDS, $body);
                break;
            case 'PATCH':
                curl_setopt($ch, CURLOPT_CUSTOMREQUEST, 'PATCH');
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
     * Make an authenticated HTTP request with raw body content (non-JSON).
     *
     * @param string $method HTTP method (PUT, POST, etc.)
     * @param string $path API endpoint path
     * @param string $publicKey API public key
     * @param string $secretKey API secret key
     * @param string $body Raw request body
     * @param string $contentType Content type header (e.g., 'text/plain')
     * @return array Decoded JSON response
     * @throws ApiException On network errors or non-2xx response
     */
    private function makeRequestRaw(string $method, string $path, string $publicKey, string $secretKey, string $body, string $contentType = 'text/plain'): array {
        $url = self::API_BASE . $path;
        $timestamp = time();

        $signature = $this->signRequest($secretKey, $timestamp, $method, $path, $body);

        $headers = [
            'Authorization: Bearer ' . $publicKey,
            'X-Timestamp: ' . $timestamp,
            'X-Signature: ' . $signature,
            'Content-Type: ' . $contentType,
        ];

        $ch = curl_init();
        curl_setopt($ch, CURLOPT_URL, $url);
        curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
        curl_setopt($ch, CURLOPT_HTTPHEADER, $headers);
        curl_setopt($ch, CURLOPT_TIMEOUT, 120);
        curl_setopt($ch, CURLOPT_CUSTOMREQUEST, $method);
        curl_setopt($ch, CURLOPT_POSTFIELDS, $body);

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

    // =========================================================================
    // CLI Methods
    // =========================================================================

    /**
     * Main CLI entry point.
     *
     * @param array $argv Command line arguments
     */
    public function cliMain(array $argv): void {
        $script = array_shift($argv);

        if (empty($argv)) {
            $this->cliShowHelp();
            exit(0);
        }

        // Parse global options and determine command
        $globalOpts = $this->cliParseGlobalOptions($argv);
        $args = $globalOpts['args'];
        $opts = $globalOpts['opts'];

        // Handle help flag
        if ($opts['help']) {
            $this->cliShowHelp();
            exit(0);
        }

        // Set credentials from options
        if (!empty($opts['public_key'])) {
            $this->defaultPublicKey = $opts['public_key'];
        }
        if (!empty($opts['secret_key'])) {
            $this->defaultSecretKey = $opts['secret_key'];
        }

        // Determine the command
        if (empty($args)) {
            $this->cliShowHelp();
            exit(0);
        }

        $command = $args[0];

        try {
            switch ($command) {
                case 'session':
                    $this->cliHandleSession(array_slice($args, 1), $opts);
                    break;
                case 'service':
                    $this->cliHandleService(array_slice($args, 1), $opts);
                    break;
                case 'snapshot':
                    $this->cliHandleSnapshot(array_slice($args, 1), $opts);
                    break;
                case 'key':
                    $this->cliHandleKey($opts);
                    break;
                case '-h':
                case '--help':
                case 'help':
                    $this->cliShowHelp();
                    break;
                default:
                    // Default: execute code file or inline code
                    $this->cliHandleExecute($args, $opts);
                    break;
            }
        } catch (CredentialsException $e) {
            $this->cliError("Authentication error: " . $e->getMessage());
            exit(3);
        } catch (ApiException $e) {
            $this->cliError("API error: " . $e->getMessage());
            exit(4);
        } catch (\Exception $e) {
            $this->cliError("Error: " . $e->getMessage());
            exit(1);
        }
    }

    /**
     * Parse global CLI options.
     *
     * @param array $argv Arguments to parse
     * @return array ['opts' => [...], 'args' => [...]]
     */
    private function cliParseGlobalOptions(array $argv): array {
        $opts = [
            'shell' => null,
            'env' => [],
            'files' => [],
            'files_path' => [],
            'artifacts' => false,
            'output' => null,
            'public_key' => null,
            'secret_key' => null,
            'network' => 'zerotrust',
            'vcpu' => 1,
            'yes' => false,
            'help' => false,
        ];
        $args = [];

        $i = 0;
        while ($i < count($argv)) {
            $arg = $argv[$i];

            if ($arg === '-s' || $arg === '--shell') {
                $i++;
                $opts['shell'] = $argv[$i] ?? null;
            } elseif ($arg === '-e' || $arg === '--env') {
                $i++;
                if (isset($argv[$i])) {
                    $opts['env'][] = $argv[$i];
                }
            } elseif ($arg === '-f' || $arg === '--file') {
                $i++;
                if (isset($argv[$i])) {
                    $opts['files'][] = $argv[$i];
                }
            } elseif ($arg === '-F' || $arg === '--file-path') {
                $i++;
                if (isset($argv[$i])) {
                    $opts['files_path'][] = $argv[$i];
                }
            } elseif ($arg === '-a' || $arg === '--artifacts') {
                $opts['artifacts'] = true;
            } elseif ($arg === '-o' || $arg === '--output') {
                $i++;
                $opts['output'] = $argv[$i] ?? null;
            } elseif ($arg === '-p' || $arg === '--public-key') {
                $i++;
                $opts['public_key'] = $argv[$i] ?? null;
            } elseif ($arg === '-k' || $arg === '--secret-key') {
                $i++;
                $opts['secret_key'] = $argv[$i] ?? null;
            } elseif ($arg === '-n' || $arg === '--network') {
                $i++;
                $opts['network'] = $argv[$i] ?? 'zerotrust';
            } elseif ($arg === '-v' || $arg === '--vcpu') {
                $i++;
                $opts['vcpu'] = (int)($argv[$i] ?? 1);
            } elseif ($arg === '-y' || $arg === '--yes') {
                $opts['yes'] = true;
            } elseif ($arg === '-h' || $arg === '--help') {
                $opts['help'] = true;
            } elseif (strpos($arg, '-') !== 0) {
                $args[] = $arg;
            }

            $i++;
        }

        return ['opts' => $opts, 'args' => $args];
    }

    /**
     * Handle execute command (default command).
     *
     * @param array $args Positional arguments
     * @param array $opts Options
     */
    private function cliHandleExecute(array $args, array $opts): void {
        $code = null;
        $language = null;

        // If shell option is set, treat first arg as code
        if (!empty($opts['shell'])) {
            $language = $opts['shell'];
            $code = $args[0] ?? '';
        } else {
            // First arg is a file
            $file = $args[0] ?? '';
            if (empty($file)) {
                $this->cliError("No source file specified");
                exit(2);
            }

            if (!file_exists($file)) {
                $this->cliError("File not found: {$file}");
                exit(2);
            }

            $code = file_get_contents($file);
            if ($code === false) {
                $this->cliError("Cannot read file: {$file}");
                exit(2);
            }

            $language = self::detectLanguage($file);
            if ($language === null) {
                $this->cliError("Cannot detect language from file extension: {$file}");
                exit(2);
            }
        }

        // Build request options
        $requestOpts = [
            'network_mode' => $opts['network'],
        ];
        if ($opts['vcpu'] > 1) {
            $requestOpts['vcpu'] = $opts['vcpu'];
        }

        // Handle environment variables
        $envVars = [];
        foreach ($opts['env'] as $envPair) {
            $pos = strpos($envPair, '=');
            if ($pos !== false) {
                $key = substr($envPair, 0, $pos);
                $val = substr($envPair, $pos + 1);
                $envVars[$key] = $val;
            }
        }

        // Handle input files
        $inputFiles = [];
        foreach ($opts['files'] as $filepath) {
            if (file_exists($filepath)) {
                $content = file_get_contents($filepath);
                $inputFiles[] = [
                    'name' => basename($filepath),
                    'content' => base64_encode($content),
                ];
            }
        }
        foreach ($opts['files_path'] as $filepath) {
            if (file_exists($filepath)) {
                $content = file_get_contents($filepath);
                $inputFiles[] = [
                    'name' => $filepath,
                    'content' => base64_encode($content),
                    'preserve_path' => true,
                ];
            }
        }

        $result = $this->executeCode($language, $code);

        // Output result
        if (isset($result['stdout'])) {
            echo $result['stdout'];
        }
        if (isset($result['stderr']) && !empty($result['stderr'])) {
            fwrite(STDERR, $result['stderr']);
        }
        echo "---\n";
        echo "Exit code: " . ($result['exit_code'] ?? 0) . "\n";
        if (isset($result['execution_time'])) {
            echo "Execution time: " . $result['execution_time'] . "ms\n";
        }

        exit($result['exit_code'] ?? 0);
    }

    /**
     * Handle session subcommand.
     *
     * @param array $args Positional arguments
     * @param array $opts Options
     */
    private function cliHandleSession(array $args, array $opts): void {
        // Parse session-specific options
        $sessionOpts = $this->cliParseSessionOptions($args);

        if ($sessionOpts['list']) {
            $sessions = $this->listSessions();
            $this->cliPrintList($sessions, 'session');
            return;
        }

        if ($sessionOpts['attach']) {
            $session = $this->getSession($sessionOpts['attach']);
            $this->cliPrintSessionInfo($session);
            echo "\nNote: Interactive attachment requires WebSocket support\n";
            return;
        }

        if ($sessionOpts['kill']) {
            $result = $this->deleteSession($sessionOpts['kill']);
            echo "Session terminated: " . $sessionOpts['kill'] . "\n";
            return;
        }

        if ($sessionOpts['freeze']) {
            $result = $this->freezeSession($sessionOpts['freeze']);
            echo "Session frozen: " . $sessionOpts['freeze'] . "\n";
            return;
        }

        if ($sessionOpts['unfreeze']) {
            $result = $this->unfreezeSession($sessionOpts['unfreeze']);
            echo "Session unfrozen: " . $sessionOpts['unfreeze'] . "\n";
            return;
        }

        if ($sessionOpts['boost']) {
            $result = $this->boostSession($sessionOpts['boost'], null, null, $opts['vcpu'] ?: 2);
            echo "Session boosted: " . $sessionOpts['boost'] . "\n";
            return;
        }

        if ($sessionOpts['unboost']) {
            $result = $this->unboostSession($sessionOpts['unboost']);
            echo "Session unboost: " . $sessionOpts['unboost'] . "\n";
            return;
        }

        if ($sessionOpts['snapshot']) {
            $snapshotId = $this->sessionSnapshot(
                $sessionOpts['snapshot'],
                null,
                null,
                $sessionOpts['snapshot_name'],
                $sessionOpts['hot']
            );
            echo "Snapshot created: {$snapshotId}\n";
            return;
        }

        // Create new session
        $createOpts = [
            'network_mode' => $opts['network'],
        ];
        if (!empty($sessionOpts['shell'])) {
            $createOpts['shell'] = $sessionOpts['shell'];
        }
        if ($sessionOpts['tmux']) {
            $createOpts['multiplexer'] = 'tmux';
        } elseif ($sessionOpts['screen']) {
            $createOpts['multiplexer'] = 'screen';
        }
        if ($opts['vcpu'] > 1) {
            $createOpts['vcpu'] = $opts['vcpu'];
        }

        $session = $this->createSession($sessionOpts['shell'] ?? 'bash', null, null, $createOpts);
        $this->cliPrintSessionInfo($session);
    }

    /**
     * Parse session-specific options.
     *
     * @param array $args Arguments to parse
     * @return array Parsed options
     */
    private function cliParseSessionOptions(array $args): array {
        $opts = [
            'list' => false,
            'attach' => null,
            'kill' => null,
            'freeze' => null,
            'unfreeze' => null,
            'boost' => null,
            'unboost' => null,
            'snapshot' => null,
            'snapshot_name' => null,
            'hot' => false,
            'tmux' => false,
            'screen' => false,
            'shell' => null,
            'audit' => false,
        ];

        $i = 0;
        while ($i < count($args)) {
            $arg = $args[$i];

            if ($arg === '--list' || $arg === '-l') {
                $opts['list'] = true;
            } elseif ($arg === '--attach') {
                $i++;
                $opts['attach'] = $args[$i] ?? null;
            } elseif ($arg === '--kill') {
                $i++;
                $opts['kill'] = $args[$i] ?? null;
            } elseif ($arg === '--freeze') {
                $i++;
                $opts['freeze'] = $args[$i] ?? null;
            } elseif ($arg === '--unfreeze') {
                $i++;
                $opts['unfreeze'] = $args[$i] ?? null;
            } elseif ($arg === '--boost') {
                $i++;
                $opts['boost'] = $args[$i] ?? null;
            } elseif ($arg === '--unboost') {
                $i++;
                $opts['unboost'] = $args[$i] ?? null;
            } elseif ($arg === '--snapshot') {
                $i++;
                $opts['snapshot'] = $args[$i] ?? null;
            } elseif ($arg === '--snapshot-name') {
                $i++;
                $opts['snapshot_name'] = $args[$i] ?? null;
            } elseif ($arg === '--hot') {
                $opts['hot'] = true;
            } elseif ($arg === '--tmux') {
                $opts['tmux'] = true;
            } elseif ($arg === '--screen') {
                $opts['screen'] = true;
            } elseif ($arg === '--shell') {
                $i++;
                $opts['shell'] = $args[$i] ?? null;
            } elseif ($arg === '--audit') {
                $opts['audit'] = true;
            }

            $i++;
        }

        return $opts;
    }

    /**
     * Handle service subcommand.
     *
     * @param array $args Positional arguments
     * @param array $opts Global options
     */
    private function cliHandleService(array $args, array $opts): void {
        // Check for env subcommand
        if (!empty($args) && $args[0] === 'env') {
            $this->cliHandleServiceEnv(array_slice($args, 1), $opts);
            return;
        }

        // Parse service-specific options
        $serviceOpts = $this->cliParseServiceOptions($args);

        if ($serviceOpts['list']) {
            $services = $this->listServices();
            $this->cliPrintList($services, 'service');
            return;
        }

        if ($serviceOpts['info']) {
            $service = $this->getService($serviceOpts['info']);
            $this->cliPrintServiceInfo($service);
            return;
        }

        if ($serviceOpts['logs']) {
            $logs = $this->getServiceLogs($serviceOpts['logs'], true);
            echo $logs['logs'] ?? '';
            return;
        }

        if ($serviceOpts['tail']) {
            $logs = $this->getServiceLogs($serviceOpts['tail'], false);
            echo $logs['logs'] ?? '';
            return;
        }

        if ($serviceOpts['freeze']) {
            $result = $this->freezeService($serviceOpts['freeze']);
            echo "Service frozen: " . $serviceOpts['freeze'] . "\n";
            return;
        }

        if ($serviceOpts['unfreeze']) {
            $result = $this->unfreezeService($serviceOpts['unfreeze']);
            echo "Service unfrozen: " . $serviceOpts['unfreeze'] . "\n";
            return;
        }

        if ($serviceOpts['destroy']) {
            if (!$opts['yes']) {
                fwrite(STDERR, "Warning: This will permanently destroy the service. Use -y to confirm.\n");
                exit(2);
            }
            $result = $this->deleteService($serviceOpts['destroy']);
            echo "Service destroyed: " . $serviceOpts['destroy'] . "\n";
            return;
        }

        if ($serviceOpts['lock']) {
            $result = $this->lockService($serviceOpts['lock']);
            echo "Service locked: " . $serviceOpts['lock'] . "\n";
            return;
        }

        if ($serviceOpts['unlock']) {
            $result = $this->unlockService($serviceOpts['unlock']);
            echo "Service unlocked: " . $serviceOpts['unlock'] . "\n";
            return;
        }

        if ($serviceOpts['execute']) {
            $result = $this->executeInService($serviceOpts['execute'], $serviceOpts['execute_cmd']);
            if (isset($result['stdout'])) {
                echo $result['stdout'];
            }
            if (isset($result['stderr']) && !empty($result['stderr'])) {
                fwrite(STDERR, $result['stderr']);
            }
            exit($result['exit_code'] ?? 0);
        }

        if ($serviceOpts['redeploy']) {
            $result = $this->redeployService($serviceOpts['redeploy']);
            echo "Service redeploying: " . $serviceOpts['redeploy'] . "\n";
            return;
        }

        if ($serviceOpts['snapshot']) {
            $snapshotId = $this->serviceSnapshot($serviceOpts['snapshot'], null, null, $serviceOpts['snapshot_name']);
            echo "Snapshot created: {$snapshotId}\n";
            return;
        }

        if ($serviceOpts['resize']) {
            $result = $this->updateService($serviceOpts['resize'], ['vcpu' => $opts['vcpu']]);
            echo "Service resized: " . $serviceOpts['resize'] . "\n";
            return;
        }

        // Create new service
        if (!empty($serviceOpts['name'])) {
            $createOpts = [];
            if (!empty($opts['network'])) {
                $createOpts['network_mode'] = $opts['network'];
            }
            if ($opts['vcpu'] > 1) {
                $createOpts['vcpu'] = $opts['vcpu'];
            }
            if (!empty($serviceOpts['type'])) {
                $createOpts['service_type'] = $serviceOpts['type'];
            }
            if (!empty($serviceOpts['domains'])) {
                $createOpts['custom_domains'] = explode(',', $serviceOpts['domains']);
            }

            // Handle bootstrap from file
            $bootstrap = $serviceOpts['bootstrap'] ?? '';
            if (!empty($serviceOpts['bootstrap_file'])) {
                if (file_exists($serviceOpts['bootstrap_file'])) {
                    $bootstrap = file_get_contents($serviceOpts['bootstrap_file']);
                } else {
                    $this->cliError("Bootstrap file not found: " . $serviceOpts['bootstrap_file']);
                    exit(2);
                }
            }

            // Handle env file
            if (!empty($serviceOpts['env_file'])) {
                if (file_exists($serviceOpts['env_file'])) {
                    // Will be applied after creation
                }
            }

            $service = $this->createService(
                $serviceOpts['name'],
                $serviceOpts['ports'] ?? '80',
                $bootstrap,
                null,
                null,
                $createOpts
            );
            $this->cliPrintServiceInfo($service);
            return;
        }

        $this->cliError("No action specified for service command. Use --list, --info, --name, etc.");
        exit(2);
    }

    /**
     * Parse service-specific options.
     *
     * @param array $args Arguments to parse
     * @return array Parsed options
     */
    private function cliParseServiceOptions(array $args): array {
        $opts = [
            'list' => false,
            'name' => null,
            'ports' => null,
            'domains' => null,
            'type' => null,
            'bootstrap' => null,
            'bootstrap_file' => null,
            'env_file' => null,
            'info' => null,
            'logs' => null,
            'tail' => null,
            'freeze' => null,
            'unfreeze' => null,
            'destroy' => null,
            'lock' => null,
            'unlock' => null,
            'resize' => null,
            'redeploy' => null,
            'execute' => null,
            'execute_cmd' => null,
            'snapshot' => null,
            'snapshot_name' => null,
        ];

        $i = 0;
        while ($i < count($args)) {
            $arg = $args[$i];

            if ($arg === '--list' || $arg === '-l') {
                $opts['list'] = true;
            } elseif ($arg === '--name') {
                $i++;
                $opts['name'] = $args[$i] ?? null;
            } elseif ($arg === '--ports') {
                $i++;
                $opts['ports'] = $args[$i] ?? null;
            } elseif ($arg === '--domains') {
                $i++;
                $opts['domains'] = $args[$i] ?? null;
            } elseif ($arg === '--type') {
                $i++;
                $opts['type'] = $args[$i] ?? null;
            } elseif ($arg === '--bootstrap') {
                $i++;
                $opts['bootstrap'] = $args[$i] ?? null;
            } elseif ($arg === '--bootstrap-file') {
                $i++;
                $opts['bootstrap_file'] = $args[$i] ?? null;
            } elseif ($arg === '--env-file') {
                $i++;
                $opts['env_file'] = $args[$i] ?? null;
            } elseif ($arg === '--info') {
                $i++;
                $opts['info'] = $args[$i] ?? null;
            } elseif ($arg === '--logs') {
                $i++;
                $opts['logs'] = $args[$i] ?? null;
            } elseif ($arg === '--tail') {
                $i++;
                $opts['tail'] = $args[$i] ?? null;
            } elseif ($arg === '--freeze') {
                $i++;
                $opts['freeze'] = $args[$i] ?? null;
            } elseif ($arg === '--unfreeze') {
                $i++;
                $opts['unfreeze'] = $args[$i] ?? null;
            } elseif ($arg === '--destroy') {
                $i++;
                $opts['destroy'] = $args[$i] ?? null;
            } elseif ($arg === '--lock') {
                $i++;
                $opts['lock'] = $args[$i] ?? null;
            } elseif ($arg === '--unlock') {
                $i++;
                $opts['unlock'] = $args[$i] ?? null;
            } elseif ($arg === '--resize') {
                $i++;
                $opts['resize'] = $args[$i] ?? null;
            } elseif ($arg === '--redeploy') {
                $i++;
                $opts['redeploy'] = $args[$i] ?? null;
            } elseif ($arg === '--execute') {
                $i++;
                $opts['execute'] = $args[$i] ?? null;
                // Next argument is the command
                $i++;
                $opts['execute_cmd'] = $args[$i] ?? '';
            } elseif ($arg === '--snapshot') {
                $i++;
                $opts['snapshot'] = $args[$i] ?? null;
            } elseif ($arg === '--snapshot-name') {
                $i++;
                $opts['snapshot_name'] = $args[$i] ?? null;
            }

            $i++;
        }

        return $opts;
    }

    /**
     * Handle service env subcommand.
     *
     * @param array $args Positional arguments
     * @param array $opts Global options
     */
    private function cliHandleServiceEnv(array $args, array $opts): void {
        if (empty($args)) {
            $this->cliError("Usage: service env <status|set|export|delete> <service_id>");
            exit(2);
        }

        $action = $args[0];
        $serviceId = $args[1] ?? null;

        if (empty($serviceId)) {
            $this->cliError("Service ID required");
            exit(2);
        }

        switch ($action) {
            case 'status':
                $result = $this->getServiceEnv($serviceId);
                echo "Has vault: " . ($result['has_vault'] ? 'yes' : 'no') . "\n";
                if (isset($result['count'])) {
                    echo "Variables: " . $result['count'] . "\n";
                }
                if (isset($result['updated_at'])) {
                    echo "Updated: " . $result['updated_at'] . "\n";
                }
                break;

            case 'set':
                // Read from stdin or --env-file
                $envContent = '';
                if (stream_isatty(STDIN)) {
                    $this->cliError("Provide env content via stdin or --env-file");
                    exit(2);
                } else {
                    $envContent = file_get_contents('php://stdin');
                }
                $result = $this->setServiceEnv($serviceId, $envContent);
                echo "Environment vault updated\n";
                break;

            case 'export':
                $result = $this->exportServiceEnv($serviceId);
                echo $result['env'] ?? '';
                break;

            case 'delete':
                if (!$opts['yes']) {
                    fwrite(STDERR, "Warning: This will delete the environment vault. Use -y to confirm.\n");
                    exit(2);
                }
                $result = $this->deleteServiceEnv($serviceId);
                echo "Environment vault deleted\n";
                break;

            default:
                $this->cliError("Unknown env action: {$action}");
                exit(2);
        }
    }

    /**
     * Handle snapshot subcommand.
     *
     * @param array $args Positional arguments
     * @param array $opts Global options
     */
    private function cliHandleSnapshot(array $args, array $opts): void {
        $snapshotOpts = $this->cliParseSnapshotOptions($args);

        if ($snapshotOpts['list']) {
            $snapshots = $this->listSnapshots();
            $this->cliPrintList($snapshots, 'snapshot');
            return;
        }

        if ($snapshotOpts['info']) {
            // Get snapshot info via restore endpoint without actually restoring
            $snapshots = $this->listSnapshots();
            foreach ($snapshots as $snapshot) {
                if (($snapshot['snapshot_id'] ?? $snapshot['id'] ?? '') === $snapshotOpts['info']) {
                    $this->cliPrintSnapshotInfo($snapshot);
                    return;
                }
            }
            $this->cliError("Snapshot not found: " . $snapshotOpts['info']);
            exit(4);
        }

        if ($snapshotOpts['delete']) {
            if (!$opts['yes']) {
                fwrite(STDERR, "Warning: This will permanently delete the snapshot. Use -y to confirm.\n");
                exit(2);
            }
            $result = $this->deleteSnapshot($snapshotOpts['delete']);
            echo "Snapshot deleted: " . $snapshotOpts['delete'] . "\n";
            return;
        }

        if ($snapshotOpts['lock']) {
            $result = $this->lockSnapshot($snapshotOpts['lock']);
            echo "Snapshot locked: " . $snapshotOpts['lock'] . "\n";
            return;
        }

        if ($snapshotOpts['unlock']) {
            $result = $this->unlockSnapshot($snapshotOpts['unlock']);
            echo "Snapshot unlocked: " . $snapshotOpts['unlock'] . "\n";
            return;
        }

        if ($snapshotOpts['clone']) {
            $cloneOpts = [];
            if (!empty($snapshotOpts['type'])) {
                $cloneOpts['type'] = $snapshotOpts['type'];
            }
            if (!empty($snapshotOpts['shell'])) {
                $cloneOpts['shell'] = $snapshotOpts['shell'];
            }
            if (!empty($snapshotOpts['ports'])) {
                $cloneOpts['ports'] = explode(',', $snapshotOpts['ports']);
            }

            $result = $this->cloneSnapshot($snapshotOpts['clone'], $snapshotOpts['name'], null, null, $cloneOpts);
            echo "Snapshot cloned:\n";
            echo json_encode($result, JSON_PRETTY_PRINT) . "\n";
            return;
        }

        $this->cliError("No action specified for snapshot command. Use --list, --info, --delete, --clone, etc.");
        exit(2);
    }

    /**
     * Parse snapshot-specific options.
     *
     * @param array $args Arguments to parse
     * @return array Parsed options
     */
    private function cliParseSnapshotOptions(array $args): array {
        $opts = [
            'list' => false,
            'info' => null,
            'delete' => null,
            'lock' => null,
            'unlock' => null,
            'clone' => null,
            'type' => null,
            'name' => null,
            'shell' => null,
            'ports' => null,
        ];

        $i = 0;
        while ($i < count($args)) {
            $arg = $args[$i];

            if ($arg === '--list' || $arg === '-l') {
                $opts['list'] = true;
            } elseif ($arg === '--info') {
                $i++;
                $opts['info'] = $args[$i] ?? null;
            } elseif ($arg === '--delete') {
                $i++;
                $opts['delete'] = $args[$i] ?? null;
            } elseif ($arg === '--lock') {
                $i++;
                $opts['lock'] = $args[$i] ?? null;
            } elseif ($arg === '--unlock') {
                $i++;
                $opts['unlock'] = $args[$i] ?? null;
            } elseif ($arg === '--clone') {
                $i++;
                $opts['clone'] = $args[$i] ?? null;
            } elseif ($arg === '--type') {
                $i++;
                $opts['type'] = $args[$i] ?? null;
            } elseif ($arg === '--name') {
                $i++;
                $opts['name'] = $args[$i] ?? null;
            } elseif ($arg === '--shell') {
                $i++;
                $opts['shell'] = $args[$i] ?? null;
            } elseif ($arg === '--ports') {
                $i++;
                $opts['ports'] = $args[$i] ?? null;
            }

            $i++;
        }

        return $opts;
    }

    /**
     * Handle key subcommand.
     *
     * @param array $opts Global options
     */
    private function cliHandleKey(array $opts): void {
        $result = $this->validateKeys();
        echo "API Key Valid: " . ($result['valid'] ? 'yes' : 'no') . "\n";
        if (isset($result['account_id'])) {
            echo "Account ID: " . $result['account_id'] . "\n";
        }
        if (isset($result['email'])) {
            echo "Email: " . $result['email'] . "\n";
        }
    }

    /**
     * Print list of resources in tabular format.
     *
     * @param array $items Items to print
     * @param string $type Resource type (session, service, snapshot)
     */
    private function cliPrintList(array $items, string $type): void {
        if (empty($items)) {
            echo "No {$type}s found.\n";
            return;
        }

        // Print header
        printf("%-40s  %-20s  %-12s  %s\n", 'ID', 'NAME', 'STATUS', 'CREATED');
        echo str_repeat('-', 90) . "\n";

        foreach ($items as $item) {
            $id = $item["{$type}_id"] ?? $item['id'] ?? 'N/A';
            $name = $item['name'] ?? 'N/A';
            $status = $item['status'] ?? 'N/A';
            $created = $item['created_at'] ?? $item['created'] ?? 'N/A';

            printf("%-40s  %-20s  %-12s  %s\n", $id, $name, $status, $created);
        }
    }

    /**
     * Print session information.
     *
     * @param array $session Session data
     */
    private function cliPrintSessionInfo(array $session): void {
        echo "Session ID: " . ($session['session_id'] ?? $session['id'] ?? 'N/A') . "\n";
        if (isset($session['container_name'])) {
            echo "Container: " . $session['container_name'] . "\n";
        }
        if (isset($session['status'])) {
            echo "Status: " . $session['status'] . "\n";
        }
        if (isset($session['shell'])) {
            echo "Shell: " . $session['shell'] . "\n";
        }
        if (isset($session['network_mode'])) {
            echo "Network: " . $session['network_mode'] . "\n";
        }
        if (isset($session['websocket_url'])) {
            echo "WebSocket URL: " . $session['websocket_url'] . "\n";
        }
    }

    /**
     * Print service information.
     *
     * @param array $service Service data
     */
    private function cliPrintServiceInfo(array $service): void {
        echo "Service ID: " . ($service['service_id'] ?? $service['id'] ?? 'N/A') . "\n";
        if (isset($service['name'])) {
            echo "Name: " . $service['name'] . "\n";
        }
        if (isset($service['status'])) {
            echo "Status: " . $service['status'] . "\n";
        }
        if (isset($service['ports'])) {
            echo "Ports: " . (is_array($service['ports']) ? implode(', ', $service['ports']) : $service['ports']) . "\n";
        }
        if (isset($service['url'])) {
            echo "URL: " . $service['url'] . "\n";
        }
        if (isset($service['network_mode'])) {
            echo "Network: " . $service['network_mode'] . "\n";
        }
    }

    /**
     * Print snapshot information.
     *
     * @param array $snapshot Snapshot data
     */
    private function cliPrintSnapshotInfo(array $snapshot): void {
        echo "Snapshot ID: " . ($snapshot['snapshot_id'] ?? $snapshot['id'] ?? 'N/A') . "\n";
        if (isset($snapshot['name'])) {
            echo "Name: " . $snapshot['name'] . "\n";
        }
        if (isset($snapshot['type'])) {
            echo "Type: " . $snapshot['type'] . "\n";
        }
        if (isset($snapshot['status'])) {
            echo "Status: " . $snapshot['status'] . "\n";
        }
        if (isset($snapshot['size'])) {
            echo "Size: " . $snapshot['size'] . "\n";
        }
        if (isset($snapshot['locked'])) {
            echo "Locked: " . ($snapshot['locked'] ? 'yes' : 'no') . "\n";
        }
        if (isset($snapshot['created_at'])) {
            echo "Created: " . $snapshot['created_at'] . "\n";
        }
    }

    /**
     * Print error message to stderr.
     *
     * @param string $message Error message
     */
    private function cliError(string $message): void {
        fwrite(STDERR, "Error: {$message}\n");
    }

    /**
     * Show CLI help.
     */
    private function cliShowHelp(): void {
        $help = <<<HELP
Unsandbox PHP CLI

USAGE:
    php un.php [options] <source_file>
    php un.php -s <language> '<code>'
    php un.php session [options]
    php un.php service [options]
    php un.php snapshot [options]
    php un.php key

GLOBAL OPTIONS:
    -s, --shell LANG        Language for inline code execution
    -e, --env KEY=VAL       Set environment variable (can be repeated)
    -f, --file FILE         Add input file to /tmp/ (can be repeated)
    -F, --file-path FILE    Add input file preserving path (can be repeated)
    -a, --artifacts         Return compiled artifacts
    -o, --output DIR        Output directory for artifacts
    -p, --public-key KEY    API public key
    -k, --secret-key KEY    API secret key
    -n, --network MODE      Network mode: zerotrust (default) or semitrusted
    -v, --vcpu N            vCPU count (1-8)
    -y, --yes               Skip confirmation prompts
    -h, --help              Show this help

COMMANDS:
    <file>                  Execute code file (default command)
    session                 Manage interactive sessions
    service                 Manage persistent services
    snapshot                Manage snapshots
    key                     Validate API key

SESSION OPTIONS:
    --list, -l              List active sessions
    --attach ID             Reconnect to session
    --kill ID               Terminate session
    --freeze ID             Pause session
    --unfreeze ID           Resume session
    --boost ID              Boost session resources
    --unboost ID            Remove session boost
    --snapshot ID           Create session snapshot
    --snapshot-name NAME    Name for snapshot
    --hot                   Hot snapshot (no freeze)
    --tmux                  Use tmux for persistence
    --screen                Use screen for persistence
    --shell SHELL           Shell/REPL to use

SERVICE OPTIONS:
    --list, -l              List all services
    --name NAME             Create service with name
    --ports PORTS           Ports to expose (comma-separated)
    --domains DOMAINS       Custom domains (comma-separated)
    --type TYPE             Service type (minecraft, tcp, udp)
    --bootstrap CMD         Bootstrap command
    --bootstrap-file FILE   Bootstrap from file
    --env-file FILE         Load environment from file
    --info ID               Get service details
    --logs ID               Get all service logs
    --tail ID               Get last 9000 lines of logs
    --freeze ID             Pause service
    --unfreeze ID           Resume service
    --destroy ID            Delete service (requires -y)
    --lock ID               Prevent service deletion
    --unlock ID             Allow service deletion
    --resize ID             Resize service (with --vcpu)
    --redeploy ID           Re-run bootstrap
    --execute ID 'cmd'      Execute command in service
    --snapshot ID           Create service snapshot

SERVICE ENV SUBCOMMAND:
    service env status ID   Show vault status
    service env set ID      Set vault (stdin or --env-file)
    service env export ID   Export vault to stdout
    service env delete ID   Delete vault (requires -y)

SNAPSHOT OPTIONS:
    --list, -l              List all snapshots
    --info ID               Get snapshot details
    --delete ID             Delete snapshot (requires -y)
    --lock ID               Prevent snapshot deletion
    --unlock ID             Allow snapshot deletion
    --clone ID              Clone snapshot
    --type TYPE             Clone type: session or service
    --name NAME             Name for cloned resource
    --shell SHELL           Shell for cloned session
    --ports PORTS           Ports for cloned service

EXAMPLES:
    php un.php script.py
    php un.php -s bash 'echo hello'
    php un.php -n semitrusted crawler.py
    php un.php session --shell python3 --tmux
    php un.php session --list
    php un.php service --name myapp --ports 80 --bootstrap 'python -m http.server 80'
    php un.php service --execute <id> 'ls -la'
    php un.php snapshot --list
    php un.php key

HELP;
        echo $help;
    }
}

// CLI entry point
if (php_sapi_name() === 'cli' && basename(__FILE__) === basename($argv[0])) {
    $client = new Unsandbox();
    $client->cliMain($argv);
}
