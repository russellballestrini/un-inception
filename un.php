# PUBLIC DOMAIN - NO LICENSE, NO WARRANTY
#
# This is free public domain software for the public good of a permacomputer hosted
# at permacomputer.com - an always-on computer by the people, for the people. One
# which is durable, easy to repair, and distributed like tap water for machine
# learning intelligence.
#
# The permacomputer is community-owned infrastructure optimized around four values:
#
#   TRUTH    - First principles, math & science, open source code freely distributed
#   FREEDOM  - Voluntary partnerships, freedom from tyranny & corporate control
#   HARMONY  - Minimal waste, self-renewing systems with diverse thriving connections
#   LOVE     - Be yourself without hurting others, cooperation through natural law
#
# This software contributes to that vision by enabling code execution across 42+
# programming languages through a unified interface, accessible to all. Code is
# seeds to sprout on any abandoned technology.
#
# Learn more: https://www.permacomputer.com
#
# Anyone is free to copy, modify, publish, use, compile, sell, or distribute this
# software, either in source code form or as a compiled binary, for any purpose,
# commercial or non-commercial, and by any means.
#
# NO WARRANTY. THE SOFTWARE IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY KIND.
#
# That said, our permacomputer's digital membrane stratum continuously runs unit,
# integration, and functional tests on all of it's own software - with our
# permacomputer monitoring itself, repairing itself, with minimal human in the
# loop guidance. Our agents do their best.
#
# Copyright 2025 TimeHexOn & foxhop & russell@unturf
# https://www.timehexon.com
# https://www.foxhop.net
# https://www.unturf.com/software

#!/usr/bin/env php
<?php
/**
 * un.php - Unsandbox CLI Client (PHP Implementation)
 *
 * Full-featured CLI matching un.c capabilities:
 * - Execute code with env vars, input files, artifacts
 * - Interactive sessions with shell/REPL support
 * - Persistent services with domains and ports
 *
 * Usage:
 *   un.php [options] <source_file>
 *   un.php session [options]
 *   un.php service [options]
 *
 * Requires: UNSANDBOX_API_KEY environment variable
 */

const API_BASE = 'https://api.unsandbox.com';
const PORTAL_BASE = 'https://unsandbox.com';
const BLUE = "\033[34m";
const RED = "\033[31m";
const GREEN = "\033[32m";
const YELLOW = "\033[33m";
const RESET = "\033[0m";

const EXT_MAP = [
    '.py' => 'python', '.js' => 'javascript', '.ts' => 'typescript',
    '.rb' => 'ruby', '.php' => 'php', '.pl' => 'perl', '.lua' => 'lua',
    '.sh' => 'bash', '.go' => 'go', '.rs' => 'rust', '.c' => 'c',
    '.cpp' => 'cpp', '.cc' => 'cpp', '.cxx' => 'cpp',
    '.java' => 'java', '.kt' => 'kotlin', '.cs' => 'csharp', '.fs' => 'fsharp',
    '.hs' => 'haskell', '.ml' => 'ocaml', '.clj' => 'clojure', '.scm' => 'scheme',
    '.lisp' => 'commonlisp', '.erl' => 'erlang', '.ex' => 'elixir', '.exs' => 'elixir',
    '.jl' => 'julia', '.r' => 'r', '.R' => 'r', '.cr' => 'crystal',
    '.d' => 'd', '.nim' => 'nim', '.zig' => 'zig', '.v' => 'v',
    '.dart' => 'dart', '.groovy' => 'groovy', '.scala' => 'scala',
    '.f90' => 'fortran', '.f95' => 'fortran', '.cob' => 'cobol',
    '.pro' => 'prolog', '.forth' => 'forth', '.4th' => 'forth',
    '.tcl' => 'tcl', '.raku' => 'raku', '.m' => 'objc'
];

function get_api_keys($args_key = null) {
    $public_key = getenv('UNSANDBOX_PUBLIC_KEY');
    $secret_key = getenv('UNSANDBOX_SECRET_KEY');

    if (!$public_key || !$secret_key) {
        $old_key = $args_key ?: getenv('UNSANDBOX_API_KEY');
        if ($old_key) {
            $public_key = $old_key;
            $secret_key = $old_key;
        } else {
            fwrite(STDERR, RED . "Error: UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY not set" . RESET . "\n");
            fwrite(STDERR, RED . "       (or legacy UNSANDBOX_API_KEY for backwards compatibility)" . RESET . "\n");
            exit(1);
        }
    }

    return ['public_key' => $public_key, 'secret_key' => $secret_key];
}

function detect_language($filename) {
    $ext = '.' . strtolower(pathinfo($filename, PATHINFO_EXTENSION));
    $lang = EXT_MAP[$ext] ?? null;
    if (!$lang) {
        $file = @fopen($filename, 'r');
        if ($file) {
            $first_line = fgets($file);
            fclose($file);
            if (str_starts_with($first_line, '#!')) {
                if (str_contains($first_line, 'python')) return 'python';
                if (str_contains($first_line, 'node')) return 'javascript';
                if (str_contains($first_line, 'ruby')) return 'ruby';
                if (str_contains($first_line, 'perl')) return 'perl';
                if (str_contains($first_line, 'bash') || str_contains($first_line, '/sh')) return 'bash';
                if (str_contains($first_line, 'lua')) return 'lua';
                if (str_contains($first_line, 'php')) return 'php';
            }
        }
        fwrite(STDERR, RED . "Error: Cannot detect language for $filename" . RESET . "\n");
        exit(1);
    }
    return $lang;
}

function api_request($endpoint, $method = 'GET', $data = null, $keys = null) {
    $url = API_BASE . $endpoint;
    $ch = curl_init($url);

    $timestamp = (string)time();
    $body = $data ? json_encode($data) : '';

    // Parse URL to get path and query
    $parsed_url = parse_url($url);
    $path = $parsed_url['path'] . (isset($parsed_url['query']) ? '?' . $parsed_url['query'] : '');
    $message = "$timestamp:$method:$path:$body";
    $signature = hash_hmac('sha256', $message, $keys['secret_key']);

    $headers = [
        'Authorization: Bearer ' . $keys['public_key'],
        'X-Timestamp: ' . $timestamp,
        'X-Signature: ' . $signature,
        'Content-Type: application/json'
    ];

    curl_setopt_array($ch, [
        CURLOPT_CUSTOMREQUEST => $method,
        CURLOPT_RETURNTRANSFER => true,
        CURLOPT_HTTPHEADER => $headers,
        CURLOPT_TIMEOUT => 300
    ]);

    if ($data) {
        curl_setopt($ch, CURLOPT_POSTFIELDS, $body);
    }

    $response = curl_exec($ch);
    $http_code = curl_getinfo($ch, CURLINFO_HTTP_CODE);

    if ($response === false) {
        fwrite(STDERR, RED . "Error: " . curl_error($ch) . RESET . "\n");
        curl_close($ch);
        exit(1);
    }

    curl_close($ch);

    if ($http_code < 200 || $http_code >= 300) {
        if ($http_code === 401 && stripos($response, 'timestamp') !== false) {
            fwrite(STDERR, RED . "Error: Request timestamp expired (must be within 5 minutes of server time)" . RESET . "\n");
            fwrite(STDERR, YELLOW . "Your computer's clock may have drifted." . RESET . "\n");
            fwrite(STDERR, YELLOW . "Check your system time and sync with NTP if needed:" . RESET . "\n");
            fwrite(STDERR, "  Linux:   sudo ntpdate -s time.nist.gov\n");
            fwrite(STDERR, "  macOS:   sudo sntp -sS time.apple.com\n");
            fwrite(STDERR, "  Windows: w32tm /resync\n");
        } else {
            fwrite(STDERR, RED . "Error: HTTP $http_code - $response" . RESET . "\n");
        }
        exit(1);
    }

    return json_decode($response, true);
}

function api_request_text($endpoint, $method, $body, $keys) {
    $url = API_BASE . $endpoint;
    $ch = curl_init($url);

    $timestamp = (string)time();

    // Parse URL to get path
    $parsed_url = parse_url($url);
    $path = $parsed_url['path'];
    $message = "$timestamp:$method:$path:$body";
    $signature = hash_hmac('sha256', $message, $keys['secret_key']);

    $headers = [
        'Authorization: Bearer ' . $keys['public_key'],
        'X-Timestamp: ' . $timestamp,
        'X-Signature: ' . $signature,
        'Content-Type: text/plain'
    ];

    curl_setopt_array($ch, [
        CURLOPT_CUSTOMREQUEST => $method,
        CURLOPT_RETURNTRANSFER => true,
        CURLOPT_HTTPHEADER => $headers,
        CURLOPT_TIMEOUT => 300,
        CURLOPT_POSTFIELDS => $body
    ]);

    $response = curl_exec($ch);
    $http_code = curl_getinfo($ch, CURLINFO_HTTP_CODE);

    if ($response === false) {
        curl_close($ch);
        return ['error' => curl_error($ch)];
    }

    curl_close($ch);

    if ($http_code < 200 || $http_code >= 300) {
        return ['error' => "HTTP $http_code - $response"];
    }

    return json_decode($response, true);
}

// ============================================================================
// Environment Secrets Vault Functions
// ============================================================================

const MAX_ENV_CONTENT_SIZE = 64 * 1024; // 64KB max

function service_env_status($service_id, $keys) {
    $result = api_request("/services/$service_id/env", 'GET', null, $keys);
    $has_vault = $result['has_vault'] ?? false;

    if (!$has_vault) {
        echo "Vault exists: no\n";
        echo "Variable count: 0\n";
    } else {
        echo "Vault exists: yes\n";
        echo "Variable count: " . ($result['count'] ?? 0) . "\n";
        if (isset($result['updated_at'])) {
            echo "Last updated: " . date('Y-m-d H:i:s', $result['updated_at']) . "\n";
        }
    }
}

function service_env_set($service_id, $env_content, $keys) {
    if (empty($env_content)) {
        fwrite(STDERR, RED . "Error: No environment content provided" . RESET . "\n");
        return false;
    }

    if (strlen($env_content) > MAX_ENV_CONTENT_SIZE) {
        fwrite(STDERR, RED . "Error: Environment content too large (max " . MAX_ENV_CONTENT_SIZE . " bytes)" . RESET . "\n");
        return false;
    }

    $result = api_request_text("/services/$service_id/env", 'PUT', $env_content, $keys);

    if (isset($result['error'])) {
        fwrite(STDERR, RED . "Error: " . $result['error'] . RESET . "\n");
        return false;
    }

    $count = $result['count'] ?? 0;
    $plural = $count === 1 ? '' : 's';
    echo GREEN . "Environment vault updated: $count variable$plural" . RESET . "\n";
    if (!empty($result['message'])) echo $result['message'] . "\n";
    return true;
}

function service_env_export($service_id, $keys) {
    $result = api_request("/services/$service_id/env/export", 'POST', [], $keys);
    $env_content = $result['env'] ?? '';
    if (!empty($env_content)) {
        echo $env_content;
        if (!str_ends_with($env_content, "\n")) echo "\n";
    }
}

function service_env_delete($service_id, $keys) {
    api_request("/services/$service_id/env", 'DELETE', null, $keys);
    echo GREEN . "Environment vault deleted" . RESET . "\n";
}

function read_env_file($filepath) {
    if (!file_exists($filepath)) {
        fwrite(STDERR, RED . "Error: Env file not found: $filepath" . RESET . "\n");
        exit(1);
    }
    return file_get_contents($filepath);
}

function build_env_content($envs, $env_file) {
    $parts = [];

    // Read from env file first
    if (!empty($env_file)) {
        $parts[] = read_env_file($env_file);
    }

    // Add -e flags
    foreach ($envs as $e) {
        if (str_contains($e, '=')) {
            $parts[] = $e;
        }
    }

    return implode("\n", $parts);
}

function cmd_service_env($action, $target, $envs, $env_file, $keys) {
    if (empty($action)) {
        fwrite(STDERR, RED . "Error: env action required (status, set, export, delete)" . RESET . "\n");
        exit(1);
    }

    if (empty($target)) {
        fwrite(STDERR, RED . "Error: Service ID required for env command" . RESET . "\n");
        exit(1);
    }

    switch ($action) {
        case 'status':
            service_env_status($target, $keys);
            break;
        case 'set':
            $env_content = build_env_content($envs, $env_file);
            if (empty($env_content)) {
                fwrite(STDERR, RED . "Error: No env content provided. Use -e KEY=VAL or --env-file" . RESET . "\n");
                exit(1);
            }
            service_env_set($target, $env_content, $keys);
            break;
        case 'export':
            service_env_export($target, $keys);
            break;
        case 'delete':
            service_env_delete($target, $keys);
            break;
        default:
            fwrite(STDERR, RED . "Error: Unknown env action '$action'. Use: status, set, export, delete" . RESET . "\n");
            exit(1);
    }
}

function cmd_execute($options) {
    $keys = get_api_keys($options['api_key']);

    if (!file_exists($options['source_file'])) {
        fwrite(STDERR, RED . "Error: File not found: {$options['source_file']}" . RESET . "\n");
        exit(1);
    }

    $code = file_get_contents($options['source_file']);
    $language = detect_language($options['source_file']);

    $payload = ['language' => $language, 'code' => $code];

    if (!empty($options['env'])) {
        $env_vars = [];
        foreach ($options['env'] as $e) {
            $parts = explode('=', $e, 2);
            if (count($parts) === 2) {
                $env_vars[$parts[0]] = $parts[1];
            }
        }
        if (!empty($env_vars)) {
            $payload['env'] = $env_vars;
        }
    }

    if (!empty($options['files'])) {
        $input_files = [];
        foreach ($options['files'] as $filepath) {
            if (!file_exists($filepath)) {
                fwrite(STDERR, RED . "Error: Input file not found: $filepath" . RESET . "\n");
                exit(1);
            }
            $input_files[] = [
                'filename' => basename($filepath),
                'content_base64' => base64_encode(file_get_contents($filepath))
            ];
        }
        $payload['input_files'] = $input_files;
    }

    if ($options['artifacts']) $payload['return_artifacts'] = true;
    if ($options['network']) $payload['network'] = $options['network'];
    if ($options['vcpu']) $payload['vcpu'] = $options['vcpu'];

    $result = api_request('/execute', 'POST', $payload, $keys);

    if (!empty($result['stdout'])) {
        echo BLUE . $result['stdout'] . RESET;
    }
    if (!empty($result['stderr'])) {
        fwrite(STDERR, RED . $result['stderr'] . RESET);
    }

    if ($options['artifacts'] && !empty($result['artifacts'])) {
        $out_dir = $options['output_dir'] ?: '.';
        if (!is_dir($out_dir)) {
            mkdir($out_dir, 0755, true);
        }
        foreach ($result['artifacts'] as $artifact) {
            $filename = $artifact['filename'] ?? 'artifact';
            $content = base64_decode($artifact['content_base64']);
            $filepath = $out_dir . '/' . $filename;
            file_put_contents($filepath, $content);
            chmod($filepath, 0755);
            fwrite(STDERR, GREEN . "Saved: $filepath" . RESET . "\n");
        }
    }

    exit($result['exit_code'] ?? 0);
}

function cmd_session($options) {
    $keys = get_api_keys($options['api_key']);

    if ($options['list']) {
        $result = api_request('/sessions', 'GET', null, $keys);
        $sessions = $result['sessions'] ?? [];
        if (empty($sessions)) {
            echo "No active sessions\n";
        } else {
            printf("%-40s %-10s %-10s %s\n", 'ID', 'Shell', 'Status', 'Created');
            foreach ($sessions as $s) {
                printf("%-40s %-10s %-10s %s\n",
                    $s['id'] ?? 'N/A', $s['shell'] ?? 'N/A',
                    $s['status'] ?? 'N/A', $s['created_at'] ?? 'N/A');
            }
        }
        return;
    }

    if ($options['kill']) {
        api_request("/sessions/{$options['kill']}", 'DELETE', null, $keys);
        echo GREEN . "Session terminated: {$options['kill']}" . RESET . "\n";
        return;
    }

    if ($options['attach']) {
        echo YELLOW . "Attaching to session {$options['attach']}..." . RESET . "\n";
        echo YELLOW . "(Interactive sessions require WebSocket - use un2 for full support)" . RESET . "\n";
        return;
    }

    $payload = ['shell' => $options['shell'] ?: 'bash'];
    if ($options['network']) $payload['network'] = $options['network'];
    if ($options['vcpu']) $payload['vcpu'] = $options['vcpu'];
    if ($options['tmux']) $payload['persistence'] = 'tmux';
    if ($options['screen']) $payload['persistence'] = 'screen';
    if ($options['audit']) $payload['audit'] = true;

    // Add input files
    if (!empty($options['files'])) {
        $input_files = [];
        foreach ($options['files'] as $filepath) {
            if (!file_exists($filepath)) {
                fwrite(STDERR, RED . "Error: Input file not found: $filepath" . RESET . "\n");
                exit(1);
            }
            $input_files[] = [
                'filename' => basename($filepath),
                'content_base64' => base64_encode(file_get_contents($filepath))
            ];
        }
        $payload['input_files'] = $input_files;
    }

    echo YELLOW . "Creating session..." . RESET . "\n";
    $result = api_request('/sessions', 'POST', $payload, $keys);
    echo GREEN . "Session created: " . ($result['id'] ?? 'N/A') . RESET . "\n";
    echo YELLOW . "(Interactive sessions require WebSocket - use un2 for full support)" . RESET . "\n";
}

function validate_key($keys) {
    $url = PORTAL_BASE . '/keys/validate';
    $ch = curl_init($url);

    $timestamp = (string)time();
    $body = '';

    $parsed_url = parse_url($url);
    $path = $parsed_url['path'];
    $message = "$timestamp:POST:$path:$body";
    $signature = hash_hmac('sha256', $message, $keys['secret_key']);

    $headers = [
        'Authorization: Bearer ' . $keys['public_key'],
        'X-Timestamp: ' . $timestamp,
        'X-Signature: ' . $signature,
        'Content-Type: application/json'
    ];

    curl_setopt_array($ch, [
        CURLOPT_CUSTOMREQUEST => 'POST',
        CURLOPT_RETURNTRANSFER => true,
        CURLOPT_HTTPHEADER => $headers,
        CURLOPT_TIMEOUT => 30
    ]);

    $response = curl_exec($ch);
    $http_code = curl_getinfo($ch, CURLINFO_HTTP_CODE);

    if ($response === false) {
        fwrite(STDERR, RED . "Error: " . curl_error($ch) . RESET . "\n");
        curl_close($ch);
        exit(1);
    }

    curl_close($ch);

    $data = json_decode($response, true);

    if ($http_code === 200 && isset($data['valid']) && $data['valid']) {
        echo GREEN . "Valid" . RESET . "\n\n";
        echo "Public Key:   " . ($data['public_key'] ?? 'N/A') . "\n";
        echo "Tier:         " . ($data['tier'] ?? 'N/A') . "\n";
        echo "Status:       " . ($data['status'] ?? 'N/A') . "\n";
        echo "Expires:      " . ($data['expires_at'] ?? 'N/A') . "\n";
        echo "Time Remaining: " . ($data['time_remaining'] ?? 'N/A') . "\n";
        echo "Rate Limit:   " . ($data['rate_limit'] ?? 'N/A') . " req/min\n";
        echo "Burst:        " . ($data['burst'] ?? 'N/A') . "\n";
        echo "Concurrency:  " . ($data['concurrency'] ?? 'N/A') . "\n";
    } elseif ($http_code === 200 && isset($data['valid']) && !$data['valid'] && isset($data['status']) && $data['status'] === 'expired') {
        echo RED . "Expired" . RESET . "\n\n";
        echo "Public Key:   " . ($data['public_key'] ?? 'N/A') . "\n";
        echo "Tier:         " . ($data['tier'] ?? 'N/A') . "\n";
        echo "Expired:      " . ($data['expires_at'] ?? 'N/A') . "\n\n";
        echo YELLOW . "To renew: Visit https://unsandbox.com/keys/extend" . RESET . "\n";
    } else {
        echo RED . "Invalid" . RESET . "\n\n";
        if (isset($data['error'])) {
            echo "Error: " . $data['error'] . "\n";
        } elseif (isset($data['reason'])) {
            echo "Reason: " . $data['reason'] . "\n";
        } else {
            echo "HTTP $http_code - $response\n";
        }
    }
}

function cmd_key($options) {
    $keys = get_api_keys($options['api_key']);

    if ($options['extend']) {
        // First validate to get public_key
        $url = PORTAL_BASE . '/keys/validate';
        $ch = curl_init($url);

        $timestamp = (string)time();
        $body = '';

        $parsed_url = parse_url($url);
        $path = $parsed_url['path'];
        $message = "$timestamp:POST:$path:$body";
        $signature = hash_hmac('sha256', $message, $keys['secret_key']);

        $headers = [
            'Authorization: Bearer ' . $keys['public_key'],
            'X-Timestamp: ' . $timestamp,
            'X-Signature: ' . $signature,
            'Content-Type: application/json'
        ];

        curl_setopt_array($ch, [
            CURLOPT_CUSTOMREQUEST => 'POST',
            CURLOPT_RETURNTRANSFER => true,
            CURLOPT_HTTPHEADER => $headers,
            CURLOPT_TIMEOUT => 30
        ]);

        $response = curl_exec($ch);
        curl_close($ch);

        $data = json_decode($response, true);
        $public_key = $data['public_key'] ?? null;

        if (!$public_key) {
            fwrite(STDERR, RED . "Error: Could not retrieve public key" . RESET . "\n");
            exit(1);
        }

        $extend_url = PORTAL_BASE . '/keys/extend?pk=' . urlencode($public_key);
        echo "Opening browser to: $extend_url\n";

        // Detect platform and open browser
        if (PHP_OS_FAMILY === 'Linux') {
            exec('xdg-open ' . escapeshellarg($extend_url) . ' > /dev/null 2>&1 &');
        } elseif (PHP_OS_FAMILY === 'Darwin') {
            exec('open ' . escapeshellarg($extend_url) . ' > /dev/null 2>&1 &');
        } elseif (PHP_OS_FAMILY === 'Windows') {
            exec('start ' . escapeshellarg($extend_url) . ' > NUL 2>&1');
        } else {
            echo YELLOW . "Cannot auto-open browser on this platform. Please visit:" . RESET . "\n";
            echo "$extend_url\n";
        }
    } else {
        validate_key($keys);
    }
}

function cmd_service($options) {
    $keys = get_api_keys($options['api_key']);

    if ($options['list']) {
        $result = api_request('/services', 'GET', null, $keys);
        $services = $result['services'] ?? [];
        if (empty($services)) {
            echo "No services\n";
        } else {
            printf("%-20s %-15s %-10s %-15s %s\n", 'ID', 'Name', 'Status', 'Ports', 'Domains');
            foreach ($services as $s) {
                $ports = implode(',', $s['ports'] ?? []);
                $domains = implode(',', $s['domains'] ?? []);
                printf("%-20s %-15s %-10s %-15s %s\n",
                    $s['id'] ?? 'N/A', $s['name'] ?? 'N/A',
                    $s['status'] ?? 'N/A', $ports, $domains);
            }
        }
        return;
    }

    if ($options['info']) {
        $result = api_request("/services/{$options['info']}", 'GET', null, $keys);
        echo json_encode($result, JSON_PRETTY_PRINT) . "\n";
        return;
    }

    if ($options['logs']) {
        $result = api_request("/services/{$options['logs']}/logs", 'GET', null, $keys);
        echo $result['logs'] ?? '';
        return;
    }

    if ($options['tail']) {
        $result = api_request("/services/{$options['tail']}/logs?lines=9000", 'GET', null, $keys);
        echo $result['logs'] ?? '';
        return;
    }

    if ($options['sleep']) {
        api_request("/services/{$options['sleep']}/freeze", 'POST', null, $keys);
        echo GREEN . "Service frozen: {$options['sleep']}" . RESET . "\n";
        return;
    }

    if ($options['wake']) {
        api_request("/services/{$options['wake']}/unfreeze", 'POST', null, $keys);
        echo GREEN . "Service unfreezing: {$options['wake']}" . RESET . "\n";
        return;
    }

    if ($options['destroy']) {
        api_request("/services/{$options['destroy']}", 'DELETE', null, $keys);
        echo GREEN . "Service destroyed: {$options['destroy']}" . RESET . "\n";
        return;
    }

    if ($options['resize']) {
        if (!$options['vcpu']) {
            fwrite(STDERR, RED . "Error: --vcpu is required with --resize" . RESET . "\n");
            exit(1);
        }
        $payload = ['vcpu' => $options['vcpu']];
        api_request("/services/{$options['resize']}", 'PATCH', $payload, $keys);
        $ram = $options['vcpu'] * 2;
        echo GREEN . "Service resized to {$options['vcpu']} vCPU, {$ram} GB RAM" . RESET . "\n";
        return;
    }

    if ($options['execute']) {
        $payload = ['command' => $options['command']];
        $result = api_request("/services/{$options['execute']}/execute", 'POST', $payload, $keys);
        if (!empty($result['stdout'])) echo BLUE . $result['stdout'] . RESET;
        if (!empty($result['stderr'])) fwrite(STDERR, RED . $result['stderr'] . RESET);
        return;
    }

    if ($options['dump_bootstrap']) {
        fwrite(STDERR, "Fetching bootstrap script from {$options['dump_bootstrap']}...\n");
        $payload = ['command' => 'cat /tmp/bootstrap.sh'];
        $result = api_request("/services/{$options['dump_bootstrap']}/execute", 'POST', $payload, $keys);

        if (!empty($result['stdout'])) {
            $bootstrap = $result['stdout'];
            if ($options['dump_file']) {
                // Write to file
                if (file_put_contents($options['dump_file'], $bootstrap) === false) {
                    fwrite(STDERR, RED . "Error: Could not write to {$options['dump_file']}" . RESET . "\n");
                    exit(1);
                }
                chmod($options['dump_file'], 0755);
                echo "Bootstrap saved to {$options['dump_file']}\n";
            } else {
                // Print to stdout
                echo $bootstrap;
            }
        } else {
            fwrite(STDERR, RED . "Error: Failed to fetch bootstrap (service not running or no bootstrap file)" . RESET . "\n");
            exit(1);
        }
        return;
    }

    if ($options['name']) {
        $payload = ['name' => $options['name']];
        if ($options['ports']) {
            $payload['ports'] = array_map('intval', explode(',', $options['ports']));
        }
        if ($options['domains']) {
            $payload['domains'] = explode(',', $options['domains']);
        }
        if ($options['type']) {
            $payload['service_type'] = $options['type'];
        }
        if ($options['bootstrap']) {
            $payload['bootstrap'] = $options['bootstrap'];
        }
        if ($options['bootstrap_file']) {
            if (!file_exists($options['bootstrap_file'])) {
                fwrite(STDERR, RED . "Error: Bootstrap file not found: {$options['bootstrap_file']}" . RESET . "\n");
                exit(1);
            }
            $payload['bootstrap_content'] = file_get_contents($options['bootstrap_file']);
        }
        // Add input files
        if (!empty($options['files'])) {
            $input_files = [];
            foreach ($options['files'] as $filepath) {
                if (!file_exists($filepath)) {
                    fwrite(STDERR, RED . "Error: Input file not found: $filepath" . RESET . "\n");
                    exit(1);
                }
                $input_files[] = [
                    'filename' => basename($filepath),
                    'content_base64' => base64_encode(file_get_contents($filepath))
                ];
            }
            $payload['input_files'] = $input_files;
        }
        if ($options['network']) $payload['network'] = $options['network'];
        if ($options['vcpu']) $payload['vcpu'] = $options['vcpu'];

        $result = api_request('/services', 'POST', $payload, $keys);
        $service_id = $result['id'] ?? null;
        echo GREEN . "Service created: " . ($service_id ?? 'N/A') . RESET . "\n";
        echo "Name: " . ($result['name'] ?? 'N/A') . "\n";
        if (!empty($result['url'])) echo "URL: {$result['url']}\n";

        // Auto-set vault if -e or --env-file provided
        $env_content = build_env_content($options['env'] ?? [], $options['env_file']);
        if (!empty($env_content) && $service_id) {
            service_env_set($service_id, $env_content, $keys);
        }
        return;
    }

    fwrite(STDERR, RED . "Error: Specify --name to create a service, or use --list, --info, etc." . RESET . "\n");
    exit(1);
}

function main() {
    global $argv;

    $options = [
        'command' => null,
        'source_file' => null,
        'env' => [],
        'files' => [],
        'artifacts' => false,
        'output_dir' => null,
        'network' => null,
        'vcpu' => null,
        'api_key' => null,
        'shell' => null,
        'list' => false,
        'attach' => null,
        'kill' => null,
        'audit' => false,
        'tmux' => false,
        'screen' => false,
        'name' => null,
        'ports' => null,
        'domains' => null,
        'type' => null,
        'bootstrap' => null,
        'bootstrap_file' => null,
        'info' => null,
        'logs' => null,
        'tail' => null,
        'sleep' => null,
        'wake' => null,
        'destroy' => null,
        'resize' => null,
        'execute' => null,
        'command' => null,
        'dump_bootstrap' => null,
        'dump_file' => null,
        'extend' => false,
        'env_file' => null,
        'env_action' => null,
        'env_target' => null
    ];

    for ($i = 1; $i < count($argv); $i++) {
        $arg = $argv[$i];

        switch ($arg) {
            case 'session':
            case 'service':
            case 'key':
                $options['command'] = $arg;
                break;
            case '-e':
                $options['env'][] = $argv[++$i];
                break;
            case '-f':
                $options['files'][] = $argv[++$i];
                break;
            case '-a':
                $options['artifacts'] = true;
                break;
            case '-o':
                $options['output_dir'] = $argv[++$i];
                break;
            case '-n':
                $options['network'] = $argv[++$i];
                break;
            case '-v':
                $options['vcpu'] = (int)$argv[++$i];
                break;
            case '-k':
                $options['api_key'] = $argv[++$i];
                break;
            case '-s':
            case '--shell':
                $options['shell'] = $argv[++$i];
                break;
            case '-l':
            case '--list':
                $options['list'] = true;
                break;
            case '--attach':
                $options['attach'] = $argv[++$i];
                break;
            case '--kill':
                $options['kill'] = $argv[++$i];
                break;
            case '--audit':
                $options['audit'] = true;
                break;
            case '--tmux':
                $options['tmux'] = true;
                break;
            case '--screen':
                $options['screen'] = true;
                break;
            case '--name':
                $options['name'] = $argv[++$i];
                break;
            case '--ports':
                $options['ports'] = $argv[++$i];
                break;
            case '--domains':
                $options['domains'] = $argv[++$i];
                break;
            case '--type':
                $options['type'] = $argv[++$i];
                break;
            case '--bootstrap':
                $options['bootstrap'] = $argv[++$i];
                break;
            case '--bootstrap-file':
                $options['bootstrap_file'] = $argv[++$i];
                break;
            case '--env-file':
                $options['env_file'] = $argv[++$i];
                break;
            case 'env':
                // Handle "service env <action> <target>" subcommand
                if ($options['command'] === 'service') {
                    if (isset($argv[$i + 1])) {
                        $options['env_action'] = $argv[++$i];
                    }
                    if (isset($argv[$i + 1]) && !str_starts_with($argv[$i + 1], '-')) {
                        $options['env_target'] = $argv[++$i];
                    }
                }
                break;
            case '--info':
                $options['info'] = $argv[++$i];
                break;
            case '--logs':
                $options['logs'] = $argv[++$i];
                break;
            case '--tail':
                $options['tail'] = $argv[++$i];
                break;
            case '--freeze':
                $options['sleep'] = $argv[++$i];
                break;
            case '--unfreeze':
                $options['wake'] = $argv[++$i];
                break;
            case '--destroy':
                $options['destroy'] = $argv[++$i];
                break;
            case '--resize':
                $options['resize'] = $argv[++$i];
                break;
            case '--execute':
                $options['execute'] = $argv[++$i];
                break;
            case '--command':
                $options['command'] = $argv[++$i];
                break;
            case '--dump-bootstrap':
                $options['dump_bootstrap'] = $argv[++$i];
                break;
            case '--dump-file':
                $options['dump_file'] = $argv[++$i];
                break;
            case '--extend':
                $options['extend'] = true;
                break;
            default:
                if (str_starts_with($arg, '-')) {
                    fwrite(STDERR, RED . "Unknown option: $arg" . RESET . "\n");
                    exit(1);
                } else {
                    $options['source_file'] = $arg;
                }
                break;
        }
    }

    if ($options['command'] === 'session') {
        cmd_session($options);
    } elseif ($options['command'] === 'service') {
        // Check for "service env" subcommand
        if ($options['env_action']) {
            $keys = get_api_keys($options['api_key']);
            cmd_service_env($options['env_action'], $options['env_target'], $options['env'], $options['env_file'], $keys);
        } else {
            cmd_service($options);
        }
    } elseif ($options['command'] === 'key') {
        cmd_key($options);
    } elseif ($options['source_file']) {
        cmd_execute($options);
    } else {
        echo "Unsandbox CLI - Execute code in secure sandboxes

Usage:
  {$argv[0]} [options] <source_file>
  {$argv[0]} session [options]
  {$argv[0]} service [options]
  {$argv[0]} key [options]

Execute options:
  -e KEY=VALUE      Environment variable (multiple allowed)
  -f FILE          Input file (multiple allowed)
  -a               Return artifacts
  -o DIR           Output directory for artifacts
  -n MODE          Network mode (zerotrust|semitrusted)
  -v N             vCPU count (1-8)
  -k KEY           API key

Session options:
  -s, --shell NAME  Shell/REPL (default: bash)
  -l, --list       List sessions
  --attach ID      Attach to session
  --kill ID        Terminate session
  --audit          Record session
  --tmux           Enable tmux persistence
  --screen         Enable screen persistence

Service options:
  --name NAME      Service name
  --ports PORTS    Comma-separated ports
  --domains DOMAINS Custom domains
  --type TYPE      Service type (minecraft|mumble|teamspeak|source|tcp|udp)
  --bootstrap CMD  Bootstrap command or URI
  --bootstrap-file FILE  Upload local file as bootstrap script
  -l, --list       List services
  --info ID        Get service details
  --logs ID        Get all logs
  --tail ID        Get last 9000 lines
  --freeze ID       Freeze service
  --unfreeze ID        Unfreeze service
  --destroy ID     Destroy service
  --resize ID      Resize service (requires -v)
  --execute ID     Execute command in service
  --command CMD    Command to execute (with --execute)
  --dump-bootstrap ID  Dump bootstrap script
  --dump-file FILE     File to save bootstrap (with --dump-bootstrap)

Key options:
  -k KEY           API key (or use UNSANDBOX_API_KEY env var)
  --extend         Open browser to extend/renew key
";
        exit(1);
    }
}

main();
