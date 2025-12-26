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

function get_api_key($args_key = null) {
    $key = $args_key ?: getenv('UNSANDBOX_API_KEY');
    if (!$key) {
        fwrite(STDERR, RED . "Error: UNSANDBOX_API_KEY not set" . RESET . "\n");
        exit(1);
    }
    return $key;
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

function api_request($endpoint, $method = 'GET', $data = null, $api_key = null) {
    $url = API_BASE . $endpoint;
    $ch = curl_init($url);

    $headers = [
        'Authorization: Bearer ' . $api_key,
        'Content-Type: application/json'
    ];

    curl_setopt_array($ch, [
        CURLOPT_CUSTOMREQUEST => $method,
        CURLOPT_RETURNTRANSFER => true,
        CURLOPT_HTTPHEADER => $headers,
        CURLOPT_TIMEOUT => 300
    ]);

    if ($data) {
        curl_setopt($ch, CURLOPT_POSTFIELDS, json_encode($data));
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
        fwrite(STDERR, RED . "Error: HTTP $http_code - $response" . RESET . "\n");
        exit(1);
    }

    return json_decode($response, true);
}

function cmd_execute($options) {
    $api_key = get_api_key($options['api_key']);

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

    $result = api_request('/execute', 'POST', $payload, $api_key);

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
    $api_key = get_api_key($options['api_key']);

    if ($options['list']) {
        $result = api_request('/sessions', 'GET', null, $api_key);
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
        api_request("/sessions/{$options['kill']}", 'DELETE', null, $api_key);
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

    echo YELLOW . "Creating session..." . RESET . "\n";
    $result = api_request('/sessions', 'POST', $payload, $api_key);
    echo GREEN . "Session created: " . ($result['id'] ?? 'N/A') . RESET . "\n";
    echo YELLOW . "(Interactive sessions require WebSocket - use un2 for full support)" . RESET . "\n";
}

function cmd_service($options) {
    $api_key = get_api_key($options['api_key']);

    if ($options['list']) {
        $result = api_request('/services', 'GET', null, $api_key);
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
        $result = api_request("/services/{$options['info']}", 'GET', null, $api_key);
        echo json_encode($result, JSON_PRETTY_PRINT) . "\n";
        return;
    }

    if ($options['logs']) {
        $result = api_request("/services/{$options['logs']}/logs", 'GET', null, $api_key);
        echo $result['logs'] ?? '';
        return;
    }

    if ($options['tail']) {
        $result = api_request("/services/{$options['tail']}/logs?lines=9000", 'GET', null, $api_key);
        echo $result['logs'] ?? '';
        return;
    }

    if ($options['sleep']) {
        api_request("/services/{$options['sleep']}/sleep", 'POST', null, $api_key);
        echo GREEN . "Service sleeping: {$options['sleep']}" . RESET . "\n";
        return;
    }

    if ($options['wake']) {
        api_request("/services/{$options['wake']}/wake", 'POST', null, $api_key);
        echo GREEN . "Service waking: {$options['wake']}" . RESET . "\n";
        return;
    }

    if ($options['destroy']) {
        api_request("/services/{$options['destroy']}", 'DELETE', null, $api_key);
        echo GREEN . "Service destroyed: {$options['destroy']}" . RESET . "\n";
        return;
    }

    if ($options['execute']) {
        $payload = ['command' => $options['command']];
        $result = api_request("/services/{$options['execute']}/execute", 'POST', $payload, $api_key);
        if (!empty($result['stdout'])) echo BLUE . $result['stdout'] . RESET;
        if (!empty($result['stderr'])) fwrite(STDERR, RED . $result['stderr'] . RESET);
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
            if (file_exists($options['bootstrap'])) {
                $payload['bootstrap'] = file_get_contents($options['bootstrap']);
            } else {
                $payload['bootstrap'] = $options['bootstrap'];
            }
        }
        if ($options['network']) $payload['network'] = $options['network'];
        if ($options['vcpu']) $payload['vcpu'] = $options['vcpu'];

        $result = api_request('/services', 'POST', $payload, $api_key);
        echo GREEN . "Service created: " . ($result['id'] ?? 'N/A') . RESET . "\n";
        echo "Name: " . ($result['name'] ?? 'N/A') . "\n";
        if (!empty($result['url'])) echo "URL: {$result['url']}\n";
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
        'info' => null,
        'logs' => null,
        'tail' => null,
        'sleep' => null,
        'wake' => null,
        'destroy' => null,
        'execute' => null,
        'command' => null
    ];

    for ($i = 1; $i < count($argv); $i++) {
        $arg = $argv[$i];

        switch ($arg) {
            case 'session':
            case 'service':
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
            case '--info':
                $options['info'] = $argv[++$i];
                break;
            case '--logs':
                $options['logs'] = $argv[++$i];
                break;
            case '--tail':
                $options['tail'] = $argv[++$i];
                break;
            case '--sleep':
                $options['sleep'] = $argv[++$i];
                break;
            case '--wake':
                $options['wake'] = $argv[++$i];
                break;
            case '--destroy':
                $options['destroy'] = $argv[++$i];
                break;
            case '--execute':
                $options['execute'] = $argv[++$i];
                break;
            case '--command':
                $options['command'] = $argv[++$i];
                break;
            default:
                if (!str_starts_with($arg, '-')) {
                    $options['source_file'] = $arg;
                }
                break;
        }
    }

    if ($options['command'] === 'session') {
        cmd_session($options);
    } elseif ($options['command'] === 'service') {
        cmd_service($options);
    } elseif ($options['source_file']) {
        cmd_execute($options);
    } else {
        echo "Unsandbox CLI - Execute code in secure sandboxes

Usage:
  {$argv[0]} [options] <source_file>
  {$argv[0]} session [options]
  {$argv[0]} service [options]

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
  --bootstrap CMD  Bootstrap command/file
  -l, --list       List services
  --info ID        Get service details
  --logs ID        Get all logs
  --tail ID        Get last 9000 lines
  --sleep ID       Freeze service
  --wake ID        Unfreeze service
  --destroy ID     Destroy service
  --execute ID     Execute command in service
  --command CMD    Command to execute (with --execute)
";
        exit(1);
    }
}

main();
