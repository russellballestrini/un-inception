<?php
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
//
// unsandbox SDK for PHP - Execute code in secure sandboxes
// https://unsandbox.com | https://api.unsandbox.com/openapi

class Un {
    const API_BASE = 'https://api.unsandbox.com';
    const VERSION = '2.0.0';

    public static function loadAccountsCsv($path = null) {
        $path = $path ?: getenv('HOME') . '/.unsandbox/accounts.csv';
        if (!file_exists($path)) return [];

        $accounts = [];
        $lines = file($path);
        foreach ($lines as $line) {
            $line = trim($line);
            if (!$line) continue;
            list($pk, $sk) = explode(',', $line, 2);
            $accounts[] = [trim($pk), trim($sk)];
        }
        return $accounts;
    }

    public static function getCredentials($publicKey = null, $secretKey = null) {
        // Tier 1: Arguments
        if ($publicKey && $secretKey) return [$publicKey, $secretKey];

        // Tier 2: Environment
        $pk = getenv('UNSANDBOX_PUBLIC_KEY');
        $sk = getenv('UNSANDBOX_SECRET_KEY');
        if ($pk && $sk) return [$pk, $sk];

        // Tier 3: Home directory
        $accounts = self::loadAccountsCsv();
        if ($accounts) return $accounts[0];

        // Tier 4: Local directory
        $accounts = self::loadAccountsCsv('./accounts.csv');
        if ($accounts) return $accounts[0];

        throw new Exception("No credentials found\n");
    }

    public static function signRequest($secret, $timestamp, $method, $endpoint, $body) {
        $message = "$timestamp:$method:$endpoint:$body";
        return hash_hmac('sha256', $message, $secret);
    }

    public static function apiRequest($method, $endpoint, $body = null, $opts = []) {
        list($pk, $sk) = self::getCredentials(
            $opts['publicKey'] ?? null,
            $opts['secretKey'] ?? null
        );

        $timestamp = time();
        $url = self::API_BASE . $endpoint;
        $bodyStr = $body ? json_encode($body) : '{}';
        $signature = self::signRequest($sk, $timestamp, $method, $endpoint, $bodyStr);

        $ch = curl_init($url);
        curl_setopt($ch, CURLOPT_CUSTOMREQUEST, $method);
        curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
        curl_setopt($ch, CURLOPT_SSL_VERIFYPEER, true);
        curl_setopt($ch, CURLOPT_HTTPHEADER, [
            'Authorization: Bearer ' . $pk,
            'X-Timestamp: ' . $timestamp,
            'X-Signature: ' . $signature,
            'Content-Type: application/json'
        ]);

        if ($body) {
            curl_setopt($ch, CURLOPT_POSTFIELDS, $bodyStr);
        }

        $response = curl_exec($ch);
        $code = curl_getinfo($ch, CURLINFO_HTTP_CODE);
        curl_close($ch);

        if ($code != 200) throw new Exception("API error ($code)");
        return json_decode($response, true);
    }

    public static function languages($cacheTtl = 3600) {
        $cacheDir = getenv('HOME') . '/.unsandbox';
        $cachePath = $cacheDir . '/languages.json';

        if (file_exists($cachePath)) {
            $age = time() - filemtime($cachePath);
            if ($age < $cacheTtl) {
                return json_decode(file_get_contents($cachePath), true);
            }
        }

        $result = self::apiRequest('GET', '/languages');
        $langs = $result['languages'] ?? [];

        if (!is_dir($cacheDir)) mkdir($cacheDir, 0700, true);
        file_put_contents($cachePath, json_encode($langs));

        return $langs;
    }

    public static function execute($language, $code, $opts = []) {
        $body = [
            'language' => $language,
            'code' => $code,
            'network_mode' => $opts['networkMode'] ?? 'zerotrust',
            'ttl' => $opts['ttl'] ?? 60
        ];
        return self::apiRequest('POST', '/execute', $body, $opts);
    }

    public static function executeAsync($language, $code, $opts = []) {
        $body = [
            'language' => $language,
            'code' => $code,
            'network_mode' => $opts['networkMode'] ?? 'zerotrust',
            'ttl' => $opts['ttl'] ?? 300
        ];
        return self::apiRequest('POST', '/execute/async', $body, $opts);
    }

    public static function run($file, $opts = []) {
        $code = file_get_contents($file);
        return self::execute(self::detectLanguage($file), $code, $opts);
    }

    public static function getJob($jobId, $opts = []) {
        return self::apiRequest('GET', "/jobs/$jobId", null, $opts);
    }

    public static function wait($jobId, $timeout = 3600, $opts = []) {
        $delays = [300, 450, 700, 900, 650, 1600, 2000];
        $start = time();

        for ($i = 0; $i < 120; $i++) {
            $job = self::getJob($jobId, $opts);
            if ($job['status'] === 'completed') return $job;
            if ($job['status'] === 'failed') throw new Exception("Job failed");
            if ($job['status'] === 'timeout') throw new Exception("Job timeout");

            if (time() - $start > $timeout) throw new Exception("Polling timeout");

            $delay = $delays[$i] ?? 2000;
            usleep($delay * 1000);
        }

        throw new Exception("Max polls exceeded");
    }

    public static function cancelJob($jobId, $opts = []) {
        return self::apiRequest('DELETE', "/jobs/$jobId", null, $opts);
    }

    public static function detectLanguage($filename) {
        $ext = pathinfo($filename, PATHINFO_EXTENSION);
        $map = ['py' => 'python', 'rb' => 'ruby', 'js' => 'javascript', 'php' => 'php',
                'lua' => 'lua', 'sh' => 'bash', 'go' => 'go', 'pl' => 'perl'];
        return $map[$ext] ?? throw new Exception("Unknown file type");
    }

    public static function image($code, $format = 'png', $opts = []) {
        return self::apiRequest('POST', '/image', ['code' => $code, 'format' => $format], $opts);
    }
}

// CLI
if (php_sapi_name() === 'cli' && !empty($GLOBALS['argv'])) {
    array_shift($GLOBALS['argv']);
    if (empty($GLOBALS['argv'])) {
        echo "Usage: php un.php <file>\n";
        exit(1);
    }

    try {
        $result = Un::run($GLOBALS['argv'][0]);
        if (!empty($result['stdout'])) echo $result['stdout'];
        if (!empty($result['stderr'])) fwrite(STDERR, $result['stderr']);
        exit($result['exit_code'] ?? 0);
    } catch (Exception $e) {
        fwrite(STDERR, "Error: " . $e->getMessage() . "\n");
        exit(1);
    }
}
?>
