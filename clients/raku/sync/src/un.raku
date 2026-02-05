#!/usr/bin/env raku
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
#
# unsandbox SDK for Raku - Execute code in secure sandboxes
# https://unsandbox.com | https://api.unsandbox.com/openapi
#
# Library Usage:
#   use lib '.';
#   use un;
#   my %result = execute("python", 'print("Hello")');
#   my %job = execute-async("python", $code);
#   my %result = wait(%job<job_id>);
#
# CLI Usage:
#   raku un.raku script.py
#   raku un.raku -s python 'print("Hello")'
#   raku un.raku session --shell python3
#
# Authentication (in priority order):
#   1. Function arguments: execute(..., :public-key<...>, :secret-key<...>)
#   2. Environment variables: UNSANDBOX_PUBLIC_KEY + UNSANDBOX_SECRET_KEY
#   3. Config file: ~/.unsandbox/accounts.csv (public_key,secret_key per line)

#!/usr/bin/env raku

unit module un;

use JSON::Fast;
use Digest::SHA;

# ============================================================================
# Configuration
# ============================================================================

constant $API_BASE is export = "https://api.unsandbox.com";
constant $PORTAL_BASE is export = "https://unsandbox.com";
constant $DEFAULT_TIMEOUT is export = 300;
constant $DEFAULT_TTL is export = 60;

# Polling delays (ms) - exponential backoff
my @POLL_DELAYS = (300, 450, 700, 900, 650, 1600, 2000);

# ANSI colors
constant $BLUE = "\e[34m";
constant $RED = "\e[31m";
constant $GREEN = "\e[32m";
constant $YELLOW = "\e[33m";
constant $RESET = "\e[0m";

# Extension to language mapping
my %EXT_MAP is export = (
    py => 'python', js => 'javascript', ts => 'typescript',
    rb => 'ruby', php => 'php', pl => 'perl', lua => 'lua',
    sh => 'bash', go => 'go', rs => 'rust', c => 'c',
    cpp => 'cpp', cc => 'cpp', cxx => 'cpp',
    java => 'java', kt => 'kotlin', cs => 'csharp', fs => 'fsharp',
    hs => 'haskell', ml => 'ocaml', clj => 'clojure', scm => 'scheme',
    lisp => 'commonlisp', erl => 'erlang', ex => 'elixir', exs => 'elixir',
    jl => 'julia', r => 'r', R => 'r', cr => 'crystal',
    d => 'd', nim => 'nim', zig => 'zig', v => 'vlang',
    dart => 'dart', groovy => 'groovy', scala => 'scala',
    f90 => 'fortran', f95 => 'fortran', cob => 'cobol',
    pro => 'prolog', forth => 'forth', '4th' => 'forth',
    tcl => 'tcl', raku => 'raku', pl6 => 'raku', p6 => 'raku',
    m => 'objc', awk => 'awk'
);

# ============================================================================
# Exceptions
# ============================================================================

#| Base exception class for unsandbox errors
class UnsandboxError is Exception is export {
    has $.message;
    method new($message) { self.bless(:$message) }
    method Str { $.message }
}

#| Authentication failed - invalid or missing credentials
class AuthenticationError is UnsandboxError is export { }

#| Code execution failed
class ExecutionError is UnsandboxError is export {
    has $.exit-code;
    has $.stderr;
}

#| API request failed
class APIError is UnsandboxError is export {
    has $.status-code;
    has $.response;
}

#| Execution timed out
class TimeoutError is UnsandboxError is export { }

# ============================================================================
# HMAC Authentication
# ============================================================================

#| Generate HMAC-SHA256 signature for API request
#| Signature = HMAC-SHA256(secret_key, "timestamp:METHOD:path:body")
sub sign-request(Str $secret-key, Int $timestamp, Str $method, Str $path, Str $body = "") returns Str is export {
    my $message = "{$timestamp}:{$method}:{$path}:{$body}";
    return hmac-hex($message, $secret-key, &sha256);
}

#| Get API credentials in priority order:
#| 1. Function arguments
#| 2. Environment variables
#| 3. ~/.unsandbox/accounts.csv
sub get-credentials(Str :$public-key, Str :$secret-key, Int :$account-index = 0) returns List is export {
    # Priority 1: Function arguments
    if $public-key && $secret-key {
        return ($public-key, $secret-key);
    }

    # Priority 2: Environment variables
    my $env-pk = %*ENV<UNSANDBOX_PUBLIC_KEY> // '';
    my $env-sk = %*ENV<UNSANDBOX_SECRET_KEY> // '';
    if $env-pk && $env-sk {
        return ($env-pk, $env-sk);
    }

    # Priority 3: Config file
    my $accounts-path = $*HOME.add('.unsandbox').add('accounts.csv');
    if $accounts-path.e {
        try {
            my @lines = $accounts-path.slurp.trim.split("\n");
            my @valid-accounts;
            for @lines -> $line {
                my $trimmed = $line.trim;
                next if !$trimmed || $trimmed.starts-with('#');
                if $trimmed.contains(',') {
                    my ($pk, $sk) = $trimmed.split(',', 2);
                    if $pk.starts-with('unsb-pk-') && $sk.starts-with('unsb-sk-') {
                        @valid-accounts.push(($pk, $sk));
                    }
                }
            }
            if @valid-accounts && $account-index < @valid-accounts.elems {
                return @valid-accounts[$account-index];
            }
        }
    }

    die AuthenticationError.new(
        "No credentials found. Set UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY, " ~
        "or create ~/.unsandbox/accounts.csv, or pass credentials to function."
    );
}

# ============================================================================
# HTTP Client
# ============================================================================

#| Make authenticated API request with HMAC signature
sub api-request(
    Str $endpoint,
    Str $method = 'GET',
    %data?,
    Str :$body-text,
    Str :$content-type = 'application/json',
    Str :$public-key,
    Str :$secret-key,
    Int :$timeout = $DEFAULT_TIMEOUT
) returns Hash is export {
    my ($pk, $sk) = get-credentials(:$public-key, :$secret-key);

    my $url = $API_BASE ~ $endpoint;
    my @args = 'curl', '-s', '--max-time', $timeout.Str;
    my $body = '';

    if $method eq 'GET' {
        @args.append: '-X', 'GET';
    } elsif $method eq 'DELETE' {
        @args.append: '-X', 'DELETE';
    } elsif $method eq 'POST' || $method eq 'PUT' || $method eq 'PATCH' {
        @args.append: '-X', $method;
        @args.append: '-H', "Content-Type: $content-type";
        if $body-text.defined {
            $body = $body-text;
            @args.append: '-d', $body;
        } elsif %data {
            $body = to-json(%data);
            @args.append: '-d', $body;
        }
    }

    @args.append: '-H', "Authorization: Bearer $pk";

    # Add HMAC signature
    my $timestamp = now.Int;
    my $signature = sign-request($sk, $timestamp, $method, $endpoint, $body);
    @args.append: '-H', "X-Timestamp: $timestamp";
    @args.append: '-H', "X-Signature: $signature";

    @args.append: $url;

    my $proc = run |@args, :out, :err;
    my $resp-body = $proc.out.slurp;
    my $err = $proc.err.slurp;

    if $proc.exitcode != 0 {
        die APIError.new("API request failed: $err", :status-code(0), :response($err));
    }

    # Check for clock drift errors
    if $resp-body.contains('timestamp') && ($resp-body.contains('401') || $resp-body.contains('expired') || $resp-body.contains('invalid')) {
        die AuthenticationError.new(
            "Request timestamp expired (must be within 5 minutes of server time). " ~
            "Your computer's clock may have drifted. Sync with NTP."
        );
    }

    return from-json($resp-body);
}

#| Make authenticated API request returning both status code and body
sub api-request-with-status(
    Str $endpoint,
    Str $method = 'GET',
    %data?,
    Str :$body-text,
    Str :$content-type = 'application/json',
    Str :$public-key,
    Str :$secret-key,
    Int :$timeout = $DEFAULT_TIMEOUT,
    Str :$sudo-otp,
    Str :$sudo-challenge
) returns List is export {
    my ($pk, $sk) = get-credentials(:$public-key, :$secret-key);

    my $url = $API_BASE ~ $endpoint;
    my @args = 'curl', '-s', '-w', '%{http_code}', '--max-time', $timeout.Str;
    my $body = '';

    if $method eq 'GET' {
        @args.append: '-X', 'GET';
    } elsif $method eq 'DELETE' {
        @args.append: '-X', 'DELETE';
    } elsif $method eq 'POST' || $method eq 'PUT' || $method eq 'PATCH' {
        @args.append: '-X', $method;
        @args.append: '-H', "Content-Type: $content-type";
        if $body-text.defined {
            $body = $body-text;
            @args.append: '-d', $body;
        } elsif %data {
            $body = to-json(%data);
            @args.append: '-d', $body;
        }
    }

    @args.append: '-H', "Authorization: Bearer $pk";

    # Add HMAC signature
    my $timestamp = now.Int;
    my $signature = sign-request($sk, $timestamp, $method, $endpoint, $body);
    @args.append: '-H', "X-Timestamp: $timestamp";
    @args.append: '-H', "X-Signature: $signature";

    # Add sudo OTP headers if provided
    if $sudo-otp.defined && $sudo-challenge.defined {
        @args.append: '-H', "X-Sudo-OTP: $sudo-otp";
        @args.append: '-H', "X-Sudo-Challenge: $sudo-challenge";
    }

    @args.append: $url;

    my $proc = run |@args, :out, :err;
    my $output = $proc.out.slurp;
    my $err = $proc.err.slurp;

    if $proc.exitcode != 0 {
        die APIError.new("API request failed: $err", :status-code(0), :response($err));
    }

    # Extract status code from end of output
    my $status-code = 0;
    my $resp-body = $output;
    if $output.chars >= 3 {
        my $code-str = $output.substr($output.chars - 3);
        $status-code = $code-str.Int // 0;
        $resp-body = $output.substr(0, $output.chars - 3);
    }

    return ($status-code, $resp-body);
}

#| Handle HTTP 428 sudo OTP challenge
#| Returns True if retry succeeded, False otherwise
sub handle-sudo-challenge(
    Str $response-body,
    Str $endpoint,
    Str $method,
    Str :$body-text,
    %data?,
    Str :$public-key,
    Str :$secret-key,
    Int :$timeout = $DEFAULT_TIMEOUT
) returns Bool is export {
    # Extract challenge_id from response
    my $challenge-id = '';
    try {
        my %resp = from-json($response-body);
        $challenge-id = %resp<challenge_id> // '';
    }

    unless $challenge-id {
        note "{$RED}Error: Could not extract challenge_id from response{$RESET}";
        return False;
    }

    # Prompt user for OTP
    note "{$YELLOW}Confirmation required. Check your email for a one-time code.{$RESET}";
    $*ERR.print("Enter OTP: ");
    my $otp = $*IN.get.trim;

    unless $otp {
        note "{$RED}Error: No OTP provided{$RESET}";
        return False;
    }

    # Retry request with sudo headers
    my ($status, $body) = api-request-with-status(
        $endpoint, $method, %data,
        :$body-text,
        :$public-key, :$secret-key, :$timeout,
        :sudo-otp($otp), :sudo-challenge($challenge-id)
    );

    if $status >= 200 && $status < 300 {
        return True;
    } else {
        note "{$RED}Error: OTP verification failed{$RESET}";
        return False;
    }
}

# ============================================================================
# Core Execution Functions
# ============================================================================

#| Execute code synchronously and return results
#|
#| Parameters:
#|   $language   - Programming language (python, javascript, go, rust, etc.)
#|   $code       - Source code to execute
#|   :%env       - Environment variables
#|   :@input-files - List of {filename => "...", content => "..."}
#|   :$network-mode - "zerotrust" (no network) or "semitrusted" (internet access)
#|   :$ttl       - Execution timeout in seconds (1-900, default 60)
#|   :$vcpu      - Virtual CPUs (1-8, default 1)
#|   :$return-artifact - Return compiled binary
#|   :$public-key - API public key
#|   :$secret-key - API secret key
#|
#| Returns: Hash with stdout, stderr, exit_code, language, job_id, etc.
#|
#| Example:
#|   my %result = execute("python", 'print("Hello World")');
#|   say %result<stdout>;
sub execute(
    Str $language,
    Str $code,
    :%env,
    :@input-files,
    Str :$network-mode = 'zerotrust',
    Int :$ttl = $DEFAULT_TTL,
    Int :$vcpu = 1,
    Bool :$return-artifact = False,
    Str :$public-key,
    Str :$secret-key,
    Int :$timeout = $DEFAULT_TIMEOUT
) returns Hash is export {
    my %payload = language => $language, code => $code, network_mode => $network-mode, ttl => $ttl, vcpu => $vcpu;

    %payload<env> = %env if %env;

    if @input-files {
        my @files;
        for @input-files -> %f {
            if %f<content_base64>:exists {
                @files.push(%f);
            } elsif %f<content>:exists {
                @files.push({
                    filename => %f<filename>,
                    content_base64 => %f<content>.encode.base64
                });
            } else {
                @files.push(%f);
            }
        }
        %payload<input_files> = @files;
    }

    %payload<return_artifact> = True if $return-artifact;

    return api-request('/execute', 'POST', %payload, :$public-key, :$secret-key, :$timeout);
}

#| Execute code asynchronously. Returns immediately with job_id for polling.
#|
#| Parameters: Same as execute()
#|
#| Returns: Hash with job_id, status ("pending")
#|
#| Example:
#|   my %job = execute-async("python", $long-running-code);
#|   say "Job submitted: ", %job<job_id>;
#|   my %result = wait(%job<job_id>);
sub execute-async(
    Str $language,
    Str $code,
    :%env,
    :@input-files,
    Str :$network-mode = 'zerotrust',
    Int :$ttl = $DEFAULT_TTL,
    Int :$vcpu = 1,
    Bool :$return-artifact = False,
    Str :$public-key,
    Str :$secret-key
) returns Hash is export {
    my %payload = language => $language, code => $code, network_mode => $network-mode, ttl => $ttl, vcpu => $vcpu;

    %payload<env> = %env if %env;

    if @input-files {
        my @files;
        for @input-files -> %f {
            if %f<content_base64>:exists {
                @files.push(%f);
            } elsif %f<content>:exists {
                @files.push({
                    filename => %f<filename>,
                    content_base64 => %f<content>.encode.base64
                });
            } else {
                @files.push(%f);
            }
        }
        %payload<input_files> = @files;
    }

    %payload<return_artifact> = True if $return-artifact;

    return api-request('/execute/async', 'POST', %payload, :$public-key, :$secret-key);
}

#| Execute code with automatic language detection from shebang
#|
#| Parameters:
#|   $code - Source code with shebang (e.g., #!/usr/bin/env python3)
#|   :%env - Environment variables
#|   :$network-mode - "zerotrust" or "semitrusted"
#|   :$ttl - Execution timeout in seconds
#|
#| Returns: Hash with detected_language, stdout, stderr, etc.
#|
#| Example:
#|   my $code = q:to/END/;
#|   #!/usr/bin/env python3
#|   print("Auto-detected!")
#|   END
#|   my %result = run($code);
#|   say %result<detected_language>;
sub run(
    Str $code,
    :%env,
    Str :$network-mode = 'zerotrust',
    Int :$ttl = $DEFAULT_TTL,
    Str :$public-key,
    Str :$secret-key,
    Int :$timeout = $DEFAULT_TIMEOUT
) returns Hash is export {
    my $endpoint = "/run?ttl={$ttl}&network_mode={$network-mode}";
    if %env {
        $endpoint ~= "&env=" ~ uri-encode(to-json(%env));
    }

    return api-request($endpoint, 'POST', :body-text($code), :content-type('text/plain'), :$public-key, :$secret-key, :$timeout);
}

#| Execute code asynchronously with automatic language detection
#|
#| Returns: Hash with job_id, detected_language, status ("pending")
sub run-async(
    Str $code,
    :%env,
    Str :$network-mode = 'zerotrust',
    Int :$ttl = $DEFAULT_TTL,
    Str :$public-key,
    Str :$secret-key
) returns Hash is export {
    my $endpoint = "/run/async?ttl={$ttl}&network_mode={$network-mode}";
    if %env {
        $endpoint ~= "&env=" ~ uri-encode(to-json(%env));
    }

    return api-request($endpoint, 'POST', :body-text($code), :content-type('text/plain'), :$public-key, :$secret-key);
}

# ============================================================================
# Job Management
# ============================================================================

#| Get job status and results
#|
#| Parameters:
#|   $job-id - Job ID from execute-async or run-async
#|
#| Returns: Hash with job_id, status, result (if completed), timestamps
#|
#| Status values: pending, running, completed, failed, timeout, cancelled
sub get-job(Str $job-id, Str :$public-key, Str :$secret-key) returns Hash is export {
    return api-request("/jobs/{$job-id}", 'GET', :$public-key, :$secret-key);
}

#| Wait for job completion with exponential backoff polling
#|
#| Parameters:
#|   $job-id - Job ID from execute-async or run-async
#|   :$max-polls - Maximum number of poll attempts (default 100)
#|
#| Returns: Final job result Hash
#|
#| Example:
#|   my %job = execute-async("python", $code);
#|   my %result = wait(%job<job_id>);
#|   say %result<stdout>;
sub wait(
    Str $job-id,
    Int :$max-polls = 100,
    Str :$public-key,
    Str :$secret-key
) returns Hash is export {
    my @terminal-states = <completed failed timeout cancelled>;

    for ^$max-polls -> $i {
        my $delay-idx = min($i, @POLL_DELAYS.elems - 1);
        sleep @POLL_DELAYS[$delay-idx] / 1000;

        my %result = get-job($job-id, :$public-key, :$secret-key);
        my $status = %result<status> // '';

        if $status (elem) @terminal-states {
            if $status eq 'failed' {
                die ExecutionError.new(
                    "Job failed: " ~ (%result<error> // 'Unknown error'),
                    :exit-code(%result<exit_code>),
                    :stderr(%result<stderr>)
                );
            }
            if $status eq 'timeout' {
                die TimeoutError.new("Job timed out: $job-id");
            }
            return %result;
        }
    }

    die TimeoutError.new("Max polls ($max-polls) exceeded for job $job-id");
}

#| Cancel a running job
#|
#| Returns: Partial output and artifacts collected before cancellation
sub cancel-job(Str $job-id, Str :$public-key, Str :$secret-key) returns Hash is export {
    return api-request("/jobs/{$job-id}", 'DELETE', :$public-key, :$secret-key);
}

#| List all active jobs for this API key
#|
#| Returns: List of job summary hashes with job_id, language, status, submitted_at
sub list-jobs(Str :$public-key, Str :$secret-key) returns Array is export {
    my %result = api-request('/jobs', 'GET', :$public-key, :$secret-key);
    return %result<jobs> // [];
}

# ============================================================================
# Image Generation
# ============================================================================

#| Generate images from text prompt
#|
#| Parameters:
#|   $prompt - Text description of the image to generate
#|   :$model - Model to use (optional)
#|   :$size - Image size (e.g., "1024x1024")
#|   :$quality - "standard" or "hd"
#|   :$n - Number of images to generate
#|
#| Returns: Hash with images array, created_at
#|
#| Example:
#|   my %result = image("A sunset over mountains");
#|   say %result<images>[0];
sub image(
    Str $prompt,
    Str :$model,
    Str :$size = '1024x1024',
    Str :$quality = 'standard',
    Int :$n = 1,
    Str :$public-key,
    Str :$secret-key
) returns Hash is export {
    my %payload = prompt => $prompt, size => $size, quality => $quality, n => $n;
    %payload<model> = $model if $model;

    return api-request('/image', 'POST', %payload, :$public-key, :$secret-key);
}

# ============================================================================
# Languages Cache
# ============================================================================

constant $LANGUAGES_CACHE_TTL = 3600;  # 1 hour in seconds

#| Get languages cache file path
sub languages-cache-path() returns IO::Path {
    return $*HOME.add('.unsandbox').add('languages.json');
}

#| Check if languages cache is valid (less than 1 hour old)
sub is-cache-valid() returns Bool {
    my $cache-path = languages-cache-path();
    return False unless $cache-path.e;

    my $mtime = $cache-path.modified;
    my $age = now - $mtime;
    return $age < $LANGUAGES_CACHE_TTL;
}

#| Read languages from cache
sub read-languages-cache() returns Hash {
    my $cache-path = languages-cache-path();
    return {} unless $cache-path.e;

    try {
        return from-json($cache-path.slurp);
        CATCH {
            default { return {}; }
        }
    }
}

#| Write languages to cache
sub write-languages-cache(%data) {
    my $cache-path = languages-cache-path();
    my $dir = $cache-path.parent;
    $dir.mkdir unless $dir.e;

    try {
        $cache-path.spurt(to-json(%data));
    }
}

# ============================================================================
# Utility Functions
# ============================================================================

#| Get list of supported programming languages with caching.
#| Languages are cached in ~/.unsandbox/languages.json for 1 hour.
#|
#| Returns: Hash with languages array, count, aliases
sub languages(Str :$public-key, Str :$secret-key) returns Hash is export {
    # Check cache first
    if is-cache-valid() {
        my %cached = read-languages-cache();
        return %cached if %cached;
    }

    # Fetch from API
    my %result = api-request('/languages', 'GET', :$public-key, :$secret-key);

    # Cache result
    write-languages-cache(%result);

    return %result;
}

# ============================================================================
# Session Functions
# ============================================================================

#| List all sessions for this API key
sub session-list(Str :$public-key, Str :$secret-key) returns Array is export {
    my %result = api-request('/sessions', 'GET', :$public-key, :$secret-key);
    return %result<sessions> // [];
}

#| Get session details by ID
sub session-get(Str $session-id, Str :$public-key, Str :$secret-key) returns Hash is export {
    return api-request("/sessions/{$session-id}", 'GET', :$public-key, :$secret-key);
}

#| Create a new interactive session
sub session-create(
    Str :$network-mode = 'zerotrust',
    Str :$shell = 'bash',
    Str :$public-key,
    Str :$secret-key
) returns Hash is export {
    my %payload = shell => $shell, network_mode => $network-mode;
    return api-request('/sessions', 'POST', %payload, :$public-key, :$secret-key);
}

#| Destroy a session
sub session-destroy(Str $session-id, Str :$public-key, Str :$secret-key) returns Bool is export {
    api-request("/sessions/{$session-id}", 'DELETE', :$public-key, :$secret-key);
    return True;
}

#| Freeze a session
sub session-freeze(Str $session-id, Str :$public-key, Str :$secret-key) returns Bool is export {
    api-request("/sessions/{$session-id}/freeze", 'POST', :$public-key, :$secret-key);
    return True;
}

#| Unfreeze a session
sub session-unfreeze(Str $session-id, Str :$public-key, Str :$secret-key) returns Bool is export {
    api-request("/sessions/{$session-id}/unfreeze", 'POST', :$public-key, :$secret-key);
    return True;
}

#| Boost session CPU (1-8 vCPUs)
sub session-boost(Str $session-id, Int $vcpu, Str :$public-key, Str :$secret-key) returns Bool is export {
    my %payload = vcpu => $vcpu;
    api-request("/sessions/{$session-id}/boost", 'POST', %payload, :$public-key, :$secret-key);
    return True;
}

#| Remove CPU boost from session
sub session-unboost(Str $session-id, Str :$public-key, Str :$secret-key) returns Bool is export {
    api-request("/sessions/{$session-id}/unboost", 'POST', :$public-key, :$secret-key);
    return True;
}

#| Execute command in a session
sub session-execute(Str $session-id, Str $command, Str :$public-key, Str :$secret-key) returns Hash is export {
    my %payload = command => $command;
    return api-request("/sessions/{$session-id}/execute", 'POST', %payload, :$public-key, :$secret-key);
}

# ============================================================================
# Service Functions
# ============================================================================

#| List all services for this API key
sub service-list(Str :$public-key, Str :$secret-key) returns Array is export {
    my %result = api-request('/services', 'GET', :$public-key, :$secret-key);
    return %result<services> // [];
}

#| Get service details by ID
sub service-get(Str $service-id, Str :$public-key, Str :$secret-key) returns Hash is export {
    return api-request("/services/{$service-id}", 'GET', :$public-key, :$secret-key);
}

#| Create a new service
sub service-create(
    Str :$name!,
    Str :$ports,
    Str :$domains,
    Str :$bootstrap,
    Str :$network-mode,
    Str :$public-key,
    Str :$secret-key
) returns Str is export {
    my %payload = name => $name;
    %payload<ports> = $ports.split(',')>>.Int if $ports;
    %payload<domains> = $domains.split(',') if $domains;
    %payload<bootstrap> = $bootstrap if $bootstrap;
    %payload<network_mode> = $network-mode if $network-mode;
    my %result = api-request('/services', 'POST', %payload, :$public-key, :$secret-key);
    return %result<id> // '';
}

#| Destroy a service
sub service-destroy(Str $service-id, Str :$public-key, Str :$secret-key) returns Bool is export {
    api-request("/services/{$service-id}", 'DELETE', :$public-key, :$secret-key);
    return True;
}

#| Freeze a service
sub service-freeze(Str $service-id, Str :$public-key, Str :$secret-key) returns Bool is export {
    api-request("/services/{$service-id}/freeze", 'POST', :$public-key, :$secret-key);
    return True;
}

#| Unfreeze a service
sub service-unfreeze(Str $service-id, Str :$public-key, Str :$secret-key) returns Bool is export {
    api-request("/services/{$service-id}/unfreeze", 'POST', :$public-key, :$secret-key);
    return True;
}

#| Lock a service (prevent deletion)
sub service-lock(Str $service-id, Str :$public-key, Str :$secret-key) returns Bool is export {
    api-request("/services/{$service-id}/lock", 'POST', :$public-key, :$secret-key);
    return True;
}

#| Unlock a service
sub service-unlock(Str $service-id, Str :$public-key, Str :$secret-key) returns Bool is export {
    api-request("/services/{$service-id}/unlock", 'POST', :$public-key, :$secret-key);
    return True;
}

#| Set unfreeze-on-demand for a service
sub service-set-unfreeze-on-demand(Str $service-id, Bool $enabled, Str :$public-key, Str :$secret-key) returns Bool is export {
    my %payload = unfreeze_on_demand => $enabled;
    api-request("/services/{$service-id}", 'PATCH', %payload, :$public-key, :$secret-key);
    return True;
}

#| Redeploy a service with optional new bootstrap
sub service-redeploy(Str $service-id, Str :$bootstrap, Str :$public-key, Str :$secret-key) returns Bool is export {
    my %payload;
    %payload<bootstrap> = $bootstrap if $bootstrap;
    api-request("/services/{$service-id}/redeploy", 'POST', %payload, :$public-key, :$secret-key);
    return True;
}

#| Get service logs
sub service-logs(Str $service-id, Bool :$all = False, Str :$public-key, Str :$secret-key) returns Str is export {
    my $endpoint = "/services/{$service-id}/logs";
    $endpoint ~= "?all=true" if $all;
    my %result = api-request($endpoint, 'GET', :$public-key, :$secret-key);
    return %result<logs> // '';
}

#| Execute command in a service
sub service-execute(Str $service-id, Str $command, Int :$timeout-ms, Str :$public-key, Str :$secret-key) returns Hash is export {
    my %payload = command => $command;
    %payload<timeout_ms> = $timeout-ms if $timeout-ms;
    return api-request("/services/{$service-id}/execute", 'POST', %payload, :$public-key, :$secret-key);
}

#| Get service environment vault
sub service-env-get(Str $service-id, Str :$public-key, Str :$secret-key) returns Str is export {
    my %result = api-request("/services/{$service-id}/env", 'GET', :$public-key, :$secret-key);
    return %result<content> // '';
}

#| Set service environment vault
sub service-env-set(Str $service-id, Str $content, Str :$public-key, Str :$secret-key) returns Bool is export {
    my ($pk, $sk) = get-credentials(:$public-key, :$secret-key);
    my $url = "{$API_BASE}/services/{$service-id}/env";
    my $timestamp = now.Int;
    my $signature = sign-request($sk, $timestamp, 'PUT', "/services/{$service-id}/env", $content);

    my @args = 'curl', '-s', '-X', 'PUT', $url;
    @args.append: '-H', 'Content-Type: text/plain';
    @args.append: '-H', "Authorization: Bearer $pk";
    @args.append: '-H', "X-Timestamp: $timestamp";
    @args.append: '-H', "X-Signature: $signature";
    @args.append: '-d', $content;

    my $proc = run |@args, :out, :err;
    return $proc.exitcode == 0;
}

#| Delete service environment vault
sub service-env-delete(Str $service-id, Str :$public-key, Str :$secret-key) returns Bool is export {
    api-request("/services/{$service-id}/env", 'DELETE', :$public-key, :$secret-key);
    return True;
}

#| Export service environment vault
sub service-env-export(Str $service-id, Str :$public-key, Str :$secret-key) returns Str is export {
    my %result = api-request("/services/{$service-id}/env/export", 'POST', :$public-key, :$secret-key);
    return %result<content> // '';
}

#| Resize service (change vCPU count)
sub service-resize(Str $service-id, Int $vcpu, Str :$public-key, Str :$secret-key) returns Bool is export {
    my %payload = vcpu => $vcpu;
    api-request("/services/{$service-id}", 'PATCH', %payload, :$public-key, :$secret-key);
    return True;
}

# ============================================================================
# Snapshot Functions
# ============================================================================

#| List all snapshots for this API key
sub snapshot-list(Str :$public-key, Str :$secret-key) returns Array is export {
    my %result = api-request('/snapshots', 'GET', :$public-key, :$secret-key);
    return %result<snapshots> // [];
}

#| Get snapshot details by ID
sub snapshot-get(Str $snapshot-id, Str :$public-key, Str :$secret-key) returns Hash is export {
    return api-request("/snapshots/{$snapshot-id}", 'GET', :$public-key, :$secret-key);
}

#| Create snapshot from a session
sub snapshot-session(Str $session-id, Str :$name, Bool :$hot = False, Str :$public-key, Str :$secret-key) returns Str is export {
    my %payload = session_id => $session-id, hot => $hot;
    %payload<name> = $name if $name;
    my %result = api-request('/snapshots', 'POST', %payload, :$public-key, :$secret-key);
    return %result<id> // '';
}

#| Create snapshot from a service
sub snapshot-service(Str $service-id, Str :$name, Bool :$hot = False, Str :$public-key, Str :$secret-key) returns Str is export {
    my %payload = service_id => $service-id, hot => $hot;
    %payload<name> = $name if $name;
    my %result = api-request('/snapshots', 'POST', %payload, :$public-key, :$secret-key);
    return %result<id> // '';
}

#| Restore a snapshot
sub snapshot-restore(Str $snapshot-id, Str :$public-key, Str :$secret-key) returns Bool is export {
    api-request("/snapshots/{$snapshot-id}/restore", 'POST', :$public-key, :$secret-key);
    return True;
}

#| Delete a snapshot
sub snapshot-delete(Str $snapshot-id, Str :$public-key, Str :$secret-key) returns Bool is export {
    api-request("/snapshots/{$snapshot-id}", 'DELETE', :$public-key, :$secret-key);
    return True;
}

#| Lock a snapshot (prevent deletion)
sub snapshot-lock(Str $snapshot-id, Str :$public-key, Str :$secret-key) returns Bool is export {
    api-request("/snapshots/{$snapshot-id}/lock", 'POST', :$public-key, :$secret-key);
    return True;
}

#| Unlock a snapshot
sub snapshot-unlock(Str $snapshot-id, Str :$public-key, Str :$secret-key) returns Bool is export {
    api-request("/snapshots/{$snapshot-id}/unlock", 'POST', :$public-key, :$secret-key);
    return True;
}

#| Clone a snapshot to create a new session or service
sub snapshot-clone(
    Str $snapshot-id,
    Str :$clone-type!,      # "session" or "service"
    Str :$name,             # name for cloned service
    Str :$ports,            # ports for cloned service
    Str :$shell,            # shell for cloned session
    Str :$public-key,
    Str :$secret-key
) returns Str is export {
    my %payload = clone_type => $clone-type;
    %payload<name> = $name if $name;
    %payload<ports> = $ports.split(',')>>.Int if $ports;
    %payload<shell> = $shell if $shell;
    my %result = api-request("/snapshots/{$snapshot-id}/clone", 'POST', %payload, :$public-key, :$secret-key);
    return %result<id> // '';
}

# ============================================================================
# Image Functions
# ============================================================================

#| List images (filter: owned, shared, public, or all)
sub image-list(Str :$filter, Str :$public-key, Str :$secret-key) returns Array is export {
    my $endpoint = '/images';
    $endpoint ~= "?filter={$filter}" if $filter;
    my %result = api-request($endpoint, 'GET', :$public-key, :$secret-key);
    return %result<images> // [];
}

#| Get image details by ID
sub image-get(Str $image-id, Str :$public-key, Str :$secret-key) returns Hash is export {
    return api-request("/images/{$image-id}", 'GET', :$public-key, :$secret-key);
}

#| Publish an image from a service or snapshot
sub image-publish(
    Str :$source-type!,     # "service" or "snapshot"
    Str :$source-id!,
    Str :$name,
    Str :$description,
    Str :$public-key,
    Str :$secret-key
) returns Str is export {
    my %payload = source_type => $source-type, source_id => $source-id;
    %payload<name> = $name if $name;
    %payload<description> = $description if $description;
    my %result = api-request('/images/publish', 'POST', %payload, :$public-key, :$secret-key);
    return %result<id> // '';
}

#| Delete an image
sub image-delete(Str $image-id, Str :$public-key, Str :$secret-key) returns Bool is export {
    api-request("/images/{$image-id}", 'DELETE', :$public-key, :$secret-key);
    return True;
}

#| Lock an image (prevent deletion)
sub image-lock(Str $image-id, Str :$public-key, Str :$secret-key) returns Bool is export {
    api-request("/images/{$image-id}/lock", 'POST', :$public-key, :$secret-key);
    return True;
}

#| Unlock an image
sub image-unlock(Str $image-id, Str :$public-key, Str :$secret-key) returns Bool is export {
    api-request("/images/{$image-id}/unlock", 'POST', :$public-key, :$secret-key);
    return True;
}

#| Set image visibility (private, unlisted, public)
sub image-set-visibility(Str $image-id, Str $visibility, Str :$public-key, Str :$secret-key) returns Bool is export {
    my %payload = visibility => $visibility;
    api-request("/images/{$image-id}/visibility", 'POST', %payload, :$public-key, :$secret-key);
    return True;
}

#| Grant access to an image for another API key
sub image-grant-access(Str $image-id, Str $trusted-api-key, Str :$public-key, Str :$secret-key) returns Bool is export {
    my %payload = trusted_api_key => $trusted-api-key;
    api-request("/images/{$image-id}/access/grant", 'POST', %payload, :$public-key, :$secret-key);
    return True;
}

#| Revoke access to an image from another API key
sub image-revoke-access(Str $image-id, Str $trusted-api-key, Str :$public-key, Str :$secret-key) returns Bool is export {
    my %payload = trusted_api_key => $trusted-api-key;
    api-request("/images/{$image-id}/access/revoke", 'POST', %payload, :$public-key, :$secret-key);
    return True;
}

#| List trusted API keys for an image
sub image-list-trusted(Str $image-id, Str :$public-key, Str :$secret-key) returns Array is export {
    my %result = api-request("/images/{$image-id}/access", 'GET', :$public-key, :$secret-key);
    return %result<trusted_keys> // [];
}

#| Transfer image ownership to another API key
sub image-transfer(Str $image-id, Str $to-api-key, Str :$public-key, Str :$secret-key) returns Bool is export {
    my %payload = to_api_key => $to-api-key;
    api-request("/images/{$image-id}/transfer", 'POST', %payload, :$public-key, :$secret-key);
    return True;
}

#| Spawn a new service from an image
sub image-spawn(
    Str $image-id,
    Str :$name,
    Str :$ports,
    Str :$bootstrap,
    Str :$network-mode,
    Str :$public-key,
    Str :$secret-key
) returns Str is export {
    my %payload;
    %payload<name> = $name if $name;
    %payload<ports> = $ports.split(',')>>.Int if $ports;
    %payload<bootstrap> = $bootstrap if $bootstrap;
    %payload<network_mode> = $network-mode if $network-mode;
    my %result = api-request("/images/{$image-id}/spawn", 'POST', %payload, :$public-key, :$secret-key);
    return %result<id> // '';
}

#| Clone an image
sub image-clone(Str $image-id, Str :$name, Str :$description, Str :$public-key, Str :$secret-key) returns Str is export {
    my %payload;
    %payload<name> = $name if $name;
    %payload<description> = $description if $description;
    my %result = api-request("/images/{$image-id}/clone", 'POST', %payload, :$public-key, :$secret-key);
    return %result<id> // '';
}

# ============================================================================
# PaaS Logs Functions
# ============================================================================

#| Fetch batch logs from portal
#| source: "all", "api", "portal", "pool/cammy", "pool/ai"
#| lines: number of lines (1-10000)
#| since: time window ("1m", "5m", "1h", "1d")
#| grep: optional filter pattern
sub logs-fetch(
    Str :$source = 'all',
    Int :$lines = 100,
    Str :$since = '1h',
    Str :$grep,
    Str :$public-key,
    Str :$secret-key
) returns Str is export {
    my $endpoint = "/logs?source={$source}&lines={$lines}&since={$since}";
    $endpoint ~= "&grep={uri-encode($grep)}" if $grep;
    my %result = api-request($endpoint, 'GET', :$public-key, :$secret-key);
    return to-json(%result);
}

#| Stream logs via SSE (blocking call)
#| Returns when interrupted or server closes connection
sub logs-stream(
    Str :$source = 'all',
    Str :$grep,
    :&callback,
    Str :$public-key,
    Str :$secret-key
) returns Bool is export {
    my ($pk, $sk) = get-credentials(:$public-key, :$secret-key);
    my $endpoint = "/logs/stream?source={$source}";
    $endpoint ~= "&grep={uri-encode($grep)}" if $grep;

    my $timestamp = now.Int;
    my $signature = sign-request($sk, $timestamp, 'GET', $endpoint, '');

    my @args = 'curl', '-s', '-N', "{$API_BASE}{$endpoint}";
    @args.append: '-H', "Authorization: Bearer $pk";
    @args.append: '-H', "X-Timestamp: $timestamp";
    @args.append: '-H', "X-Signature: $signature";
    @args.append: '-H', 'Accept: text/event-stream';

    my $proc = run |@args, :out;
    for $proc.out.lines -> $line {
        if $line.starts-with('data: ') {
            my $data = $line.substr(6);
            &callback($source, $data) if &callback;
        }
    }
    return True;
}

# ============================================================================
# Key Validation
# ============================================================================

#| Validate API keys and get account info
sub validate-keys(Str :$public-key, Str :$secret-key) returns Hash is export {
    my ($pk, $sk) = get-credentials(:$public-key, :$secret-key);

    my $url = "{$PORTAL_BASE}/keys/validate";
    my $timestamp = now.Int;
    my $signature = sign-request($sk, $timestamp, 'POST', '/keys/validate', '');

    my @args = 'curl', '-s', '-X', 'POST', $url;
    @args.append: '-H', 'Content-Type: application/json';
    @args.append: '-H', "Authorization: Bearer $pk";
    @args.append: '-H', "X-Timestamp: $timestamp";
    @args.append: '-H', "X-Signature: $signature";

    my $proc = run |@args, :out;
    my $resp = $proc.out.slurp;
    return from-json($resp);
}

# ============================================================================
# Utility Functions
# ============================================================================

#| Check API health
sub health-check(Str :$public-key, Str :$secret-key) returns Bool is export {
    try {
        my %result = api-request('/health', 'GET', :$public-key, :$secret-key);
        return %result<status> eq 'ok';
        CATCH { default { return False; } }
    }
}

#| Get SDK version
sub version() returns Str is export {
    return '0.1.0';
}

#| Detect programming language from file extension or shebang
#|
#| Returns: Language name or Nil if undetected
sub detect-language(Str $filename) returns Str is export {
    my $ext = $filename.IO.extension;

    return %EXT_MAP{$ext} if %EXT_MAP{$ext}:exists;

    # Try reading shebang
    if $filename.IO.e {
        my $first-line = $filename.IO.lines.head;
        if $first-line.starts-with('#!') {
            return 'python' if $first-line.contains('python');
            return 'javascript' if $first-line.contains('node');
            return 'ruby' if $first-line.contains('ruby');
            return 'perl' if $first-line.contains('perl');
            return 'bash' if $first-line.contains('bash') || $first-line.contains('/sh');
            return 'lua' if $first-line.contains('lua');
            return 'php' if $first-line.contains('php');
        }
    }

    return Nil;
}

# ============================================================================
# Client Class
# ============================================================================

#| Unsandbox API client with stored credentials
#|
#| Example:
#|   my $client = Client.new(:public-key<unsb-pk-...>, :secret-key<unsb-sk-...>);
#|   my %result = $client.execute("python", 'print("Hello")');
#|
#|   # Or load from environment/config automatically:
#|   my $client = Client.new;
#|   my %result = $client.execute("python", $code);
class Client is export {
    has Str $.public-key;
    has Str $.secret-key;

    #| Initialize client with credentials
    #|
    #| Parameters:
    #|   :$public-key - API public key (unsb-pk-...)
    #|   :$secret-key - API secret key (unsb-sk-...)
    #|   :$account-index - Account index in ~/.unsandbox/accounts.csv (default 0)
    method new(Str :$public-key, Str :$secret-key, Int :$account-index = 0) {
        my ($pk, $sk) = get-credentials(:$public-key, :$secret-key, :$account-index);
        self.bless(:public-key($pk), :secret-key($sk));
    }

    #| Execute code synchronously. See module execute() for parameters.
    method execute(Str $language, Str $code, *%opts) returns Hash {
        return execute($language, $code, :$.public-key, :$.secret-key, |%opts);
    }

    #| Execute code asynchronously. See module execute-async() for parameters.
    method execute-async(Str $language, Str $code, *%opts) returns Hash {
        return execute-async($language, $code, :$.public-key, :$.secret-key, |%opts);
    }

    #| Execute with auto-detect. See module run() for parameters.
    method run(Str $code, *%opts) returns Hash {
        return run($code, :$.public-key, :$.secret-key, |%opts);
    }

    #| Execute async with auto-detect. See module run-async() for parameters.
    method run-async(Str $code, *%opts) returns Hash {
        return run-async($code, :$.public-key, :$.secret-key, |%opts);
    }

    #| Get job status. See module get-job() for details.
    method get-job(Str $job-id) returns Hash {
        return get-job($job-id, :$.public-key, :$.secret-key);
    }

    #| Wait for job completion. See module wait() for details.
    method wait(Str $job-id, *%opts) returns Hash {
        return wait($job-id, :$.public-key, :$.secret-key, |%opts);
    }

    #| Cancel a job. See module cancel-job() for details.
    method cancel-job(Str $job-id) returns Hash {
        return cancel-job($job-id, :$.public-key, :$.secret-key);
    }

    #| List active jobs. See module list-jobs() for details.
    method list-jobs() returns Array {
        return list-jobs(:$.public-key, :$.secret-key);
    }

    #| Generate image. See module image() for parameters.
    method image(Str $prompt, *%opts) returns Hash {
        return image($prompt, :$.public-key, :$.secret-key, |%opts);
    }

    #| Get supported languages.
    method languages() returns Hash {
        return languages(:$.public-key, :$.secret-key);
    }

    # Session methods
    method session-list() returns Array { return session-list(:$.public-key, :$.secret-key); }
    method session-get(Str $id) returns Hash { return session-get($id, :$.public-key, :$.secret-key); }
    method session-create(*%opts) returns Hash { return session-create(:$.public-key, :$.secret-key, |%opts); }
    method session-destroy(Str $id) returns Bool { return session-destroy($id, :$.public-key, :$.secret-key); }
    method session-freeze(Str $id) returns Bool { return session-freeze($id, :$.public-key, :$.secret-key); }
    method session-unfreeze(Str $id) returns Bool { return session-unfreeze($id, :$.public-key, :$.secret-key); }
    method session-boost(Str $id, Int $vcpu) returns Bool { return session-boost($id, $vcpu, :$.public-key, :$.secret-key); }
    method session-unboost(Str $id) returns Bool { return session-unboost($id, :$.public-key, :$.secret-key); }
    method session-execute(Str $id, Str $cmd) returns Hash { return session-execute($id, $cmd, :$.public-key, :$.secret-key); }

    # Service methods
    method service-list() returns Array { return service-list(:$.public-key, :$.secret-key); }
    method service-get(Str $id) returns Hash { return service-get($id, :$.public-key, :$.secret-key); }
    method service-create(*%opts) returns Str { return service-create(:$.public-key, :$.secret-key, |%opts); }
    method service-destroy(Str $id) returns Bool { return service-destroy($id, :$.public-key, :$.secret-key); }
    method service-freeze(Str $id) returns Bool { return service-freeze($id, :$.public-key, :$.secret-key); }
    method service-unfreeze(Str $id) returns Bool { return service-unfreeze($id, :$.public-key, :$.secret-key); }
    method service-lock(Str $id) returns Bool { return service-lock($id, :$.public-key, :$.secret-key); }
    method service-unlock(Str $id) returns Bool { return service-unlock($id, :$.public-key, :$.secret-key); }
    method service-redeploy(Str $id, *%opts) returns Bool { return service-redeploy($id, :$.public-key, :$.secret-key, |%opts); }
    method service-logs(Str $id, *%opts) returns Str { return service-logs($id, :$.public-key, :$.secret-key, |%opts); }
    method service-execute(Str $id, Str $cmd, *%opts) returns Hash { return service-execute($id, $cmd, :$.public-key, :$.secret-key, |%opts); }
    method service-resize(Str $id, Int $vcpu) returns Bool { return service-resize($id, $vcpu, :$.public-key, :$.secret-key); }

    # Snapshot methods
    method snapshot-list() returns Array { return snapshot-list(:$.public-key, :$.secret-key); }
    method snapshot-get(Str $id) returns Hash { return snapshot-get($id, :$.public-key, :$.secret-key); }
    method snapshot-session(Str $id, *%opts) returns Str { return snapshot-session($id, :$.public-key, :$.secret-key, |%opts); }
    method snapshot-service(Str $id, *%opts) returns Str { return snapshot-service($id, :$.public-key, :$.secret-key, |%opts); }
    method snapshot-restore(Str $id) returns Bool { return snapshot-restore($id, :$.public-key, :$.secret-key); }
    method snapshot-delete(Str $id) returns Bool { return snapshot-delete($id, :$.public-key, :$.secret-key); }
    method snapshot-lock(Str $id) returns Bool { return snapshot-lock($id, :$.public-key, :$.secret-key); }
    method snapshot-unlock(Str $id) returns Bool { return snapshot-unlock($id, :$.public-key, :$.secret-key); }
    method snapshot-clone(Str $id, *%opts) returns Str { return snapshot-clone($id, :$.public-key, :$.secret-key, |%opts); }

    # Image methods
    method image-list(*%opts) returns Array { return image-list(:$.public-key, :$.secret-key, |%opts); }
    method image-get(Str $id) returns Hash { return image-get($id, :$.public-key, :$.secret-key); }
    method image-publish(*%opts) returns Str { return image-publish(:$.public-key, :$.secret-key, |%opts); }
    method image-delete(Str $id) returns Bool { return image-delete($id, :$.public-key, :$.secret-key); }
    method image-lock(Str $id) returns Bool { return image-lock($id, :$.public-key, :$.secret-key); }
    method image-unlock(Str $id) returns Bool { return image-unlock($id, :$.public-key, :$.secret-key); }
    method image-set-visibility(Str $id, Str $v) returns Bool { return image-set-visibility($id, $v, :$.public-key, :$.secret-key); }
    method image-grant-access(Str $id, Str $key) returns Bool { return image-grant-access($id, $key, :$.public-key, :$.secret-key); }
    method image-revoke-access(Str $id, Str $key) returns Bool { return image-revoke-access($id, $key, :$.public-key, :$.secret-key); }
    method image-list-trusted(Str $id) returns Array { return image-list-trusted($id, :$.public-key, :$.secret-key); }
    method image-transfer(Str $id, Str $key) returns Bool { return image-transfer($id, $key, :$.public-key, :$.secret-key); }
    method image-spawn(Str $id, *%opts) returns Str { return image-spawn($id, :$.public-key, :$.secret-key, |%opts); }
    method image-clone(Str $id, *%opts) returns Str { return image-clone($id, :$.public-key, :$.secret-key, |%opts); }

    # Logs methods
    method logs-fetch(*%opts) returns Str { return logs-fetch(:$.public-key, :$.secret-key, |%opts); }
    method logs-stream(*%opts) returns Bool { return logs-stream(:$.public-key, :$.secret-key, |%opts); }

    # Utility methods
    method validate-keys() returns Hash { return validate-keys(:$.public-key, :$.secret-key); }
    method health-check() returns Bool { return health-check(:$.public-key, :$.secret-key); }
    method version() returns Str { return version(); }
}

# ============================================================================
# CLI Interface
# ============================================================================

sub uri-encode(Str $s) {
    return $s.subst(/<-[A-Za-z0-9\-_.~]>/, { .encode.list.map({ '%' ~ .fmt('%02X') }).join }, :g);
}

sub cmd-execute(@args) {
    my ($public-key, $secret-key) = get-credentials();
    my $source-file = '';
    my %env-vars;
    my @input-files;
    my $artifacts = False;
    my $output-dir = '.';
    my $network = '';
    my $vcpu = 0;

    # Parse arguments
    my $i = 0;
    while $i < @args.elems {
        given @args[$i] {
            when '-e' {
                $i++;
                my ($key, $value) = @args[$i].split('=', 2);
                %env-vars{$key} = $value;
            }
            when '-f' {
                $i++;
                @input-files.push(@args[$i]);
            }
            when '-a' {
                $artifacts = True;
            }
            when '-o' {
                $i++;
                $output-dir = @args[$i];
            }
            when '-n' {
                $i++;
                $network = @args[$i];
            }
            when '-v' {
                $i++;
                $vcpu = @args[$i].Int;
            }
            default {
                if @args[$i].starts-with('-') {
                    note "$RED\Unknown option: {@args[$i]}$RESET";
                    exit 1;
                } else {
                    $source-file = @args[$i];
                }
            }
        }
        $i++;
    }

    unless $source-file {
        note "Usage: un.raku [options] <source_file>";
        exit 1;
    }

    unless $source-file.IO.e {
        note "{$RED}Error: File not found: $source-file{$RESET}";
        exit 1;
    }

    # Read source file
    my $code = $source-file.IO.slurp;
    my $language = detect-language($source-file);

    unless $language {
        note "{$RED}Error: Cannot detect language for $source-file{$RESET}";
        exit 1;
    }

    # Build request payload
    my %payload = language => $language, code => $code;

    # Add environment variables
    %payload<env> = %env-vars if %env-vars;

    # Add input files
    if @input-files {
        my @files;
        for @input-files -> $filepath {
            unless $filepath.IO.e {
                note "{$RED}Error: Input file not found: $filepath{$RESET}";
                exit 1;
            }
            my $content = $filepath.IO.slurp(:bin);
            @files.push({
                filename => $filepath.IO.basename,
                content_base64 => $content.encode('latin1').decode('latin1').encode.base64
            });
        }
        %payload<input_files> = @files;
    }

    # Add options
    %payload<return_artifacts> = True if $artifacts;
    %payload<network> = $network if $network;
    %payload<vcpu> = $vcpu if $vcpu > 0;

    # Execute
    my %result = api-request('/execute', 'POST', %payload, :$public-key, :$secret-key);

    # Print output
    if %result<stdout> {
        print "{$BLUE}{%result<stdout>}{$RESET}";
    }
    if %result<stderr> {
        note "{$RED}{%result<stderr>}{$RESET}";
    }

    # Save artifacts
    if $artifacts && %result<artifacts> {
        mkdir $output-dir unless $output-dir.IO.d;
        for %result<artifacts>.list -> %artifact {
            my $filename = %artifact<filename>;
            my $content = %artifact<content_base64>.decode('base64');
            my $path = "$output-dir/$filename";
            $path.IO.spurt($content, :bin);
            run 'chmod', '755', $path;
            note "{$GREEN}Saved: $path{$RESET}";
        }
    }

    exit %result<exit_code> // 0;
}

sub cmd-session(@args) {
    my ($public-key, $secret-key) = get-credentials();
    my $list-mode = False;
    my $kill-id = '';
    my $shell = '';
    my $network = '';
    my $vcpu = 0;
    my @input-files;

    # Parse arguments
    my $i = 0;
    while $i < @args.elems {
        given @args[$i] {
            when '--list' {
                $list-mode = True;
            }
            when '--kill' {
                $i++;
                $kill-id = @args[$i];
            }
            when '--shell' {
                $i++;
                $shell = @args[$i];
            }
            when '-n' {
                $i++;
                $network = @args[$i];
            }
            when '-v' {
                $i++;
                $vcpu = @args[$i].Int;
            }
            when '-f' {
                $i++;
                @input-files.push(@args[$i]);
            }
        }
        $i++;
    }

    if $list-mode {
        my %result = api-request('/sessions', 'GET', :$public-key, :$secret-key);
        my @sessions = %result<sessions>.list;
        unless @sessions {
            say "No active sessions";
            return;
        }
        say sprintf("%-40s %-10s %-10s %s", 'ID', 'Shell', 'Status', 'Created');
        for @sessions -> %s {
            say sprintf("%-40s %-10s %-10s %s",
                %s<id>, %s<shell>, %s<status>, %s<created_at>);
        }
        return;
    }

    if $kill-id {
        api-request("/sessions/$kill-id", 'DELETE', :$public-key, :$secret-key);
        say "{$GREEN}Session terminated: $kill-id{$RESET}";
        return;
    }

    # Create new session
    my %payload = shell => ($shell || 'bash');
    %payload<network> = $network if $network;
    %payload<vcpu> = $vcpu if $vcpu > 0;

    # Add input files
    if @input-files {
        my @files;
        for @input-files -> $filepath {
            unless $filepath.IO.e {
                note "{$RED}Error: Input file not found: $filepath{$RESET}";
                exit 1;
            }
            my $content = $filepath.IO.slurp(:bin);
            @files.push({
                filename => $filepath.IO.basename,
                content_base64 => $content.encode('latin1').decode('latin1').encode.base64
            });
        }
        %payload<input_files> = @files;
    }

    say "{$YELLOW}Creating session...{$RESET}";
    my %result = api-request('/sessions', 'POST', %payload, :$public-key, :$secret-key);
    say "{$GREEN}Session created: {%result<id>}{$RESET}";
    say "{$YELLOW}(Interactive sessions require WebSocket - use un2 for full support){$RESET}";
}

sub cmd-service(@args) {
    my ($public-key, $secret-key) = get-credentials();
    my $list-mode = False;
    my $info-id = '';
    my $logs-id = '';
    my $sleep-id = '';
    my $wake-id = '';
    my $destroy-id = '';
    my $resize-id = '';
    my $set-unfreeze-on-demand-id = '';
    my $set-unfreeze-on-demand-val = False;
    my $name = '';
    my $ports = '';
    my $type = '';
    my $bootstrap = '';
    my $bootstrap-file = '';
    my $network = '';
    my $vcpu = 0;
    my $unfreeze-on-demand = False;
    my @input-files;

    # Parse arguments
    my $i = 0;
    while $i < @args.elems {
        given @args[$i] {
            when '--list' {
                $list-mode = True;
            }
            when '--info' {
                $i++;
                $info-id = @args[$i];
            }
            when '--logs' {
                $i++;
                $logs-id = @args[$i];
            }
            when '--freeze' {
                $i++;
                $sleep-id = @args[$i];
            }
            when '--unfreeze' {
                $i++;
                $wake-id = @args[$i];
            }
            when '--destroy' {
                $i++;
                $destroy-id = @args[$i];
            }
            when '--resize' {
                $i++;
                $resize-id = @args[$i];
            }
            when '--set-unfreeze-on-demand' {
                $i++;
                $set-unfreeze-on-demand-id = @args[$i];
                $i++;
                my $val = @args[$i];
                $set-unfreeze-on-demand-val = ($val eq 'true' || $val eq '1');
            }
            when '--unfreeze-on-demand' {
                $unfreeze-on-demand = True;
            }
            when '--name' {
                $i++;
                $name = @args[$i];
            }
            when '--ports' {
                $i++;
                $ports = @args[$i];
            }
            when '--type' {
                $i++;
                $type = @args[$i];
            }
            when '--bootstrap' {
                $i++;
                $bootstrap = @args[$i];
            }
            when '--bootstrap-file' {
                $i++;
                $bootstrap-file = @args[$i];
            }
            when '-n' {
                $i++;
                $network = @args[$i];
            }
            when '-v' {
                $i++;
                $vcpu = @args[$i].Int;
            }
            when '-f' {
                $i++;
                @input-files.push(@args[$i]);
            }
        }
        $i++;
    }

    if $list-mode {
        my %result = api-request('/services', 'GET', :$public-key, :$secret-key);
        my @services = %result<services>.list;
        unless @services {
            say "No services";
            return;
        }
        say sprintf("%-20s %-15s %-10s %-15s %s", 'ID', 'Name', 'Status', 'Ports', 'Domains');
        for @services -> %s {
            my $port-str = %s<ports>.join(',');
            my $domain-str = %s<domains>.join(',');
            say sprintf("%-20s %-15s %-10s %-15s %s",
                %s<id>, %s<name>, %s<status>, $port-str, $domain-str);
        }
        return;
    }

    if $info-id {
        my %result = api-request("/services/$info-id", 'GET', :$public-key, :$secret-key);
        say to-json(%result, :pretty);
        return;
    }

    if $logs-id {
        my %result = api-request("/services/$logs-id/logs", 'GET', :$public-key, :$secret-key);
        say %result<logs>;
        return;
    }

    if $sleep-id {
        api-request("/services/$sleep-id/freeze", 'POST', :$public-key, :$secret-key);
        say "{$GREEN}Service frozen: $sleep-id{$RESET}";
        return;
    }

    if $wake-id {
        api-request("/services/$wake-id/unfreeze", 'POST', :$public-key, :$secret-key);
        say "{$GREEN}Service unfreezing: $wake-id{$RESET}";
        return;
    }

    if $destroy-id {
        my ($status, $body) = api-request-with-status("/services/$destroy-id", 'DELETE', :$public-key, :$secret-key);
        if $status == 428 {
            if handle-sudo-challenge($body, "/services/$destroy-id", 'DELETE', :$public-key, :$secret-key) {
                say "{$GREEN}Service destroyed: $destroy-id{$RESET}";
            } else {
                note "{$RED}Error: Failed to destroy service (OTP verification failed){$RESET}";
                exit 1;
            }
        } elsif $status >= 200 && $status < 300 {
            say "{$GREEN}Service destroyed: $destroy-id{$RESET}";
        } else {
            note "{$RED}Error: Failed to destroy service (HTTP $status){$RESET}";
            exit 1;
        }
        return;
    }

    if $resize-id {
        unless $vcpu >= 1 && $vcpu <= 8 {
            note "{$RED}Error: --resize requires --vcpu N (1-8){$RESET}";
            exit 1;
        }
        my %payload = vcpu => $vcpu;
        api-request("/services/$resize-id", 'PATCH', %payload, :$public-key, :$secret-key);
        my $ram = $vcpu * 2;
        say "{$GREEN}Service resized to $vcpu vCPU, $ram GB RAM{$RESET}";
        return;
    }

    if $set-unfreeze-on-demand-id {
        my %payload = unfreeze_on_demand => $set-unfreeze-on-demand-val;
        api-request("/services/$set-unfreeze-on-demand-id", 'PATCH', %payload, :$public-key, :$secret-key);
        my $status = $set-unfreeze-on-demand-val ?? 'enabled' !! 'disabled';
        say "{$GREEN}Unfreeze-on-demand $status for service: $set-unfreeze-on-demand-id{$RESET}";
        return;
    }

    # Create new service
    if $name {
        my %payload = name => $name;

        if $ports {
            %payload<ports> = $ports.split(',')>>.Int;
        }

        if $type {
            %payload<service_type> = $type;
        }

        if $bootstrap {
            %payload<bootstrap> = $bootstrap;
        }

        if $bootstrap-file {
            if $bootstrap-file.IO.e && $bootstrap-file.IO.f {
                %payload<bootstrap_content> = $bootstrap-file.IO.slurp;
            } else {
                note "{$RED}Error: Bootstrap file not found: $bootstrap-file{$RESET}";
                exit 1;
            }
        }

        %payload<network> = $network if $network;
        %payload<vcpu> = $vcpu if $vcpu > 0;
        %payload<unfreeze_on_demand> = True if $unfreeze-on-demand;

        # Add input files
        if @input-files {
            my @files;
            for @input-files -> $filepath {
                unless $filepath.IO.e {
                    note "{$RED}Error: Input file not found: $filepath{$RESET}";
                    exit 1;
                }
                my $content = $filepath.IO.slurp(:bin);
                @files.push({
                    filename => $filepath.IO.basename,
                    content_base64 => $content.encode('latin1').decode('latin1').encode.base64
                });
            }
            %payload<input_files> = @files;
        }

        my %result = api-request('/services', 'POST', %payload, :$public-key, :$secret-key);
        say "{$GREEN}Service created: {%result<id>}{$RESET}";
        say "Name: {%result<name>}";
        say "URL: {%result<url>}" if %result<url>;
        return;
    }

    note "{$RED}Error: Specify --name to create a service, or use --list, --info, etc.{$RESET}";
    exit 1;
}

sub cmd-languages(@args) {
    my ($public-key, $secret-key) = get-credentials();
    my $json-output = False;

    for @args -> $arg {
        if $arg eq '--json' {
            $json-output = True;
        }
    }

    my %result = languages(:$public-key, :$secret-key);
    my @langs = %result<languages>.list;

    if $json-output {
        # JSON array output
        say to-json(@langs);
    } else {
        # One language per line (default)
        for @langs -> $lang {
            say $lang;
        }
    }
}

sub cmd-key(@args) {
    my ($public-key, $secret-key) = get-credentials();
    my $extend = False;

    for @args -> $arg {
        if $arg eq '--extend' {
            $extend = True;
        }
    }

    # Validate key (using portal endpoint)
    my $url = $PORTAL_BASE ~ "/keys/validate";
    my @curl-args = 'curl', '-s', '-X', 'POST';
    @curl-args.append: $url;
    @curl-args.append: '-H', 'Content-Type: application/json';
    @curl-args.append: '-H', "Authorization: Bearer $public-key";

    my $timestamp = now.Int;
    my $sig-input = "{$timestamp}:POST:/keys/validate:";
    my $signature = hmac-hex($sig-input, $secret-key, &sha256);
    @curl-args.append: '-H', "X-Timestamp: $timestamp";
    @curl-args.append: '-H', "X-Signature: $signature";

    my $proc = run |@curl-args, :out, :err;
    my $body = $proc.out.slurp;

    my %result = from-json($body);

    if $extend {
        my $pk = %result<public_key>;
        if $pk {
            say "{$BLUE}Opening browser to extend key...{$RESET}";
            run 'xdg-open', "$PORTAL_BASE/keys/extend?pk=$pk";
            return;
        } else {
            note "{$RED}Error: Could not retrieve public key{$RESET}";
            exit 1;
        }
    }

    if %result<expired> {
        say "{$RED}Expired{$RESET}";
        say "Public Key: {%result<public_key> // 'N/A'}";
        say "Tier: {%result<tier> // 'N/A'}";
        say "Expired: {%result<expires_at> // 'N/A'}";
        say "{$YELLOW}To renew: Visit https://unsandbox.com/keys/extend{$RESET}";
        exit 1;
    }

    say "{$GREEN}Valid{$RESET}";
    say "Public Key: {%result<public_key> // 'N/A'}";
    say "Tier: {%result<tier> // 'N/A'}";
    say "Status: {%result<status> // 'N/A'}";
    say "Expires: {%result<expires_at> // 'N/A'}";
    say "Time Remaining: {%result<time_remaining> // 'N/A'}";
    say "Rate Limit: {%result<rate_limit> // 'N/A'}";
    say "Burst: {%result<burst> // 'N/A'}";
    say "Concurrency: {%result<concurrency> // 'N/A'}";
}

sub cmd-image(@args) {
    my ($public-key, $secret-key) = get-credentials();
    my $list-mode = False;
    my $info-id = '';
    my $delete-id = '';
    my $lock-id = '';
    my $unlock-id = '';
    my $publish-id = '';
    my $source-type = '';
    my $visibility-id = '';
    my $visibility-mode = '';
    my $spawn-id = '';
    my $clone-id = '';
    my $name = '';
    my $ports = '';

    # Parse arguments
    my $i = 0;
    while $i < @args.elems {
        given @args[$i] {
            when '--list' {
                $list-mode = True;
            }
            when '-l' {
                $list-mode = True;
            }
            when '--info' {
                $i++;
                $info-id = @args[$i];
            }
            when '--delete' {
                $i++;
                $delete-id = @args[$i];
            }
            when '--lock' {
                $i++;
                $lock-id = @args[$i];
            }
            when '--unlock' {
                $i++;
                $unlock-id = @args[$i];
            }
            when '--publish' {
                $i++;
                $publish-id = @args[$i];
            }
            when '--source-type' {
                $i++;
                $source-type = @args[$i];
            }
            when '--visibility' {
                $i++;
                $visibility-id = @args[$i];
                $i++;
                $visibility-mode = @args[$i] if $i < @args.elems && !@args[$i].starts-with('-');
            }
            when '--spawn' {
                $i++;
                $spawn-id = @args[$i];
            }
            when '--clone' {
                $i++;
                $clone-id = @args[$i];
            }
            when '--name' {
                $i++;
                $name = @args[$i];
            }
            when '--ports' {
                $i++;
                $ports = @args[$i];
            }
        }
        $i++;
    }

    if $list-mode {
        my %result = api-request('/images', 'GET', :$public-key, :$secret-key);
        my @images = %result<images>.list;
        unless @images {
            say "No images found";
            return;
        }
        say sprintf("%-40s %-20s %-12s %s", 'ID', 'Name', 'Visibility', 'Created');
        for @images -> %img {
            say sprintf("%-40s %-20s %-12s %s",
                %img<id> // 'N/A', %img<name> // '-', %img<visibility> // 'N/A', %img<created_at> // 'N/A');
        }
        return;
    }

    if $info-id {
        my %result = api-request("/images/$info-id", 'GET', :$public-key, :$secret-key);
        say "{$BLUE}Image Details{$RESET}";
        say "";
        say "Image ID: {%result<id> // 'N/A'}";
        say "Name: {%result<name> // '-'}";
        say "Visibility: {%result<visibility> // 'N/A'}";
        say "Created: {%result<created_at> // 'N/A'}";
        return;
    }

    if $delete-id {
        my ($status, $body) = api-request-with-status("/images/$delete-id", 'DELETE', :$public-key, :$secret-key);
        if $status == 428 {
            if handle-sudo-challenge($body, "/images/$delete-id", 'DELETE', :$public-key, :$secret-key) {
                say "{$GREEN}Image deleted successfully{$RESET}";
            } else {
                note "{$RED}Error: Failed to delete image (OTP verification failed){$RESET}";
                exit 1;
            }
        } elsif $status >= 200 && $status < 300 {
            say "{$GREEN}Image deleted successfully{$RESET}";
        } else {
            note "{$RED}Error: Failed to delete image (HTTP $status){$RESET}";
            exit 1;
        }
        return;
    }

    if $lock-id {
        api-request("/images/$lock-id/lock", 'POST', :$public-key, :$secret-key);
        say "{$GREEN}Image locked successfully{$RESET}";
        return;
    }

    if $unlock-id {
        my ($status, $body) = api-request-with-status("/images/$unlock-id/unlock", 'POST', :$public-key, :$secret-key);
        if $status == 428 {
            if handle-sudo-challenge($body, "/images/$unlock-id/unlock", 'POST', :$public-key, :$secret-key) {
                say "{$GREEN}Image unlocked successfully{$RESET}";
            } else {
                note "{$RED}Error: Failed to unlock image (OTP verification failed){$RESET}";
                exit 1;
            }
        } elsif $status >= 200 && $status < 300 {
            say "{$GREEN}Image unlocked successfully{$RESET}";
        } else {
            note "{$RED}Error: Failed to unlock image (HTTP $status){$RESET}";
            exit 1;
        }
        return;
    }

    if $publish-id {
        unless $source-type {
            note "{$RED}Error: --source-type required for --publish (service or snapshot){$RESET}";
            exit 1;
        }
        my %payload = source_type => $source-type, source_id => $publish-id;
        %payload<name> = $name if $name;
        my %result = api-request('/images/publish', 'POST', %payload, :$public-key, :$secret-key);
        say "{$GREEN}Image published successfully{$RESET}";
        say "Image ID: {%result<id> // 'N/A'}";
        return;
    }

    if $visibility-id {
        unless $visibility-mode {
            note "{$RED}Error: visibility mode required (private, unlisted, or public){$RESET}";
            exit 1;
        }
        my %payload = visibility => $visibility-mode;
        api-request("/images/$visibility-id/visibility", 'POST', %payload, :$public-key, :$secret-key);
        say "{$GREEN}Image visibility set to $visibility-mode{$RESET}";
        return;
    }

    if $spawn-id {
        my %payload;
        %payload<name> = $name if $name;
        if $ports {
            %payload<ports> = $ports.split(',')>>.Int;
        }
        my %result = api-request("/images/$spawn-id/spawn", 'POST', %payload, :$public-key, :$secret-key);
        say "{$GREEN}Service spawned from image{$RESET}";
        say "Service ID: {%result<id> // 'N/A'}";
        return;
    }

    if $clone-id {
        my %payload;
        %payload<name> = $name if $name;
        my %result = api-request("/images/$clone-id/clone", 'POST', %payload, :$public-key, :$secret-key);
        say "{$GREEN}Image cloned successfully{$RESET}";
        say "Image ID: {%result<id> // 'N/A'}";
        return;
    }

    note "{$RED}Error: Specify --list, --info ID, --delete ID, --lock ID, --unlock ID, --publish ID, --visibility ID MODE, --spawn ID, or --clone ID{$RESET}";
    exit 1;
}

sub cmd-snapshot(@args) {
    my ($public-key, $secret-key) = get-credentials();
    my $list-mode = False;
    my $info-id = '';
    my $delete-id = '';
    my $lock-id = '';
    my $unlock-id = '';
    my $restore-id = '';
    my $clone-id = '';
    my $session-id = '';
    my $service-id = '';
    my $name = '';
    my $hot = False;
    my $clone-type = '';
    my $ports = '';
    my $shell = '';

    # Parse arguments
    my $i = 0;
    while $i < @args.elems {
        given @args[$i] {
            when '--list' | '-l' { $list-mode = True; }
            when '--info' { $i++; $info-id = @args[$i]; }
            when '--delete' { $i++; $delete-id = @args[$i]; }
            when '--lock' { $i++; $lock-id = @args[$i]; }
            when '--unlock' { $i++; $unlock-id = @args[$i]; }
            when '--restore' { $i++; $restore-id = @args[$i]; }
            when '--clone' { $i++; $clone-id = @args[$i]; }
            when '--session' { $i++; $session-id = @args[$i]; }
            when '--service' { $i++; $service-id = @args[$i]; }
            when '--name' { $i++; $name = @args[$i]; }
            when '--hot' { $hot = True; }
            when '--clone-type' { $i++; $clone-type = @args[$i]; }
            when '--ports' { $i++; $ports = @args[$i]; }
            when '--shell' { $i++; $shell = @args[$i]; }
        }
        $i++;
    }

    if $list-mode {
        my @snapshots = snapshot-list(:$public-key, :$secret-key);
        unless @snapshots {
            say "No snapshots found";
            return;
        }
        say sprintf("%-40s %-20s %-10s %-10s %s", 'ID', 'Name', 'Type', 'Locked', 'Created');
        for @snapshots -> %s {
            say sprintf("%-40s %-20s %-10s %-10s %s",
                %s<id> // 'N/A', %s<name> // '-', %s<type> // 'N/A',
                %s<locked> ?? 'Yes' !! 'No', %s<created_at> // 'N/A');
        }
        return;
    }

    if $info-id {
        my %result = snapshot-get($info-id, :$public-key, :$secret-key);
        say to-json(%result, :pretty);
        return;
    }

    if $delete-id {
        my ($status, $body) = api-request-with-status("/snapshots/$delete-id", 'DELETE', :$public-key, :$secret-key);
        if $status == 428 {
            if handle-sudo-challenge($body, "/snapshots/$delete-id", 'DELETE', :$public-key, :$secret-key) {
                say "{$GREEN}Snapshot deleted: $delete-id{$RESET}";
            } else {
                note "{$RED}Error: Failed to delete snapshot (OTP verification failed){$RESET}";
                exit 1;
            }
        } elsif $status >= 200 && $status < 300 {
            say "{$GREEN}Snapshot deleted: $delete-id{$RESET}";
        } else {
            note "{$RED}Error: Failed to delete snapshot (HTTP $status){$RESET}";
            exit 1;
        }
        return;
    }

    if $lock-id {
        snapshot-lock($lock-id, :$public-key, :$secret-key);
        say "{$GREEN}Snapshot locked: $lock-id{$RESET}";
        return;
    }

    if $unlock-id {
        my ($status, $body) = api-request-with-status("/snapshots/$unlock-id/unlock", 'POST', :$public-key, :$secret-key);
        if $status == 428 {
            if handle-sudo-challenge($body, "/snapshots/$unlock-id/unlock", 'POST', :$public-key, :$secret-key) {
                say "{$GREEN}Snapshot unlocked: $unlock-id{$RESET}";
            } else {
                note "{$RED}Error: Failed to unlock snapshot (OTP verification failed){$RESET}";
                exit 1;
            }
        } elsif $status >= 200 && $status < 300 {
            say "{$GREEN}Snapshot unlocked: $unlock-id{$RESET}";
        } else {
            note "{$RED}Error: Failed to unlock snapshot (HTTP $status){$RESET}";
            exit 1;
        }
        return;
    }

    if $restore-id {
        snapshot-restore($restore-id, :$public-key, :$secret-key);
        say "{$GREEN}Snapshot restored: $restore-id{$RESET}";
        return;
    }

    if $clone-id {
        unless $clone-type {
            note "{$RED}Error: --clone-type required (session or service){$RESET}";
            exit 1;
        }
        my $new-id = snapshot-clone($clone-id, :$clone-type, :$name, :$ports, :$shell, :$public-key, :$secret-key);
        say "{$GREEN}Cloned to $clone-type: $new-id{$RESET}";
        return;
    }

    if $session-id {
        my $snap-id = snapshot-session($session-id, :$name, :$hot, :$public-key, :$secret-key);
        say "{$GREEN}Snapshot created: $snap-id{$RESET}";
        return;
    }

    if $service-id {
        my $snap-id = snapshot-service($service-id, :$name, :$hot, :$public-key, :$secret-key);
        say "{$GREEN}Snapshot created: $snap-id{$RESET}";
        return;
    }

    note "{$RED}Error: Specify --list, --info ID, --delete ID, --lock ID, --unlock ID, --restore ID, --clone ID, --session ID, or --service ID{$RESET}";
    exit 1;
}

sub MAIN(*@args) is export {
    unless @args {
        note "Usage: un.raku [options] <source_file>";
        note "       un.raku session [options]";
        note "       un.raku service [options]";
        note "       un.raku snapshot [options]";
        note "       un.raku image [options]";
        note "       un.raku key [options]";
        note "       un.raku languages [--json]";
        note "";
        note "Languages options:";
        note "  --json              Output as JSON array";
        note "";
        note "Snapshot options:";
        note "  --list              List all snapshots";
        note "  --info ID           Get snapshot details";
        note "  --delete ID         Delete a snapshot";
        note "  --lock ID           Lock snapshot to prevent deletion";
        note "  --unlock ID         Unlock snapshot";
        note "  --restore ID        Restore a snapshot";
        note "  --clone ID          Clone snapshot (requires --clone-type)";
        note "  --session ID        Create snapshot from session";
        note "  --service ID        Create snapshot from service";
        note "  --name NAME         Name for the snapshot";
        note "  --hot               Create hot snapshot (while running)";
        note "  --clone-type TYPE   Clone type: session or service";
        note "  --ports PORTS       Ports for cloned service";
        note "  --shell SHELL       Shell for cloned session";
        note "";
        note "Image options:";
        note "  --list              List all images";
        note "  --info ID           Get image details";
        note "  --delete ID         Delete an image";
        note "  --lock ID           Lock image to prevent deletion";
        note "  --unlock ID         Unlock image";
        note "  --publish ID        Publish image from service/snapshot";
        note "  --source-type TYPE  Source type: service or snapshot";
        note "  --visibility ID MODE  Set visibility: private, unlisted, public";
        note "  --spawn ID          Spawn new service from image";
        note "  --clone ID          Clone an image";
        note "  --name NAME         Name for spawned service or cloned image";
        note "  --ports PORTS       Ports for spawned service";
        note "  --grant ID KEY      Grant image access to API key";
        note "  --revoke ID KEY     Revoke image access from API key";
        note "  --trusted ID        List trusted API keys for image";
        note "  --transfer ID KEY   Transfer image ownership";
        exit 1;
    }

    given @args[0] {
        when 'session' {
            cmd-session(@args[1..*]);
        }
        when 'service' {
            cmd-service(@args[1..*]);
        }
        when 'snapshot' {
            cmd-snapshot(@args[1..*]);
        }
        when 'image' {
            cmd-image(@args[1..*]);
        }
        when 'key' {
            cmd-key(@args[1..*]);
        }
        when 'languages' {
            cmd-languages(@args[1..*]);
        }
        default {
            cmd-execute(@args);
        }
    }
}
