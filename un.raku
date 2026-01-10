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

#!/usr/bin/env raku

# unsandbox CLI - Raku implementation
# Full-featured CLI matching un.c/un.py capabilities

use JSON::Fast;
use Digest::SHA;

constant $API_BASE = "https://api.unsandbox.com";
constant $PORTAL_BASE = "https://unsandbox.com";
constant $BLUE = "\e[34m";
constant $RED = "\e[31m";
constant $GREEN = "\e[32m";
constant $YELLOW = "\e[33m";
constant $RESET = "\e[0m";

my %EXT_MAP = (
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
    m => 'objc'
);

sub get-api-keys() {
    my $public-key = %*ENV<UNSANDBOX_PUBLIC_KEY> // '';
    my $secret-key = %*ENV<UNSANDBOX_SECRET_KEY> // '';

    # Fallback to old UNSANDBOX_API_KEY for backwards compat
    if !$public-key && %*ENV<UNSANDBOX_API_KEY> {
        $public-key = %*ENV<UNSANDBOX_API_KEY>;
        $secret-key = '';
    }

    unless $public-key {
        note "{$RED}Error: UNSANDBOX_PUBLIC_KEY or UNSANDBOX_API_KEY not set{$RESET}";
        exit 1;
    }
    return ($public-key, $secret-key);
}

sub detect-language(Str $filename --> Str) {
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

    note "{$RED}Error: Cannot detect language for $filename{$RESET}";
    exit 1;
}

sub api-request(Str $endpoint, Str $method, %data?, Str :$public-key!, Str :$secret-key!) {
    my $url = $API_BASE ~ $endpoint;
    my @args = 'curl', '-s';
    my $body = '';

    if $method eq 'GET' {
        @args.append: '-X', 'GET';
    } elsif $method eq 'DELETE' {
        @args.append: '-X', 'DELETE';
    } elsif $method eq 'POST' {
        @args.append: '-X', 'POST';
        @args.append: '-H', 'Content-Type: application/json';
        if %data {
            $body = to-json(%data);
            @args.append: '-d', $body;
        }
    }

    @args.append: '-H', "Authorization: Bearer $public-key";

    # Add HMAC signature if secret-key is present
    if $secret-key {
        my $timestamp = now.Int;
        my $sig-input = "{$timestamp}:{$method}:{$endpoint}:{$body}";
        my $signature = hmac-hex($sig-input, $secret-key, &sha256);
        @args.append: '-H', "X-Timestamp: $timestamp";
        @args.append: '-H', "X-Signature: $signature";
    }

    @args.append: $url;

    my $proc = run |@args, :out, :err;
    my $resp-body = $proc.out.slurp;
    my $err = $proc.err.slurp;

    if $proc.exitcode != 0 {
        note "{$RED}Error: API request failed{$RESET}";
        note $err if $err;
        exit 1;
    }

    # Check for clock drift errors
    if $resp-body.contains('timestamp') && ($resp-body.contains('401') || $resp-body.contains('expired') || $resp-body.contains('invalid')) {
        note "{$RED}Error: Request timestamp expired (must be within 5 minutes of server time){$RESET}";
        note "{$YELLOW}Your computer's clock may have drifted.{$RESET}";
        note "{$YELLOW}Check your system time and sync with NTP if needed:{$RESET}";
        note "{$YELLOW}  Linux:   sudo ntpdate -s time.nist.gov{$RESET}";
        note "{$YELLOW}  macOS:   sudo sntp -sS time.apple.com{$RESET}";
        note "{$YELLOW}  Windows: w32tm /resync{$RESET}";
        exit 1;
    }

    return from-json($resp-body);
}

# API request for PUT with text/plain body (used for vault)
sub api-request-put-text(Str $endpoint, Str $content, Str :$public-key!, Str :$secret-key!) {
    my $url = $API_BASE ~ $endpoint;
    my @args = 'curl', '-s', '-X', 'PUT';
    @args.append: '-H', 'Content-Type: text/plain';
    @args.append: '-H', "Authorization: Bearer $public-key";

    # Add HMAC signature if secret-key is present
    if $secret-key {
        my $timestamp = now.Int;
        my $sig-input = "{$timestamp}:PUT:{$endpoint}:{$content}";
        my $signature = hmac-hex($sig-input, $secret-key, &sha256);
        @args.append: '-H', "X-Timestamp: $timestamp";
        @args.append: '-H', "X-Signature: $signature";
    }

    @args.append: '--data-binary', $content;
    @args.append: $url;

    my $proc = run |@args, :out, :err;
    my $resp-body = $proc.out.slurp;

    return from-json($resp-body);
}

sub cmd-execute(@args) {
    my ($public-key, $secret-key) = get-api-keys();
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
    my ($public-key, $secret-key) = get-api-keys();
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

# Service vault functions
sub service-env-status(Str $service-id, Str :$public-key!, Str :$secret-key!) {
    my %result = api-request("/services/$service-id/env", 'GET', :$public-key, :$secret-key);
    say to-json(%result, :pretty);
}

sub service-env-set(Str $service-id, Str $content, Str :$public-key!, Str :$secret-key!) {
    my %result = api-request-put-text("/services/$service-id/env", $content, :$public-key, :$secret-key);
    say to-json(%result, :pretty);
}

sub service-env-export(Str $service-id, Str :$public-key!, Str :$secret-key!) {
    my %result = api-request("/services/$service-id/env/export", 'POST', :$public-key, :$secret-key);
    say %result<content> if %result<content>;
}

sub service-env-delete(Str $service-id, Str :$public-key!, Str :$secret-key!) {
    api-request("/services/$service-id/env", 'DELETE', :$public-key, :$secret-key);
    say "{$GREEN}Vault deleted for: $service-id{$RESET}";
}

sub build-env-content(@env-vars, Str $env-file --> Str) {
    my @lines;
    for @env-vars -> $var {
        @lines.push($var);
    }
    if $env-file && $env-file.IO.e {
        for $env-file.IO.lines -> $line {
            next if $line.starts-with('#') || $line.trim eq '';
            @lines.push($line);
        }
    }
    return @lines.join("\n");
}

sub cmd-service(@args) {
    my ($public-key, $secret-key) = get-api-keys();
    my $list-mode = False;
    my $info-id = '';
    my $logs-id = '';
    my $sleep-id = '';
    my $wake-id = '';
    my $destroy-id = '';
    my $dump-bootstrap-id = '';
    my $dump-file = '';
    my $name = '';
    my $ports = '';
    my $type = '';
    my $bootstrap = '';
    my $bootstrap-file = '';
    my $network = '';
    my $vcpu = 0;
    my @input-files;
    my @env-vars;
    my $env-file = '';
    my $env-action = '';
    my $env-target = '';

    # Check for 'env' subcommand first
    if @args.elems >= 1 && @args[0] eq 'env' {
        if @args.elems < 3 {
            note "Usage: un.raku service env <status|set|export|delete> <service_id> [options]";
            exit 1;
        }
        $env-action = @args[1];
        $env-target = @args[2];

        # Parse remaining args for -e and --env-file
        my $i = 3;
        while $i < @args.elems {
            given @args[$i] {
                when '-e' {
                    $i++;
                    @env-vars.push(@args[$i]);
                }
                when '--env-file' {
                    $i++;
                    $env-file = @args[$i];
                }
            }
            $i++;
        }

        given $env-action {
            when 'status' {
                service-env-status($env-target, :$public-key, :$secret-key);
            }
            when 'set' {
                my $content = build-env-content(@env-vars, $env-file);
                if !$content {
                    note "{$RED}Error: No environment variables to set{$RESET}";
                    exit 1;
                }
                service-env-set($env-target, $content, :$public-key, :$secret-key);
            }
            when 'export' {
                service-env-export($env-target, :$public-key, :$secret-key);
            }
            when 'delete' {
                service-env-delete($env-target, :$public-key, :$secret-key);
            }
            default {
                note "{$RED}Error: Unknown env action '$env-action'. Use status, set, export, or delete{$RESET}";
                exit 1;
            }
        }
        return;
    }

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
            when '--dump-bootstrap' {
                $i++;
                $dump-bootstrap-id = @args[$i];
            }
            when '--dump-file' {
                $i++;
                $dump-file = @args[$i];
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
            when '-e' {
                $i++;
                @env-vars.push(@args[$i]);
            }
            when '--env-file' {
                $i++;
                $env-file = @args[$i];
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
        api-request("/services/$sleep-id/sleep", 'POST', :$public-key, :$secret-key);
        say "{$GREEN}Service sleeping: $sleep-id{$RESET}";
        return;
    }

    if $wake-id {
        api-request("/services/$wake-id/wake", 'POST', :$public-key, :$secret-key);
        say "{$GREEN}Service waking: $wake-id{$RESET}";
        return;
    }

    if $destroy-id {
        api-request("/services/$destroy-id", 'DELETE', :$public-key, :$secret-key);
        say "{$GREEN}Service destroyed: $destroy-id{$RESET}";
        return;
    }

    if $dump-bootstrap-id {
        note "Fetching bootstrap script from $dump-bootstrap-id...";
        my %payload = command => "cat /tmp/bootstrap.sh";
        my %result = api-request("/services/$dump-bootstrap-id/execute", 'POST', %payload, :$public-key, :$secret-key);

        if %result<stdout> && %result<stdout> ne '' {
            my $bootstrap = %result<stdout>;
            if $dump-file {
                # Write to file
                $dump-file.IO.spurt($bootstrap);
                run 'chmod', '755', $dump-file;
                say "Bootstrap saved to $dump-file";
            } else {
                # Print to stdout
                print $bootstrap;
            }
        } else {
            note "{$RED}Error: Failed to fetch bootstrap (service not running or no bootstrap file){$RESET}";
            exit 1;
        }
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

        # Auto-set vault if -e or --env-file were provided
        my $env-content = build-env-content(@env-vars, $env-file);
        if $env-content && %result<id> {
            say "{$YELLOW}Setting vault for service...{$RESET}";
            service-env-set(%result<id>, $env-content, :$public-key, :$secret-key);
        }
        return;
    }

    note "{$RED}Error: Specify --name to create a service, or use --list, --info, env, etc.{$RESET}";
    exit 1;
}

sub validate-key(Bool $extend) {
    my ($public-key, $secret-key) = get-api-keys();

    # Build curl command
    my @args = 'curl', '-s', '-X', 'POST';
    @args.append: "$PORTAL_BASE/keys/validate";
    @args.append: '-H', 'Content-Type: application/json';
    @args.append: '-H', "Authorization: Bearer $public-key";

    # Add HMAC signature if secret-key is present
    if $secret-key {
        my $timestamp = now.Int;
        my $sig-input = "{$timestamp}:POST:/keys/validate:";
        my $signature = hmac-hex($sig-input, $secret-key, &sha256);
        @args.append: '-H', "X-Timestamp: $timestamp";
        @args.append: '-H', "X-Signature: $signature";
    }

    my $proc = run |@args, :out, :err;
    my $body = $proc.out.slurp;
    my $err-msg = $proc.err.slurp;

    if $proc.exitcode != 0 {
        say "{$RED}Invalid{$RESET}";
        note "Reason: $err-msg" if $err-msg;
        exit 1;
    }

    my %result = from-json($body);

    # Handle --extend flag
    if $extend {
        my $public-key = %result<public_key>;
        if $public-key {
            say "{$BLUE}Opening browser to extend key...{$RESET}";
            run 'xdg-open', "$PORTAL_BASE/keys/extend?pk=$public-key";
            return;
        } else {
            note "{$RED}Error: Could not retrieve public key{$RESET}";
            exit 1;
        }
    }

    # Check if key is expired
    if %result<expired> {
        say "{$RED}Expired{$RESET}";
        say "Public Key: {%result<public_key> // 'N/A'}";
        say "Tier: {%result<tier> // 'N/A'}";
        say "Expired: {%result<expires_at> // 'N/A'}";
        say "{$YELLOW}To renew: Visit https://unsandbox.com/keys/extend{$RESET}";
        exit 1;
    }

    # Valid key
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

sub cmd-key(@args) {
    my $extend = False;

    # Parse arguments
    for @args -> $arg {
        if $arg eq '--extend' {
            $extend = True;
        }
    }

    validate-key($extend);
}

sub MAIN(*@args) {
    unless @args {
        note "Usage: un.raku [options] <source_file>";
        note "       un.raku session [options]";
        note "       un.raku service [options]";
        note "       un.raku key [options]";
        exit 1;
    }

    given @args[0] {
        when 'session' {
            cmd-session(@args[1..*]);
        }
        when 'service' {
            cmd-service(@args[1..*]);
        }
        when 'key' {
            cmd-key(@args[1..*]);
        }
        default {
            cmd-execute(@args);
        }
    }
}
