#!/usr/bin/env perl
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
# unsandbox SDK for Perl - Execute code in secure sandboxes
# https://unsandbox.com | https://api.unsandbox.com/openapi

use strict;
use warnings;
use JSON;
use LWP::UserAgent;
use HTTP::Request;
use Digest::HMAC_SHA256 qw(hmac_sha256_hex);
use File::HomeDir;
use Time::HiRes qw(time sleep);

our $VERSION = "4.2.22";
our $API_BASE = 'https://api.unsandbox.com';

# Credential system
sub load_accounts_csv {
    my ($path) = @_;
    $path ||= File::HomeDir->my_home . "/.unsandbox/accounts.csv";
    return [] unless -e $path;

    my @accounts;
    open my $fh, '<', $path or return [];
    while (my $line = <$fh>) {
        chomp $line;
        next if !$line;
        my ($pk, $sk) = split /,/, $line, 2;
        push @accounts, [$pk, $sk] if $pk && $sk;
    }
    close $fh;
    return \@accounts;
}

sub get_credentials {
    my (%opts) = @_;

    # Tier 1: Arguments
    return ($opts{public_key}, $opts{secret_key}) if $opts{public_key} && $opts{secret_key};

    # Tier 2: Environment
    if ($ENV{UNSANDBOX_PUBLIC_KEY} && $ENV{UNSANDBOX_SECRET_KEY}) {
        return ($ENV{UNSANDBOX_PUBLIC_KEY}, $ENV{UNSANDBOX_SECRET_KEY});
    }

    # Tier 3: Home directory
    my $home_accounts = load_accounts_csv();
    return @{$home_accounts->[0]} if @$home_accounts;

    # Tier 4: Local directory
    my $local_accounts = load_accounts_csv("./accounts.csv");
    return @{$local_accounts->[0]} if @$local_accounts;

    die "No credentials found\n";
}

# HMAC signature
sub sign_request {
    my ($secret, $timestamp, $method, $endpoint, $body) = @_;
    my $message = "$timestamp:$method:$endpoint:$body";
    return hmac_sha256_hex($message, $secret);
}

# API communication
sub api_request {
    my ($method, $endpoint, $body, %opts) = @_;
    my ($pk, $sk) = get_credentials(%opts);

    my $timestamp = int(time);
    my $body_str = $body ? JSON::to_json($body) : '{}';
    my $signature = sign_request($sk, $timestamp, $method, $endpoint, $body_str);

    my $ua = LWP::UserAgent->new;
    my $url = "$API_BASE$endpoint";
    my $req = HTTP::Request->new($method, $url);

    $req->header('Authorization' => "Bearer $pk");
    $req->header('X-Timestamp' => $timestamp);
    $req->header('X-Signature' => $signature);
    $req->header('Content-Type' => 'application/json');
    $req->content($body_str) if $body;

    my $res = $ua->request($req);
    die "API error (" . $res->code . ")\n" unless $res->is_success;

    return JSON::from_json($res->content);
}

# Languages with cache
sub languages {
    my (%opts) = @_;
    my $cache_ttl = $opts{cache_ttl} || 3600;
    my $cache_path = File::HomeDir->my_home . "/.unsandbox/languages.json";

    if (-e $cache_path) {
        my $age = time - (stat $cache_path)[9];
        if ($age < $cache_ttl) {
            open my $fh, '<', $cache_path;
            my $content = do { local $/; <$fh> };
            close $fh;
            return JSON::from_json($content);
        }
    }

    my $result = api_request('GET', '/languages', undef, %opts);
    my $langs = $result->{languages} || [];

    my $cache_dir = File::HomeDir->my_home . "/.unsandbox";
    mkdir $cache_dir unless -d $cache_dir;
    open my $fh, '>', $cache_path;
    print $fh JSON::to_json($langs);
    close $fh;

    return $langs;
}

# Execution functions
sub execute {
    my ($language, $code, %opts) = @_;
    my $body = {
        language => $language,
        code => $code,
        network_mode => $opts{network_mode} || 'zerotrust',
        ttl => $opts{ttl} || 60
    };
    return api_request('POST', '/execute', $body, %opts);
}

sub execute_async {
    my ($language, $code, %opts) = @_;
    my $body = {
        language => $language,
        code => $code,
        network_mode => $opts{network_mode} || 'zerotrust',
        ttl => $opts{ttl} || 300
    };
    return api_request('POST', '/execute/async', $body, %opts);
}

sub run {
    my ($file, %opts) = @_;
    open my $fh, '<', $file or die "Can't read $file\n";
    my $code = do { local $/; <$fh> };
    close $fh;
    return execute(detect_language($file), $code, %opts);
}

# Job management
sub get_job {
    my ($job_id, %opts) = @_;
    return api_request('GET', "/jobs/$job_id", undef, %opts);
}

sub wait_job {
    my ($job_id, %opts) = @_;
    my @delays = (300, 450, 700, 900, 650, 1600, 2000);

    for my $i (0..119) {
        my $job = get_job($job_id, %opts);
        return $job if $job->{status} eq 'completed';
        die "Job failed\n" if $job->{status} eq 'failed';

        my $delay = $delays[$i] || 2000;
        sleep($delay / 1000);
    }

    die "Max polls exceeded\n";
}

sub cancel_job {
    my ($job_id, %opts) = @_;
    return api_request('DELETE', "/jobs/$job_id", undef, %opts);
}

# Utilities
my %ext_map = (
    py => 'python', rb => 'ruby', js => 'javascript', pl => 'perl',
    php => 'php', lua => 'lua', sh => 'bash', go => 'go'
);

sub detect_language {
    my ($filename) = @_;
    my ($ext) = $filename =~ /\.([^.]+)$/;
    return $ext_map{$ext} || die "Unknown file type\n";
}

# CLI
sub cli_main {
    my @args = @ARGV;
    die "Usage: perl un.pl <file>\n" unless @args;

    my $result = run($args[0]);
    print $result->{stdout} if $result->{stdout};
    print STDERR $result->{stderr} if $result->{stderr};
    exit($result->{exit_code} || 0);
}

#!/usr/bin/env perl
# un.pl - Unsandbox CLI Client (Perl Implementation)
#
# Full-featured CLI matching un.c capabilities:
# - Execute code with env vars, input files, artifacts
# - Interactive sessions with shell/REPL support
# - Persistent services with domains and ports
#
# Usage:
#   un.pl [options] <source_file>
#   un.pl session [options]
#   un.pl service [options]
#
# Requires: UNSANDBOX_API_KEY environment variable

use strict;
use warnings;
use File::Basename;
use JSON::PP;
use LWP::UserAgent;
use HTTP::Request;
use MIME::Base64;
use File::Path qw(make_path);
use Digest::SHA qw(hmac_sha256_hex);

my $API_BASE = 'https://api.unsandbox.com';
my $PORTAL_BASE = 'https://unsandbox.com';
my $BLUE = "\033[34m";
my $RED = "\033[31m";
my $GREEN = "\033[32m";
my $YELLOW = "\033[33m";
my $RESET = "\033[0m";

my %EXT_MAP = (
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
);

sub get_api_key {
    my ($args_key) = @_;
    my $public_key = $ENV{'UNSANDBOX_PUBLIC_KEY'} || '';
    my $secret_key = $ENV{'UNSANDBOX_SECRET_KEY'} || '';

    # Fallback to old UNSANDBOX_API_KEY for backwards compat
    if (!$public_key && $ENV{'UNSANDBOX_API_KEY'}) {
        $public_key = $ENV{'UNSANDBOX_API_KEY'};
        $secret_key = '';
    }

    unless ($public_key) {
        print STDERR "${RED}Error: UNSANDBOX_PUBLIC_KEY or UNSANDBOX_API_KEY not set${RESET}\n";
        exit 1;
    }
    return ($public_key, $secret_key);
}

sub detect_language {
    my ($filename) = @_;
    my ($name, $dir, $ext) = fileparse($filename, qr/\.[^.]*/);
    my $lang = $EXT_MAP{lc($ext)};
    unless ($lang) {
        if (open my $fh, '<', $filename) {
            my $first_line = <$fh>;
            close $fh;
            if ($first_line && $first_line =~ /^#!/) {
                return 'python' if $first_line =~ /python/;
                return 'javascript' if $first_line =~ /node/;
                return 'ruby' if $first_line =~ /ruby/;
                return 'perl' if $first_line =~ /perl/;
                return 'bash' if $first_line =~ /bash|\/sh/;
                return 'lua' if $first_line =~ /lua/;
                return 'php' if $first_line =~ /php/;
            }
        }
        print STDERR "${RED}Error: Cannot detect language for $filename${RESET}\n";
        exit 1;
    }
    return $lang;
}

sub api_request {
    my ($endpoint, $method, $data, $public_key, $secret_key) = @_;
    $method //= 'GET';

    my $url = "$API_BASE$endpoint";
    my $ua = LWP::UserAgent->new(timeout => 300);
    my $request = HTTP::Request->new($method => $url);
    $request->header('Authorization' => "Bearer $public_key");
    $request->header('Content-Type' => 'application/json');

    my $body = '';
    if ($data) {
        $body = encode_json($data);
        $request->content($body);
    }

    # Add HMAC signature if secret_key is present
    if ($secret_key) {
        my $timestamp = time();
        my $sig_input = "${timestamp}:${method}:${endpoint}:${body}";
        my $signature = hmac_sha256_hex($sig_input, $secret_key);
        $request->header('X-Timestamp' => $timestamp);
        $request->header('X-Signature' => $signature);
    }

    my $response = $ua->request($request);

    unless ($response->is_success) {
        if ($response->code == 401 && $response->content =~ /timestamp/i) {
            print STDERR "${RED}Error: Request timestamp expired (must be within 5 minutes of server time)${RESET}\n";
            print STDERR "${YELLOW}Your computer's clock may have drifted.${RESET}\n";
            print STDERR "${YELLOW}Check your system time and sync with NTP if needed:${RESET}\n";
            print STDERR "  Linux:   sudo ntpdate -s time.nist.gov\n";
            print STDERR "  macOS:   sudo sntp -sS time.apple.com\n";
            print STDERR "  Windows: w32tm /resync\n";
        } else {
            print STDERR "${RED}Error: HTTP ", $response->code, " - ", $response->content, "${RESET}\n";
        }
        exit 1;
    }

    return decode_json($response->content);
}

sub api_request_text {
    my ($endpoint, $method, $body, $public_key, $secret_key) = @_;

    my $url = "$API_BASE$endpoint";
    my $ua = LWP::UserAgent->new(timeout => 300);
    my $request = HTTP::Request->new($method => $url);
    $request->header('Authorization' => "Bearer $public_key");
    $request->header('Content-Type' => 'text/plain');
    $request->content($body);

    # Add HMAC signature if secret_key is present
    if ($secret_key) {
        my $timestamp = time();
        my $sig_input = "${timestamp}:${method}:${endpoint}:${body}";
        my $signature = hmac_sha256_hex($sig_input, $secret_key);
        $request->header('X-Timestamp' => $timestamp);
        $request->header('X-Signature' => $signature);
    }

    my $response = $ua->request($request);

    unless ($response->is_success) {
        return { error => "HTTP " . $response->code . " - " . $response->content };
    }

    return decode_json($response->content);
}

# ============================================================================
# Environment Secrets Vault Functions
# ============================================================================

my $MAX_ENV_CONTENT_SIZE = 64 * 1024;  # 64KB max

sub service_env_status {
    my ($service_id, $public_key, $secret_key) = @_;
    my $result = api_request("/services/$service_id/env", 'GET', undef, $public_key, $secret_key);
    my $has_vault = $result->{has_vault};

    if (!$has_vault) {
        print "Vault exists: no\n";
        print "Variable count: 0\n";
    } else {
        print "Vault exists: yes\n";
        print "Variable count: ", ($result->{count} // 0), "\n";
        if ($result->{updated_at}) {
            my @t = localtime($result->{updated_at});
            printf "Last updated: %04d-%02d-%02d %02d:%02d:%02d\n",
                $t[5]+1900, $t[4]+1, $t[3], $t[2], $t[1], $t[0];
        }
    }
}

sub service_env_set {
    my ($service_id, $env_content, $public_key, $secret_key) = @_;

    unless ($env_content) {
        print STDERR "${RED}Error: No environment content provided${RESET}\n";
        return 0;
    }

    if (length($env_content) > $MAX_ENV_CONTENT_SIZE) {
        print STDERR "${RED}Error: Environment content too large (max $MAX_ENV_CONTENT_SIZE bytes)${RESET}\n";
        return 0;
    }

    my $result = api_request_text("/services/$service_id/env", 'PUT', $env_content, $public_key, $secret_key);

    if ($result->{error}) {
        print STDERR "${RED}Error: $result->{error}${RESET}\n";
        return 0;
    }

    my $count = $result->{count} // 0;
    my $plural = $count == 1 ? '' : 's';
    print "${GREEN}Environment vault updated: $count variable$plural${RESET}\n";
    print "$result->{message}\n" if $result->{message};
    return 1;
}

sub service_env_export {
    my ($service_id, $public_key, $secret_key) = @_;
    my $result = api_request("/services/$service_id/env/export", 'POST', {}, $public_key, $secret_key);
    my $env_content = $result->{env} // '';
    if ($env_content) {
        print $env_content;
        print "\n" unless $env_content =~ /\n$/;
    }
}

sub service_env_delete {
    my ($service_id, $public_key, $secret_key) = @_;
    api_request("/services/$service_id/env", 'DELETE', undef, $public_key, $secret_key);
    print "${GREEN}Environment vault deleted${RESET}\n";
}

sub read_env_file {
    my ($filepath) = @_;
    unless (-e $filepath) {
        print STDERR "${RED}Error: Env file not found: $filepath${RESET}\n";
        exit 1;
    }
    open my $fh, '<', $filepath or die "Cannot read file: $!";
    local $/;
    my $content = <$fh>;
    close $fh;
    return $content;
}

sub build_env_content {
    my ($envs, $env_file) = @_;
    my @parts;

    # Read from env file first
    if ($env_file) {
        push @parts, read_env_file($env_file);
    }

    # Add -e flags
    foreach my $e (@$envs) {
        push @parts, $e if $e =~ /=/;
    }

    return join("\n", @parts);
}

sub cmd_service_env {
    my ($action, $target, $envs, $env_file, $public_key, $secret_key) = @_;

    unless ($action) {
        print STDERR "${RED}Error: env action required (status, set, export, delete)${RESET}\n";
        exit 1;
    }

    unless ($target) {
        print STDERR "${RED}Error: Service ID required for env command${RESET}\n";
        exit 1;
    }

    if ($action eq 'status') {
        service_env_status($target, $public_key, $secret_key);
    } elsif ($action eq 'set') {
        my $env_content = build_env_content($envs, $env_file);
        unless ($env_content) {
            print STDERR "${RED}Error: No env content provided. Use -e KEY=VAL or --env-file${RESET}\n";
            exit 1;
        }
        service_env_set($target, $env_content, $public_key, $secret_key);
    } elsif ($action eq 'export') {
        service_env_export($target, $public_key, $secret_key);
    } elsif ($action eq 'delete') {
        service_env_delete($target, $public_key, $secret_key);
    } else {
        print STDERR "${RED}Error: Unknown env action '$action'. Use: status, set, export, delete${RESET}\n";
        exit 1;
    }
}

sub cmd_execute {
    my ($options) = @_;
    my ($public_key, $secret_key) = get_api_key($options->{api_key});

    unless (-e $options->{source_file}) {
        print STDERR "${RED}Error: File not found: $options->{source_file}${RESET}\n";
        exit 1;
    }

    open my $fh, '<', $options->{source_file} or die "Cannot read file: $!";
    local $/;
    my $code = <$fh>;
    close $fh;

    my $language = detect_language($options->{source_file});
    my $payload = { language => $language, code => $code };

    if ($options->{env} && @{$options->{env}}) {
        my %env_vars;
        foreach my $e (@{$options->{env}}) {
            if ($e =~ /^([^=]+)=(.*)$/) {
                $env_vars{$1} = $2;
            }
        }
        $payload->{env} = \%env_vars if %env_vars;
    }

    if ($options->{files} && @{$options->{files}}) {
        my @input_files;
        foreach my $filepath (@{$options->{files}}) {
            unless (-e $filepath) {
                print STDERR "${RED}Error: Input file not found: $filepath${RESET}\n";
                exit 1;
            }
            open my $f, '<:raw', $filepath or die "Cannot read file: $!";
            local $/;
            my $content = <$f>;
            close $f;
            push @input_files, {
                filename => basename($filepath),
                content_base64 => encode_base64($content, '')
            };
        }
        $payload->{input_files} = \@input_files;
    }

    $payload->{return_artifacts} = JSON::PP::true if $options->{artifacts};
    $payload->{network} = $options->{network} if $options->{network};
    $payload->{vcpu} = $options->{vcpu} if $options->{vcpu};

    my $result = api_request('/execute', 'POST', $payload, $public_key, $secret_key);

    print "${BLUE}$result->{stdout}${RESET}" if $result->{stdout};
    print STDERR "${RED}$result->{stderr}${RESET}" if $result->{stderr};

    if ($options->{artifacts} && $result->{artifacts}) {
        my $out_dir = $options->{output_dir} || '.';
        make_path($out_dir) unless -d $out_dir;
        foreach my $artifact (@{$result->{artifacts}}) {
            my $filename = $artifact->{filename} || 'artifact';
            my $content = decode_base64($artifact->{content_base64});
            my $filepath = "$out_dir/$filename";
            open my $f, '>:raw', $filepath or die "Cannot write file: $!";
            print $f $content;
            close $f;
            chmod 0755, $filepath;
            print STDERR "${GREEN}Saved: $filepath${RESET}\n";
        }
    }

    exit($result->{exit_code} // 0);
}

sub cmd_session {
    my ($options) = @_;
    my ($public_key, $secret_key) = get_api_key($options->{api_key});

    if ($options->{list}) {
        my $result = api_request('/sessions', 'GET', undef, $public_key, $secret_key);
        my $sessions = $result->{sessions} || [];
        if (@$sessions == 0) {
            print "No active sessions\n";
        } else {
            printf "%-40s %-10s %-10s %s\n", 'ID', 'Shell', 'Status', 'Created';
            foreach my $s (@$sessions) {
                printf "%-40s %-10s %-10s %s\n",
                    $s->{id} // 'N/A', $s->{shell} // 'N/A',
                    $s->{status} // 'N/A', $s->{created_at} // 'N/A';
            }
        }
        return;
    }

    if ($options->{kill}) {
        api_request("/sessions/$options->{kill}", 'DELETE', undef, $public_key, $secret_key);
        print "${GREEN}Session terminated: $options->{kill}${RESET}\n";
        return;
    }

    if ($options->{attach}) {
        print "${YELLOW}Attaching to session $options->{attach}...${RESET}\n";
        print "${YELLOW}(Interactive sessions require WebSocket - use un2 for full support)${RESET}\n";
        return;
    }

    my $payload = { shell => $options->{shell} || 'bash' };
    $payload->{network} = $options->{network} if $options->{network};
    $payload->{vcpu} = $options->{vcpu} if $options->{vcpu};
    $payload->{persistence} = 'tmux' if $options->{tmux};
    $payload->{persistence} = 'screen' if $options->{screen};
    $payload->{audit} = JSON::PP::true if $options->{audit};

    # Add input files
    if ($options->{files} && @{$options->{files}}) {
        my @input_files;
        foreach my $filepath (@{$options->{files}}) {
            unless (-e $filepath) {
                print STDERR "${RED}Error: Input file not found: $filepath${RESET}\n";
                exit 1;
            }
            open my $f, '<:raw', $filepath or die "Cannot read file: $!";
            local $/;
            my $content = <$f>;
            close $f;
            push @input_files, {
                filename => basename($filepath),
                content_base64 => encode_base64($content, '')
            };
        }
        $payload->{input_files} = \@input_files;
    }

    print "${YELLOW}Creating session...${RESET}\n";
    my $result = api_request('/sessions', 'POST', $payload, $public_key, $secret_key);
    print "${GREEN}Session created: ", ($result->{id} // 'N/A'), "${RESET}\n";
    print "${YELLOW}(Interactive sessions require WebSocket - use un2 for full support)${RESET}\n";
}

sub cmd_service {
    my ($options) = @_;
    my ($public_key, $secret_key) = get_api_key($options->{api_key});

    if ($options->{list}) {
        my $result = api_request('/services', 'GET', undef, $public_key, $secret_key);
        my $services = $result->{services} || [];
        if (@$services == 0) {
            print "No services\n";
        } else {
            printf "%-20s %-15s %-10s %-15s %s\n", 'ID', 'Name', 'Status', 'Ports', 'Domains';
            foreach my $s (@$services) {
                my $ports = join(',', @{$s->{ports} || []});
                my $domains = join(',', @{$s->{domains} || []});
                printf "%-20s %-15s %-10s %-15s %s\n",
                    $s->{id} // 'N/A', $s->{name} // 'N/A',
                    $s->{status} // 'N/A', $ports, $domains;
            }
        }
        return;
    }

    if ($options->{info}) {
        my $result = api_request("/services/$options->{info}", 'GET', undef, $public_key, $secret_key);
        print encode_json($result);
        print "\n";
        return;
    }

    if ($options->{logs}) {
        my $result = api_request("/services/$options->{logs}/logs", 'GET', undef, $public_key, $secret_key);
        print $result->{logs} // '';
        return;
    }

    if ($options->{tail}) {
        my $result = api_request("/services/$options->{tail}/logs?lines=9000", 'GET', undef, $public_key, $secret_key);
        print $result->{logs} // '';
        return;
    }

    if ($options->{sleep}) {
        api_request("/services/$options->{sleep}/freeze", 'POST', undef, $public_key, $secret_key);
        print "${GREEN}Service frozen: $options->{sleep}${RESET}\n";
        return;
    }

    if ($options->{wake}) {
        api_request("/services/$options->{wake}/unfreeze", 'POST', undef, $public_key, $secret_key);
        print "${GREEN}Service unfreezing: $options->{wake}${RESET}\n";
        return;
    }

    if ($options->{destroy}) {
        api_request("/services/$options->{destroy}", 'DELETE', undef, $public_key, $secret_key);
        print "${GREEN}Service destroyed: $options->{destroy}${RESET}\n";
        return;
    }

    if ($options->{resize}) {
        unless ($options->{vcpu}) {
            print STDERR "${RED}Error: --vcpu is required with --resize${RESET}\n";
            exit 1;
        }
        my $payload = { vcpu => $options->{vcpu} };
        api_request("/services/$options->{resize}", 'PATCH', $payload, $public_key, $secret_key);
        my $ram = $options->{vcpu} * 2;
        print "${GREEN}Service resized to $options->{vcpu} vCPU, $ram GB RAM${RESET}\n";
        return;
    }

    if ($options->{set_unfreeze_on_demand}) {
        my $enabled = ($options->{set_unfreeze_on_demand_enabled} &&
                       ($options->{set_unfreeze_on_demand_enabled} eq 'true' || $options->{set_unfreeze_on_demand_enabled} eq '1'))
                       ? JSON::PP::true : JSON::PP::false;
        my $payload = { unfreeze_on_demand => $enabled };
        api_request("/services/$options->{set_unfreeze_on_demand}", 'PATCH', $payload, $public_key, $secret_key);
        my $status = $enabled ? 'enabled' : 'disabled';
        print "${GREEN}Unfreeze-on-demand $status for service: $options->{set_unfreeze_on_demand}${RESET}\n";
        return;
    }

    if ($options->{execute}) {
        my $payload = { command => $options->{command} };
        my $result = api_request("/services/$options->{execute}/execute", 'POST', $payload, $public_key, $secret_key);
        print "${BLUE}$result->{stdout}${RESET}" if $result->{stdout};
        print STDERR "${RED}$result->{stderr}${RESET}" if $result->{stderr};
        return;
    }

    if ($options->{dump_bootstrap}) {
        print STDERR "Fetching bootstrap script from $options->{dump_bootstrap}...\n";
        my $payload = { command => 'cat /tmp/bootstrap.sh' };
        my $result = api_request("/services/$options->{dump_bootstrap}/execute", 'POST', $payload, $public_key, $secret_key);

        if ($result->{stdout}) {
            my $bootstrap = $result->{stdout};
            if ($options->{dump_file}) {
                # Write to file
                open my $fh, '>', $options->{dump_file} or do {
                    print STDERR "${RED}Error: Could not write to $options->{dump_file}: $!${RESET}\n";
                    exit 1;
                };
                print $fh $bootstrap;
                close $fh;
                chmod 0755, $options->{dump_file};
                print "Bootstrap saved to $options->{dump_file}\n";
            } else {
                # Print to stdout
                print $bootstrap;
            }
        } else {
            print STDERR "${RED}Error: Failed to fetch bootstrap (service not running or no bootstrap file)${RESET}\n";
            exit 1;
        }
        return;
    }

    if ($options->{name}) {
        my $payload = { name => $options->{name} };
        if ($options->{ports}) {
            my @ports = map { int($_) } split(',', $options->{ports});
            $payload->{ports} = \@ports;
        }
        if ($options->{domains}) {
            my @domains = split(',', $options->{domains});
            $payload->{domains} = \@domains;
        }
        if ($options->{type}) {
            $payload->{service_type} = $options->{type};
        }
        if ($options->{bootstrap}) {
            $payload->{bootstrap} = $options->{bootstrap};
        }
        if ($options->{bootstrap_file}) {
            if (! -e $options->{bootstrap_file}) {
                print STDERR "${RED}Error: Bootstrap file not found: $options->{bootstrap_file}${RESET}\n";
                exit 1;
            }
            open my $fh, '<', $options->{bootstrap_file} or die "Cannot read file: $!";
            local $/;
            $payload->{bootstrap_content} = <$fh>;
            close $fh;
        }
        # Add input files
        if ($options->{files} && @{$options->{files}}) {
            my @input_files;
            foreach my $filepath (@{$options->{files}}) {
                unless (-e $filepath) {
                    print STDERR "${RED}Error: Input file not found: $filepath${RESET}\n";
                    exit 1;
                }
                open my $f, '<:raw', $filepath or die "Cannot read file: $!";
                local $/;
                my $content = <$f>;
                close $f;
                push @input_files, {
                    filename => basename($filepath),
                    content_base64 => encode_base64($content, '')
                };
            }
            $payload->{input_files} = \@input_files;
        }
        $payload->{network} = $options->{network} if $options->{network};
        $payload->{vcpu} = $options->{vcpu} if $options->{vcpu};
        $payload->{unfreeze_on_demand} = JSON::PP::true if $options->{unfreeze_on_demand};

        my $result = api_request('/services', 'POST', $payload, $public_key, $secret_key);
        my $service_id = $result->{id};
        print "${GREEN}Service created: ", ($service_id // 'N/A'), "${RESET}\n";
        print "Name: ", ($result->{name} // 'N/A'), "\n";
        print "URL: $result->{url}\n" if $result->{url};

        # Auto-set vault if -e or --env-file provided
        my $env_content = build_env_content($options->{env} || [], $options->{env_file});
        if ($env_content && $service_id) {
            service_env_set($service_id, $env_content, $public_key, $secret_key);
        }
        return;
    }

    print STDERR "${RED}Error: Specify --name to create a service, or use --list, --info, etc.${RESET}\n";
    exit 1;
}

sub open_browser {
    my ($url) = @_;

    # Try different browser open commands based on platform
    if ($^O eq 'darwin') {
        system('open', $url);
    } elsif ($^O eq 'MSWin32') {
        system('start', $url);
    } else {
        # Linux/Unix
        system('xdg-open', $url, '>/dev/null', '2>&1', '&');
    }
}

sub validate_key {
    my ($public_key, $secret_key, $should_extend) = @_;

    # Call /keys/validate endpoint
    my $url = "$API_BASE/keys/validate";
    my $ua = LWP::UserAgent->new(timeout => 30);
    my $request = HTTP::Request->new('POST' => $url);
    $request->header('Authorization' => "Bearer $public_key");
    $request->header('Content-Type' => 'application/json');

    # Add HMAC signature if secret_key is present
    if ($secret_key) {
        my $timestamp = time();
        my $sig_input = "${timestamp}:POST:/keys/validate:";
        my $signature = hmac_sha256_hex($sig_input, $secret_key);
        $request->header('X-Timestamp' => $timestamp);
        $request->header('X-Signature' => $signature);
    }

    my $response = $ua->request($request);
    my $result = decode_json($response->content);

    # Handle --extend flag first
    if ($should_extend) {
        my $public_key = $result->{public_key};
        if ($public_key) {
            my $extend_url = "$PORTAL_BASE/keys/extend?pk=$public_key";
            print "${BLUE}Opening browser to extend key...${RESET}\n";
            open_browser($extend_url);
            return;
        } else {
            print STDERR "${RED}Error: Could not retrieve public key${RESET}\n";
            exit 1;
        }
    }

    # Check if key is expired
    if ($result->{expired}) {
        print "${RED}Expired${RESET}\n";
        print "Public Key: ", ($result->{public_key} // 'N/A'), "\n";
        print "Tier: ", ($result->{tier} // 'N/A'), "\n";
        print "Expired: ", ($result->{expires_at} // 'N/A'), "\n";
        print "${YELLOW}To renew: Visit https://unsandbox.com/keys/extend${RESET}\n";
        exit 1;
    }

    # Valid key
    print "${GREEN}Valid${RESET}\n";
    print "Public Key: ", ($result->{public_key} // 'N/A'), "\n";
    print "Tier: ", ($result->{tier} // 'N/A'), "\n";
    print "Status: ", ($result->{status} // 'N/A'), "\n";
    print "Expires: ", ($result->{expires_at} // 'N/A'), "\n";
    print "Time Remaining: ", ($result->{time_remaining} // 'N/A'), "\n";
    print "Rate Limit: ", ($result->{rate_limit} // 'N/A'), "\n";
    print "Burst: ", ($result->{burst} // 'N/A'), "\n";
    print "Concurrency: ", ($result->{concurrency} // 'N/A'), "\n";
}

sub cmd_key {
    my ($options) = @_;
    my ($public_key, $secret_key) = get_api_key($options->{api_key});
    validate_key($public_key, $secret_key, $options->{extend});
}

sub cmd_languages {
    my ($options) = @_;
    my ($public_key, $secret_key) = get_api_key($options->{api_key});

    my $result = api_request('/languages', 'GET', undef, $public_key, $secret_key);
    my $languages_list = $result->{languages} || [];

    if ($options->{json}) {
        # Output as JSON array
        print encode_json($languages_list);
        print "\n";
    } else {
        # Output one language per line
        foreach my $lang (@$languages_list) {
            print "$lang\n";
        }
    }
}

sub cmd_image {
    my ($options) = @_;
    my ($public_key, $secret_key) = get_api_key($options->{api_key});

    if ($options->{list}) {
        my $result = api_request('/images', 'GET', undef, $public_key, $secret_key);
        print encode_json($result);
        print "\n";
        return;
    }

    if ($options->{info}) {
        my $result = api_request("/images/$options->{info}", 'GET', undef, $public_key, $secret_key);
        print encode_json($result);
        print "\n";
        return;
    }

    if ($options->{delete}) {
        api_request("/images/$options->{delete}", 'DELETE', undef, $public_key, $secret_key);
        print "${GREEN}Image deleted: $options->{delete}${RESET}\n";
        return;
    }

    if ($options->{lock}) {
        api_request("/images/$options->{lock}/lock", 'POST', undef, $public_key, $secret_key);
        print "${GREEN}Image locked: $options->{lock}${RESET}\n";
        return;
    }

    if ($options->{unlock}) {
        api_request("/images/$options->{unlock}/unlock", 'POST', undef, $public_key, $secret_key);
        print "${GREEN}Image unlocked: $options->{unlock}${RESET}\n";
        return;
    }

    if ($options->{publish}) {
        unless ($options->{source_type}) {
            print STDERR "${RED}Error: --publish requires --source-type (service or snapshot)${RESET}\n";
            exit 1;
        }
        my $payload = {
            source_type => $options->{source_type},
            source_id => $options->{publish}
        };
        $payload->{name} = $options->{name} if $options->{name};
        my $result = api_request('/images/publish', 'POST', $payload, $public_key, $secret_key);
        print "${GREEN}Image published${RESET}\n";
        print encode_json($result);
        print "\n";
        return;
    }

    if ($options->{visibility_id} && $options->{visibility_mode}) {
        my $payload = { visibility => $options->{visibility_mode} };
        api_request("/images/$options->{visibility_id}/visibility", 'POST', $payload, $public_key, $secret_key);
        print "${GREEN}Image visibility set to $options->{visibility_mode}: $options->{visibility_id}${RESET}\n";
        return;
    }

    if ($options->{spawn}) {
        my $payload = {};
        $payload->{name} = $options->{name} if $options->{name};
        if ($options->{ports}) {
            my @ports = map { int($_) } split(',', $options->{ports});
            $payload->{ports} = \@ports;
        }
        my $result = api_request("/images/$options->{spawn}/spawn", 'POST', $payload, $public_key, $secret_key);
        print "${GREEN}Service spawned from image${RESET}\n";
        print encode_json($result);
        print "\n";
        return;
    }

    if ($options->{clone}) {
        my $payload = {};
        $payload->{name} = $options->{name} if $options->{name};
        my $result = api_request("/images/$options->{clone}/clone", 'POST', $payload, $public_key, $secret_key);
        print "${GREEN}Image cloned${RESET}\n";
        print encode_json($result);
        print "\n";
        return;
    }

    print STDERR "${RED}Error: Use --list, --info, --delete, --lock, --unlock, --publish, --visibility, --spawn, or --clone${RESET}\n";
    exit 1;
}

sub main {
    my %options = (
        command => undef,
        source_file => undef,
        env => [],
        files => [],
        artifacts => 0,
        output_dir => undef,
        network => undef,
        vcpu => undef,
        api_key => undef,
        shell => undef,
        list => 0,
        attach => undef,
        kill => undef,
        audit => 0,
        tmux => 0,
        screen => 0,
        name => undef,
        ports => undef,
        domains => undef,
        type => undef,
        bootstrap => undef,
        bootstrap_file => undef,
        info => undef,
        logs => undef,
        tail => undef,
        sleep => undef,
        wake => undef,
        destroy => undef,
        resize => undef,
        execute => undef,
        command => undef,
        dump_bootstrap => undef,
        dump_file => undef,
        extend => 0,
        env_file => undef,
        env_action => undef,
        env_target => undef,
        json => 0,
        # Image options
        delete => undef,
        lock => undef,
        unlock => undef,
        publish => undef,
        source_type => undef,
        visibility_id => undef,
        visibility_mode => undef,
        spawn => undef,
        clone => undef
    );

    for (my $i = 0; $i < @ARGV; $i++) {
        my $arg = $ARGV[$i];

        if ($arg eq 'session' || $arg eq 'service' || $arg eq 'key' || $arg eq 'languages' || $arg eq 'image') {
            $options{command} = $arg;
        } elsif ($arg eq '-e') {
            push @{$options{env}}, $ARGV[++$i];
        } elsif ($arg eq '-f') {
            push @{$options{files}}, $ARGV[++$i];
        } elsif ($arg eq '-a') {
            $options{artifacts} = 1;
        } elsif ($arg eq '-o') {
            $options{output_dir} = $ARGV[++$i];
        } elsif ($arg eq '-n') {
            $options{network} = $ARGV[++$i];
        } elsif ($arg eq '-v') {
            $options{vcpu} = int($ARGV[++$i]);
        } elsif ($arg eq '-k') {
            $options{api_key} = $ARGV[++$i];
        } elsif ($arg eq '-s' || $arg eq '--shell') {
            $options{shell} = $ARGV[++$i];
        } elsif ($arg eq '-l' || $arg eq '--list') {
            $options{list} = 1;
        } elsif ($arg eq '--attach') {
            $options{attach} = $ARGV[++$i];
        } elsif ($arg eq '--kill') {
            $options{kill} = $ARGV[++$i];
        } elsif ($arg eq '--audit') {
            $options{audit} = 1;
        } elsif ($arg eq '--tmux') {
            $options{tmux} = 1;
        } elsif ($arg eq '--screen') {
            $options{screen} = 1;
        } elsif ($arg eq '--name') {
            $options{name} = $ARGV[++$i];
        } elsif ($arg eq '--ports') {
            $options{ports} = $ARGV[++$i];
        } elsif ($arg eq '--domains') {
            $options{domains} = $ARGV[++$i];
        } elsif ($arg eq '--type') {
            $options{type} = $ARGV[++$i];
        } elsif ($arg eq '--bootstrap') {
            $options{bootstrap} = $ARGV[++$i];
        } elsif ($arg eq '--bootstrap-file') {
            $options{bootstrap_file} = $ARGV[++$i];
        } elsif ($arg eq '--env-file') {
            $options{env_file} = $ARGV[++$i];
        } elsif ($arg eq 'env') {
            # Handle "service env <action> <target>" subcommand
            if ($options{command} && $options{command} eq 'service') {
                $options{env_action} = $ARGV[++$i] if defined $ARGV[$i + 1];
                if (defined $ARGV[$i + 1] && $ARGV[$i + 1] !~ /^-/) {
                    $options{env_target} = $ARGV[++$i];
                }
            }
        } elsif ($arg eq '--info') {
            $options{info} = $ARGV[++$i];
        } elsif ($arg eq '--logs') {
            $options{logs} = $ARGV[++$i];
        } elsif ($arg eq '--tail') {
            $options{tail} = $ARGV[++$i];
        } elsif ($arg eq '--freeze') {
            $options{sleep} = $ARGV[++$i];
        } elsif ($arg eq '--unfreeze') {
            $options{wake} = $ARGV[++$i];
        } elsif ($arg eq '--destroy') {
            $options{destroy} = $ARGV[++$i];
        } elsif ($arg eq '--resize') {
            $options{resize} = $ARGV[++$i];
        } elsif ($arg eq '--execute') {
            $options{execute} = $ARGV[++$i];
        } elsif ($arg eq '--command') {
            $options{command} = $ARGV[++$i];
        } elsif ($arg eq '--dump-bootstrap') {
            $options{dump_bootstrap} = $ARGV[++$i];
        } elsif ($arg eq '--dump-file') {
            $options{dump_file} = $ARGV[++$i];
        } elsif ($arg eq '--extend') {
            $options{extend} = 1;
        } elsif ($arg eq '--json') {
            $options{json} = 1;
        } elsif ($arg eq '--delete') {
            $options{delete} = $ARGV[++$i];
        } elsif ($arg eq '--lock') {
            $options{lock} = $ARGV[++$i];
        } elsif ($arg eq '--unlock') {
            $options{unlock} = $ARGV[++$i];
        } elsif ($arg eq '--publish') {
            $options{publish} = $ARGV[++$i];
        } elsif ($arg eq '--source-type') {
            $options{source_type} = $ARGV[++$i];
        } elsif ($arg eq '--visibility') {
            $options{visibility_id} = $ARGV[++$i];
            $options{visibility_mode} = $ARGV[++$i] if defined $ARGV[$i + 1];
        } elsif ($arg eq '--spawn') {
            $options{spawn} = $ARGV[++$i];
        } elsif ($arg eq '--clone') {
            $options{clone} = $ARGV[++$i];
        } elsif ($arg eq '--unfreeze-on-demand') {
            $options{unfreeze_on_demand} = 1;
        } elsif ($arg eq '--set-unfreeze-on-demand') {
            $options{set_unfreeze_on_demand} = $ARGV[++$i];
            $options{set_unfreeze_on_demand_enabled} = $ARGV[++$i] if defined $ARGV[$i + 1];
        } elsif ($arg =~ /^-/) {
            print STDERR "${RED}Unknown option: $arg${RESET}\n";
            exit 1;
        } else {
            $options{source_file} = $arg;
        }
    }

    if ($options{command} && $options{command} eq 'session') {
        cmd_session(\%options);
    } elsif ($options{command} && $options{command} eq 'service') {
        # Check for "service env" subcommand
        if ($options{env_action}) {
            my ($public_key, $secret_key) = get_api_key($options{api_key});
            cmd_service_env($options{env_action}, $options{env_target}, $options{env}, $options{env_file}, $public_key, $secret_key);
        } else {
            cmd_service(\%options);
        }
    } elsif ($options{command} && $options{command} eq 'languages') {
        cmd_languages(\%options);
    } elsif ($options{command} && $options{command} eq 'image') {
        cmd_image(\%options);
    } elsif ($options{command} && $options{command} eq 'key') {
        cmd_key(\%options);
    } elsif ($options{source_file}) {
        cmd_execute(\%options);
    } else {
        print <<'HELP';
Unsandbox CLI - Execute code in secure sandboxes

Usage:
  $0 [options] <source_file>
  $0 session [options]
  $0 service [options]
  $0 image [options]
  $0 languages [--json]
  $0 key [options]

Languages options:
  --json           Output as JSON array

Image options:
  --list, -l           List all images
  --info ID            Get image details
  --delete ID          Delete an image
  --lock ID            Lock image to prevent deletion
  --unlock ID          Unlock image
  --publish ID         Publish image from service/snapshot
  --source-type TYPE   Source type: service or snapshot
  --visibility ID MODE Set visibility: private, unlisted, or public
  --spawn ID           Spawn new service from image
  --clone ID           Clone an image
  --name NAME          Name for spawned service or cloned image
  --ports PORTS        Ports for spawned service

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
  --extend         Open browser to extend/renew key
HELP
        exit 1;
    }
}

main();
