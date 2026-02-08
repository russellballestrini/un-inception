#!/usr/bin/env perl
# PUBLIC DOMAIN - NO LICENSE, NO WARRANTY
#
# unsandbox.com Perl SDK (Synchronous)
# Full API with execution, sessions, services, snapshots, and images.
#
# Library Usage:
#     use Un;
#     my $result = Un::execute("python", "print(42)");
#     print $result->{stdout};
#
# CLI Usage:
#     perl un.pl script.py
#     perl un.pl -s python 'print(42)'
#     perl un.pl session --list
#     perl un.pl service --list
#     perl un.pl snapshot --list
#     perl un.pl image --list
#
# Authentication Priority (4-tier):
#     1. Function arguments (public_key, secret_key)
#     2. Environment variables (UNSANDBOX_PUBLIC_KEY, UNSANDBOX_SECRET_KEY)
#     3. Config file (~/.unsandbox/accounts.csv, line 0 by default)
#     4. Local directory (./accounts.csv, line 0 by default)
#
# Copyright 2025 TimeHexOn & foxhop & russell@unturf

use strict;
use warnings;
use JSON::PP;
use LWP::UserAgent;
use HTTP::Request;
use Digest::SHA qw(hmac_sha256_hex);
use MIME::Base64;
use File::Basename;
use File::Path qw(make_path);
use Time::HiRes qw(time sleep);

package Un;

our $VERSION = "4.3.1";
our $API_BASE = 'https://api.unsandbox.com';
our $PORTAL_BASE = 'https://unsandbox.com';

# Thread-local error storage
our $LAST_ERROR = "";

# Colors
my $BLUE = "\033[34m";
my $RED = "\033[31m";
my $GREEN = "\033[32m";
my $YELLOW = "\033[33m";
my $RESET = "\033[0m";

# Extension to language mapping
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

# ============================================================================
# Utility Functions
# ============================================================================

sub version { return $VERSION; }

sub last_error { return $LAST_ERROR; }

sub set_error {
    my ($msg) = @_;
    $LAST_ERROR = $msg;
}

sub detect_language {
    my ($filename) = @_;
    return undef unless $filename;
    my ($name, $dir, $ext) = fileparse($filename, qr/\.[^.]*/);
    return $EXT_MAP{lc($ext)};
}

sub hmac_sign {
    my ($secret, $message) = @_;
    return undef unless defined $secret && defined $message;
    return hmac_sha256_hex($message, $secret);
}

# ============================================================================
# Credential Management
# ============================================================================

sub load_accounts_csv {
    my ($path) = @_;
    $path ||= "$ENV{HOME}/.unsandbox/accounts.csv";
    return [] unless -e $path;

    my @accounts;
    open my $fh, '<', $path or return [];
    while (my $line = <$fh>) {
        chomp $line;
        next if !$line || $line =~ /^\s*#/;
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

    # Legacy fallback
    if ($ENV{UNSANDBOX_API_KEY}) {
        return ($ENV{UNSANDBOX_API_KEY}, '');
    }

    # Tier 3: Home directory
    my $home_accounts = load_accounts_csv();
    return @{$home_accounts->[0]} if @$home_accounts;

    # Tier 4: Local directory
    my $local_accounts = load_accounts_csv("./accounts.csv");
    return @{$local_accounts->[0]} if @$local_accounts;

    set_error("No credentials found");
    return (undef, undef);
}

# ============================================================================
# API Communication
# ============================================================================

sub api_request {
    my ($method, $endpoint, $body, %opts) = @_;
    my $extra_headers = delete $opts{extra_headers} || {};
    my $content_type = delete $opts{content_type} || 'application/json';
    my ($pk, $sk) = get_credentials(%opts);

    unless ($pk) {
        set_error("No credentials available");
        return undef;
    }

    my $timestamp = int(time);
    my $body_str = '';
    if ($body) {
        $body_str = ref($body) ? encode_json($body) : $body;
    }

    my $ua = LWP::UserAgent->new(timeout => 300);
    my $url = "$API_BASE$endpoint";
    my $req = HTTP::Request->new($method, $url);

    $req->header('Authorization' => "Bearer $pk");
    $req->header('Content-Type' => $content_type);

    # HMAC signature
    if ($sk) {
        my $sig_input = "$timestamp:$method:$endpoint:$body_str";
        my $signature = hmac_sign($sk, $sig_input);
        $req->header('X-Timestamp' => $timestamp);
        $req->header('X-Signature' => $signature);
    }

    # Add extra headers
    for my $key (keys %$extra_headers) {
        $req->header($key => $extra_headers->{$key});
    }

    $req->content($body_str) if $body_str;

    my $res = $ua->request($req);

    unless ($res->is_success) {
        set_error("API error (" . $res->code . "): " . $res->content);
        return undef;
    }

    return decode_json($res->content) if $res->content;
    return { success => 1 };
}

sub api_request_with_sudo {
    my ($method, $endpoint, $body, %opts) = @_;
    my ($pk, $sk) = get_credentials(%opts);

    my $timestamp = int(time);
    my $body_str = '';
    if ($body) {
        $body_str = ref($body) ? encode_json($body) : $body;
    }

    my $ua = LWP::UserAgent->new(timeout => 300);
    my $url = "$API_BASE$endpoint";
    my $req = HTTP::Request->new($method, $url);

    $req->header('Authorization' => "Bearer $pk");
    $req->header('Content-Type' => 'application/json');

    if ($sk) {
        my $sig_input = "$timestamp:$method:$endpoint:$body_str";
        my $signature = hmac_sign($sk, $sig_input);
        $req->header('X-Timestamp' => $timestamp);
        $req->header('X-Signature' => $signature);
    }

    $req->content($body_str) if $body_str;

    my $res = $ua->request($req);

    # Handle 428 - Sudo OTP required
    if ($res->code == 428) {
        my $response_data = eval { decode_json($res->content) } || {};
        my $challenge_id = $response_data->{challenge_id} || '';

        print STDERR "${YELLOW}Confirmation required. Check your email for a one-time code.${RESET}\n";
        print STDERR "Enter OTP: ";

        my $otp = <STDIN>;
        return undef unless defined $otp;
        chomp $otp;
        $otp =~ s/\r//g;

        if ($otp eq '') {
            set_error("Operation cancelled");
            return undef;
        }

        my $extra = { 'X-Sudo-OTP' => $otp };
        $extra->{'X-Sudo-Challenge'} = $challenge_id if $challenge_id;

        return api_request($method, $endpoint, $body, %opts, extra_headers => $extra);
    }

    unless ($res->is_success) {
        set_error("API error (" . $res->code . "): " . $res->content);
        return undef;
    }

    return decode_json($res->content) if $res->content;
    return { success => 1 };
}

# ============================================================================
# Execution Functions (8)
# ============================================================================

sub execute {
    my ($language, $code, %opts) = @_;
    my $body = {
        language => $language,
        code => $code,
        network_mode => $opts{network_mode} || 'zerotrust',
        ttl => $opts{ttl} || 60
    };
    $body->{env} = $opts{env} if $opts{env};
    $body->{input_files} = $opts{input_files} if $opts{input_files};
    $body->{return_artifacts} = JSON::PP::true if $opts{return_artifacts};
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

sub wait_job {
    my ($job_id, %opts) = @_;
    my $timeout = $opts{timeout} || 120;
    my @delays = (300, 450, 700, 900, 650, 1600, 2000);

    for my $i (0..119) {
        my $job = get_job($job_id, %opts);
        return $job if $job && $job->{status} eq 'completed';
        if ($job && $job->{status} eq 'failed') {
            set_error("Job failed: " . ($job->{error} || 'unknown'));
            return undef;
        }

        my $delay = $delays[$i % 7] || 2000;
        sleep($delay / 1000);
    }

    set_error("Max polls exceeded");
    return undef;
}

sub get_job {
    my ($job_id, %opts) = @_;
    return api_request('GET', "/jobs/$job_id", undef, %opts);
}

sub cancel_job {
    my ($job_id, %opts) = @_;
    return api_request('DELETE', "/jobs/$job_id", undef, %opts);
}

sub list_jobs {
    my (%opts) = @_;
    return api_request('GET', '/jobs', undef, %opts);
}

sub get_languages {
    my (%opts) = @_;
    my $cache_ttl = $opts{cache_ttl} || 3600;
    my $cache_path = "$ENV{HOME}/.unsandbox/languages.json";

    if (-e $cache_path) {
        my $age = time - (stat $cache_path)[9];
        if ($age < $cache_ttl) {
            open my $fh, '<', $cache_path;
            my $content = do { local $/; <$fh> };
            close $fh;
            return decode_json($content);
        }
    }

    my $result = api_request('GET', '/languages', undef, %opts);
    my $langs = $result->{languages} || [];

    my $cache_dir = "$ENV{HOME}/.unsandbox";
    mkdir $cache_dir unless -d $cache_dir;
    if (open my $fh, '>', $cache_path) {
        print $fh encode_json($langs);
        close $fh;
    }

    return $langs;
}

# ============================================================================
# Session Functions (9)
# ============================================================================

sub session_list {
    my (%opts) = @_;
    return api_request('GET', '/sessions', undef, %opts);
}

sub session_get {
    my ($session_id, %opts) = @_;
    return api_request('GET', "/sessions/$session_id", undef, %opts);
}

sub session_create {
    my (%opts) = @_;
    my $body = {
        shell => $opts{shell} || 'bash',
    };
    $body->{network} = $opts{network} if $opts{network};
    $body->{vcpu} = $opts{vcpu} if $opts{vcpu};
    $body->{input_files} = $opts{input_files} if $opts{input_files};
    $body->{persistence} = $opts{persistence} if $opts{persistence};
    return api_request('POST', '/sessions', $body, %opts);
}

sub session_destroy {
    my ($session_id, %opts) = @_;
    return api_request('DELETE', "/sessions/$session_id", undef, %opts);
}

sub session_freeze {
    my ($session_id, %opts) = @_;
    return api_request('POST', "/sessions/$session_id/freeze", {}, %opts);
}

sub session_unfreeze {
    my ($session_id, %opts) = @_;
    return api_request('POST', "/sessions/$session_id/unfreeze", {}, %opts);
}

sub session_boost {
    my ($session_id, $vcpu, %opts) = @_;
    return api_request('POST', "/sessions/$session_id/boost", { vcpu => $vcpu }, %opts);
}

sub session_unboost {
    my ($session_id, %opts) = @_;
    return api_request('POST', "/sessions/$session_id/unboost", {}, %opts);
}

sub session_execute {
    my ($session_id, $command, %opts) = @_;
    return api_request('POST', "/sessions/$session_id/execute", { command => $command }, %opts);
}

# ============================================================================
# Service Functions (17)
# ============================================================================

sub service_list {
    my (%opts) = @_;
    return api_request('GET', '/services', undef, %opts);
}

sub service_get {
    my ($service_id, %opts) = @_;
    return api_request('GET', "/services/$service_id", undef, %opts);
}

sub service_create {
    my (%opts) = @_;
    my $body = { name => $opts{name} };
    $body->{ports} = $opts{ports} if $opts{ports};
    $body->{domains} = $opts{domains} if $opts{domains};
    $body->{bootstrap} = $opts{bootstrap} if $opts{bootstrap};
    $body->{bootstrap_content} = $opts{bootstrap_content} if $opts{bootstrap_content};
    $body->{network} = $opts{network} if $opts{network};
    $body->{vcpu} = $opts{vcpu} if $opts{vcpu};
    $body->{service_type} = $opts{service_type} if $opts{service_type};
    $body->{input_files} = $opts{input_files} if $opts{input_files};
    $body->{unfreeze_on_demand} = JSON::PP::true if $opts{unfreeze_on_demand};
    return api_request('POST', '/services', $body, %opts);
}

sub service_destroy {
    my ($service_id, %opts) = @_;
    return api_request_with_sudo('DELETE', "/services/$service_id", undef, %opts);
}

sub service_freeze {
    my ($service_id, %opts) = @_;
    return api_request('POST', "/services/$service_id/freeze", {}, %opts);
}

sub service_unfreeze {
    my ($service_id, %opts) = @_;
    return api_request('POST', "/services/$service_id/unfreeze", {}, %opts);
}

sub service_lock {
    my ($service_id, %opts) = @_;
    return api_request('POST', "/services/$service_id/lock", {}, %opts);
}

sub service_unlock {
    my ($service_id, %opts) = @_;
    return api_request_with_sudo('POST', "/services/$service_id/unlock", {}, %opts);
}

sub service_set_unfreeze_on_demand {
    my ($service_id, $enabled, %opts) = @_;
    my $body = { unfreeze_on_demand => ($enabled ? JSON::PP::true : JSON::PP::false) };
    return api_request('PATCH', "/services/$service_id", $body, %opts);
}

sub service_redeploy {
    my ($service_id, %opts) = @_;
    my $body = {};
    $body->{bootstrap} = $opts{bootstrap} if $opts{bootstrap};
    return api_request('POST', "/services/$service_id/redeploy", $body, %opts);
}

sub service_logs {
    my ($service_id, %opts) = @_;
    my $endpoint = "/services/$service_id/logs";
    $endpoint .= "?lines=$opts{lines}" if $opts{lines};
    return api_request('GET', $endpoint, undef, %opts);
}

sub service_execute {
    my ($service_id, $command, %opts) = @_;
    my $body = { command => $command };
    $body->{timeout} = $opts{timeout} if $opts{timeout};
    return api_request('POST', "/services/$service_id/execute", $body, %opts);
}

sub service_env_get {
    my ($service_id, %opts) = @_;
    return api_request('GET', "/services/$service_id/env", undef, %opts);
}

sub service_env_set {
    my ($service_id, $env_content, %opts) = @_;
    return api_request('PUT', "/services/$service_id/env", $env_content,
                       %opts, content_type => 'text/plain');
}

sub service_env_delete {
    my ($service_id, %opts) = @_;
    return api_request('DELETE', "/services/$service_id/env", undef, %opts);
}

sub service_env_export {
    my ($service_id, %opts) = @_;
    return api_request('POST', "/services/$service_id/env/export", {}, %opts);
}

sub service_resize {
    my ($service_id, $vcpu, %opts) = @_;
    return api_request('PATCH', "/services/$service_id", { vcpu => $vcpu }, %opts);
}

# ============================================================================
# Snapshot Functions (9)
# ============================================================================

sub snapshot_list {
    my (%opts) = @_;
    return api_request('GET', '/snapshots', undef, %opts);
}

sub snapshot_get {
    my ($snapshot_id, %opts) = @_;
    return api_request('GET', "/snapshots/$snapshot_id", undef, %opts);
}

sub snapshot_session {
    my ($session_id, %opts) = @_;
    my $body = {};
    $body->{name} = $opts{name} if $opts{name};
    $body->{hot} = JSON::PP::true if $opts{hot};
    return api_request('POST', "/sessions/$session_id/snapshot", $body, %opts);
}

sub snapshot_service {
    my ($service_id, %opts) = @_;
    my $body = {};
    $body->{name} = $opts{name} if $opts{name};
    $body->{hot} = JSON::PP::true if $opts{hot};
    return api_request('POST', "/services/$service_id/snapshot", $body, %opts);
}

sub snapshot_restore {
    my ($snapshot_id, %opts) = @_;
    return api_request('POST', "/snapshots/$snapshot_id/restore", {}, %opts);
}

sub snapshot_delete {
    my ($snapshot_id, %opts) = @_;
    return api_request_with_sudo('DELETE', "/snapshots/$snapshot_id", undef, %opts);
}

sub snapshot_lock {
    my ($snapshot_id, %opts) = @_;
    return api_request('POST', "/snapshots/$snapshot_id/lock", {}, %opts);
}

sub snapshot_unlock {
    my ($snapshot_id, %opts) = @_;
    return api_request_with_sudo('POST', "/snapshots/$snapshot_id/unlock", {}, %opts);
}

sub snapshot_clone {
    my ($snapshot_id, %opts) = @_;
    my $body = { clone_type => $opts{clone_type} || 'session' };
    $body->{name} = $opts{name} if $opts{name};
    $body->{ports} = $opts{ports} if $opts{ports};
    $body->{shell} = $opts{shell} if $opts{shell};
    return api_request('POST', "/snapshots/$snapshot_id/clone", $body, %opts);
}

# ============================================================================
# Image Functions (13)
# ============================================================================

sub image_list {
    my (%opts) = @_;
    my $endpoint = '/images';
    $endpoint .= "?filter=$opts{filter}" if $opts{filter};
    return api_request('GET', $endpoint, undef, %opts);
}

sub image_get {
    my ($image_id, %opts) = @_;
    return api_request('GET', "/images/$image_id", undef, %opts);
}

sub image_publish {
    my (%opts) = @_;
    my $body = {
        source_type => $opts{source_type},
        source_id => $opts{source_id}
    };
    $body->{name} = $opts{name} if $opts{name};
    $body->{description} = $opts{description} if $opts{description};
    return api_request('POST', '/images/publish', $body, %opts);
}

sub image_delete {
    my ($image_id, %opts) = @_;
    return api_request_with_sudo('DELETE', "/images/$image_id", undef, %opts);
}

sub image_lock {
    my ($image_id, %opts) = @_;
    return api_request('POST', "/images/$image_id/lock", {}, %opts);
}

sub image_unlock {
    my ($image_id, %opts) = @_;
    return api_request_with_sudo('POST', "/images/$image_id/unlock", {}, %opts);
}

sub image_set_visibility {
    my ($image_id, $visibility, %opts) = @_;
    return api_request('POST', "/images/$image_id/visibility", { visibility => $visibility }, %opts);
}

sub image_grant_access {
    my ($image_id, $trusted_api_key, %opts) = @_;
    return api_request('POST', "/images/$image_id/access", { api_key => $trusted_api_key }, %opts);
}

sub image_revoke_access {
    my ($image_id, $trusted_api_key, %opts) = @_;
    return api_request('DELETE', "/images/$image_id/access/$trusted_api_key", undef, %opts);
}

sub image_list_trusted {
    my ($image_id, %opts) = @_;
    return api_request('GET', "/images/$image_id/access", undef, %opts);
}

sub image_transfer {
    my ($image_id, $to_api_key, %opts) = @_;
    return api_request('POST', "/images/$image_id/transfer", { to_api_key => $to_api_key }, %opts);
}

sub image_spawn {
    my ($image_id, %opts) = @_;
    my $body = {};
    $body->{name} = $opts{name} if $opts{name};
    $body->{ports} = $opts{ports} if $opts{ports};
    $body->{bootstrap} = $opts{bootstrap} if $opts{bootstrap};
    $body->{network_mode} = $opts{network_mode} if $opts{network_mode};
    return api_request('POST', "/images/$image_id/spawn", $body, %opts);
}

sub image_clone {
    my ($image_id, %opts) = @_;
    my $body = {};
    $body->{name} = $opts{name} if $opts{name};
    $body->{description} = $opts{description} if $opts{description};
    return api_request('POST', "/images/$image_id/clone", $body, %opts);
}

# ============================================================================
# PaaS Logs Functions (2)
# ============================================================================

sub logs_fetch {
    my (%opts) = @_;
    my $body = {
        source => $opts{source} || 'all',
        lines => $opts{lines} || 100,
        since => $opts{since} || '1h'
    };
    $body->{grep} = $opts{grep} if $opts{grep};
    return api_request('POST', '/paas/logs', $body, %opts);
}

sub logs_stream {
    my (%opts) = @_;
    # SSE streaming - returns immediately, callback for each line
    my $callback = $opts{callback};
    return undef unless $callback;

    # SSE streaming not easily supported in sync Perl, return placeholder
    set_error("logs_stream requires async support");
    return undef;
}

# ============================================================================
# Key Validation
# ============================================================================

sub validate_keys {
    my (%opts) = @_;
    my ($pk, $sk) = get_credentials(%opts);

    my $ua = LWP::UserAgent->new(timeout => 30);
    my $url = "$PORTAL_BASE/keys/validate";
    my $req = HTTP::Request->new('POST', $url);

    $req->header('Authorization' => "Bearer $pk");
    $req->header('Content-Type' => 'application/json');

    if ($sk) {
        my $timestamp = int(time);
        my $sig_input = "$timestamp:POST:/keys/validate:";
        my $signature = hmac_sign($sk, $sig_input);
        $req->header('X-Timestamp' => $timestamp);
        $req->header('X-Signature' => $signature);
    }

    my $res = $ua->request($req);
    return decode_json($res->content) if $res->content;
    return undef;
}

sub health_check {
    my (%opts) = @_;
    my $ua = LWP::UserAgent->new(timeout => 10);
    my $res = $ua->get("$API_BASE/health");
    return $res->is_success ? 1 : 0;
}

# ============================================================================
# CLI Implementation
# ============================================================================

package main;

sub build_input_files {
    my @files = @_;
    my @input_files;
    foreach my $filepath (@files) {
        unless (-e $filepath) {
            print STDERR "${RED}Error: Input file not found: $filepath${RESET}\n";
            exit 1;
        }
        open my $f, '<:raw', $filepath or die "Cannot read file: $!";
        my $content = do { local $/; <$f> };
        close $f;
        push @input_files, {
            filename => basename($filepath),
            content_base64 => encode_base64($content, '')
        };
    }
    return \@input_files;
}

sub build_env_content {
    my ($envs, $env_file) = @_;
    my @parts;

    if ($env_file && -e $env_file) {
        open my $fh, '<', $env_file or die "Cannot read file: $!";
        my $content = do { local $/; <$fh> };
        close $fh;
        push @parts, $content;
    }

    foreach my $e (@$envs) {
        push @parts, $e if $e =~ /=/;
    }

    return join("\n", @parts);
}

sub cmd_execute {
    my ($options) = @_;
    my $source = $options->{source_file};
    my $inline_code = $options->{inline_code};
    my $language = $options->{language};

    my $code;
    if ($inline_code) {
        $code = $inline_code;
    } else {
        unless (-e $source) {
            print STDERR "${RED}Error: File not found: $source${RESET}\n";
            exit 1;
        }
        open my $fh, '<', $source or die "Cannot read file: $!";
        $code = do { local $/; <$fh> };
        close $fh;
        $language ||= Un::detect_language($source);
    }

    unless ($language) {
        print STDERR "${RED}Error: Could not detect language${RESET}\n";
        exit 1;
    }

    my %opts = (network_mode => $options->{network} || 'zerotrust');
    $opts{vcpu} = $options->{vcpu} if $options->{vcpu};

    if ($options->{env} && @{$options->{env}}) {
        my %env_vars;
        foreach my $e (@{$options->{env}}) {
            if ($e =~ /^([^=]+)=(.*)$/) {
                $env_vars{$1} = $2;
            }
        }
        $opts{env} = \%env_vars if %env_vars;
    }

    if ($options->{files} && @{$options->{files}}) {
        $opts{input_files} = build_input_files(@{$options->{files}});
    }

    $opts{return_artifacts} = 1 if $options->{artifacts};

    my $result = Un::execute($language, $code, %opts);

    unless ($result) {
        print STDERR "${RED}Error: " . Un::last_error() . "${RESET}\n";
        exit 1;
    }

    print $result->{stdout} if $result->{stdout};
    print STDERR $result->{stderr} if $result->{stderr};

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

    exit($result->{exit_code} || 0);
}

sub cmd_session {
    my ($options) = @_;

    if ($options->{list}) {
        my $result = Un::session_list();
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
        Un::session_destroy($options->{kill});
        print "${GREEN}Session terminated: $options->{kill}${RESET}\n";
        return;
    }

    if ($options->{info}) {
        my $result = Un::session_get($options->{info});
        print encode_json($result) . "\n";
        return;
    }

    if ($options->{freeze}) {
        Un::session_freeze($options->{freeze});
        print "${GREEN}Session frozen: $options->{freeze}${RESET}\n";
        return;
    }

    if ($options->{unfreeze}) {
        Un::session_unfreeze($options->{unfreeze});
        print "${GREEN}Session unfreezing: $options->{unfreeze}${RESET}\n";
        return;
    }

    if ($options->{boost}) {
        my $vcpu = $options->{vcpu} || 2;
        Un::session_boost($options->{boost}, $vcpu);
        print "${GREEN}Session boosted: $options->{boost}${RESET}\n";
        return;
    }

    if ($options->{unboost}) {
        Un::session_unboost($options->{unboost});
        print "${GREEN}Session unboosted: $options->{unboost}${RESET}\n";
        return;
    }

    if ($options->{execute}) {
        my $result = Un::session_execute($options->{execute}, $options->{command});
        print $result->{stdout} if $result->{stdout};
        print STDERR $result->{stderr} if $result->{stderr};
        return;
    }

    if ($options->{snapshot}) {
        my $result = Un::snapshot_session($options->{snapshot},
            name => $options->{snapshot_name}, hot => $options->{hot});
        print "${GREEN}Snapshot created${RESET}\n";
        print encode_json($result) . "\n";
        return;
    }

    # Create new session
    my %opts = (shell => $options->{shell} || 'bash');
    $opts{network} = $options->{network} if $options->{network};
    $opts{vcpu} = $options->{vcpu} if $options->{vcpu};
    $opts{persistence} = 'tmux' if $options->{tmux};
    $opts{persistence} = 'screen' if $options->{screen};

    if ($options->{files} && @{$options->{files}}) {
        $opts{input_files} = build_input_files(@{$options->{files}});
    }

    print "${YELLOW}Creating session...${RESET}\n";
    my $result = Un::session_create(%opts);
    print "${GREEN}Session created: " . ($result->{id} // 'N/A') . "${RESET}\n";
    print "${YELLOW}(Interactive sessions require WebSocket - use un2 for full support)${RESET}\n";
}

sub cmd_service {
    my ($options) = @_;

    if ($options->{list}) {
        my $result = Un::service_list();
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
        my $result = Un::service_get($options->{info});
        print encode_json($result) . "\n";
        return;
    }

    if ($options->{logs}) {
        my $result = Un::service_logs($options->{logs}, lines => $options->{lines});
        print $result->{logs} // '';
        return;
    }

    if ($options->{freeze}) {
        Un::service_freeze($options->{freeze});
        print "${GREEN}Service frozen: $options->{freeze}${RESET}\n";
        return;
    }

    if ($options->{unfreeze}) {
        Un::service_unfreeze($options->{unfreeze});
        print "${GREEN}Service unfreezing: $options->{unfreeze}${RESET}\n";
        return;
    }

    if ($options->{lock}) {
        Un::service_lock($options->{lock});
        print "${GREEN}Service locked: $options->{lock}${RESET}\n";
        return;
    }

    if ($options->{unlock}) {
        Un::service_unlock($options->{unlock});
        print "${GREEN}Service unlocked: $options->{unlock}${RESET}\n";
        return;
    }

    if ($options->{destroy}) {
        Un::service_destroy($options->{destroy});
        print "${GREEN}Service destroyed: $options->{destroy}${RESET}\n";
        return;
    }

    if ($options->{resize}) {
        unless ($options->{vcpu}) {
            print STDERR "${RED}Error: --vcpu is required with --resize${RESET}\n";
            exit 1;
        }
        Un::service_resize($options->{resize}, $options->{vcpu});
        my $ram = $options->{vcpu} * 2;
        print "${GREEN}Service resized to $options->{vcpu} vCPU, $ram GB RAM${RESET}\n";
        return;
    }

    if ($options->{redeploy}) {
        Un::service_redeploy($options->{redeploy}, bootstrap => $options->{bootstrap});
        print "${GREEN}Service redeployed: $options->{redeploy}${RESET}\n";
        return;
    }

    if ($options->{execute}) {
        my $result = Un::service_execute($options->{execute}, $options->{command});
        print $result->{stdout} if $result->{stdout};
        print STDERR $result->{stderr} if $result->{stderr};
        return;
    }

    if ($options->{set_unfreeze_on_demand}) {
        my $enabled = $options->{set_unfreeze_on_demand_value};
        Un::service_set_unfreeze_on_demand($options->{set_unfreeze_on_demand}, $enabled);
        my $status = $enabled ? 'enabled' : 'disabled';
        print "${GREEN}Unfreeze-on-demand $status${RESET}\n";
        return;
    }

    if ($options->{snapshot}) {
        my $result = Un::snapshot_service($options->{snapshot},
            name => $options->{snapshot_name}, hot => $options->{hot});
        print "${GREEN}Snapshot created${RESET}\n";
        print encode_json($result) . "\n";
        return;
    }

    if ($options->{name}) {
        my %opts = (name => $options->{name});
        if ($options->{ports}) {
            $opts{ports} = [map { int($_) } split(',', $options->{ports})];
        }
        if ($options->{domains}) {
            $opts{domains} = [split(',', $options->{domains})];
        }
        $opts{service_type} = $options->{type} if $options->{type};
        $opts{bootstrap} = $options->{bootstrap} if $options->{bootstrap};

        if ($options->{bootstrap_file}) {
            if (! -e $options->{bootstrap_file}) {
                print STDERR "${RED}Error: Bootstrap file not found${RESET}\n";
                exit 1;
            }
            open my $fh, '<', $options->{bootstrap_file};
            $opts{bootstrap_content} = do { local $/; <$fh> };
            close $fh;
        }

        if ($options->{files} && @{$options->{files}}) {
            $opts{input_files} = build_input_files(@{$options->{files}});
        }

        $opts{network} = $options->{network} if $options->{network};
        $opts{vcpu} = $options->{vcpu} if $options->{vcpu};
        $opts{unfreeze_on_demand} = 1 if $options->{unfreeze_on_demand};

        my $result = Un::service_create(%opts);
        print "${GREEN}Service created: " . ($result->{id} // 'N/A') . "${RESET}\n";
        print "Name: " . ($result->{name} // 'N/A') . "\n";
        print "URL: $result->{url}\n" if $result->{url};

        # Auto-set vault
        my $env_content = build_env_content($options->{env} || [], $options->{env_file});
        if ($env_content && $result->{id}) {
            Un::service_env_set($result->{id}, $env_content);
            print "${GREEN}Vault configured${RESET}\n";
        }
        return;
    }

    print STDERR "${RED}Error: Specify --name to create or use --list, --info, etc.${RESET}\n";
    exit 1;
}

sub cmd_service_env {
    my ($action, $target, $envs, $env_file) = @_;

    unless ($target) {
        print STDERR "${RED}Error: Service ID required${RESET}\n";
        exit 1;
    }

    if ($action eq 'status') {
        my $result = Un::service_env_get($target);
        print encode_json($result) . "\n";
    } elsif ($action eq 'set') {
        my $content = build_env_content($envs, $env_file);
        unless ($content) {
            print STDERR "${RED}Error: No env content provided${RESET}\n";
            exit 1;
        }
        Un::service_env_set($target, $content);
        print "${GREEN}Vault updated${RESET}\n";
    } elsif ($action eq 'export') {
        my $result = Un::service_env_export($target);
        print $result->{env} // $result->{content} // '';
    } elsif ($action eq 'delete') {
        Un::service_env_delete($target);
        print "${GREEN}Vault deleted${RESET}\n";
    } else {
        print STDERR "${RED}Error: Unknown env action '$action'${RESET}\n";
        exit 1;
    }
}

sub cmd_snapshot {
    my ($options) = @_;

    if ($options->{list}) {
        my $result = Un::snapshot_list();
        my $snapshots = $result->{snapshots} || [];
        if (@$snapshots == 0) {
            print "No snapshots\n";
        } else {
            printf "%-40s %-20s %-10s %s\n", 'ID', 'Name', 'Type', 'Created';
            foreach my $s (@$snapshots) {
                printf "%-40s %-20s %-10s %s\n",
                    $s->{id} // 'N/A', $s->{name} // 'N/A',
                    $s->{type} // 'N/A', $s->{created_at} // 'N/A';
            }
        }
        return;
    }

    if ($options->{info}) {
        my $result = Un::snapshot_get($options->{info});
        print encode_json($result) . "\n";
        return;
    }

    if ($options->{delete}) {
        Un::snapshot_delete($options->{delete});
        print "${GREEN}Snapshot deleted: $options->{delete}${RESET}\n";
        return;
    }

    if ($options->{restore}) {
        Un::snapshot_restore($options->{restore});
        print "${GREEN}Snapshot restored${RESET}\n";
        return;
    }

    if ($options->{lock}) {
        Un::snapshot_lock($options->{lock});
        print "${GREEN}Snapshot locked: $options->{lock}${RESET}\n";
        return;
    }

    if ($options->{unlock}) {
        Un::snapshot_unlock($options->{unlock});
        print "${GREEN}Snapshot unlocked: $options->{unlock}${RESET}\n";
        return;
    }

    if ($options->{clone}) {
        my $result = Un::snapshot_clone($options->{clone},
            clone_type => $options->{clone_type} || 'session',
            name => $options->{clone_name});
        print "${GREEN}Snapshot cloned${RESET}\n";
        print encode_json($result) . "\n";
        return;
    }

    print STDERR "${RED}Error: Use --list, --info, --delete, --restore, --lock, --unlock, or --clone${RESET}\n";
    exit 1;
}

sub cmd_image {
    my ($options) = @_;

    if ($options->{list}) {
        my $result = Un::image_list(filter => $options->{filter});
        my $images = $result->{images} || [];
        if (@$images == 0) {
            print "No images\n";
        } else {
            printf "%-40s %-20s %-10s %s\n", 'ID', 'Name', 'Visibility', 'Created';
            foreach my $i (@$images) {
                printf "%-40s %-20s %-10s %s\n",
                    $i->{id} // 'N/A', $i->{name} // 'N/A',
                    $i->{visibility} // 'N/A', $i->{created_at} // 'N/A';
            }
        }
        return;
    }

    if ($options->{info}) {
        my $result = Un::image_get($options->{info});
        print encode_json($result) . "\n";
        return;
    }

    if ($options->{delete}) {
        Un::image_delete($options->{delete});
        print "${GREEN}Image deleted: $options->{delete}${RESET}\n";
        return;
    }

    if ($options->{lock}) {
        Un::image_lock($options->{lock});
        print "${GREEN}Image locked: $options->{lock}${RESET}\n";
        return;
    }

    if ($options->{unlock}) {
        Un::image_unlock($options->{unlock});
        print "${GREEN}Image unlocked: $options->{unlock}${RESET}\n";
        return;
    }

    if ($options->{publish}) {
        unless ($options->{source_type}) {
            print STDERR "${RED}Error: --source-type required${RESET}\n";
            exit 1;
        }
        my $result = Un::image_publish(
            source_type => $options->{source_type},
            source_id => $options->{publish},
            name => $options->{pub_name});
        print "${GREEN}Image published${RESET}\n";
        print encode_json($result) . "\n";
        return;
    }

    if ($options->{visibility_id} && $options->{visibility_mode}) {
        Un::image_set_visibility($options->{visibility_id}, $options->{visibility_mode});
        print "${GREEN}Visibility set to $options->{visibility_mode}${RESET}\n";
        return;
    }

    if ($options->{grant_access}) {
        Un::image_grant_access($options->{grant_access}, $options->{trusted_key});
        print "${GREEN}Access granted${RESET}\n";
        return;
    }

    if ($options->{revoke_access}) {
        Un::image_revoke_access($options->{revoke_access}, $options->{trusted_key});
        print "${GREEN}Access revoked${RESET}\n";
        return;
    }

    if ($options->{list_trusted}) {
        my $result = Un::image_list_trusted($options->{list_trusted});
        print encode_json($result) . "\n";
        return;
    }

    if ($options->{transfer}) {
        Un::image_transfer($options->{transfer}, $options->{to_key});
        print "${GREEN}Image transferred${RESET}\n";
        return;
    }

    if ($options->{spawn}) {
        my $result = Un::image_spawn($options->{spawn},
            name => $options->{spawn_name},
            ports => $options->{spawn_ports} ? [map { int($_) } split(',', $options->{spawn_ports})] : undef);
        print "${GREEN}Service spawned from image${RESET}\n";
        print encode_json($result) . "\n";
        return;
    }

    if ($options->{clone}) {
        my $result = Un::image_clone($options->{clone}, name => $options->{clone_name});
        print "${GREEN}Image cloned${RESET}\n";
        print encode_json($result) . "\n";
        return;
    }

    print STDERR "${RED}Error: Use --list, --info, --delete, etc.${RESET}\n";
    exit 1;
}

sub cmd_key {
    my ($options) = @_;
    my $result = Un::validate_keys();

    if ($options->{extend}) {
        my $pk = $result->{public_key};
        if ($pk) {
            my $url = "$Un::PORTAL_BASE/keys/extend?pk=$pk";
            print "${BLUE}Opening browser to extend key...${RESET}\n";
            system("xdg-open '$url' 2>/dev/null || open '$url' 2>/dev/null &");
        }
        return;
    }

    if ($result->{expired}) {
        print "${RED}Expired${RESET}\n";
        print "Public Key: " . ($result->{public_key} // 'N/A') . "\n";
        print "Tier: " . ($result->{tier} // 'N/A') . "\n";
        print "${YELLOW}To renew: Visit $Un::PORTAL_BASE/keys/extend${RESET}\n";
        exit 1;
    }

    print "${GREEN}Valid${RESET}\n";
    print "Public Key: " . ($result->{public_key} // 'N/A') . "\n";
    print "Tier: " . ($result->{tier} // 'N/A') . "\n";
    print "Status: " . ($result->{status} // 'N/A') . "\n";
    print "Expires: " . ($result->{expires_at} // 'N/A') . "\n";
    print "Time Remaining: " . ($result->{time_remaining} // 'N/A') . "\n";
}

sub cmd_languages {
    my ($options) = @_;
    my $langs = Un::get_languages();

    if ($options->{json}) {
        print encode_json($langs) . "\n";
    } else {
        foreach my $lang (@$langs) {
            print "$lang\n";
        }
    }
}

sub show_help {
    print <<"HELP";
Unsandbox CLI - Execute code in secure sandboxes

Usage:
  perl un.pl [options] <source_file>
  perl un.pl -s <language> '<code>'
  perl un.pl session [options]
  perl un.pl service [options]
  perl un.pl service env <action> <service_id> [options]
  perl un.pl snapshot [options]
  perl un.pl image [options]
  perl un.pl languages [--json]
  perl un.pl key [--extend]

Execute options:
  -e KEY=VALUE      Environment variable (multiple allowed)
  -f FILE           Input file (multiple allowed)
  -a                Return artifacts
  -o DIR            Output directory for artifacts
  -n MODE           Network mode (zerotrust|semitrusted)
  -v N              vCPU count (1-8)
  -s LANG           Language for inline code

Session options:
  --list            List sessions
  --info ID         Get session details
  --kill ID         Terminate session
  --freeze ID       Freeze session
  --unfreeze ID     Unfreeze session
  --boost ID        Boost session (with -v)
  --unboost ID      Unboost session
  --execute ID      Execute command (with --command)
  --snapshot ID     Create snapshot
  --shell SHELL     Shell/REPL (default: bash)
  --tmux            Enable tmux persistence
  --screen          Enable screen persistence

Service options:
  --list            List services
  --info ID         Get service details
  --name NAME       Create service with name
  --ports PORTS     Comma-separated ports
  --domains DOMS    Custom domains
  --bootstrap CMD   Bootstrap command
  --bootstrap-file  Bootstrap script file
  --logs ID         Get logs
  --freeze ID       Freeze service
  --unfreeze ID     Unfreeze service
  --lock ID         Lock service
  --unlock ID       Unlock service
  --destroy ID      Destroy service
  --resize ID       Resize (with -v)
  --redeploy ID     Redeploy service
  --execute ID      Execute command (with --command)
  --snapshot ID     Create snapshot

Service env commands:
  env status ID     Check vault status
  env set ID        Set vault (use -e or --env-file)
  env export ID     Export vault contents
  env delete ID     Delete vault

Snapshot options:
  --list            List snapshots
  --info ID         Get snapshot details
  --delete ID       Delete snapshot
  --restore ID      Restore from snapshot
  --lock ID         Lock snapshot
  --unlock ID       Unlock snapshot
  --clone ID        Clone snapshot (with --clone-type, --clone-name)

Image options:
  --list            List images (with optional --filter)
  --info ID         Get image details
  --delete ID       Delete image
  --lock ID         Lock image
  --unlock ID       Unlock image
  --publish ID      Publish from service/snapshot (--source-type)
  --visibility ID MODE  Set visibility (private|unlisted|public)
  --grant-access ID     Grant access (with --trusted-key)
  --revoke-access ID    Revoke access (with --trusted-key)
  --list-trusted ID     List trusted keys
  --transfer ID         Transfer ownership (with --to-key)
  --spawn ID            Spawn service from image
  --clone ID            Clone image
HELP
    exit 1;
}

sub main {
    my %options = (
        env => [],
        files => []
    );

    my $i = 0;
    while ($i < @ARGV) {
        my $arg = $ARGV[$i];

        if ($arg eq 'session' || $arg eq 'service' || $arg eq 'snapshot' ||
            $arg eq 'image' || $arg eq 'key' || $arg eq 'languages') {
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
        } elsif ($arg eq '-s' || $arg eq '--shell') {
            $options{language} = $ARGV[++$i] if !$options{command};
            $options{shell} = $ARGV[$i] if $options{command} && $options{command} eq 'session';
        } elsif ($arg eq '-l' || $arg eq '--list') {
            $options{list} = 1;
        } elsif ($arg eq '--info') {
            $options{info} = $ARGV[++$i];
        } elsif ($arg eq '--kill') {
            $options{kill} = $ARGV[++$i];
        } elsif ($arg eq '--freeze') {
            $options{freeze} = $ARGV[++$i];
        } elsif ($arg eq '--unfreeze') {
            $options{unfreeze} = $ARGV[++$i];
        } elsif ($arg eq '--lock') {
            $options{lock} = $ARGV[++$i];
        } elsif ($arg eq '--unlock') {
            $options{unlock} = $ARGV[++$i];
        } elsif ($arg eq '--boost') {
            $options{boost} = $ARGV[++$i];
        } elsif ($arg eq '--unboost') {
            $options{unboost} = $ARGV[++$i];
        } elsif ($arg eq '--destroy') {
            $options{destroy} = $ARGV[++$i];
        } elsif ($arg eq '--resize') {
            $options{resize} = $ARGV[++$i];
        } elsif ($arg eq '--redeploy') {
            $options{redeploy} = $ARGV[++$i];
        } elsif ($arg eq '--logs') {
            $options{logs} = $ARGV[++$i];
        } elsif ($arg eq '--lines') {
            $options{lines} = $ARGV[++$i];
        } elsif ($arg eq '--execute') {
            $options{execute} = $ARGV[++$i];
        } elsif ($arg eq '--command') {
            $options{command_str} = $ARGV[++$i];
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
        } elsif ($arg eq '--snapshot') {
            $options{snapshot} = $ARGV[++$i];
        } elsif ($arg eq '--snapshot-name') {
            $options{snapshot_name} = $ARGV[++$i];
        } elsif ($arg eq '--hot') {
            $options{hot} = 1;
        } elsif ($arg eq '--restore') {
            $options{restore} = $ARGV[++$i];
        } elsif ($arg eq '--delete') {
            $options{delete} = $ARGV[++$i];
        } elsif ($arg eq '--clone') {
            $options{clone} = $ARGV[++$i];
        } elsif ($arg eq '--clone-type') {
            $options{clone_type} = $ARGV[++$i];
        } elsif ($arg eq '--clone-name') {
            $options{clone_name} = $ARGV[++$i];
        } elsif ($arg eq '--publish') {
            $options{publish} = $ARGV[++$i];
        } elsif ($arg eq '--source-type') {
            $options{source_type} = $ARGV[++$i];
        } elsif ($arg eq '--visibility') {
            $options{visibility_id} = $ARGV[++$i];
            $options{visibility_mode} = $ARGV[++$i];
        } elsif ($arg eq '--spawn') {
            $options{spawn} = $ARGV[++$i];
        } elsif ($arg eq '--grant-access') {
            $options{grant_access} = $ARGV[++$i];
        } elsif ($arg eq '--revoke-access') {
            $options{revoke_access} = $ARGV[++$i];
        } elsif ($arg eq '--list-trusted') {
            $options{list_trusted} = $ARGV[++$i];
        } elsif ($arg eq '--trusted-key') {
            $options{trusted_key} = $ARGV[++$i];
        } elsif ($arg eq '--transfer') {
            $options{transfer} = $ARGV[++$i];
        } elsif ($arg eq '--to-key') {
            $options{to_key} = $ARGV[++$i];
        } elsif ($arg eq '--filter') {
            $options{filter} = $ARGV[++$i];
        } elsif ($arg eq '--tmux') {
            $options{tmux} = 1;
        } elsif ($arg eq '--screen') {
            $options{screen} = 1;
        } elsif ($arg eq '--unfreeze-on-demand') {
            $options{unfreeze_on_demand} = 1;
        } elsif ($arg eq '--set-unfreeze-on-demand') {
            $options{set_unfreeze_on_demand} = $ARGV[++$i];
            $options{set_unfreeze_on_demand_value} = ($ARGV[++$i] =~ /^(true|1)$/i);
        } elsif ($arg eq '--json') {
            $options{json} = 1;
        } elsif ($arg eq '--extend') {
            $options{extend} = 1;
        } elsif ($arg eq 'env' && $options{command} && $options{command} eq 'service') {
            $options{env_action} = $ARGV[++$i];
            $options{env_target} = $ARGV[++$i] if defined $ARGV[$i+1] && $ARGV[$i+1] !~ /^-/;
        } elsif ($arg eq '--help' || $arg eq '-h') {
            show_help();
        } elsif ($arg =~ /^-/) {
            print STDERR "${RED}Unknown option: $arg${RESET}\n";
            exit 1;
        } else {
            if ($options{language} && !$options{inline_code}) {
                $options{inline_code} = $arg;
            } else {
                $options{source_file} = $arg;
            }
        }
        $i++;
    }

    # Handle commands
    if ($options{command}) {
        if ($options{command} eq 'session') {
            cmd_session(\%options);
        } elsif ($options{command} eq 'service') {
            if ($options{env_action}) {
                cmd_service_env($options{env_action}, $options{env_target},
                    $options{env}, $options{env_file});
            } else {
                $options{command} = $options{command_str} if $options{command_str};
                cmd_service(\%options);
            }
        } elsif ($options{command} eq 'snapshot') {
            cmd_snapshot(\%options);
        } elsif ($options{command} eq 'image') {
            cmd_image(\%options);
        } elsif ($options{command} eq 'languages') {
            cmd_languages(\%options);
        } elsif ($options{command} eq 'key') {
            cmd_key(\%options);
        }
    } elsif ($options{source_file} || $options{inline_code}) {
        cmd_execute(\%options);
    } else {
        show_help();
    }
}

main() unless caller;
1;
