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

constant $API_BASE = "https://api.unsandbox.com";
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

sub get-api-key() {
    my $key = %*ENV<UNSANDBOX_API_KEY>;
    unless $key {
        note "{$RED}Error: UNSANDBOX_API_KEY not set{$RESET}";
        exit 1;
    }
    return $key;
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

sub api-request(Str $endpoint, Str $method, %data?, Str :$api-key!) {
    my $url = $API_BASE ~ $endpoint;
    my @args = 'curl', '-s';

    if $method eq 'GET' {
        @args.append: '-X', 'GET';
    } elsif $method eq 'DELETE' {
        @args.append: '-X', 'DELETE';
    } elsif $method eq 'POST' {
        @args.append: '-X', 'POST';
        @args.append: '-H', 'Content-Type: application/json';
        if %data {
            @args.append: '-d', to-json(%data);
        }
    }

    @args.append: '-H', "Authorization: Bearer $api-key";
    @args.append: $url;

    my $proc = run |@args, :out, :err;
    my $body = $proc.out.slurp;
    my $err = $proc.err.slurp;

    if $proc.exitcode != 0 {
        note "{$RED}Error: API request failed{$RESET}";
        note $err if $err;
        exit 1;
    }

    return from-json($body);
}

sub cmd-execute(@args) {
    my $api-key = get-api-key();
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
                $source-file = @args[$i];
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
    my %result = api-request('/execute', 'POST', %payload, :$api-key);

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
    my $api-key = get-api-key();
    my $list-mode = False;
    my $kill-id = '';
    my $shell = '';
    my $network = '';
    my $vcpu = 0;

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
        }
        $i++;
    }

    if $list-mode {
        my %result = api-request('/sessions', 'GET', :$api-key);
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
        api-request("/sessions/$kill-id", 'DELETE', :$api-key);
        say "{$GREEN}Session terminated: $kill-id{$RESET}";
        return;
    }

    # Create new session
    my %payload = shell => ($shell || 'bash');
    %payload<network> = $network if $network;
    %payload<vcpu> = $vcpu if $vcpu > 0;

    say "{$YELLOW}Creating session...{$RESET}";
    my %result = api-request('/sessions', 'POST', %payload, :$api-key);
    say "{$GREEN}Session created: {%result<id>}{$RESET}";
    say "{$YELLOW}(Interactive sessions require WebSocket - use un2 for full support){$RESET}";
}

sub cmd-service(@args) {
    my $api-key = get-api-key();
    my $list-mode = False;
    my $info-id = '';
    my $logs-id = '';
    my $sleep-id = '';
    my $wake-id = '';
    my $destroy-id = '';
    my $name = '';
    my $ports = '';
    my $type = '';
    my $bootstrap = '';
    my $network = '';
    my $vcpu = 0;

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
            when '--sleep' {
                $i++;
                $sleep-id = @args[$i];
            }
            when '--wake' {
                $i++;
                $wake-id = @args[$i];
            }
            when '--destroy' {
                $i++;
                $destroy-id = @args[$i];
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
            when '-n' {
                $i++;
                $network = @args[$i];
            }
            when '-v' {
                $i++;
                $vcpu = @args[$i].Int;
            }
        }
        $i++;
    }

    if $list-mode {
        my %result = api-request('/services', 'GET', :$api-key);
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
        my %result = api-request("/services/$info-id", 'GET', :$api-key);
        say to-json(%result, :pretty);
        return;
    }

    if $logs-id {
        my %result = api-request("/services/$logs-id/logs", 'GET', :$api-key);
        say %result<logs>;
        return;
    }

    if $sleep-id {
        api-request("/services/$sleep-id/sleep", 'POST', :$api-key);
        say "{$GREEN}Service sleeping: $sleep-id{$RESET}";
        return;
    }

    if $wake-id {
        api-request("/services/$wake-id/wake", 'POST', :$api-key);
        say "{$GREEN}Service waking: $wake-id{$RESET}";
        return;
    }

    if $destroy-id {
        api-request("/services/$destroy-id", 'DELETE', :$api-key);
        say "{$GREEN}Service destroyed: $destroy-id{$RESET}";
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
            # Check if bootstrap is a file
            if $bootstrap.IO.e && $bootstrap.IO.f {
                %payload<bootstrap> = $bootstrap.IO.slurp;
            } else {
                %payload<bootstrap> = $bootstrap;
            }
        }

        %payload<network> = $network if $network;
        %payload<vcpu> = $vcpu if $vcpu > 0;

        my %result = api-request('/services', 'POST', %payload, :$api-key);
        say "{$GREEN}Service created: {%result<id>}{$RESET}";
        say "Name: {%result<name>}";
        say "URL: {%result<url>}" if %result<url>;
        return;
    }

    note "{$RED}Error: Specify --name to create a service, or use --list, --info, etc.{$RESET}";
    exit 1;
}

sub MAIN(*@args) {
    unless @args {
        note "Usage: un.raku [options] <source_file>";
        note "       un.raku session [options]";
        note "       un.raku service [options]";
        exit 1;
    }

    given @args[0] {
        when 'session' {
            cmd-session(@args[1..*]);
        }
        when 'service' {
            cmd-service(@args[1..*]);
        }
        default {
            cmd-execute(@args);
        }
    }
}
