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
    my $key = $args_key || $ENV{'UNSANDBOX_API_KEY'};
    unless ($key) {
        print STDERR "${RED}Error: UNSANDBOX_API_KEY not set${RESET}\n";
        exit 1;
    }
    return $key;
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
    my ($endpoint, $method, $data, $api_key) = @_;
    $method //= 'GET';

    my $url = "$API_BASE$endpoint";
    my $ua = LWP::UserAgent->new(timeout => 300);
    my $request = HTTP::Request->new($method => $url);
    $request->header('Authorization' => "Bearer $api_key");
    $request->header('Content-Type' => 'application/json');

    if ($data) {
        $request->content(encode_json($data));
    }

    my $response = $ua->request($request);

    unless ($response->is_success) {
        print STDERR "${RED}Error: HTTP ", $response->code, " - ", $response->content, "${RESET}\n";
        exit 1;
    }

    return decode_json($response->content);
}

sub cmd_execute {
    my ($options) = @_;
    my $api_key = get_api_key($options->{api_key});

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

    my $result = api_request('/execute', 'POST', $payload, $api_key);

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
    my $api_key = get_api_key($options->{api_key});

    if ($options->{list}) {
        my $result = api_request('/sessions', 'GET', undef, $api_key);
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
        api_request("/sessions/$options->{kill}", 'DELETE', undef, $api_key);
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

    print "${YELLOW}Creating session...${RESET}\n";
    my $result = api_request('/sessions', 'POST', $payload, $api_key);
    print "${GREEN}Session created: ", ($result->{id} // 'N/A'), "${RESET}\n";
    print "${YELLOW}(Interactive sessions require WebSocket - use un2 for full support)${RESET}\n";
}

sub cmd_service {
    my ($options) = @_;
    my $api_key = get_api_key($options->{api_key});

    if ($options->{list}) {
        my $result = api_request('/services', 'GET', undef, $api_key);
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
        my $result = api_request("/services/$options->{info}", 'GET', undef, $api_key);
        print encode_json($result);
        print "\n";
        return;
    }

    if ($options->{logs}) {
        my $result = api_request("/services/$options->{logs}/logs", 'GET', undef, $api_key);
        print $result->{logs} // '';
        return;
    }

    if ($options->{tail}) {
        my $result = api_request("/services/$options->{tail}/logs?lines=9000", 'GET', undef, $api_key);
        print $result->{logs} // '';
        return;
    }

    if ($options->{sleep}) {
        api_request("/services/$options->{sleep}/sleep", 'POST', undef, $api_key);
        print "${GREEN}Service sleeping: $options->{sleep}${RESET}\n";
        return;
    }

    if ($options->{wake}) {
        api_request("/services/$options->{wake}/wake", 'POST', undef, $api_key);
        print "${GREEN}Service waking: $options->{wake}${RESET}\n";
        return;
    }

    if ($options->{destroy}) {
        api_request("/services/$options->{destroy}", 'DELETE', undef, $api_key);
        print "${GREEN}Service destroyed: $options->{destroy}${RESET}\n";
        return;
    }

    if ($options->{execute}) {
        my $payload = { command => $options->{command} };
        my $result = api_request("/services/$options->{execute}/execute", 'POST', $payload, $api_key);
        print "${BLUE}$result->{stdout}${RESET}" if $result->{stdout};
        print STDERR "${RED}$result->{stderr}${RESET}" if $result->{stderr};
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
            if (-e $options->{bootstrap}) {
                open my $fh, '<', $options->{bootstrap} or die "Cannot read file: $!";
                local $/;
                $payload->{bootstrap} = <$fh>;
                close $fh;
            } else {
                $payload->{bootstrap} = $options->{bootstrap};
            }
        }
        $payload->{network} = $options->{network} if $options->{network};
        $payload->{vcpu} = $options->{vcpu} if $options->{vcpu};

        my $result = api_request('/services', 'POST', $payload, $api_key);
        print "${GREEN}Service created: ", ($result->{id} // 'N/A'), "${RESET}\n";
        print "Name: ", ($result->{name} // 'N/A'), "\n";
        print "URL: $result->{url}\n" if $result->{url};
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
    my ($api_key, $should_extend) = @_;

    # Call /keys/validate endpoint
    my $url = "$PORTAL_BASE/keys/validate";
    my $ua = LWP::UserAgent->new(timeout => 30);
    my $request = HTTP::Request->new('POST' => $url);
    $request->header('Authorization' => "Bearer $api_key");
    $request->header('Content-Type' => 'application/json');

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
    my $api_key = get_api_key($options->{api_key});
    validate_key($api_key, $options->{extend});
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
        info => undef,
        logs => undef,
        tail => undef,
        sleep => undef,
        wake => undef,
        destroy => undef,
        execute => undef,
        command => undef,
        extend => 0
    );

    for (my $i = 0; $i < @ARGV; $i++) {
        my $arg = $ARGV[$i];

        if ($arg eq 'session' || $arg eq 'service' || $arg eq 'key') {
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
        } elsif ($arg eq '--info') {
            $options{info} = $ARGV[++$i];
        } elsif ($arg eq '--logs') {
            $options{logs} = $ARGV[++$i];
        } elsif ($arg eq '--tail') {
            $options{tail} = $ARGV[++$i];
        } elsif ($arg eq '--sleep') {
            $options{sleep} = $ARGV[++$i];
        } elsif ($arg eq '--wake') {
            $options{wake} = $ARGV[++$i];
        } elsif ($arg eq '--destroy') {
            $options{destroy} = $ARGV[++$i];
        } elsif ($arg eq '--execute') {
            $options{execute} = $ARGV[++$i];
        } elsif ($arg eq '--command') {
            $options{command} = $ARGV[++$i];
        } elsif ($arg eq '--extend') {
            $options{extend} = 1;
        } elsif ($arg !~ /^-/) {
            $options{source_file} = $arg;
        }
    }

    if ($options{command} && $options{command} eq 'session') {
        cmd_session(\%options);
    } elsif ($options{command} && $options{command} eq 'service') {
        cmd_service(\%options);
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
  $0 key [options]

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

Key options:
  --extend         Open browser to extend/renew key
HELP
        exit 1;
    }
}

main();
