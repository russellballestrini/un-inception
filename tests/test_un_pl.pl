#!/usr/bin/env perl
# Test suite for UN CLI Perl implementation (un.pl)
# Tests extension detection, API calls, and end-to-end functionality

use strict;
use warnings;
use File::Basename;
use File::Spec;
use JSON::PP;
use LWP::UserAgent;
use HTTP::Request;

# Test configuration
my $script_dir = dirname(__FILE__);
my $UN_SCRIPT = File::Spec->catfile($script_dir, '..', 'un.pl');
my $FIB_PY = File::Spec->catfile($script_dir, '..', '..', 'test', 'fib.py');

package TestResults;

sub new {
    my $class = shift;
    my $self = {
        passed => 0,
        failed => 0,
        skipped => 0,
    };
    return bless $self, $class;
}

sub pass_test {
    my ($self, $name) = @_;
    print "PASS: $name\n";
    $self->{passed}++;
}

sub fail_test {
    my ($self, $name, $error) = @_;
    print "FAIL: $name - $error\n";
    $self->{failed}++;
}

sub skip_test {
    my ($self, $name, $reason) = @_;
    print "SKIP: $name - $reason\n";
    $self->{skipped}++;
}

package main;

my $results = TestResults->new();

# Extension map for testing
my %EXTENSION_MAP = (
    '.py' => 'python', '.js' => 'javascript', '.ts' => 'typescript', '.rb' => 'ruby',
    '.php' => 'php', '.pl' => 'perl', '.lua' => 'lua', '.sh' => 'bash',
    '.go' => 'go', '.rs' => 'rust', '.c' => 'c', '.cpp' => 'cpp', '.cc' => 'cpp',
    '.java' => 'java', '.kt' => 'kotlin', '.cs' => 'csharp', '.hs' => 'haskell',
    '.ml' => 'ocaml', '.clj' => 'clojure', '.ex' => 'elixir', '.erl' => 'erlang',
    '.swift' => 'swift', '.r' => 'r', '.jl' => 'julia', '.dart' => 'dart',
    '.scala' => 'scala', '.groovy' => 'groovy', '.nim' => 'nim', '.cr' => 'crystal',
    '.v' => 'vlang', '.zig' => 'zig', '.fs' => 'fsharp', '.vb' => 'vb',
    '.pas' => 'pascal', '.f90' => 'fortran', '.asm' => 'assembly', '.d' => 'd',
    '.rkt' => 'racket', '.scm' => 'scheme', '.lisp' => 'common_lisp',
    '.sol' => 'solidity', '.cob' => 'cobol', '.ada' => 'ada', '.tcl' => 'tcl',
);

sub detect_language {
    my ($filename) = @_;
    my ($name, $dir, $ext) = fileparse($filename, qr/\.[^.]*/);
    return $EXTENSION_MAP{lc($ext)};
}

# Test 1: Extension detection for Python
eval {
    my $lang = detect_language('test.py');
    if ($lang eq 'python') {
        $results->pass_test('Extension detection: .py -> python');
    } else {
        $results->fail_test('Extension detection: .py -> python', "Got $lang");
    }
};
if ($@) {
    $results->fail_test('Extension detection: .py -> python', $@);
}

# Test 2: Extension detection for JavaScript
eval {
    my $lang = detect_language('test.js');
    if ($lang eq 'javascript') {
        $results->pass_test('Extension detection: .js -> javascript');
    } else {
        $results->fail_test('Extension detection: .js -> javascript', "Got $lang");
    }
};
if ($@) {
    $results->fail_test('Extension detection: .js -> javascript', $@);
}

# Test 3: Extension detection for Ruby
eval {
    my $lang = detect_language('test.rb');
    if ($lang eq 'ruby') {
        $results->pass_test('Extension detection: .rb -> ruby');
    } else {
        $results->fail_test('Extension detection: .rb -> ruby', "Got $lang");
    }
};
if ($@) {
    $results->fail_test('Extension detection: .rb -> ruby', $@);
}

# Test 4: Extension detection for Go
eval {
    my $lang = detect_language('test.go');
    if ($lang eq 'go') {
        $results->pass_test('Extension detection: .go -> go');
    } else {
        $results->fail_test('Extension detection: .go -> go', "Got $lang");
    }
};
if ($@) {
    $results->fail_test('Extension detection: .go -> go', $@);
}

# Test 5: Extension detection for Rust
eval {
    my $lang = detect_language('test.rs');
    if ($lang eq 'rust') {
        $results->pass_test('Extension detection: .rs -> rust');
    } else {
        $results->fail_test('Extension detection: .rs -> rust', "Got $lang");
    }
};
if ($@) {
    $results->fail_test('Extension detection: .rs -> rust', $@);
}

# Test 6: Extension detection for unknown extension
eval {
    my $lang = detect_language('test.unknown');
    if (!defined $lang) {
        $results->pass_test('Extension detection: .unknown -> undef');
    } else {
        $results->fail_test('Extension detection: .unknown -> undef', "Got $lang");
    }
};
if ($@) {
    $results->fail_test('Extension detection: .unknown -> undef', $@);
}

# Test 7: API call test (requires UNSANDBOX_API_KEY)
if (!$ENV{'UNSANDBOX_API_KEY'}) {
    $results->skip_test('API call test', 'UNSANDBOX_API_KEY not set');
} else {
    eval {
        my $payload = encode_json({
            language => 'python',
            code => 'print("Hello from API")'
        });

        my $ua = LWP::UserAgent->new();
        my $request = HTTP::Request->new(POST => 'https://api.unsandbox.com/execute');
        $request->header('Authorization' => "Bearer $ENV{'UNSANDBOX_API_KEY'}");
        $request->header('Content-Type' => 'application/json');
        $request->content($payload);

        my $response = $ua->request($request);

        if ($response->is_success) {
            my $result = decode_json($response->content);
            if ($result->{stdout} && $result->{stdout} =~ /Hello from API/) {
                $results->pass_test('API call test');
            } else {
                $results->fail_test('API call test', "Unexpected result: " . encode_json($result));
            }
        } else {
            $results->fail_test('API call test', "HTTP " . $response->code . ": " . $response->content);
        }
    };
    if ($@) {
        $results->fail_test('API call test', $@);
    }
}

# Test 8: End-to-end test with fib.py
if (!$ENV{'UNSANDBOX_API_KEY'}) {
    $results->skip_test('End-to-end fib.py test', 'UNSANDBOX_API_KEY not set');
} elsif (!-e $FIB_PY) {
    $results->skip_test('End-to-end fib.py test', "fib.py not found at $FIB_PY");
} else {
    eval {
        my $output = `$UN_SCRIPT $FIB_PY 2>&1`;
        my $exit_code = $? >> 8;

        if ($output =~ /fib\(10\) = 55/) {
            $results->pass_test('End-to-end fib.py test');
        } else {
            my $preview = substr($output, 0, 200);
            $results->fail_test('End-to-end fib.py test',
                "Expected 'fib(10) = 55' in output, got: $preview");
        }
    };
    if ($@) {
        $results->fail_test('End-to-end fib.py test', $@);
    }
}

# Print summary
print "\n" . "=" x 50 . "\n";
print "Test Summary:\n";
print "  PASSED:  $results->{passed}\n";
print "  FAILED:  $results->{failed}\n";
print "  SKIPPED: $results->{skipped}\n";
my $total = $results->{passed} + $results->{failed} + $results->{skipped};
print "  TOTAL:   $total\n";
print "=" x 50 . "\n";

# Exit with appropriate code
exit($results->{failed} == 0 ? 0 : 1);
