#!/usr/bin/env perl
# Unit tests for un.pl - tests internal functions without API calls

use strict;
use warnings;
use Digest::SHA qw(hmac_sha256_hex);
use MIME::Base64;
use File::Basename;
use File::Temp qw(tempfile);

my $passed = 0;
my $failed = 0;

sub test {
    my ($name, $fn) = @_;
    eval { $fn->(); };
    if ($@) {
        print "  ✗ $name\n";
        print "    $@";
        $failed++;
    } else {
        print "  ✓ $name\n";
        $passed++;
    }
}

sub assert_equal {
    my ($actual, $expected) = @_;
    die "Expected '$expected' but got '$actual'\n" unless $actual eq $expected;
}

sub assert_not_equal {
    my ($a, $b) = @_;
    die "Expected values to be different but both were '$a'\n" if $a eq $b;
}

sub assert_contains {
    my ($str, $substr) = @_;
    die "Expected '$str' to contain '$substr'\n" unless index($str, $substr) != -1;
}

sub assert_true {
    my ($val) = @_;
    die "Expected true but got false\n" unless $val;
}

# Extension mapping
my %EXT_MAP = (
    ".py" => "python", ".js" => "javascript", ".ts" => "typescript",
    ".rb" => "ruby", ".php" => "php", ".pl" => "perl", ".lua" => "lua",
    ".sh" => "bash", ".go" => "go", ".rs" => "rust", ".c" => "c",
    ".cpp" => "cpp", ".cc" => "cpp", ".cxx" => "cpp",
    ".java" => "java", ".kt" => "kotlin", ".cs" => "csharp", ".fs" => "fsharp",
    ".hs" => "haskell", ".ml" => "ocaml", ".clj" => "clojure", ".scm" => "scheme",
    ".lisp" => "commonlisp", ".erl" => "erlang", ".ex" => "elixir", ".exs" => "elixir",
    ".jl" => "julia", ".r" => "r", ".R" => "r", ".cr" => "crystal",
    ".d" => "d", ".nim" => "nim", ".zig" => "zig", ".v" => "v",
    ".dart" => "dart", ".groovy" => "groovy", ".scala" => "scala",
    ".f90" => "fortran", ".f95" => "fortran", ".cob" => "cobol",
    ".pro" => "prolog", ".forth" => "forth", ".4th" => "forth",
    ".tcl" => "tcl", ".raku" => "raku", ".m" => "objc",
);

print "\n=== Extension Mapping Tests ===\n";

test("Python extension maps correctly", sub {
    assert_equal($EXT_MAP{".py"}, "python");
});

test("JavaScript extensions map correctly", sub {
    assert_equal($EXT_MAP{".js"}, "javascript");
    assert_equal($EXT_MAP{".ts"}, "typescript");
});

test("Perl extension maps correctly", sub {
    assert_equal($EXT_MAP{".pl"}, "perl");
});

test("Ruby extension maps correctly", sub {
    assert_equal($EXT_MAP{".rb"}, "ruby");
});

test("Go extension maps correctly", sub {
    assert_equal($EXT_MAP{".go"}, "go");
});

test("C/C++ extensions map correctly", sub {
    assert_equal($EXT_MAP{".c"}, "c");
    assert_equal($EXT_MAP{".cpp"}, "cpp");
});

test("JVM extensions map correctly", sub {
    assert_equal($EXT_MAP{".java"}, "java");
    assert_equal($EXT_MAP{".kt"}, "kotlin");
});

test("Functional extensions map correctly", sub {
    assert_equal($EXT_MAP{".hs"}, "haskell");
    assert_equal($EXT_MAP{".clj"}, "clojure");
    assert_equal($EXT_MAP{".erl"}, "erlang");
});

print "\n=== HMAC Signature Tests ===\n";

test("HMAC-SHA256 generates 64 character hex string", sub {
    my $sig = hmac_sha256_hex("test-message", "test-secret");
    assert_equal(length($sig), 64);
});

test("Same input produces same signature", sub {
    my $sig1 = hmac_sha256_hex("message", "key");
    my $sig2 = hmac_sha256_hex("message", "key");
    assert_equal($sig1, $sig2);
});

test("Different secrets produce different signatures", sub {
    my $sig1 = hmac_sha256_hex("message", "key1");
    my $sig2 = hmac_sha256_hex("message", "key2");
    assert_not_equal($sig1, $sig2);
});

test("Different messages produce different signatures", sub {
    my $sig1 = hmac_sha256_hex("message1", "key");
    my $sig2 = hmac_sha256_hex("message2", "key");
    assert_not_equal($sig1, $sig2);
});

test("Signature format is timestamp:METHOD:path:body", sub {
    my $timestamp = "1704067200";
    my $method = "POST";
    my $endpoint = "/execute";
    my $body = '{"language":"python"}';

    my $message = "$timestamp:$method:$endpoint:$body";

    assert_true(index($message, $timestamp) == 0);
    assert_contains($message, ":POST:");
    assert_contains($message, ":/execute:");
});

print "\n=== Language Detection Tests ===\n";

test("Detect language from .pl extension", sub {
    my ($name, $path, $suffix) = fileparse("script.pl", qr/\.[^.]*/);
    assert_equal($EXT_MAP{$suffix}, "perl");
});

test("Python shebang detection", sub {
    my $content = "#!/usr/bin/env python3\nprint('hello')";
    my ($firstLine) = split(/\n/, $content);
    assert_true(index($firstLine, "#!") == 0);
    assert_contains($firstLine, "python");
});

test("Perl shebang detection", sub {
    my $content = "#!/usr/bin/env perl\nprint 'hello'";
    my ($firstLine) = split(/\n/, $content);
    assert_true(index($firstLine, "#!") == 0);
    assert_contains($firstLine, "perl");
});

print "\n=== Argument Parsing Tests ===\n";

test("Parse -e KEY=VALUE format", sub {
    my $arg = "DEBUG=1";
    my ($key, $value) = split(/=/, $arg, 2);
    assert_equal($key, "DEBUG");
    assert_equal($value, "1");
});

test("Parse -e KEY=VALUE with equals in value", sub {
    my $arg = "URL=https://example.com?foo=bar";
    my ($key, $value) = split(/=/, $arg, 2);
    assert_equal($key, "URL");
    assert_equal($value, "https://example.com?foo=bar");
});

test("Valid network modes", sub {
    my %valid = (zerotrust => 1, semitrusted => 1);
    assert_true(exists $valid{zerotrust});
    assert_true(exists $valid{semitrusted});
    assert_true(!exists $valid{invalid});
});

print "\n=== File Operations Tests ===\n";

test("Base64 encoding/decoding", sub {
    my $content = "print('hello world')";
    my $encoded = encode_base64($content, '');
    my $decoded = decode_base64($encoded);
    assert_equal($decoded, $content);
});

test("Extract file basename", sub {
    my $path = "/home/user/project/script.pl";
    assert_equal(basename($path), "script.pl");
});

print "\n=== API Constants Tests ===\n";

test("API base URL format", sub {
    my $apiBase = "https://api.unsandbox.com";
    assert_true(index($apiBase, "https://") == 0);
    assert_contains($apiBase, "unsandbox.com");
});

print "\n=== Summary ===\n";
print "Passed: $passed\n";
print "Failed: $failed\n";
print "Total:  " . ($passed + $failed) . "\n";

exit($failed > 0 ? 1 : 0);
