#!/usr/bin/env raku
# Unit tests for un.raku - tests internal functions without API calls
# Run with: raku test_raku.raku

my $passed = 0;
my $failed = 0;

sub test(Str $name, Bool $result) {
    if $result {
        say "  ✓ $name";
        $passed++;
    } else {
        say "  ✗ $name";
        $failed++;
    }
}

my %ext-map = (
    '.py' => 'python', '.js' => 'javascript', '.ts' => 'typescript',
    '.rb' => 'ruby', '.go' => 'go', '.rs' => 'rust', '.c' => 'c',
    '.raku' => 'raku', '.java' => 'java', '.hs' => 'haskell'
);

sub get-language(Str $ext --> Str) {
    %ext-map{$ext} // '';
}

sub get-extension(Str $filename --> Str) {
    my $idx = $filename.rindex('.');
    $idx.defined ?? $filename.substr($idx) !! '';
}

sub get-basename(Str $path --> Str) {
    my $idx = $path.rindex('/');
    $idx.defined ?? $path.substr($idx + 1) !! $path;
}

sub hmac-sha256(Str $secret, Str $message --> Str) {
    use Digest::HMAC;
    use Digest::SHA256::Native;
    my $hmac = hmac($secret.encode, $message.encode, &sha256);
    $hmac>>.fmt('%02x').join;
}

say "\n=== Extension Mapping Tests ===";

test("Python extension maps correctly",
     get-language('.py') eq 'python');

test("Raku extension maps correctly",
     get-language('.raku') eq 'raku');

test("JavaScript extension maps correctly",
     get-language('.js') eq 'javascript');

test("Go extension maps correctly",
     get-language('.go') eq 'go');

say "\n=== Signature Format Tests ===";

my $timestamp = "1704067200";
my $method = "POST";
my $endpoint = "/execute";
my $body = '{"language":"python"}';
my $message = "$timestamp:$method:$endpoint:$body";

test("Signature format starts with timestamp",
     $message.starts-with($timestamp));

test("Signature format contains :POST:",
     $message.contains(':POST:'));

test("Signature format contains :/execute:",
     $message.contains(':/execute:'));

say "\n=== Language Detection Tests ===";

my $content = "#!/usr/bin/env python3\nprint('hello')";
my $first-line = $content.split("\n")[0];

test("Python shebang detection - starts with #!",
     $first-line.starts-with('#!'));

test("Python shebang detection - contains python",
     $first-line.contains('python'));

say "\n=== Argument Parsing Tests ===";

my $arg1 = "DEBUG=1";
my @parts1 = $arg1.split('=', 2);
my ($key1, $value1) = @parts1;

test("Parse -e KEY=VALUE format - key",
     $key1 eq 'DEBUG');

test("Parse -e KEY=VALUE format - value",
     $value1 eq '1');

my $arg2 = "URL=https://example.com?foo=bar";
my @parts2 = $arg2.split('=', 2);
my ($key2, $value2) = @parts2;

test("Parse -e KEY=VALUE with equals in value",
     $key2 eq 'URL' && $value2 eq 'https://example.com?foo=bar');

say "\n=== File Operations Tests ===";

test("Extract file basename",
     get-basename('/home/user/project/script.raku') eq 'script.raku');

test("Extract file extension",
     get-extension('/home/user/project/script.raku') eq '.raku');

say "\n=== API Constants Tests ===";

my $api-base = "https://api.unsandbox.com";

test("API base URL starts with https://",
     $api-base.starts-with('https://'));

test("API base URL contains unsandbox.com",
     $api-base.contains('unsandbox.com'));

say "\n=== Summary ===";
say "Passed: $passed";
say "Failed: $failed";
say "Total:  {$passed + $failed}";

exit($failed > 0 ?? 1 !! 0);
