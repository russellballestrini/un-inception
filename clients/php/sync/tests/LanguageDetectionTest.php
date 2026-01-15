<?php
/**
 * Tests for language detection from filename extension
 */

declare(strict_types=1);

namespace Unsandbox\Tests;

use PHPUnit\Framework\TestCase;

require_once __DIR__ . '/../src/un.php';

use Unsandbox\Unsandbox;

class LanguageDetectionTest extends TestCase
{
    public function testDetectPython(): void
    {
        $this->assertEquals('python', Unsandbox::detectLanguage('script.py'));
        $this->assertEquals('python', Unsandbox::detectLanguage('test.py'));
    }

    public function testDetectJavaScript(): void
    {
        $this->assertEquals('javascript', Unsandbox::detectLanguage('app.js'));
        $this->assertEquals('javascript', Unsandbox::detectLanguage('index.js'));
    }

    public function testDetectTypeScript(): void
    {
        $this->assertEquals('typescript', Unsandbox::detectLanguage('main.ts'));
    }

    public function testDetectRuby(): void
    {
        $this->assertEquals('ruby', Unsandbox::detectLanguage('script.rb'));
    }

    public function testDetectPhp(): void
    {
        $this->assertEquals('php', Unsandbox::detectLanguage('index.php'));
    }

    public function testDetectGo(): void
    {
        $this->assertEquals('go', Unsandbox::detectLanguage('main.go'));
    }

    public function testDetectRust(): void
    {
        $this->assertEquals('rust', Unsandbox::detectLanguage('main.rs'));
    }

    public function testDetectC(): void
    {
        $this->assertEquals('c', Unsandbox::detectLanguage('program.c'));
    }

    public function testDetectCpp(): void
    {
        $this->assertEquals('cpp', Unsandbox::detectLanguage('program.cpp'));
        $this->assertEquals('cpp', Unsandbox::detectLanguage('code.cc'));
        $this->assertEquals('cpp', Unsandbox::detectLanguage('main.cxx'));
    }

    public function testDetectJava(): void
    {
        $this->assertEquals('java', Unsandbox::detectLanguage('Main.java'));
    }

    public function testDetectKotlin(): void
    {
        $this->assertEquals('kotlin', Unsandbox::detectLanguage('main.kt'));
    }

    public function testDetectCSharp(): void
    {
        $this->assertEquals('csharp', Unsandbox::detectLanguage('Program.cs'));
    }

    public function testDetectHaskell(): void
    {
        $this->assertEquals('haskell', Unsandbox::detectLanguage('Main.hs'));
    }

    public function testDetectElixir(): void
    {
        $this->assertEquals('elixir', Unsandbox::detectLanguage('script.ex'));
        $this->assertEquals('elixir', Unsandbox::detectLanguage('script.exs'));
    }

    public function testDetectBash(): void
    {
        $this->assertEquals('bash', Unsandbox::detectLanguage('script.sh'));
    }

    public function testDetectLua(): void
    {
        $this->assertEquals('lua', Unsandbox::detectLanguage('script.lua'));
    }

    public function testDetectPerl(): void
    {
        $this->assertEquals('perl', Unsandbox::detectLanguage('script.pl'));
    }

    public function testDetectR(): void
    {
        $this->assertEquals('r', Unsandbox::detectLanguage('analysis.r'));
        $this->assertEquals('r', Unsandbox::detectLanguage('analysis.R'));
    }

    public function testDetectScheme(): void
    {
        $this->assertEquals('scheme', Unsandbox::detectLanguage('code.scm'));
        $this->assertEquals('scheme', Unsandbox::detectLanguage('code.ss'));
    }

    public function testDetectNullForEmptyFilename(): void
    {
        $this->assertNull(Unsandbox::detectLanguage(''));
    }

    public function testDetectNullForNoExtension(): void
    {
        $this->assertNull(Unsandbox::detectLanguage('Makefile'));
        $this->assertNull(Unsandbox::detectLanguage('README'));
    }

    public function testDetectNullForUnknownExtension(): void
    {
        $this->assertNull(Unsandbox::detectLanguage('file.xyz'));
        $this->assertNull(Unsandbox::detectLanguage('file.unknown'));
    }

    public function testDetectWithPath(): void
    {
        $this->assertEquals('python', Unsandbox::detectLanguage('/path/to/script.py'));
        $this->assertEquals('javascript', Unsandbox::detectLanguage('./src/app.js'));
    }

    public function testDetectWithMultipleDots(): void
    {
        $this->assertEquals('python', Unsandbox::detectLanguage('my.script.test.py'));
        $this->assertEquals('javascript', Unsandbox::detectLanguage('app.min.js'));
    }

    public function testAllSupportedLanguages(): void
    {
        $expected = [
            'py' => 'python',
            'js' => 'javascript',
            'ts' => 'typescript',
            'rb' => 'ruby',
            'php' => 'php',
            'pl' => 'perl',
            'sh' => 'bash',
            'r' => 'r',
            'lua' => 'lua',
            'go' => 'go',
            'rs' => 'rust',
            'c' => 'c',
            'cpp' => 'cpp',
            'cc' => 'cpp',
            'cxx' => 'cpp',
            'java' => 'java',
            'kt' => 'kotlin',
            'm' => 'objc',
            'cs' => 'csharp',
            'fs' => 'fsharp',
            'hs' => 'haskell',
            'ml' => 'ocaml',
            'clj' => 'clojure',
            'scm' => 'scheme',
            'ss' => 'scheme',
            'erl' => 'erlang',
            'ex' => 'elixir',
            'exs' => 'elixir',
            'jl' => 'julia',
            'd' => 'd',
            'nim' => 'nim',
            'zig' => 'zig',
            'v' => 'v',
            'cr' => 'crystal',
            'dart' => 'dart',
            'groovy' => 'groovy',
            'f90' => 'fortran',
            'f95' => 'fortran',
            'lisp' => 'commonlisp',
            'lsp' => 'commonlisp',
            'cob' => 'cobol',
            'tcl' => 'tcl',
            'raku' => 'raku',
            'pro' => 'prolog',
            'p' => 'prolog',
            '4th' => 'forth',
            'forth' => 'forth',
            'fth' => 'forth',
        ];

        foreach ($expected as $ext => $language) {
            $this->assertEquals(
                $language,
                Unsandbox::detectLanguage("test.{$ext}"),
                "Extension .{$ext} should map to {$language}"
            );
        }
    }
}
