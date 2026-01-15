/**
 * Tests for language detection from filenames
 */

import { detectLanguage } from '../src/un_async.js';

describe('detectLanguage', () => {
  describe('common languages', () => {
    test('detects Python', () => {
      expect(detectLanguage('script.py')).toBe('python');
      expect(detectLanguage('main.py')).toBe('python');
    });

    test('detects JavaScript', () => {
      expect(detectLanguage('app.js')).toBe('javascript');
      expect(detectLanguage('index.js')).toBe('javascript');
    });

    test('detects TypeScript', () => {
      expect(detectLanguage('app.ts')).toBe('typescript');
      expect(detectLanguage('index.ts')).toBe('typescript');
    });

    test('detects Go', () => {
      expect(detectLanguage('main.go')).toBe('go');
    });

    test('detects Rust', () => {
      expect(detectLanguage('main.rs')).toBe('rust');
      expect(detectLanguage('lib.rs')).toBe('rust');
    });

    test('detects Ruby', () => {
      expect(detectLanguage('app.rb')).toBe('ruby');
    });

    test('detects Java', () => {
      expect(detectLanguage('Main.java')).toBe('java');
    });

    test('detects C', () => {
      expect(detectLanguage('main.c')).toBe('c');
    });

    test('detects C++', () => {
      expect(detectLanguage('main.cpp')).toBe('cpp');
      expect(detectLanguage('main.cc')).toBe('cpp');
      expect(detectLanguage('main.cxx')).toBe('cpp');
    });
  });

  describe('scripting languages', () => {
    test('detects Bash', () => {
      expect(detectLanguage('script.sh')).toBe('bash');
    });

    test('detects PHP', () => {
      expect(detectLanguage('index.php')).toBe('php');
    });

    test('detects Perl', () => {
      expect(detectLanguage('script.pl')).toBe('perl');
    });

    test('detects Lua', () => {
      expect(detectLanguage('script.lua')).toBe('lua');
    });

    test('detects R', () => {
      expect(detectLanguage('analysis.r')).toBe('r');
    });
  });

  describe('functional languages', () => {
    test('detects Haskell', () => {
      expect(detectLanguage('Main.hs')).toBe('haskell');
    });

    test('detects OCaml', () => {
      expect(detectLanguage('main.ml')).toBe('ocaml');
    });

    test('detects Clojure', () => {
      expect(detectLanguage('core.clj')).toBe('clojure');
    });

    test('detects Scheme', () => {
      expect(detectLanguage('script.scm')).toBe('scheme');
      expect(detectLanguage('script.ss')).toBe('scheme');
    });

    test('detects Elixir', () => {
      expect(detectLanguage('app.ex')).toBe('elixir');
      expect(detectLanguage('script.exs')).toBe('elixir');
    });

    test('detects Erlang', () => {
      expect(detectLanguage('module.erl')).toBe('erlang');
    });
  });

  describe('modern languages', () => {
    test('detects Kotlin', () => {
      expect(detectLanguage('Main.kt')).toBe('kotlin');
    });

    test('detects Swift via Objective-C extension', () => {
      expect(detectLanguage('ViewController.m')).toBe('objc');
    });

    test('detects C#', () => {
      expect(detectLanguage('Program.cs')).toBe('csharp');
    });

    test('detects F#', () => {
      expect(detectLanguage('Program.fs')).toBe('fsharp');
    });

    test('detects Dart', () => {
      expect(detectLanguage('main.dart')).toBe('dart');
    });

    test('detects Julia', () => {
      expect(detectLanguage('script.jl')).toBe('julia');
    });

    test('detects Nim', () => {
      expect(detectLanguage('main.nim')).toBe('nim');
    });

    test('detects Zig', () => {
      expect(detectLanguage('main.zig')).toBe('zig');
    });

    test('detects V', () => {
      expect(detectLanguage('main.v')).toBe('v');
    });

    test('detects Crystal', () => {
      expect(detectLanguage('app.cr')).toBe('crystal');
    });
  });

  describe('other languages', () => {
    test('detects D', () => {
      expect(detectLanguage('main.d')).toBe('d');
    });

    test('detects Groovy', () => {
      expect(detectLanguage('script.groovy')).toBe('groovy');
    });

    test('detects Fortran', () => {
      expect(detectLanguage('program.f90')).toBe('fortran');
      expect(detectLanguage('program.f95')).toBe('fortran');
    });

    test('detects Common Lisp', () => {
      expect(detectLanguage('app.lisp')).toBe('commonlisp');
      expect(detectLanguage('app.lsp')).toBe('commonlisp');
    });

    test('detects COBOL', () => {
      expect(detectLanguage('program.cob')).toBe('cobol');
    });

    test('detects Tcl', () => {
      expect(detectLanguage('script.tcl')).toBe('tcl');
    });

    test('detects Raku', () => {
      expect(detectLanguage('script.raku')).toBe('raku');
    });

    test('detects Prolog', () => {
      expect(detectLanguage('rules.pro')).toBe('prolog');
      expect(detectLanguage('rules.p')).toBe('prolog');
    });

    test('detects Forth', () => {
      expect(detectLanguage('program.4th')).toBe('forth');
      expect(detectLanguage('program.forth')).toBe('forth');
      expect(detectLanguage('program.fth')).toBe('forth');
    });
  });

  describe('edge cases', () => {
    test('returns null for files without extension', () => {
      expect(detectLanguage('Makefile')).toBeNull();
      expect(detectLanguage('README')).toBeNull();
      expect(detectLanguage('Dockerfile')).toBeNull();
    });

    test('returns null for unknown extensions', () => {
      expect(detectLanguage('data.xyz')).toBeNull();
      expect(detectLanguage('config.unknown')).toBeNull();
    });

    test('returns null for null/undefined input', () => {
      expect(detectLanguage(null)).toBeNull();
      expect(detectLanguage(undefined)).toBeNull();
    });

    test('returns null for empty string', () => {
      expect(detectLanguage('')).toBeNull();
    });

    test('handles multiple dots in filename', () => {
      expect(detectLanguage('app.test.py')).toBe('python');
      expect(detectLanguage('my.script.js')).toBe('javascript');
    });

    test('is case-insensitive for extensions', () => {
      expect(detectLanguage('script.PY')).toBe('python');
      expect(detectLanguage('app.JS')).toBe('javascript');
      expect(detectLanguage('main.Go')).toBe('go');
    });
  });
});
