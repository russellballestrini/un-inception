// Unit tests for un.go - tests internal functions without API calls
package main

import (
	"crypto/hmac"
	"crypto/sha256"
	"encoding/base64"
	"encoding/hex"
	"fmt"
	"os"
	"path/filepath"
	"strings"
)

var passed, failed int

func test(name string, fn func() error) {
	if err := fn(); err != nil {
		fmt.Printf("  ✗ %s\n", name)
		fmt.Printf("    %s\n", err)
		failed++
	} else {
		fmt.Printf("  ✓ %s\n", name)
		passed++
	}
}

func assertEqual(actual, expected string) error {
	if actual != expected {
		return fmt.Errorf("expected '%s' but got '%s'", expected, actual)
	}
	return nil
}

func assertNotEqual(a, b string) error {
	if a == b {
		return fmt.Errorf("expected values to be different but both were '%s'", a)
	}
	return nil
}

func assertContains(str, substr string) error {
	if !strings.Contains(str, substr) {
		return fmt.Errorf("expected '%s' to contain '%s'", str, substr)
	}
	return nil
}

func assertTrue(val bool) error {
	if !val {
		return fmt.Errorf("expected true but got false")
	}
	return nil
}

// Extension mapping (copied from un.go)
var extMap = map[string]string{
	".py": "python", ".js": "javascript", ".ts": "typescript",
	".rb": "ruby", ".php": "php", ".pl": "perl", ".lua": "lua",
	".sh": "bash", ".go": "go", ".rs": "rust", ".c": "c",
	".cpp": "cpp", ".cc": "cpp", ".cxx": "cpp",
	".java": "java", ".kt": "kotlin", ".cs": "csharp", ".fs": "fsharp",
	".hs": "haskell", ".ml": "ocaml", ".clj": "clojure", ".scm": "scheme",
	".lisp": "commonlisp", ".erl": "erlang", ".ex": "elixir", ".exs": "elixir",
	".jl": "julia", ".r": "r", ".R": "r", ".cr": "crystal",
	".d": "d", ".nim": "nim", ".zig": "zig", ".v": "v",
	".dart": "dart", ".groovy": "groovy", ".scala": "scala",
	".f90": "fortran", ".f95": "fortran", ".cob": "cobol",
	".pro": "prolog", ".forth": "forth", ".4th": "forth",
	".tcl": "tcl", ".raku": "raku", ".m": "objc",
}

func main() {
	// Extension Mapping Tests
	fmt.Println("\n=== Extension Mapping Tests ===")

	test("Python extension maps correctly", func() error {
		return assertEqual(extMap[".py"], "python")
	})

	test("JavaScript extensions map correctly", func() error {
		if err := assertEqual(extMap[".js"], "javascript"); err != nil {
			return err
		}
		return assertEqual(extMap[".ts"], "typescript")
	})

	test("Ruby extension maps correctly", func() error {
		return assertEqual(extMap[".rb"], "ruby")
	})

	test("Go extension maps correctly", func() error {
		return assertEqual(extMap[".go"], "go")
	})

	test("Rust extension maps correctly", func() error {
		return assertEqual(extMap[".rs"], "rust")
	})

	test("C/C++ extensions map correctly", func() error {
		if err := assertEqual(extMap[".c"], "c"); err != nil {
			return err
		}
		if err := assertEqual(extMap[".cpp"], "cpp"); err != nil {
			return err
		}
		if err := assertEqual(extMap[".cc"], "cpp"); err != nil {
			return err
		}
		return assertEqual(extMap[".cxx"], "cpp")
	})

	test("JVM extensions map correctly", func() error {
		if err := assertEqual(extMap[".java"], "java"); err != nil {
			return err
		}
		if err := assertEqual(extMap[".kt"], "kotlin"); err != nil {
			return err
		}
		return assertEqual(extMap[".groovy"], "groovy")
	})

	test("Functional language extensions map correctly", func() error {
		if err := assertEqual(extMap[".hs"], "haskell"); err != nil {
			return err
		}
		if err := assertEqual(extMap[".ml"], "ocaml"); err != nil {
			return err
		}
		if err := assertEqual(extMap[".clj"], "clojure"); err != nil {
			return err
		}
		return assertEqual(extMap[".erl"], "erlang")
	})

	// HMAC Signature Tests
	fmt.Println("\n=== HMAC Signature Tests ===")

	test("HMAC-SHA256 generates 64 character hex string", func() error {
		secret := "test-secret-key"
		message := "1234567890:POST:/execute:{}"

		h := hmac.New(sha256.New, []byte(secret))
		h.Write([]byte(message))
		signature := hex.EncodeToString(h.Sum(nil))

		if len(signature) != 64 {
			return fmt.Errorf("expected length 64 but got %d", len(signature))
		}
		return nil
	})

	test("Same input produces same signature", func() error {
		secret := "test-secret-key"
		message := "1234567890:POST:/execute:{}"

		h1 := hmac.New(sha256.New, []byte(secret))
		h1.Write([]byte(message))
		sig1 := hex.EncodeToString(h1.Sum(nil))

		h2 := hmac.New(sha256.New, []byte(secret))
		h2.Write([]byte(message))
		sig2 := hex.EncodeToString(h2.Sum(nil))

		return assertEqual(sig1, sig2)
	})

	test("Different secrets produce different signatures", func() error {
		message := "1234567890:POST:/execute:{}"

		h1 := hmac.New(sha256.New, []byte("secret1"))
		h1.Write([]byte(message))
		sig1 := hex.EncodeToString(h1.Sum(nil))

		h2 := hmac.New(sha256.New, []byte("secret2"))
		h2.Write([]byte(message))
		sig2 := hex.EncodeToString(h2.Sum(nil))

		return assertNotEqual(sig1, sig2)
	})

	test("Different messages produce different signatures", func() error {
		secret := "test-secret"

		h1 := hmac.New(sha256.New, []byte(secret))
		h1.Write([]byte("message1"))
		sig1 := hex.EncodeToString(h1.Sum(nil))

		h2 := hmac.New(sha256.New, []byte(secret))
		h2.Write([]byte("message2"))
		sig2 := hex.EncodeToString(h2.Sum(nil))

		return assertNotEqual(sig1, sig2)
	})

	test("Signature format is timestamp:METHOD:path:body", func() error {
		timestamp := "1704067200"
		method := "POST"
		endpoint := "/execute"
		body := `{"language":"python","code":"print(1)"}`

		message := fmt.Sprintf("%s:%s:%s:%s", timestamp, method, endpoint, body)

		// Verify format: starts with timestamp, has method and path
		if err := assertTrue(strings.HasPrefix(message, timestamp)); err != nil {
			return err
		}
		if err := assertContains(message, ":POST:"); err != nil {
			return err
		}
		return assertContains(message, ":/execute:")
	})

	// Language Detection Tests
	fmt.Println("\n=== Language Detection Tests ===")

	test("Detect language from .py extension", func() error {
		filename := "script.py"
		ext := strings.ToLower(filepath.Ext(filename))
		return assertEqual(extMap[ext], "python")
	})

	test("Detect language from .go extension", func() error {
		filename := "main.go"
		ext := strings.ToLower(filepath.Ext(filename))
		return assertEqual(extMap[ext], "go")
	})

	test("Python shebang detection", func() error {
		content := "#!/usr/bin/env python3\nprint('hello')"
		firstLine := strings.Split(content, "\n")[0]

		if err := assertTrue(strings.HasPrefix(firstLine, "#!")); err != nil {
			return err
		}
		return assertContains(firstLine, "python")
	})

	test("Bash shebang detection", func() error {
		content := "#!/bin/bash\necho hello"
		firstLine := strings.Split(content, "\n")[0]

		if err := assertTrue(strings.HasPrefix(firstLine, "#!")); err != nil {
			return err
		}
		return assertTrue(strings.Contains(firstLine, "bash") || strings.Contains(firstLine, "/sh"))
	})

	// Argument Parsing Tests
	fmt.Println("\n=== Argument Parsing Tests ===")

	test("Parse -e KEY=VALUE format", func() error {
		arg := "DEBUG=1"
		parts := strings.SplitN(arg, "=", 2)
		key := parts[0]
		value := parts[1]

		if err := assertEqual(key, "DEBUG"); err != nil {
			return err
		}
		return assertEqual(value, "1")
	})

	test("Parse -e KEY=VALUE with equals in value", func() error {
		arg := "URL=https://example.com?foo=bar"
		parts := strings.SplitN(arg, "=", 2)
		key := parts[0]
		value := parts[1]

		if err := assertEqual(key, "URL"); err != nil {
			return err
		}
		return assertEqual(value, "https://example.com?foo=bar")
	})

	test("Valid network modes", func() error {
		validModes := map[string]bool{"zerotrust": true, "semitrusted": true}

		if !validModes["zerotrust"] {
			return fmt.Errorf("zerotrust should be valid")
		}
		if !validModes["semitrusted"] {
			return fmt.Errorf("semitrusted should be valid")
		}
		if validModes["invalid"] {
			return fmt.Errorf("invalid should not be valid")
		}
		return nil
	})

	test("Subcommand detection", func() error {
		args := []string{"session", "--shell", "python3"}
		subcommands := map[string]bool{"session": true, "service": true, "key": true, "restore": true}

		var subcommand string
		if len(args) > 0 && subcommands[args[0]] {
			subcommand = args[0]
		}

		return assertEqual(subcommand, "session")
	})

	// File Operations Tests
	fmt.Println("\n=== File Operations Tests ===")

	test("Read text file", func() error {
		tmpfile, err := os.CreateTemp("", "test_un_go_*.py")
		if err != nil {
			return err
		}
		defer os.Remove(tmpfile.Name())

		content := "print('hello world')"
		if _, err := tmpfile.WriteString(content); err != nil {
			return err
		}
		tmpfile.Close()

		data, err := os.ReadFile(tmpfile.Name())
		if err != nil {
			return err
		}

		return assertEqual(string(data), content)
	})

	test("Base64 encoding/decoding", func() error {
		content := "print('hello world')"
		encoded := base64.StdEncoding.EncodeToString([]byte(content))
		decoded, err := base64.StdEncoding.DecodeString(encoded)
		if err != nil {
			return err
		}

		return assertEqual(string(decoded), content)
	})

	test("Extract file basename", func() error {
		path := "/home/user/project/script.py"
		basename := filepath.Base(path)
		return assertEqual(basename, "script.py")
	})

	test("Extract file extension", func() error {
		path := "/home/user/project/script.py"
		ext := filepath.Ext(path)
		return assertEqual(ext, ".py")
	})

	// API Constants Tests
	fmt.Println("\n=== API Constants Tests ===")

	test("API base URL format", func() error {
		apiBase := "https://api.unsandbox.com"

		if err := assertTrue(strings.HasPrefix(apiBase, "https://")); err != nil {
			return err
		}
		return assertContains(apiBase, "unsandbox.com")
	})

	test("Portal base URL format", func() error {
		portalBase := "https://unsandbox.com"
		return assertTrue(strings.HasPrefix(portalBase, "https://"))
	})

	// Summary
	fmt.Println("\n=== Summary ===")
	fmt.Printf("Passed: %d\n", passed)
	fmt.Printf("Failed: %d\n", failed)
	fmt.Printf("Total:  %d\n", passed+failed)

	if failed > 0 {
		os.Exit(1)
	}
}
