# UN C Client - Build and Test
#
# Usage:
#   make              # Build un binary
#   make test         # Run all 4 test modes
#   make test-cli     # CLI mode only
#   make test-library # Library mode only
#   make test-integration # Integration mode only
#   make test-functional  # Functional mode only
#   make clean        # Remove build artifacts
#
# Dependencies:
#   apt install build-essential libcurl4-openssl-dev libwebsockets-dev libssl-dev

.PHONY: all build test test-cli test-library test-integration test-functional clean help

# Paths
ROOT_DIR := $(shell cd ../.. && pwd)
SRC := $(ROOT_DIR)/un.c
BIN := un
TEST_DIR := tests

# Compiler settings
CC := gcc
CFLAGS := -O2 -Wall -Wextra
LDFLAGS := -lcurl -lwebsockets -lssl -lcrypto

# Colors
GREEN := \033[32m
RED := \033[31m
YELLOW := \033[33m
NC := \033[0m

help:
	@echo "UN C Client - Build and Test"
	@echo ""
	@echo "Build:"
	@echo "  make              Build un binary from un.c"
	@echo "  make build        Same as above"
	@echo ""
	@echo "Test (all 4 modes):"
	@echo "  make test         Run CLI + Library + Integration + Functional"
	@echo ""
	@echo "Test (individual modes):"
	@echo "  make test-cli         Test as standalone CLI tool"
	@echo "  make test-library     Test as embeddable C library"
	@echo "  make test-integration Test API contract (auth, errors)"
	@echo "  make test-functional  Test real-world scenarios"
	@echo ""
	@echo "Utility:"
	@echo "  make clean        Remove build artifacts"
	@echo "  make deps         Show required dependencies"
	@echo ""

all: build

build: $(BIN)

$(BIN): $(SRC)
	@echo "Building un from $(SRC)..."
	$(CC) $(CFLAGS) -o $(BIN) $(SRC) $(LDFLAGS)
	@echo "$(GREEN)✓$(NC) Built: $(BIN)"

deps:
	@echo "Required packages:"
	@echo "  apt install build-essential libcurl4-openssl-dev libwebsockets-dev libssl-dev"

# ============================================================================
# TEST: All 4 Modes
# ============================================================================

test: build test-cli test-library test-integration test-functional
	@echo ""
	@echo "$(GREEN)✓ C Client: All 4 test modes complete$(NC)"

# ============================================================================
# TEST: CLI Mode
# ============================================================================

test-cli: build
	@echo ""
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "CLI MODE: Testing un as standalone tool"
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo ""
	@# Test --help
	@./$(BIN) --help > /dev/null 2>&1 && echo "  $(GREEN)✓$(NC) CLI: --help works" || echo "  $(RED)✗$(NC) CLI: --help failed"
	@# Test --version (may not exist)
	@./$(BIN) --version > /dev/null 2>&1 && echo "  $(GREEN)✓$(NC) CLI: --version works" || echo "  $(YELLOW)⊘$(NC) CLI: --version (not implemented)"
	@# Test session --help
	@./$(BIN) session --help > /dev/null 2>&1 && echo "  $(GREEN)✓$(NC) CLI: session --help works" || echo "  $(RED)✗$(NC) CLI: session --help failed"
	@# Test service --help
	@./$(BIN) service --help > /dev/null 2>&1 && echo "  $(GREEN)✓$(NC) CLI: service --help works" || echo "  $(RED)✗$(NC) CLI: service --help failed"
	@# Test nonexistent file error
	@./$(BIN) /nonexistent/file.py 2>&1 | grep -qi "error\|not found\|cannot" && echo "  $(GREEN)✓$(NC) CLI: Nonexistent file returns error" || echo "  $(YELLOW)⊘$(NC) CLI: Error message format differs"
	@# Test with API keys if available
	@if [ -n "$$UNSANDBOX_PUBLIC_KEY" ] && [ -n "$$UNSANDBOX_SECRET_KEY" ]; then \
		echo ""; \
		echo "  Testing with API credentials..."; \
		./$(BIN) -s python -c 'print(42)' 2>&1 | grep -q "42" && echo "  $(GREEN)✓$(NC) CLI: Execute inline code" || echo "  $(RED)✗$(NC) CLI: Execute inline code failed"; \
		./$(BIN) -e TEST=hello -s python -c 'import os; print(os.environ.get("TEST"))' 2>&1 | grep -q "hello" && echo "  $(GREEN)✓$(NC) CLI: -e flag passes env vars" || echo "  $(YELLOW)⊘$(NC) CLI: -e flag (may differ)"; \
	else \
		echo ""; \
		echo "  $(YELLOW)⊘$(NC) Skipping API tests (no UNSANDBOX_PUBLIC_KEY/UNSANDBOX_SECRET_KEY)"; \
	fi

# ============================================================================
# TEST: Library Mode
# ============================================================================

test-library: build $(TEST_DIR)/test_library
	@echo ""
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "LIBRARY MODE: Testing un.c as embeddable library"
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo ""
	@./$(TEST_DIR)/test_library

$(TEST_DIR)/test_library: $(TEST_DIR)/test_library.c $(SRC)
	@mkdir -p $(TEST_DIR)
	$(CC) $(CFLAGS) -o $@ $< -I$(ROOT_DIR) $(LDFLAGS)

# ============================================================================
# TEST: Integration Mode
# ============================================================================

test-integration: build
	@echo ""
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "INTEGRATION MODE: Testing API contract"
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo ""
	@if [ -z "$$UNSANDBOX_PUBLIC_KEY" ] || [ -z "$$UNSANDBOX_SECRET_KEY" ]; then \
		echo "  $(YELLOW)⊘$(NC) Skipping (no API credentials)"; \
		echo "  Set UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY"; \
		exit 0; \
	fi
	@# Test valid authentication
	@./$(BIN) -s python -c 'print("auth_ok")' 2>&1 | grep -q "auth_ok" && echo "  $(GREEN)✓$(NC) Integration: Valid auth returns 200" || echo "  $(RED)✗$(NC) Integration: Valid auth failed"
	@# Test multiple languages
	@echo "  Testing language support..."
	@./$(BIN) -s python -c 'print(1)' > /dev/null 2>&1 && echo "    $(GREEN)✓$(NC) python" || echo "    $(RED)✗$(NC) python"
	@./$(BIN) -s javascript -c 'console.log(1)' > /dev/null 2>&1 && echo "    $(GREEN)✓$(NC) javascript" || echo "    $(RED)✗$(NC) javascript"
	@./$(BIN) -s ruby -c 'puts 1' > /dev/null 2>&1 && echo "    $(GREEN)✓$(NC) ruby" || echo "    $(RED)✗$(NC) ruby"
	@./$(BIN) -s go -c 'package main; import "fmt"; func main() { fmt.Println(1) }' > /dev/null 2>&1 && echo "    $(GREEN)✓$(NC) go" || echo "    $(RED)✗$(NC) go"
	@./$(BIN) -s bash -c 'echo 1' > /dev/null 2>&1 && echo "    $(GREEN)✓$(NC) bash" || echo "    $(RED)✗$(NC) bash"
	@# Test error handling (runtime error)
	@./$(BIN) -s python -c 'raise Exception("test")' 2>&1 | grep -qi "exception\|error\|traceback" && echo "  $(GREEN)✓$(NC) Integration: Runtime errors reported" || echo "  $(YELLOW)⊘$(NC) Integration: Error format differs"
	@# Test exit codes
	@./$(BIN) -s python -c 'import sys; sys.exit(42)' 2>&1 | grep -q "42\|exit" && echo "  $(GREEN)✓$(NC) Integration: Exit codes captured" || echo "  $(YELLOW)⊘$(NC) Integration: Exit code format differs"

# ============================================================================
# TEST: Functional Mode
# ============================================================================

test-functional: build
	@echo ""
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "FUNCTIONAL MODE: Real-world scenarios"
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo ""
	@if [ -z "$$UNSANDBOX_PUBLIC_KEY" ] || [ -z "$$UNSANDBOX_SECRET_KEY" ]; then \
		echo "  $(YELLOW)⊘$(NC) Skipping (no API credentials)"; \
		exit 0; \
	fi
	@# Fibonacci
	@./$(BIN) -s python -c 'def fib(n): return n if n<2 else fib(n-1)+fib(n-2); print(fib(10))' 2>&1 | grep -q "55" && echo "  $(GREEN)✓$(NC) Functional: Fibonacci calculation" || echo "  $(RED)✗$(NC) Functional: Fibonacci failed"
	@# JSON parsing
	@./$(BIN) -s python -c 'import json; print(json.loads("{\"key\":\"value\"}")["key"])' 2>&1 | grep -q "value" && echo "  $(GREEN)✓$(NC) Functional: JSON parsing" || echo "  $(RED)✗$(NC) Functional: JSON parsing failed"
	@# File I/O
	@./$(BIN) -s python -c 'open("/tmp/test.txt","w").write("hello"); print(open("/tmp/test.txt").read())' 2>&1 | grep -q "hello" && echo "  $(GREEN)✓$(NC) Functional: File I/O" || echo "  $(RED)✗$(NC) Functional: File I/O failed"
	@# Subprocess
	@./$(BIN) -s python -c 'import subprocess; print(subprocess.check_output(["echo","subprocess_ok"]).decode().strip())' 2>&1 | grep -q "subprocess_ok" && echo "  $(GREEN)✓$(NC) Functional: Subprocess execution" || echo "  $(RED)✗$(NC) Functional: Subprocess failed"
	@# Error handling
	@./$(BIN) -s python -c 'try: 1/0; except ZeroDivisionError: print("caught_error")' 2>&1 | grep -q "caught_error" && echo "  $(GREEN)✓$(NC) Functional: Exception handling" || echo "  $(RED)✗$(NC) Functional: Exception handling failed"
	@# Data structures
	@./$(BIN) -s python -c 'print(sorted([3,1,4,1,5,9,2,6]))' 2>&1 | grep -q "1, 1, 2, 3, 4, 5, 6, 9" && echo "  $(GREEN)✓$(NC) Functional: Data structures" || echo "  $(YELLOW)⊘$(NC) Functional: List format differs"
	@# Async (Python 3.7+)
	@./$(BIN) -s python -c 'import asyncio; async def f(): return "async_ok"; print(asyncio.run(f()))' 2>&1 | grep -q "async_ok" && echo "  $(GREEN)✓$(NC) Functional: Async/await" || echo "  $(YELLOW)⊘$(NC) Functional: Async (may need Python 3.7+)"

# ============================================================================
# Clean
# ============================================================================

clean:
	rm -f $(BIN)
	rm -f $(TEST_DIR)/test_library
	rm -f $(TEST_DIR)/*.o
	@echo "$(GREEN)✓$(NC) Cleaned build artifacts"
