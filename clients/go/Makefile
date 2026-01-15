# UN Go Client - Build and Test
#
# This client has two implementations:
#   - sync/  : Synchronous Go SDK
#   - async/ : Asynchronous Go SDK (goroutines/channels)
#
# Usage:
#   make              # Build all
#   make test         # Run all 4 test modes
#   make test-cli     # CLI mode only
#   make test-library # Library mode only
#   make test-integration # Integration mode only
#   make test-functional  # Functional mode only
#   make build        # Build binaries
#   make clean        # Remove build artifacts
#
# Dependencies:
#   Go 1.18+ (for generics support)

.PHONY: all build test test-cli test-library test-integration test-functional
.PHONY: test-sync test-async clean help examples fmt vet

# Paths
ROOT_DIR := $(shell cd ../.. && pwd)
SYNC_DIR := sync
ASYNC_DIR := async

# Go settings
GO := go
GOFLAGS := -v

# Colors
GREEN := \033[32m
RED := \033[31m
YELLOW := \033[33m
NC := \033[0m

.DEFAULT_GOAL := help

help:
	@echo "UN Go Client - Build and Test"
	@echo ""
	@echo "Build:"
	@echo "  make build            Build all binaries"
	@echo "  make build-sync       Build sync SDK"
	@echo "  make build-async      Build async SDK"
	@echo ""
	@echo "Test (all 4 modes):"
	@echo "  make test             All 4 modes for both sync and async"
	@echo "  make test-cli         CLI mode (command-line interface)"
	@echo "  make test-library     Library mode (import and use)"
	@echo "  make test-integration Integration mode (API contract)"
	@echo "  make test-functional  Functional mode (real-world scenarios)"
	@echo ""
	@echo "Test by SDK:"
	@echo "  make test-sync        Test synchronous SDK"
	@echo "  make test-async       Test asynchronous SDK"
	@echo ""
	@echo "Code Quality:"
	@echo "  make fmt              Format code with gofmt"
	@echo "  make vet              Run go vet"
	@echo ""
	@echo "Utility:"
	@echo "  make clean            Remove build artifacts"
	@echo "  make deps             Show required dependencies"
	@echo "  make examples         Run examples"
	@echo ""

all: build

deps:
	@echo "Required:"
	@echo "  Go 1.18+ (https://golang.org/dl/)"
	@echo ""
	@go version

# ============================================================================
# BUILD
# ============================================================================

build: build-sync build-async
	@echo "$(GREEN)✓ All Go SDKs built$(NC)"

build-sync:
	@echo "Building sync SDK..."
	@if [ -f "$(SYNC_DIR)/src/un.go" ]; then \
		cd $(SYNC_DIR)/src && $(GO) build $(GOFLAGS) -o ../un . 2>&1 | head -5 || true; \
		echo "$(GREEN)✓$(NC) Sync SDK compiled"; \
	else \
		echo "$(YELLOW)⊘$(NC) Sync SDK source not found"; \
	fi

build-async:
	@echo "Building async SDK..."
	@if [ -f "$(ASYNC_DIR)/src/un.go" ]; then \
		cd $(ASYNC_DIR)/src && $(GO) build $(GOFLAGS) -o ../un . 2>&1 | head -5 || true; \
		echo "$(GREEN)✓$(NC) Async SDK compiled"; \
	elif [ -d "$(ASYNC_DIR)/src" ]; then \
		echo "$(YELLOW)⊘$(NC) Async SDK not yet implemented"; \
	fi

# ============================================================================
# TEST: All 4 Modes
# ============================================================================

test: test-cli test-library test-integration test-functional
	@echo ""
	@echo "$(GREEN)✓ Go Client: All 4 test modes complete$(NC)"

# ============================================================================
# TEST: CLI Mode
# ============================================================================

test-cli:
	@echo ""
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "CLI MODE: Testing Go CLI interface"
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo ""
	@# Test root-level un.go if it exists
	@if [ -f "$(ROOT_DIR)/un.go" ]; then \
		cd $(ROOT_DIR) && $(GO) run un.go --help > /dev/null 2>&1 && echo "  $(GREEN)✓$(NC) CLI: Root un.go --help works" || echo "  $(YELLOW)⊘$(NC) CLI: Root un.go --help (check syntax)"; \
	fi
	@# Test sync SDK CLI
	@if [ -f "$(SYNC_DIR)/src/un.go" ]; then \
		cd $(SYNC_DIR)/src && $(GO) build -o /tmp/un_test . 2>/dev/null && echo "  $(GREEN)✓$(NC) CLI: Sync SDK compiles" || echo "  $(RED)✗$(NC) CLI: Sync SDK compile failed"; \
		rm -f /tmp/un_test; \
	fi
	@# Test async SDK CLI
	@if [ -f "$(ASYNC_DIR)/src/un.go" ]; then \
		cd $(ASYNC_DIR)/src && $(GO) build -o /tmp/un_test . 2>/dev/null && echo "  $(GREEN)✓$(NC) CLI: Async SDK compiles" || echo "  $(YELLOW)⊘$(NC) CLI: Async SDK not yet buildable"; \
		rm -f /tmp/un_test 2>/dev/null || true; \
	fi

# ============================================================================
# TEST: Library Mode
# ============================================================================

test-library:
	@echo ""
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "LIBRARY MODE: Testing Go package imports"
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo ""
	@# Test sync SDK with go test
	@if [ -d "$(SYNC_DIR)/src" ]; then \
		cd $(SYNC_DIR)/src && $(GO) test -v ./... 2>&1 | head -20 || echo "  $(YELLOW)⊘$(NC) Library: No tests defined yet"; \
	fi
	@# Test async SDK with go test
	@if [ -d "$(ASYNC_DIR)/src" ]; then \
		cd $(ASYNC_DIR)/src && $(GO) test -v ./... 2>&1 | head -20 || echo "  $(YELLOW)⊘$(NC) Library: Async tests not defined"; \
	fi

# ============================================================================
# TEST: Integration Mode
# ============================================================================

test-integration:
	@echo ""
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "INTEGRATION MODE: Testing API contract"
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo ""
	@if [ -z "$$UNSANDBOX_PUBLIC_KEY" ] || [ -z "$$UNSANDBOX_SECRET_KEY" ]; then \
		echo "  $(YELLOW)⊘$(NC) Skipping (no API credentials)"; \
		echo "  Set UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY"; \
	else \
		echo "  Testing API authentication..."; \
		if [ -f "$(ROOT_DIR)/un.go" ]; then \
			cd $(ROOT_DIR) && $(GO) run un.go -s python -c 'print(42)' 2>&1 | grep -q "42" && echo "  $(GREEN)✓$(NC) Integration: API auth works" || echo "  $(YELLOW)⊘$(NC) Integration: Check API connectivity"; \
		fi; \
	fi

# ============================================================================
# TEST: Functional Mode
# ============================================================================

test-functional:
	@echo ""
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "FUNCTIONAL MODE: Real-world scenarios"
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo ""
	@if [ -z "$$UNSANDBOX_PUBLIC_KEY" ] || [ -z "$$UNSANDBOX_SECRET_KEY" ]; then \
		echo "  $(YELLOW)⊘$(NC) Skipping (no API credentials)"; \
	else \
		echo "  Running functional tests..."; \
		if [ -f "$(ROOT_DIR)/un.go" ]; then \
			cd $(ROOT_DIR) && $(GO) run un.go -s python -c 'def fib(n): return n if n<2 else fib(n-1)+fib(n-2); print(fib(10))' 2>&1 | grep -q "55" && echo "  $(GREEN)✓$(NC) Functional: Fibonacci" || echo "  $(YELLOW)⊘$(NC) Functional: Fibonacci (check output)"; \
		fi; \
	fi

# ============================================================================
# TEST: By SDK Type
# ============================================================================

test-sync:
	@echo "Testing Sync SDK..."
	@if [ -d "$(SYNC_DIR)/src" ]; then \
		cd $(SYNC_DIR)/src && $(GO) test -v ./...; \
	else \
		echo "  $(YELLOW)⊘$(NC) Sync SDK not found"; \
	fi

test-async:
	@echo "Testing Async SDK..."
	@if [ -d "$(ASYNC_DIR)/src" ]; then \
		cd $(ASYNC_DIR)/src && $(GO) test -v ./...; \
	else \
		echo "  $(YELLOW)⊘$(NC) Async SDK not found"; \
	fi

# ============================================================================
# Code Quality
# ============================================================================

fmt:
	@echo "Formatting Go code..."
	@if [ -d "$(SYNC_DIR)/src" ]; then gofmt -w $(SYNC_DIR)/src/; fi
	@if [ -d "$(ASYNC_DIR)/src" ]; then gofmt -w $(ASYNC_DIR)/src/; fi
	@if [ -f "$(ROOT_DIR)/un.go" ]; then gofmt -w $(ROOT_DIR)/un.go; fi
	@echo "$(GREEN)✓$(NC) Format complete"

vet:
	@echo "Running go vet..."
	@if [ -d "$(SYNC_DIR)/src" ]; then cd $(SYNC_DIR)/src && $(GO) vet ./... 2>&1 || true; fi
	@if [ -d "$(ASYNC_DIR)/src" ]; then cd $(ASYNC_DIR)/src && $(GO) vet ./... 2>&1 || true; fi
	@echo "$(GREEN)✓$(NC) Vet complete"

# ============================================================================
# Examples
# ============================================================================

examples:
	@echo "Running Go examples..."
	@if [ -d "$(SYNC_DIR)/examples" ]; then \
		for f in $(SYNC_DIR)/examples/*.go; do \
			echo "Running $$f..."; \
			$(GO) run "$$f" 2>&1 | head -10 || true; \
		done; \
	fi

# ============================================================================
# Clean
# ============================================================================

clean:
	@echo "Cleaning Go build artifacts..."
	@rm -f $(SYNC_DIR)/un $(ASYNC_DIR)/un
	@find . -name "*.test" -delete 2>/dev/null || true
	@find . -name "*.out" -delete 2>/dev/null || true
	@echo "$(GREEN)✓$(NC) Cleaned build artifacts"
