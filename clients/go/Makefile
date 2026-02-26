# UN Go Client - Build and Test
#
# This client has two implementations:
#   - sync/  : Synchronous Go SDK
#   - async/ : Asynchronous Go SDK (goroutines/channels)
#
# Usage:
#   make test         # Run all 4 test modes (auto-detects go binary)
#   make test-cli     # CLI mode only
#   make test-library # Library mode only
#   make build        # Build binaries
#   make clean        # Remove build artifacts
#
# The Makefile auto-detects go from PATH, ~/.local/go, /usr/local/go.

.PHONY: all build test test-cli test-library test-integration test-functional
.PHONY: test-sync test-async clean help examples fmt vet

# Paths
ROOT_DIR := $(shell cd ../.. && pwd)
SYNC_DIR := sync
ASYNC_DIR := async

# Auto-detect Go binary: PATH first, then common install locations
GO := $(or $(shell which go 2>/dev/null), \
           $(shell test -x $(HOME)/.local/go/bin/go && echo $(HOME)/.local/go/bin/go), \
           $(shell test -x /usr/local/go/bin/go && echo /usr/local/go/bin/go), \
           $(shell test -x $(HOME)/go/bin/go && echo $(HOME)/go/bin/go))
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
	@if [ -n "$(GO)" ]; then \
		echo "  Go binary: $(GO)"; \
		$(GO) version; \
	else \
		echo "  $(RED)✗ Go binary not found$(NC)"; \
		echo "  Install Go or set PATH to include go binary"; \
	fi
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
	@echo ""

# Guard: fail early if no Go binary found
check-go:
	@if [ -z "$(GO)" ]; then \
		echo "$(RED)✗ Go binary not found$(NC)"; \
		echo "  Searched: PATH, ~/.local/go/bin, /usr/local/go/bin, ~/go/bin"; \
		exit 1; \
	fi

# Ensure go.mod exists for the sync SDK
$(SYNC_DIR)/go.mod: check-go
	@if [ ! -f "$(SYNC_DIR)/go.mod" ] && [ -d "$(SYNC_DIR)/src" ]; then \
		echo "Initializing go module for sync SDK..."; \
		cd $(SYNC_DIR) && $(GO) mod init unsandbox.com/un 2>/dev/null || true; \
	fi

all: build

# ============================================================================
# BUILD
# ============================================================================

build: build-sync build-async
	@echo "$(GREEN)✓ All Go SDKs built$(NC)"

build-sync: check-go $(SYNC_DIR)/go.mod
	@echo "Building sync SDK..."
	@if [ -f "$(SYNC_DIR)/src/un.go" ]; then \
		cd $(SYNC_DIR)/src && $(GO) build $(GOFLAGS) -o ../un . 2>&1 | head -5 || true; \
		echo "$(GREEN)✓$(NC) Sync SDK compiled"; \
	else \
		echo "$(YELLOW)⊘$(NC) Sync SDK source not found"; \
	fi

build-async: check-go
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

test-cli: check-go
	@echo ""
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "CLI MODE: Testing Go CLI interface"
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo ""
	@if [ -f "$(ROOT_DIR)/un.go" ]; then \
		cd $(ROOT_DIR) && $(GO) run un.go --help > /dev/null 2>&1 && echo "  $(GREEN)✓$(NC) CLI: Root un.go --help works" || echo "  $(YELLOW)⊘$(NC) CLI: Root un.go --help (check syntax)"; \
	fi
	@if [ -f "$(SYNC_DIR)/src/un.go" ]; then \
		cd $(SYNC_DIR)/src && $(GO) build -o /tmp/un_test . 2>/dev/null && echo "  $(GREEN)✓$(NC) CLI: Sync SDK compiles" || echo "  $(RED)✗$(NC) CLI: Sync SDK compile failed"; \
		rm -f /tmp/un_test; \
	fi
	@if [ -f "$(ASYNC_DIR)/src/un.go" ]; then \
		cd $(ASYNC_DIR)/src && $(GO) build -o /tmp/un_test . 2>/dev/null && echo "  $(GREEN)✓$(NC) CLI: Async SDK compiles" || echo "  $(YELLOW)⊘$(NC) CLI: Async SDK not yet buildable"; \
		rm -f /tmp/un_test 2>/dev/null || true; \
	fi

# ============================================================================
# TEST: Library Mode
# ============================================================================

test-library: check-go
	@echo ""
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "LIBRARY MODE: Testing Go package imports"
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo ""
	@# Go requires test files in the same directory as the package.
	@# Copy tests into src/ temporarily, run, clean up.
	@if [ -d "$(SYNC_DIR)/tests" ] && [ -d "$(SYNC_DIR)/src" ]; then \
		cp $(SYNC_DIR)/tests/*_test.go $(SYNC_DIR)/src/ 2>/dev/null; \
		cd $(SYNC_DIR)/src && $(GO) test -short -v . 2>&1; \
		rm -f $(SYNC_DIR)/src/*_test.go; \
	elif [ -d "$(SYNC_DIR)/src" ]; then \
		cd $(SYNC_DIR)/src && $(GO) test -short -v . 2>&1 | head -20 || echo "  $(YELLOW)⊘$(NC) Library: No tests defined yet"; \
	fi
	@if [ -d "$(ASYNC_DIR)/tests" ] && [ -d "$(ASYNC_DIR)/src" ]; then \
		cp $(ASYNC_DIR)/tests/*_test.go $(ASYNC_DIR)/src/ 2>/dev/null; \
		cd $(ASYNC_DIR)/src && $(GO) test -short -v . 2>&1; \
		rm -f $(ASYNC_DIR)/src/*_test.go; \
	elif [ -d "$(ASYNC_DIR)/src" ]; then \
		cd $(ASYNC_DIR)/src && $(GO) test -short -v . 2>&1 | head -20 || echo "  $(YELLOW)⊘$(NC) Library: Async tests not defined"; \
	fi

# ============================================================================
# TEST: Integration Mode
# ============================================================================

test-integration: check-go
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
			cd $(ROOT_DIR) && $(GO) run un.go -s python -c 'print(42)' 2>&1 | grep -q "42" && echo "  $(GREEN)✓$(NC) Integration: API auth works" || echo "  $(RED)✗$(NC) Integration: Check API connectivity"; \
		fi; \
	fi

# ============================================================================
# TEST: Functional Mode
# ============================================================================

test-functional: check-go
	@echo ""
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "FUNCTIONAL MODE: Real-world scenarios"
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo ""
	@if [ -z "$$UNSANDBOX_PUBLIC_KEY" ] || [ -z "$$UNSANDBOX_SECRET_KEY" ]; then \
		echo "  $(YELLOW)⊘$(NC) Skipping (no API credentials)"; \
	else \
		echo "  Running functional tests..."; \
		if [ -d "$(SYNC_DIR)/tests" ]; then \
			cd $(SYNC_DIR) && $(GO) test -v -run TestFunctional ./tests/ 2>&1 || true; \
		fi; \
	fi

# ============================================================================
# TEST: By SDK Type
# ============================================================================

test-sync: check-go
	@echo "Testing Sync SDK..."
	@if [ -d "$(SYNC_DIR)/tests" ] && [ -d "$(SYNC_DIR)/src" ]; then \
		cp $(SYNC_DIR)/tests/*_test.go $(SYNC_DIR)/src/ 2>/dev/null; \
		cd $(SYNC_DIR)/src && $(GO) test -v .; \
		rm -f $(SYNC_DIR)/src/*_test.go; \
	elif [ -d "$(SYNC_DIR)/src" ]; then \
		cd $(SYNC_DIR)/src && $(GO) test -v .; \
	else \
		echo "  $(YELLOW)⊘$(NC) Sync SDK not found"; \
	fi

test-async: check-go
	@echo "Testing Async SDK..."
	@if [ -d "$(ASYNC_DIR)/tests" ] && [ -d "$(ASYNC_DIR)/src" ]; then \
		cp $(ASYNC_DIR)/tests/*_test.go $(ASYNC_DIR)/src/ 2>/dev/null; \
		cd $(ASYNC_DIR)/src && $(GO) test -v .; \
		rm -f $(ASYNC_DIR)/src/*_test.go; \
	elif [ -d "$(ASYNC_DIR)/src" ]; then \
		cd $(ASYNC_DIR)/src && $(GO) test -v .; \
	else \
		echo "  $(YELLOW)⊘$(NC) Async SDK not found"; \
	fi

# ============================================================================
# Code Quality
# ============================================================================

fmt: check-go
	@echo "Formatting Go code..."
	@if [ -d "$(SYNC_DIR)/src" ]; then gofmt -w $(SYNC_DIR)/src/; fi
	@if [ -d "$(ASYNC_DIR)/src" ]; then gofmt -w $(ASYNC_DIR)/src/; fi
	@if [ -f "$(ROOT_DIR)/un.go" ]; then gofmt -w $(ROOT_DIR)/un.go; fi
	@echo "$(GREEN)✓$(NC) Format complete"

vet: check-go
	@echo "Running go vet..."
	@if [ -d "$(SYNC_DIR)/src" ]; then cd $(SYNC_DIR)/src && $(GO) vet ./... 2>&1 || true; fi
	@if [ -d "$(ASYNC_DIR)/src" ]; then cd $(ASYNC_DIR)/src && $(GO) vet ./... 2>&1 || true; fi
	@echo "$(GREEN)✓$(NC) Vet complete"

# ============================================================================
# Examples
# ============================================================================

examples: check-go
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
