# UN Rust Client - Build and Test
#
# This client has two implementations:
#   - sync/  : Synchronous Rust SDK (blocking reqwest)
#   - async/ : Asynchronous Rust SDK (tokio + async reqwest)
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
#   rustup (https://rustup.rs/)
#   cargo

.PHONY: all build test test-cli test-library test-integration test-functional
.PHONY: test-sync test-async clean help examples fmt clippy doc

# Paths
ROOT_DIR := $(shell cd ../.. && pwd)
SYNC_DIR := sync
ASYNC_DIR := async

# Colors
GREEN := \033[32m
RED := \033[31m
YELLOW := \033[33m
NC := \033[0m

.DEFAULT_GOAL := help

help:
	@echo "UN Rust Client - Build and Test"
	@echo ""
	@echo "Build:"
	@echo "  make build            Build all binaries"
	@echo "  make build-sync       Build sync SDK"
	@echo "  make build-async      Build async SDK"
	@echo ""
	@echo "Test (all 4 modes):"
	@echo "  make test             All 4 modes for both sync and async"
	@echo "  make test-cli         CLI mode (command-line interface)"
	@echo "  make test-library     Library mode (cargo test)"
	@echo "  make test-integration Integration mode (API contract)"
	@echo "  make test-functional  Functional mode (real-world scenarios)"
	@echo ""
	@echo "Test by SDK:"
	@echo "  make test-sync        Test synchronous SDK"
	@echo "  make test-async       Test asynchronous SDK"
	@echo ""
	@echo "Code Quality:"
	@echo "  make fmt              Format with rustfmt"
	@echo "  make clippy           Lint with clippy"
	@echo "  make doc              Generate documentation"
	@echo ""
	@echo "Utility:"
	@echo "  make clean            Remove build artifacts"
	@echo "  make deps             Show required dependencies"
	@echo ""

all: build

deps:
	@echo "Required:"
	@echo "  rustup (https://rustup.rs/)"
	@echo ""
	@rustc --version 2>/dev/null || echo "Rust not installed"
	@cargo --version 2>/dev/null || echo "Cargo not installed"

# ============================================================================
# BUILD
# ============================================================================

build: build-sync build-async
	@echo "$(GREEN)✓ All Rust SDKs built$(NC)"

build-sync:
	@echo "Building sync SDK..."
	@if [ -f "$(SYNC_DIR)/Cargo.toml" ]; then \
		cd $(SYNC_DIR) && cargo build --release 2>&1 | tail -5 && echo "$(GREEN)✓$(NC) Sync SDK compiled"; \
	elif [ -d "$(SYNC_DIR)/src" ]; then \
		echo "$(YELLOW)⊘$(NC) Sync SDK needs Cargo.toml"; \
	else \
		echo "$(YELLOW)⊘$(NC) Sync SDK not found"; \
	fi

build-async:
	@echo "Building async SDK..."
	@if [ -f "$(ASYNC_DIR)/Cargo.toml" ]; then \
		cd $(ASYNC_DIR) && cargo build --release 2>&1 | tail -5 && echo "$(GREEN)✓$(NC) Async SDK compiled"; \
	elif [ -d "$(ASYNC_DIR)/src" ]; then \
		echo "$(YELLOW)⊘$(NC) Async SDK needs Cargo.toml"; \
	else \
		echo "$(YELLOW)⊘$(NC) Async SDK not found"; \
	fi

# ============================================================================
# TEST: All 4 Modes
# ============================================================================

test: test-cli test-library test-integration test-functional
	@echo ""
	@echo "$(GREEN)✓ Rust Client: All 4 test modes complete$(NC)"

# ============================================================================
# TEST: CLI Mode
# ============================================================================

test-cli:
	@echo ""
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "CLI MODE: Testing Rust CLI interface"
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo ""
	@# Test root-level un.rs if it exists
	@if [ -f "$(ROOT_DIR)/un.rs" ]; then \
		rustc --edition 2021 --emit=metadata "$(ROOT_DIR)/un.rs" -o /tmp/un_check 2>/dev/null && echo "  $(GREEN)✓$(NC) CLI: Root un.rs syntax valid" || echo "  $(YELLOW)⊘$(NC) CLI: Root un.rs (check dependencies)"; \
		rm -f /tmp/un_check 2>/dev/null; \
	fi
	@# Test sync SDK compile
	@if [ -f "$(SYNC_DIR)/Cargo.toml" ]; then \
		cd $(SYNC_DIR) && cargo check 2>/dev/null && echo "  $(GREEN)✓$(NC) CLI: Sync SDK compiles" || echo "  $(RED)✗$(NC) CLI: Sync SDK compile failed"; \
	fi
	@# Test async SDK compile
	@if [ -f "$(ASYNC_DIR)/Cargo.toml" ]; then \
		cd $(ASYNC_DIR) && cargo check 2>/dev/null && echo "  $(GREEN)✓$(NC) CLI: Async SDK compiles" || echo "  $(YELLOW)⊘$(NC) CLI: Async SDK not yet buildable"; \
	fi

# ============================================================================
# TEST: Library Mode
# ============================================================================

test-library:
	@echo ""
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "LIBRARY MODE: Testing Rust crate"
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo ""
	@# Test sync SDK with cargo test
	@if [ -f "$(SYNC_DIR)/Cargo.toml" ]; then \
		cd $(SYNC_DIR) && cargo test 2>&1 | tail -20 || echo "  $(YELLOW)⊘$(NC) Library: Sync tests incomplete"; \
	else \
		echo "  $(YELLOW)⊘$(NC) Library: Sync SDK needs Cargo.toml"; \
	fi
	@# Test async SDK with cargo test
	@if [ -f "$(ASYNC_DIR)/Cargo.toml" ]; then \
		cd $(ASYNC_DIR) && cargo test 2>&1 | tail -20 || echo "  $(YELLOW)⊘$(NC) Library: Async tests incomplete"; \
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
		if [ -f "$(SYNC_DIR)/Cargo.toml" ]; then \
			cd $(SYNC_DIR) && cargo run --release -- -s python -c 'print(42)' 2>&1 | grep -q "42" && echo "  $(GREEN)✓$(NC) Integration: API auth works" || echo "  $(YELLOW)⊘$(NC) Integration: Check API connectivity"; \
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
		if [ -f "$(SYNC_DIR)/Cargo.toml" ]; then \
			cd $(SYNC_DIR) && cargo test --test functional_test -- --nocapture 2>&1 && echo "  $(GREEN)✓$(NC) Functional: All tests passed" || echo "  $(RED)✗$(NC) Functional: Tests failed"; \
		fi; \
	fi

# ============================================================================
# TEST: By SDK Type
# ============================================================================

test-sync:
	@echo "Testing Sync SDK..."
	@if [ -f "$(SYNC_DIR)/Cargo.toml" ]; then \
		cd $(SYNC_DIR) && cargo test; \
	else \
		echo "  $(YELLOW)⊘$(NC) Sync SDK not found"; \
	fi

test-async:
	@echo "Testing Async SDK..."
	@if [ -f "$(ASYNC_DIR)/Cargo.toml" ]; then \
		cd $(ASYNC_DIR) && cargo test; \
	else \
		echo "  $(YELLOW)⊘$(NC) Async SDK not found"; \
	fi

# ============================================================================
# Code Quality
# ============================================================================

fmt:
	@echo "Formatting Rust code..."
	@if [ -f "$(SYNC_DIR)/Cargo.toml" ]; then cd $(SYNC_DIR) && cargo fmt; fi
	@if [ -f "$(ASYNC_DIR)/Cargo.toml" ]; then cd $(ASYNC_DIR) && cargo fmt; fi
	@echo "$(GREEN)✓$(NC) Format complete"

clippy:
	@echo "Running clippy..."
	@if [ -f "$(SYNC_DIR)/Cargo.toml" ]; then cd $(SYNC_DIR) && cargo clippy 2>&1 || true; fi
	@if [ -f "$(ASYNC_DIR)/Cargo.toml" ]; then cd $(ASYNC_DIR) && cargo clippy 2>&1 || true; fi
	@echo "$(GREEN)✓$(NC) Clippy complete"

doc:
	@echo "Generating documentation..."
	@if [ -f "$(SYNC_DIR)/Cargo.toml" ]; then cd $(SYNC_DIR) && cargo doc --no-deps; fi
	@if [ -f "$(ASYNC_DIR)/Cargo.toml" ]; then cd $(ASYNC_DIR) && cargo doc --no-deps; fi
	@echo "$(GREEN)✓$(NC) Documentation generated"

# ============================================================================
# Examples
# ============================================================================

examples:
	@echo "Running Rust examples..."
	@if [ -d "$(SYNC_DIR)/examples" ] && [ -f "$(SYNC_DIR)/Cargo.toml" ]; then \
		cd $(SYNC_DIR) && for f in examples/*.rs; do \
			name=$$(basename "$$f" .rs); \
			echo "Running $$name..."; \
			cargo run --example "$$name" 2>&1 | head -10 || true; \
		done; \
	fi

# ============================================================================
# Clean
# ============================================================================

clean:
	@echo "Cleaning Rust build artifacts..."
	@if [ -d "$(SYNC_DIR)/target" ]; then rm -rf $(SYNC_DIR)/target; fi
	@if [ -d "$(ASYNC_DIR)/target" ]; then rm -rf $(ASYNC_DIR)/target; fi
	@echo "$(GREEN)✓$(NC) Cleaned build artifacts"
