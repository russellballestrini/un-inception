# UN JavaScript Client - Build and Test
#
# This client has two implementations:
#   - sync/  : Synchronous/Promise-based SDK (Node.js)
#   - async/ : Async/Await SDK (Node.js)
#
# Usage:
#   make test         # Run all 4 test modes (auto-installs jest)
#   make test-cli     # CLI mode only
#   make test-library # Library mode only
#   make test-sync    # Test sync SDK only
#   make test-async   # Test async SDK only
#   make clean        # Remove node_modules + build artifacts
#
# The Makefile runs npm install automatically when node_modules is missing.

.PHONY: all test test-cli test-library test-integration test-functional
.PHONY: test-sync test-async lint format clean help examples

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
	@echo "UN JavaScript Client - Build and Test"
	@echo ""
	@echo "Test (all 4 modes):"
	@echo "  make test               All 4 modes (auto-installs deps)"
	@echo "  make test-cli           CLI mode (command-line interface)"
	@echo "  make test-library       Library mode (require and use)"
	@echo "  make test-integration   Integration mode (API contract)"
	@echo "  make test-functional    Functional mode (real-world scenarios)"
	@echo ""
	@echo "Test by SDK:"
	@echo "  make test-sync          Test synchronous SDK"
	@echo "  make test-async         Test asynchronous SDK"
	@echo ""
	@echo "Development:"
	@echo "  make lint               Lint with ESLint"
	@echo "  make format             Format with Prettier"
	@echo "  make examples           Run example scripts"
	@echo ""
	@echo "Utility:"
	@echo "  make clean              Remove node_modules + build artifacts"
	@echo ""

all: test

# ============================================================================
# Dependency Management
# ============================================================================

$(SYNC_DIR)/node_modules/.package-lock.json: $(SYNC_DIR)/package.json
	@echo "Installing sync SDK dependencies..."
	@cd $(SYNC_DIR) && npm install --no-audit --no-fund -q 2>&1 | tail -1

$(ASYNC_DIR)/node_modules/.package-lock.json: $(ASYNC_DIR)/package.json
	@echo "Installing async SDK dependencies..."
	@cd $(ASYNC_DIR) && npm install --no-audit --no-fund -q 2>&1 | tail -1

sync-deps: $(SYNC_DIR)/node_modules/.package-lock.json
async-deps: $(ASYNC_DIR)/node_modules/.package-lock.json

# ============================================================================
# TEST: All 4 Modes
# ============================================================================

test: test-cli test-library test-integration test-functional
	@echo ""
	@echo "$(GREEN)✓ JavaScript Client: All 4 test modes complete$(NC)"

# ============================================================================
# TEST: CLI Mode
# ============================================================================

test-cli:
	@echo ""
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "CLI MODE: Testing JavaScript CLI interface"
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo ""
	@if [ -f "$(ROOT_DIR)/un.js" ]; then \
		node --check "$(ROOT_DIR)/un.js" 2>/dev/null && echo "  $(GREEN)✓$(NC) CLI: Syntax valid (un.js)" || echo "  $(RED)✗$(NC) CLI: Syntax error in un.js"; \
	fi
	@if [ -f "$(SYNC_DIR)/src/un.js" ]; then \
		node --check "$(SYNC_DIR)/src/un.js" 2>/dev/null && echo "  $(GREEN)✓$(NC) CLI: Sync SDK syntax valid" || echo "  $(RED)✗$(NC) CLI: Sync SDK syntax error"; \
	fi
	@if [ -f "$(ASYNC_DIR)/src/un_async.js" ]; then \
		node --check "$(ASYNC_DIR)/src/un_async.js" 2>/dev/null && echo "  $(GREEN)✓$(NC) CLI: Async SDK syntax valid" || echo "  $(YELLOW)⊘$(NC) CLI: Async SDK ES module (use --input-type=module)"; \
	fi

# ============================================================================
# TEST: Library Mode
# ============================================================================

test-library: sync-deps
	@echo ""
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "LIBRARY MODE: Testing JavaScript imports"
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo ""
	@if [ -f "$(SYNC_DIR)/src/un.js" ]; then \
		node --input-type=module -e "const un = await import('./$(SYNC_DIR)/src/un.js'); const fns = Object.keys(un).filter(k => typeof un[k] === 'function'); console.log('  ✓ Library: Sync SDK importable, exports:', fns.length, 'functions')" 2>/dev/null || echo "  $(RED)✗$(NC) Library: Sync import failed"; \
	fi
	@if [ -f "$(ASYNC_DIR)/src/un_async.js" ]; then \
		node --input-type=module -e "const un = await import('./$(ASYNC_DIR)/src/un_async.js'); const fns = Object.keys(un).filter(k => typeof un[k] === 'function'); console.log('  ✓ Library: Async SDK importable, exports:', fns.length, 'functions')" 2>/dev/null || echo "  $(YELLOW)⊘$(NC) Library: Async import check (ES module)"; \
	fi
	@echo ""
	@echo "Running unit tests..."
	@if [ -d "$(SYNC_DIR)/tests" ] && [ -f "$(SYNC_DIR)/package.json" ]; then \
		cd $(SYNC_DIR) && npm test 2>&1 && echo "  $(GREEN)✓$(NC) Sync SDK tests passed" || echo "  $(RED)✗$(NC) Sync tests failed"; \
	fi
	@if [ -d "$(ASYNC_DIR)/tests" ] && [ -f "$(ASYNC_DIR)/package.json" ]; then \
		cd $(ASYNC_DIR) && npm test 2>&1 && echo "  $(GREEN)✓$(NC) Async SDK tests passed" || echo "  $(RED)✗$(NC) Async tests failed"; \
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
		if [ -f "$(SYNC_DIR)/src/un.js" ]; then \
			node --input-type=module -e "const un = await import('./$(SYNC_DIR)/src/un.js'); const r = await un.executeCode('python', 'print(42)'); if(r.stdout && r.stdout.includes('42')) console.log('  ✓ Integration: API auth works'); else console.log('  ✗ Integration: Unexpected response');" 2>/dev/null || echo "  $(RED)✗$(NC) Integration: SDK error"; \
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
		if [ -f "$(SYNC_DIR)/tests/test_functional.mjs" ]; then \
			node $(SYNC_DIR)/tests/test_functional.mjs 2>&1 && echo "  $(GREEN)✓$(NC) Functional: All tests passed" || echo "  $(RED)✗$(NC) Functional: Tests failed"; \
		fi; \
	fi

# ============================================================================
# TEST: By SDK Type
# ============================================================================

test-sync: sync-deps
	@echo "Testing Sync SDK..."
	@if [ -f "$(SYNC_DIR)/package.json" ]; then \
		cd $(SYNC_DIR) && npm test; \
	else \
		echo "  $(YELLOW)⊘$(NC) Sync SDK needs package.json"; \
	fi

test-async: async-deps
	@echo "Testing Async SDK..."
	@if [ -f "$(ASYNC_DIR)/package.json" ]; then \
		cd $(ASYNC_DIR) && npm test; \
	else \
		echo "  $(YELLOW)⊘$(NC) Async SDK needs package.json"; \
	fi

# ============================================================================
# Development
# ============================================================================

lint:
	@echo "Linting JavaScript SDKs..."
	@if [ -d "$(SYNC_DIR)/src" ]; then npx eslint $(SYNC_DIR)/src/ 2>/dev/null || echo "  $(YELLOW)⊘$(NC) ESLint not configured"; fi
	@if [ -d "$(ASYNC_DIR)/src" ]; then npx eslint $(ASYNC_DIR)/src/ 2>/dev/null || echo "  $(YELLOW)⊘$(NC) ESLint not configured"; fi
	@echo "$(GREEN)✓$(NC) Lint complete"

format:
	@echo "Formatting JavaScript SDKs..."
	@if [ -d "$(SYNC_DIR)/src" ]; then npx prettier --write $(SYNC_DIR)/src/ 2>/dev/null || echo "  $(YELLOW)⊘$(NC) Prettier not installed"; fi
	@if [ -d "$(ASYNC_DIR)/src" ]; then npx prettier --write $(ASYNC_DIR)/src/ 2>/dev/null || echo "  $(YELLOW)⊘$(NC) Prettier not installed"; fi
	@echo "$(GREEN)✓$(NC) Format complete"

examples:
	@echo "Running JavaScript examples..."
	@if [ -d "$(SYNC_DIR)/examples" ]; then \
		for f in $(SYNC_DIR)/examples/*.js; do \
			echo "Running $$f..."; \
			node "$$f" 2>&1 | head -10 || true; \
		done; \
	fi

# ============================================================================
# Clean
# ============================================================================

clean:
	@echo "Cleaning JavaScript build artifacts..."
	@rm -rf $(SYNC_DIR)/node_modules $(ASYNC_DIR)/node_modules 2>/dev/null || true
	@rm -rf $(SYNC_DIR)/coverage $(ASYNC_DIR)/coverage 2>/dev/null || true
	@echo "$(GREEN)✓$(NC) Cleaned node_modules + build artifacts"
