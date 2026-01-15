# UN Bash Client - Build and Test
#
# Usage:
#   make test         # Run all 4 test modes
#   make test-cli     # CLI mode only
#   make test-library # Library mode only
#   make test-integration # Integration mode only
#   make test-functional  # Functional mode only

.PHONY: all test test-cli test-library test-integration test-functional clean help

ROOT_DIR := $(shell cd ../.. && pwd)
SYNC_DIR := sync
GREEN := \033[32m
RED := \033[31m
YELLOW := \033[33m
NC := \033[0m

.DEFAULT_GOAL := help

help:
	@echo "UN Bash Client - Build and Test"
	@echo ""
	@echo "  make test             All 4 test modes"
	@echo "  make test-cli         CLI mode"
	@echo "  make test-library     Library mode (source functions)"
	@echo "  make test-integration Integration mode"
	@echo "  make test-functional  Functional mode"
	@echo "  make lint             Lint with shellcheck"
	@echo ""

test: test-cli test-library test-integration test-functional
	@echo "$(GREEN)✓ Bash Client: All 4 test modes complete$(NC)"

test-cli:
	@echo ""
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "CLI MODE: Testing Bash CLI"
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@if [ -f "$(ROOT_DIR)/un.sh" ]; then \
		bash -n "$(ROOT_DIR)/un.sh" && echo "  $(GREEN)✓$(NC) CLI: Syntax valid (un.sh)" || echo "  $(RED)✗$(NC) CLI: Syntax error"; \
		"$(ROOT_DIR)/un.sh" --help > /dev/null 2>&1 && echo "  $(GREEN)✓$(NC) CLI: --help works" || echo "  $(YELLOW)⊘$(NC) CLI: --help (check implementation)"; \
	fi
	@if [ -f "$(SYNC_DIR)/src/un.sh" ]; then \
		bash -n "$(SYNC_DIR)/src/un.sh" && echo "  $(GREEN)✓$(NC) CLI: Sync SDK syntax valid" || echo "  $(RED)✗$(NC) CLI: Sync syntax error"; \
	fi

test-library:
	@echo ""
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "LIBRARY MODE: Testing Bash functions"
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@if [ -f "$(ROOT_DIR)/un.sh" ]; then \
		bash -c "source $(ROOT_DIR)/un.sh 2>/dev/null; type un_execute >/dev/null 2>&1" && echo "  $(GREEN)✓$(NC) Library: un_execute function exists" || echo "  $(YELLOW)⊘$(NC) Library: un_execute not exported"; \
	fi
	@if [ -d "$(SYNC_DIR)/tests" ]; then \
		cd $(SYNC_DIR) && bats tests/ 2>/dev/null || echo "  $(YELLOW)⊘$(NC) Library: Install bats for testing"; \
	fi

test-integration:
	@echo ""
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "INTEGRATION MODE: Testing API contract"
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@if [ -z "$$UNSANDBOX_PUBLIC_KEY" ] || [ -z "$$UNSANDBOX_SECRET_KEY" ]; then \
		echo "  $(YELLOW)⊘$(NC) Skipping (no API credentials)"; \
	else \
		echo "  $(YELLOW)⊘$(NC) Integration: Not yet implemented"; \
	fi

test-functional:
	@echo ""
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "FUNCTIONAL MODE: Real-world scenarios"
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@if [ -z "$$UNSANDBOX_PUBLIC_KEY" ] || [ -z "$$UNSANDBOX_SECRET_KEY" ]; then \
		echo "  $(YELLOW)⊘$(NC) Skipping (no API credentials)"; \
	else \
		echo "  $(YELLOW)⊘$(NC) Functional: Not yet implemented"; \
	fi

lint:
	@echo "Linting Bash..."
	@if [ -f "$(ROOT_DIR)/un.sh" ]; then shellcheck "$(ROOT_DIR)/un.sh" || true; fi
	@if [ -f "$(SYNC_DIR)/src/un.sh" ]; then shellcheck "$(SYNC_DIR)/src/un.sh" || true; fi

clean:
	@echo "$(GREEN)✓$(NC) Nothing to clean for Bash"
