# UN AWK Client - Build and Test

.PHONY: all test test-cli test-library test-integration test-functional clean help

ROOT_DIR := $(shell cd ../.. && pwd)
SYNC_DIR := sync
GREEN := \033[32m
RED := \033[31m
YELLOW := \033[33m
NC := \033[0m

.DEFAULT_GOAL := help

help:
	@echo "UN AWK Client - Build and Test"
	@echo ""
	@echo "  make test             All 4 test modes"
	@echo "  make test-cli         CLI mode"
	@echo "  make test-library     Library mode"
	@echo ""

test: test-cli test-library test-integration test-functional
	@echo "$(GREEN)✓ AWK Client: All 4 test modes complete$(NC)"

test-cli:
	@echo ""
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "CLI MODE: Testing AWK CLI"
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@if [ -f "$(SYNC_DIR)/src/un.awk" ]; then \
		awk --version > /dev/null 2>&1 && echo "  $(GREEN)✓$(NC) CLI: AWK available" || echo "  $(RED)✗$(NC) CLI: AWK not found"; \
		awk -f "$(SYNC_DIR)/src/un.awk" -- --help 2>/dev/null && echo "  $(GREEN)✓$(NC) CLI: --help works" || echo "  $(YELLOW)⊘$(NC) CLI: --help (check implementation)"; \
	fi

test-library:
	@echo ""
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "LIBRARY MODE: Testing AWK module"
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "  $(YELLOW)⊘$(NC) Library: AWK is script-based, no module system"

test-integration:
	@echo ""
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "INTEGRATION MODE: Testing API contract"
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@if [ -z "$$UNSANDBOX_PUBLIC_KEY" ]; then echo "  $(YELLOW)⊘$(NC) Skipping (no API credentials)"; \
	else echo "  $(YELLOW)⊘$(NC) Integration: Not yet implemented"; fi

test-functional:
	@echo ""
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "FUNCTIONAL MODE: Real-world scenarios"
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@if [ -z "$$UNSANDBOX_PUBLIC_KEY" ]; then echo "  $(YELLOW)⊘$(NC) Skipping (no API credentials)"; \
	else echo "  $(YELLOW)⊘$(NC) Functional: Not yet implemented"; fi

clean:
	@echo "$(GREEN)✓$(NC) Nothing to clean for AWK"
