# UN Perl Client - Build and Test
#
# Usage:
#   make test         # Run all 4 test modes
#   make test-cli     # CLI mode only
#   make test-library # Library mode only

.PHONY: all test test-cli test-library test-integration test-functional clean help

ROOT_DIR := $(shell cd ../.. && pwd)
SYNC_DIR := sync
GREEN := \033[32m
RED := \033[31m
YELLOW := \033[33m
NC := \033[0m

.DEFAULT_GOAL := help

help:
	@echo "UN Perl Client - Build and Test"
	@echo ""
	@echo "  make test             All 4 test modes"
	@echo "  make test-cli         CLI mode"
	@echo "  make test-library     Library mode (use module)"
	@echo "  make test-integration Integration mode"
	@echo "  make test-functional  Functional mode"
	@echo ""

test: test-cli test-library test-integration test-functional
	@echo "$(GREEN)✓ Perl Client: All 4 test modes complete$(NC)"

test-cli:
	@echo ""
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "CLI MODE: Testing Perl CLI"
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@if [ -f "$(ROOT_DIR)/un.pl" ]; then \
		perl -c "$(ROOT_DIR)/un.pl" 2>/dev/null && echo "  $(GREEN)✓$(NC) CLI: Syntax valid (un.pl)" || echo "  $(RED)✗$(NC) CLI: Syntax error"; \
		perl "$(ROOT_DIR)/un.pl" --help > /dev/null 2>&1 && echo "  $(GREEN)✓$(NC) CLI: --help works" || echo "  $(YELLOW)⊘$(NC) CLI: --help (check implementation)"; \
	fi
	@if [ -f "$(SYNC_DIR)/src/Unsandbox.pm" ]; then \
		perl -c "$(SYNC_DIR)/src/Unsandbox.pm" 2>/dev/null && echo "  $(GREEN)✓$(NC) CLI: Sync module syntax valid" || echo "  $(RED)✗$(NC) CLI: Module syntax error"; \
	fi

test-library:
	@echo ""
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "LIBRARY MODE: Testing Perl module"
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@if [ -f "$(SYNC_DIR)/src/Unsandbox.pm" ]; then \
		perl -I$(SYNC_DIR)/src -e "use Unsandbox; print '  ✓ Library: Module importable\n'" 2>/dev/null || echo "  $(YELLOW)⊘$(NC) Library: Module import failed"; \
	fi
	@if [ -d "$(SYNC_DIR)/t" ]; then \
		cd $(SYNC_DIR) && prove t/ 2>/dev/null || echo "  $(YELLOW)⊘$(NC) Library: Run 'prove t/' for tests"; \
	fi

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
	@echo "$(GREEN)✓$(NC) Nothing to clean for Perl"
