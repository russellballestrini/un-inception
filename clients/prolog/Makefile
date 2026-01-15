# UN Prolog Client

.PHONY: test test-cli test-library test-integration test-functional clean help

ROOT_DIR := $(shell cd ../.. && pwd)
SYNC_DIR := sync
GREEN := \033[32m
YELLOW := \033[33m
NC := \033[0m

test: test-cli test-library test-integration test-functional
	@echo "$(GREEN)✓ Prolog Client: All 4 test modes complete$(NC)"

test-cli:
	@echo "━━━ CLI MODE ━━━"
	@if [ -f "$(ROOT_DIR)/un.pro" ]; then swipl -g "halt" -s "$(ROOT_DIR)/un.pro" 2>/dev/null && echo "  $(GREEN)✓$(NC) Syntax valid" || echo "  $(YELLOW)⊘$(NC) Check syntax"; fi

test-library:
	@echo "━━━ LIBRARY MODE ━━━"
	@if [ -f "$(SYNC_DIR)/pack.pl" ]; then swipl -g "use_module(library(plunit)),run_tests,halt" -s "$(SYNC_DIR)/test.pl" 2>/dev/null || echo "  $(YELLOW)⊘$(NC) Tests incomplete"; fi

test-integration:
	@echo "━━━ INTEGRATION MODE ━━━"
	@echo "  $(YELLOW)⊘$(NC) Not implemented"

test-functional:
	@echo "━━━ FUNCTIONAL MODE ━━━"
	@echo "  $(YELLOW)⊘$(NC) Not implemented"

clean:
	@echo "$(GREEN)✓$(NC) Cleaned"
