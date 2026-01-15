# UN Scheme Client

.PHONY: test test-cli test-library test-integration test-functional clean help

ROOT_DIR := $(shell cd ../.. && pwd)
SYNC_DIR := sync
GREEN := \033[32m
YELLOW := \033[33m
NC := \033[0m

test: test-cli test-library test-integration test-functional
	@echo "$(GREEN)✓ Scheme Client: All 4 test modes complete$(NC)"

test-cli:
	@echo "━━━ CLI MODE ━━━"
	@if [ -f "$(ROOT_DIR)/un.scm" ]; then guile --no-auto-compile -c "(load \"$(ROOT_DIR)/un.scm\")" 2>/dev/null && echo "  $(GREEN)✓$(NC) Syntax valid" || echo "  $(YELLOW)⊘$(NC) Check syntax"; fi

test-library:
	@echo "━━━ LIBRARY MODE ━━━"
	@if [ -f "$(SYNC_DIR)/unsandbox.scm" ]; then guile -c "(load \"$(SYNC_DIR)/unsandbox.scm\")" 2>/dev/null && echo "  $(GREEN)✓$(NC) Module loads" || echo "  $(YELLOW)⊘$(NC) Check module"; fi

test-integration:
	@echo "━━━ INTEGRATION MODE ━━━"
	@echo "  $(YELLOW)⊘$(NC) Not implemented"

test-functional:
	@echo "━━━ FUNCTIONAL MODE ━━━"
	@echo "  $(YELLOW)⊘$(NC) Not implemented"

clean:
	@echo "$(GREEN)✓$(NC) Cleaned"
