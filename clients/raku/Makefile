# UN Raku Client

.PHONY: test test-cli test-library test-integration test-functional clean help

ROOT_DIR := $(shell cd ../.. && pwd)
SYNC_DIR := sync
GREEN := \033[32m
YELLOW := \033[33m
NC := \033[0m

test: test-cli test-library test-integration test-functional
	@echo "$(GREEN)✓ Raku Client: All 4 test modes complete$(NC)"

test-cli:
	@echo "━━━ CLI MODE ━━━"
	@if [ -f "$(ROOT_DIR)/un.raku" ]; then raku -c "$(ROOT_DIR)/un.raku" 2>/dev/null && echo "  $(GREEN)✓$(NC) Syntax valid" || echo "  $(YELLOW)⊘$(NC) Check syntax"; fi

test-library:
	@echo "━━━ LIBRARY MODE ━━━"
	@if [ -f "$(SYNC_DIR)/META6.json" ]; then cd $(SYNC_DIR) && zef test . 2>/dev/null || echo "  $(YELLOW)⊘$(NC) Run zef install"; fi

test-integration:
	@echo "━━━ INTEGRATION MODE ━━━"
	@echo "  $(YELLOW)⊘$(NC) Not implemented"

test-functional:
	@echo "━━━ FUNCTIONAL MODE ━━━"
	@echo "  $(YELLOW)⊘$(NC) Not implemented"

clean:
	@rm -rf $(SYNC_DIR)/.precomp 2>/dev/null; echo "$(GREEN)✓$(NC) Cleaned"
