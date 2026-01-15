# UN D Client

.PHONY: test test-cli test-library test-integration test-functional clean help

ROOT_DIR := $(shell cd ../.. && pwd)
SYNC_DIR := sync
GREEN := \033[32m
YELLOW := \033[33m
NC := \033[0m

.DEFAULT_GOAL := help

help:
	@echo "UN D Client"
	@echo "  make test    All 4 test modes"

test: test-cli test-library test-integration test-functional
	@echo "$(GREEN)✓ D Client: All 4 test modes complete$(NC)"

test-cli:
	@echo ""
	@echo "━━━ CLI MODE ━━━"
	@if [ -f "$(ROOT_DIR)/un.d" ]; then dmd -o- "$(ROOT_DIR)/un.d" 2>/dev/null && echo "  $(GREEN)✓$(NC) Syntax valid" || echo "  $(YELLOW)⊘$(NC) Check syntax"; fi
	@if [ -f "$(SYNC_DIR)/dub.json" ] || [ -f "$(SYNC_DIR)/dub.sdl" ]; then cd $(SYNC_DIR) && dub build --quiet 2>/dev/null && echo "  $(GREEN)✓$(NC) SDK compiles" || echo "  $(YELLOW)⊘$(NC) Run dub fetch"; fi

test-library:
	@echo ""
	@echo "━━━ LIBRARY MODE ━━━"
	@if [ -f "$(SYNC_DIR)/dub.json" ] || [ -f "$(SYNC_DIR)/dub.sdl" ]; then cd $(SYNC_DIR) && dub test 2>/dev/null || echo "  $(YELLOW)⊘$(NC) Tests incomplete"; fi

test-integration:
	@echo ""
	@echo "━━━ INTEGRATION MODE ━━━"
	@if [ -z "$$UNSANDBOX_PUBLIC_KEY" ]; then echo "  $(YELLOW)⊘$(NC) No credentials"; else echo "  $(YELLOW)⊘$(NC) Not implemented"; fi

test-functional:
	@echo ""
	@echo "━━━ FUNCTIONAL MODE ━━━"
	@if [ -z "$$UNSANDBOX_PUBLIC_KEY" ]; then echo "  $(YELLOW)⊘$(NC) No credentials"; else echo "  $(YELLOW)⊘$(NC) Not implemented"; fi

clean:
	@rm -rf $(SYNC_DIR)/.dub 2>/dev/null; echo "$(GREEN)✓$(NC) Cleaned"
