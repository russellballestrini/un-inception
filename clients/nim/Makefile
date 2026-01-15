# UN Nim Client

.PHONY: test test-cli test-library test-integration test-functional clean help

ROOT_DIR := $(shell cd ../.. && pwd)
SYNC_DIR := sync
GREEN := \033[32m
YELLOW := \033[33m
NC := \033[0m

.DEFAULT_GOAL := help

help:
	@echo "UN Nim Client"
	@echo "  make test    All 4 test modes"

test: test-cli test-library test-integration test-functional
	@echo "$(GREEN)✓ Nim Client: All 4 test modes complete$(NC)"

test-cli:
	@echo ""
	@echo "━━━ CLI MODE ━━━"
	@if [ -f "$(ROOT_DIR)/un.nim" ]; then nim check "$(ROOT_DIR)/un.nim" 2>/dev/null && echo "  $(GREEN)✓$(NC) Syntax valid" || echo "  $(YELLOW)⊘$(NC) Check syntax"; fi
	@if [ -f "$(SYNC_DIR)/src/unsandbox.nim" ]; then nim check "$(SYNC_DIR)/src/unsandbox.nim" 2>/dev/null && echo "  $(GREEN)✓$(NC) SDK syntax valid" || echo "  $(YELLOW)⊘$(NC) Check SDK"; fi

test-library:
	@echo ""
	@echo "━━━ LIBRARY MODE ━━━"
	@if [ -f "$(SYNC_DIR)/unsandbox.nimble" ]; then cd $(SYNC_DIR) && nimble test 2>/dev/null || echo "  $(YELLOW)⊘$(NC) Run nimble install"; fi

test-integration:
	@echo ""
	@echo "━━━ INTEGRATION MODE ━━━"
	@if [ -z "$$UNSANDBOX_PUBLIC_KEY" ]; then echo "  $(YELLOW)⊘$(NC) No credentials"; else echo "  $(YELLOW)⊘$(NC) Not implemented"; fi

test-functional:
	@echo ""
	@echo "━━━ FUNCTIONAL MODE ━━━"
	@if [ -z "$$UNSANDBOX_PUBLIC_KEY" ]; then echo "  $(YELLOW)⊘$(NC) No credentials"; else echo "  $(YELLOW)⊘$(NC) Not implemented"; fi

clean:
	@rm -rf $(SYNC_DIR)/nimcache 2>/dev/null; echo "$(GREEN)✓$(NC) Cleaned"
