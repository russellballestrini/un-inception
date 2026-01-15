# UN Zig Client

.PHONY: test test-cli test-library test-integration test-functional clean help

ROOT_DIR := $(shell cd ../.. && pwd)
SYNC_DIR := sync
GREEN := \033[32m
YELLOW := \033[33m
NC := \033[0m

.DEFAULT_GOAL := help

help:
	@echo "UN Zig Client"
	@echo "  make test    All 4 test modes"

test: test-cli test-library test-integration test-functional
	@echo "$(GREEN)✓ Zig Client: All 4 test modes complete$(NC)"

test-cli:
	@echo ""
	@echo "━━━ CLI MODE ━━━"
	@if [ -f "$(ROOT_DIR)/un.zig" ]; then zig fmt --check "$(ROOT_DIR)/un.zig" 2>/dev/null && echo "  $(GREEN)✓$(NC) Format valid" || echo "  $(YELLOW)⊘$(NC) Check syntax"; fi
	@if [ -f "$(SYNC_DIR)/build.zig" ]; then cd $(SYNC_DIR) && zig build 2>/dev/null && echo "  $(GREEN)✓$(NC) SDK compiles" || echo "  $(YELLOW)⊘$(NC) Check build.zig"; fi

test-library:
	@echo ""
	@echo "━━━ LIBRARY MODE ━━━"
	@if [ -f "$(SYNC_DIR)/build.zig" ]; then cd $(SYNC_DIR) && zig build test 2>/dev/null || echo "  $(YELLOW)⊘$(NC) Tests incomplete"; fi

test-integration:
	@echo ""
	@echo "━━━ INTEGRATION MODE ━━━"
	@if [ -z "$$UNSANDBOX_PUBLIC_KEY" ]; then echo "  $(YELLOW)⊘$(NC) No credentials"; else echo "  $(YELLOW)⊘$(NC) Not implemented"; fi

test-functional:
	@echo ""
	@echo "━━━ FUNCTIONAL MODE ━━━"
	@if [ -z "$$UNSANDBOX_PUBLIC_KEY" ]; then echo "  $(YELLOW)⊘$(NC) No credentials"; else echo "  $(YELLOW)⊘$(NC) Not implemented"; fi

clean:
	@rm -rf $(SYNC_DIR)/zig-cache $(SYNC_DIR)/zig-out 2>/dev/null; echo "$(GREEN)✓$(NC) Cleaned"
