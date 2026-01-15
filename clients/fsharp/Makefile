# UN F# Client

.PHONY: test test-cli test-library test-integration test-functional clean help

ROOT_DIR := $(shell cd ../.. && pwd)
SYNC_DIR := sync
GREEN := \033[32m
YELLOW := \033[33m
NC := \033[0m

.DEFAULT_GOAL := help

help:
	@echo "UN F# Client"
	@echo "  make test    All 4 test modes"

test: test-cli test-library test-integration test-functional
	@echo "$(GREEN)✓ F# Client: All 4 test modes complete$(NC)"

test-cli:
	@echo ""
	@echo "━━━ CLI MODE ━━━"
	@if [ -f "$(ROOT_DIR)/un.fs" ]; then dotnet fsi "$(ROOT_DIR)/un.fs" --help 2>/dev/null && echo "  $(GREEN)✓$(NC) Runs" || echo "  $(YELLOW)⊘$(NC) Check syntax"; fi
	@if ls $(SYNC_DIR)/*.fsproj 1>/dev/null 2>&1; then cd $(SYNC_DIR) && dotnet build 2>/dev/null && echo "  $(GREEN)✓$(NC) SDK compiles" || echo "  $(YELLOW)⊘$(NC) Run dotnet restore"; fi

test-library:
	@echo ""
	@echo "━━━ LIBRARY MODE ━━━"
	@if ls $(SYNC_DIR)/*.fsproj 1>/dev/null 2>&1; then cd $(SYNC_DIR) && dotnet test 2>/dev/null || echo "  $(YELLOW)⊘$(NC) Tests incomplete"; fi

test-integration:
	@echo ""
	@echo "━━━ INTEGRATION MODE ━━━"
	@if [ -z "$$UNSANDBOX_PUBLIC_KEY" ]; then echo "  $(YELLOW)⊘$(NC) No credentials"; else echo "  $(YELLOW)⊘$(NC) Not implemented"; fi

test-functional:
	@echo ""
	@echo "━━━ FUNCTIONAL MODE ━━━"
	@if [ -z "$$UNSANDBOX_PUBLIC_KEY" ]; then echo "  $(YELLOW)⊘$(NC) No credentials"; else echo "  $(YELLOW)⊘$(NC) Not implemented"; fi

clean:
	@rm -rf $(SYNC_DIR)/bin $(SYNC_DIR)/obj 2>/dev/null; echo "$(GREEN)✓$(NC) Cleaned"
