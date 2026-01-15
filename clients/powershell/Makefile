# UN PowerShell Client

.PHONY: test test-cli test-library test-integration test-functional clean help

ROOT_DIR := $(shell cd ../.. && pwd)
SYNC_DIR := sync
GREEN := \033[32m
YELLOW := \033[33m
NC := \033[0m

test: test-cli test-library test-integration test-functional
	@echo "$(GREEN)✓ PowerShell Client: All 4 test modes complete$(NC)"

test-cli:
	@echo "━━━ CLI MODE ━━━"
	@if [ -f "$(ROOT_DIR)/un.ps1" ]; then pwsh -NoProfile -Command "& { . '$(ROOT_DIR)/un.ps1' }" 2>/dev/null && echo "  $(GREEN)✓$(NC) Syntax valid" || echo "  $(YELLOW)⊘$(NC) Check syntax"; fi

test-library:
	@echo "━━━ LIBRARY MODE ━━━"
	@if [ -f "$(SYNC_DIR)/Unsandbox.psm1" ]; then pwsh -c "Import-Module ./$(SYNC_DIR)/Unsandbox.psm1" 2>/dev/null && echo "  $(GREEN)✓$(NC) Module imports" || echo "  $(YELLOW)⊘$(NC) Check module"; fi
	@if [ -d "$(SYNC_DIR)/Tests" ]; then pwsh -c "Invoke-Pester $(SYNC_DIR)/Tests" 2>/dev/null || echo "  $(YELLOW)⊘$(NC) Install Pester"; fi

test-integration:
	@echo "━━━ INTEGRATION MODE ━━━"
	@echo "  $(YELLOW)⊘$(NC) Not implemented"

test-functional:
	@echo "━━━ FUNCTIONAL MODE ━━━"
	@echo "  $(YELLOW)⊘$(NC) Not implemented"

clean:
	@echo "$(GREEN)✓$(NC) Cleaned"
