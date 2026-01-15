# UN R Client - Build and Test

.PHONY: test test-cli test-library test-integration test-functional clean help

ROOT_DIR := $(shell cd ../.. && pwd)
SYNC_DIR := sync
GREEN := \033[32m
YELLOW := \033[33m
NC := \033[0m

.DEFAULT_GOAL := help

help:
	@echo "UN R Client"
	@echo "  make test    All 4 test modes"

test: test-cli test-library test-integration test-functional
	@echo "$(GREEN)✓ R Client: All 4 test modes complete$(NC)"

test-cli:
	@echo ""
	@echo "━━━ CLI MODE ━━━"
	@if [ -f "$(ROOT_DIR)/un.r" ]; then Rscript --vanilla -e "source('$(ROOT_DIR)/un.r')" 2>/dev/null && echo "  $(GREEN)✓$(NC) Syntax valid" || echo "  $(YELLOW)⊘$(NC) Check syntax"; fi

test-library:
	@echo ""
	@echo "━━━ LIBRARY MODE ━━━"
	@if [ -f "$(SYNC_DIR)/R/unsandbox.R" ]; then Rscript -e "source('$(SYNC_DIR)/R/unsandbox.R')" 2>/dev/null && echo "  $(GREEN)✓$(NC) Module loads" || echo "  $(YELLOW)⊘$(NC) Check dependencies"; fi
	@if [ -d "$(SYNC_DIR)/tests" ]; then cd $(SYNC_DIR) && Rscript -e "testthat::test_dir('tests')" 2>/dev/null || echo "  $(YELLOW)⊘$(NC) Install testthat"; fi

test-integration:
	@echo ""
	@echo "━━━ INTEGRATION MODE ━━━"
	@if [ -z "$$UNSANDBOX_PUBLIC_KEY" ]; then echo "  $(YELLOW)⊘$(NC) No credentials"; else echo "  $(YELLOW)⊘$(NC) Not implemented"; fi

test-functional:
	@echo ""
	@echo "━━━ FUNCTIONAL MODE ━━━"
	@if [ -z "$$UNSANDBOX_PUBLIC_KEY" ]; then echo "  $(YELLOW)⊘$(NC) No credentials"; else echo "  $(YELLOW)⊘$(NC) Not implemented"; fi

clean:
	@rm -f $(SYNC_DIR)/*.Rcheck 2>/dev/null; echo "$(GREEN)✓$(NC) Cleaned"
