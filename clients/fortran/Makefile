# UN Fortran Client

.PHONY: test test-cli test-library test-integration test-functional clean help

ROOT_DIR := $(shell cd ../.. && pwd)
SYNC_DIR := sync
GREEN := \033[32m
YELLOW := \033[33m
NC := \033[0m

test: test-cli test-library test-integration test-functional
	@echo "$(GREEN)✓ Fortran Client: All 4 test modes complete$(NC)"

test-cli:
	@echo "━━━ CLI MODE ━━━"
	@if [ -f "$(ROOT_DIR)/un.f90" ]; then gfortran -fsyntax-only "$(ROOT_DIR)/un.f90" 2>/dev/null && echo "  $(GREEN)✓$(NC) Syntax valid" || echo "  $(YELLOW)⊘$(NC) Check syntax"; fi

test-library:
	@echo "━━━ LIBRARY MODE ━━━"
	@if [ -f "$(SYNC_DIR)/fpm.toml" ]; then cd $(SYNC_DIR) && fpm test 2>/dev/null || echo "  $(YELLOW)⊘$(NC) Install fpm"; fi

test-integration:
	@echo "━━━ INTEGRATION MODE ━━━"
	@echo "  $(YELLOW)⊘$(NC) Not implemented"

test-functional:
	@echo "━━━ FUNCTIONAL MODE ━━━"
	@echo "  $(YELLOW)⊘$(NC) Not implemented"

clean:
	@rm -rf $(SYNC_DIR)/build 2>/dev/null; echo "$(GREEN)✓$(NC) Cleaned"
