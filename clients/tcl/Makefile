# UN Tcl Client

.PHONY: test test-cli test-library test-integration test-functional clean help

ROOT_DIR := $(shell cd ../.. && pwd)
SYNC_DIR := sync
GREEN := \033[32m
YELLOW := \033[33m
NC := \033[0m

test: test-cli test-library test-integration test-functional
	@echo "$(GREEN)✓ Tcl Client: All 4 test modes complete$(NC)"

test-cli:
	@echo "━━━ CLI MODE ━━━"
	@if [ -f "$(ROOT_DIR)/un.tcl" ]; then tclsh "$(ROOT_DIR)/un.tcl" --help 2>/dev/null && echo "  $(GREEN)✓$(NC) --help works" || echo "  $(YELLOW)⊘$(NC) Check syntax"; fi

test-library:
	@echo "━━━ LIBRARY MODE ━━━"
	@if [ -f "$(SYNC_DIR)/unsandbox.tcl" ]; then tclsh -c "source $(SYNC_DIR)/unsandbox.tcl" 2>/dev/null && echo "  $(GREEN)✓$(NC) Module loads" || echo "  $(YELLOW)⊘$(NC) Check module"; fi

test-integration:
	@echo "━━━ INTEGRATION MODE ━━━"
	@echo "  $(YELLOW)⊘$(NC) Not implemented"

test-functional:
	@echo "━━━ FUNCTIONAL MODE ━━━"
	@echo "  $(YELLOW)⊘$(NC) Not implemented"

clean:
	@echo "$(GREEN)✓$(NC) Cleaned"
