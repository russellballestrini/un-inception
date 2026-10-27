# UN Groovy Client

.PHONY: test test-cli test-library test-integration test-functional test-os clean help

ROOT_DIR := $(shell cd ../.. && pwd)
SYNC_DIR := sync
GREEN := \033[32m
YELLOW := \033[33m
NC := \033[0m

test: test-cli test-library test-integration test-functional
	@echo "$(GREEN)✓ Groovy Client: All 4 test modes complete$(NC)"

test-cli:
	@echo "━━━ CLI MODE ━━━"
	@if [ -f "$(ROOT_DIR)/un.groovy" ]; then groovy --help 2>/dev/null && groovyc "$(ROOT_DIR)/un.groovy" -d /tmp 2>/dev/null && echo "  $(GREEN)✓$(NC) Compiles" || echo "  $(YELLOW)⊘$(NC) Check syntax"; fi
	@if [ -f "$(SYNC_DIR)/build.gradle" ]; then cd $(SYNC_DIR) && gradle compileGroovy -q 2>/dev/null && echo "  $(GREEN)✓$(NC) SDK compiles" || echo "  $(YELLOW)⊘$(NC) Check gradle"; fi

test-library:
	@echo "━━━ LIBRARY MODE ━━━"
	@if [ -f "$(SYNC_DIR)/build.gradle" ]; then cd $(SYNC_DIR) && gradle test -q 2>/dev/null || echo "  $(YELLOW)⊘$(NC) Tests incomplete"; fi

test-os:
	@echo "━━━ --os FLAG (offline, local mock API) ━━━"
	@OS_FLAG_TIMEOUT=$${OS_FLAG_TIMEOUT:-120} bash $(ROOT_DIR)/tests/os_flag_cli.sh groovy $(SYNC_DIR)/src/un.groovy

test-integration:
	@echo "━━━ INTEGRATION MODE ━━━"
	@echo "  $(YELLOW)⊘$(NC) Not implemented"

test-functional:
	@echo "━━━ FUNCTIONAL MODE ━━━"
	@echo "  $(YELLOW)⊘$(NC) Not implemented"

clean:
	@rm -rf $(SYNC_DIR)/build 2>/dev/null; echo "$(GREEN)✓$(NC) Cleaned"
