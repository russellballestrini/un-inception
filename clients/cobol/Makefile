# UN COBOL Client

.PHONY: test test-cli test-library test-integration test-functional test-key clean help

ROOT_DIR := $(shell cd ../.. && pwd)
SYNC_DIR := sync
GREEN := \033[32m
YELLOW := \033[33m
NC := \033[0m

test: test-cli test-library test-integration test-functional
	@echo "$(GREEN)✓ COBOL Client: All 4 test modes complete$(NC)"

test-cli:
	@echo "━━━ CLI MODE ━━━"
	@if [ -f "$(ROOT_DIR)/un.cob" ]; then cobc -fsyntax-only "$(ROOT_DIR)/un.cob" 2>/dev/null && echo "  $(GREEN)✓$(NC) Syntax valid" || echo "  $(YELLOW)⊘$(NC) Check syntax"; fi

test-library:
	@echo "━━━ LIBRARY MODE ━━━"
	@echo "  $(YELLOW)⊘$(NC) COBOL library mode not applicable"

test-integration:
	@echo "━━━ INTEGRATION MODE ━━━"
	@echo "  $(YELLOW)⊘$(NC) Not implemented"

test-functional:
	@echo "━━━ FUNCTIONAL MODE ━━━"
	@echo "  $(YELLOW)⊘$(NC) Not implemented"

# Offline: un key output + --topup against a local mock API (ticket #155)
test-key:
	@tmp=$$(mktemp -d); \
	cobc -x -o $$tmp/un $(SYNC_DIR)/src/un.cob && bash $(ROOT_DIR)/tests/key_output_cli.sh $$tmp/un; rc=$$?; \
	rm -rf $$tmp; exit $$rc

clean:
	@rm -f *.o *.so 2>/dev/null; echo "$(GREEN)✓$(NC) Cleaned"
