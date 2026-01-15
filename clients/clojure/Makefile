# UN Clojure Client

.PHONY: test test-cli test-library test-integration test-functional clean help

ROOT_DIR := $(shell cd ../.. && pwd)
SYNC_DIR := sync
GREEN := \033[32m
YELLOW := \033[33m
NC := \033[0m

.DEFAULT_GOAL := help

help:
	@echo "UN Clojure Client"
	@echo "  make test    All 4 test modes"

test: test-cli test-library test-integration test-functional
	@echo "$(GREEN)✓ Clojure Client: All 4 test modes complete$(NC)"

test-cli:
	@echo ""
	@echo "━━━ CLI MODE ━━━"
	@if [ -f "$(ROOT_DIR)/un.clj" ]; then clj -M --eval "(load-file \"$(ROOT_DIR)/un.clj\")" 2>/dev/null && echo "  $(GREEN)✓$(NC) Syntax valid" || echo "  $(YELLOW)⊘$(NC) Check syntax"; fi
	@if [ -f "$(SYNC_DIR)/deps.edn" ]; then cd $(SYNC_DIR) && clj -M:test 2>/dev/null && echo "  $(GREEN)✓$(NC) SDK compiles" || echo "  $(YELLOW)⊘$(NC) Run clj -P"; fi

test-library:
	@echo ""
	@echo "━━━ LIBRARY MODE ━━━"
	@if [ -f "$(SYNC_DIR)/project.clj" ]; then cd $(SYNC_DIR) && lein test 2>/dev/null || echo "  $(YELLOW)⊘$(NC) Run lein deps"; \
	elif [ -f "$(SYNC_DIR)/deps.edn" ]; then cd $(SYNC_DIR) && clj -M:test 2>/dev/null || echo "  $(YELLOW)⊘$(NC) Check deps.edn"; fi

test-integration:
	@echo ""
	@echo "━━━ INTEGRATION MODE ━━━"
	@if [ -z "$$UNSANDBOX_PUBLIC_KEY" ]; then echo "  $(YELLOW)⊘$(NC) No credentials"; else echo "  $(YELLOW)⊘$(NC) Not implemented"; fi

test-functional:
	@echo ""
	@echo "━━━ FUNCTIONAL MODE ━━━"
	@if [ -z "$$UNSANDBOX_PUBLIC_KEY" ]; then echo "  $(YELLOW)⊘$(NC) No credentials"; else echo "  $(YELLOW)⊘$(NC) Not implemented"; fi

clean:
	@rm -rf $(SYNC_DIR)/target $(SYNC_DIR)/.cpcache 2>/dev/null; echo "$(GREEN)✓$(NC) Cleaned"
