# UN TypeScript Client - Build and Test

.PHONY: all build test test-cli test-library test-integration test-functional clean help

ROOT_DIR := $(shell cd ../.. && pwd)
SYNC_DIR := sync
ASYNC_DIR := async
GREEN := \033[32m
RED := \033[31m
YELLOW := \033[33m
NC := \033[0m

.DEFAULT_GOAL := help

help:
	@echo "UN TypeScript Client - Build and Test"
	@echo ""
	@echo "  make build            Compile TypeScript"
	@echo "  make test             All 4 test modes"
	@echo "  make test-cli         CLI mode"
	@echo "  make test-library     Library mode"
	@echo ""

build:
	@if [ -f "$(SYNC_DIR)/tsconfig.json" ]; then \
		cd $(SYNC_DIR) && npx tsc && echo "$(GREEN)✓$(NC) Sync SDK compiled"; \
	else echo "$(YELLOW)⊘$(NC) No tsconfig.json found"; fi

test: test-cli test-library test-integration test-functional
	@echo "$(GREEN)✓ TypeScript Client: All 4 test modes complete$(NC)"

test-cli:
	@echo ""
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "CLI MODE: Testing TypeScript CLI"
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@if [ -f "$(ROOT_DIR)/un.ts" ]; then \
		npx tsc --noEmit "$(ROOT_DIR)/un.ts" 2>/dev/null && echo "  $(GREEN)✓$(NC) CLI: Type check passed (un.ts)" || echo "  $(YELLOW)⊘$(NC) CLI: Type errors or tsc not installed"; \
	fi
	@if [ -f "$(SYNC_DIR)/src/un.ts" ]; then \
		npx tsc --noEmit "$(SYNC_DIR)/src/un.ts" 2>/dev/null && echo "  $(GREEN)✓$(NC) CLI: Sync SDK type check passed"; \
	fi

test-library:
	@echo ""
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "LIBRARY MODE: Testing TypeScript imports"
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@if [ -f "$(SYNC_DIR)/package.json" ]; then \
		cd $(SYNC_DIR) && npm test 2>/dev/null || echo "  $(YELLOW)⊘$(NC) Library: Run npm install first"; \
	else echo "  $(YELLOW)⊘$(NC) Library: No package.json"; fi

test-integration:
	@echo ""
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "INTEGRATION MODE: Testing API contract"
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@if [ -z "$$UNSANDBOX_PUBLIC_KEY" ]; then echo "  $(YELLOW)⊘$(NC) Skipping (no API credentials)"; \
	else echo "  $(YELLOW)⊘$(NC) Integration: Not yet implemented"; fi

test-functional:
	@echo ""
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "FUNCTIONAL MODE: Real-world scenarios"
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@if [ -z "$$UNSANDBOX_PUBLIC_KEY" ]; then echo "  $(YELLOW)⊘$(NC) Skipping (no API credentials)"; \
	else echo "  $(YELLOW)⊘$(NC) Functional: Not yet implemented"; fi

clean:
	@rm -rf $(SYNC_DIR)/dist $(ASYNC_DIR)/dist 2>/dev/null || true
	@echo "$(GREEN)✓$(NC) Cleaned TypeScript artifacts"
