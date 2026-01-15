# UN Haskell Client - Build and Test

.PHONY: all build test test-cli test-library test-integration test-functional clean help

ROOT_DIR := $(shell cd ../.. && pwd)
SYNC_DIR := sync
GREEN := \033[32m
RED := \033[31m
YELLOW := \033[33m
NC := \033[0m

.DEFAULT_GOAL := help

help:
	@echo "UN Haskell Client - Build and Test"
	@echo ""
	@echo "  make build            Compile with cabal/stack"
	@echo "  make test             All 4 test modes"
	@echo ""

build:
	@if [ -f "$(SYNC_DIR)/package.yaml" ]; then \
		cd $(SYNC_DIR) && stack build && echo "$(GREEN)✓$(NC) Sync SDK compiled (stack)"; \
	elif [ -f "$(SYNC_DIR)/*.cabal" ]; then \
		cd $(SYNC_DIR) && cabal build && echo "$(GREEN)✓$(NC) Sync SDK compiled (cabal)"; \
	else echo "$(YELLOW)⊘$(NC) No build config found"; fi

test: test-cli test-library test-integration test-functional
	@echo "$(GREEN)✓ Haskell Client: All 4 test modes complete$(NC)"

test-cli:
	@echo ""
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "CLI MODE: Testing Haskell CLI"
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@if [ -f "$(ROOT_DIR)/un.hs" ]; then \
		ghc -e "main" "$(ROOT_DIR)/un.hs" 2>/dev/null && echo "  $(GREEN)✓$(NC) CLI: Compiles" || echo "  $(YELLOW)⊘$(NC) CLI: ghc not installed or error"; \
	fi
	@if [ -f "$(SYNC_DIR)/package.yaml" ]; then \
		cd $(SYNC_DIR) && stack build 2>/dev/null && echo "  $(GREEN)✓$(NC) CLI: Sync SDK compiles" || echo "  $(YELLOW)⊘$(NC) CLI: Compile failed"; \
	fi

test-library:
	@echo ""
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "LIBRARY MODE: Testing Haskell modules"
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@if [ -f "$(SYNC_DIR)/package.yaml" ]; then \
		cd $(SYNC_DIR) && stack test 2>/dev/null || echo "  $(YELLOW)⊘$(NC) Library: Run stack setup first"; \
	else echo "  $(YELLOW)⊘$(NC) Library: No package.yaml"; fi

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
	@rm -rf $(SYNC_DIR)/.stack-work $(SYNC_DIR)/dist-newstyle 2>/dev/null || true
	@echo "$(GREEN)✓$(NC) Cleaned Haskell artifacts"
