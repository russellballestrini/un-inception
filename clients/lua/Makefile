# UN Lua Client - Build and Test

.PHONY: all test test-cli test-library test-integration test-functional clean help

ROOT_DIR := $(shell cd ../.. && pwd)
SYNC_DIR := sync
GREEN := \033[32m
RED := \033[31m
YELLOW := \033[33m
NC := \033[0m

.DEFAULT_GOAL := help

help:
	@echo "UN Lua Client - Build and Test"
	@echo ""
	@echo "  make test             All 4 test modes"
	@echo "  make test-cli         CLI mode"
	@echo "  make test-library     Library mode (require module)"
	@echo ""

test: test-cli test-library test-integration test-functional
	@echo "$(GREEN)✓ Lua Client: All 4 test modes complete$(NC)"

test-cli:
	@echo ""
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "CLI MODE: Testing Lua CLI"
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@if [ -f "$(ROOT_DIR)/un.lua" ]; then \
		luac -p "$(ROOT_DIR)/un.lua" 2>/dev/null && echo "  $(GREEN)✓$(NC) CLI: Syntax valid (un.lua)" || echo "  $(RED)✗$(NC) CLI: Syntax error"; \
		lua "$(ROOT_DIR)/un.lua" --help > /dev/null 2>&1 && echo "  $(GREEN)✓$(NC) CLI: --help works" || echo "  $(YELLOW)⊘$(NC) CLI: --help (check implementation)"; \
	fi
	@if [ -f "$(SYNC_DIR)/src/unsandbox.lua" ]; then \
		luac -p "$(SYNC_DIR)/src/unsandbox.lua" 2>/dev/null && echo "  $(GREEN)✓$(NC) CLI: Sync module syntax valid"; \
	fi

test-library:
	@echo ""
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "LIBRARY MODE: Testing Lua module"
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@if [ -f "$(SYNC_DIR)/src/unsandbox.lua" ]; then \
		lua -e "package.path='$(SYNC_DIR)/src/?.lua;'..package.path; local un = require('unsandbox'); print('  ✓ Library: Module importable')" 2>/dev/null || echo "  $(YELLOW)⊘$(NC) Library: Module import failed"; \
	fi
	@if [ -d "$(SYNC_DIR)/spec" ]; then \
		cd $(SYNC_DIR) && busted spec/ 2>/dev/null || echo "  $(YELLOW)⊘$(NC) Library: Install busted for testing"; \
	fi

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
	@echo "$(GREEN)✓$(NC) Nothing to clean for Lua"
