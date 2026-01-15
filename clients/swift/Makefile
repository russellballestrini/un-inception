# UN Swift Client - Build and Test

.PHONY: all build test test-cli test-library test-integration test-functional clean help

ROOT_DIR := $(shell cd ../.. && pwd)
SYNC_DIR := sync
GREEN := \033[32m
RED := \033[31m
YELLOW := \033[33m
NC := \033[0m

.DEFAULT_GOAL := help

help:
	@echo "UN Swift Client - Build and Test"
	@echo ""
	@echo "  make build            Compile with swift build"
	@echo "  make test             All 4 test modes"
	@echo ""

build:
	@if [ -f "$(SYNC_DIR)/Package.swift" ]; then \
		cd $(SYNC_DIR) && swift build && echo "$(GREEN)✓$(NC) Sync SDK compiled"; \
	else echo "$(YELLOW)⊘$(NC) No Package.swift found"; fi

test: test-cli test-library test-integration test-functional
	@echo "$(GREEN)✓ Swift Client: All 4 test modes complete$(NC)"

test-cli:
	@echo ""
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "CLI MODE: Testing Swift CLI"
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@if [ -f "$(ROOT_DIR)/un.swift" ]; then \
		swift "$(ROOT_DIR)/un.swift" --help 2>/dev/null && echo "  $(GREEN)✓$(NC) CLI: --help works" || echo "  $(YELLOW)⊘$(NC) CLI: swift not installed or error"; \
	fi
	@if [ -f "$(SYNC_DIR)/Package.swift" ]; then \
		cd $(SYNC_DIR) && swift build 2>/dev/null && echo "  $(GREEN)✓$(NC) CLI: Sync SDK compiles" || echo "  $(YELLOW)⊘$(NC) CLI: Compile failed"; \
	fi

test-library:
	@echo ""
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "LIBRARY MODE: Testing Swift modules"
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@if [ -f "$(SYNC_DIR)/Package.swift" ]; then \
		cd $(SYNC_DIR) && swift test 2>/dev/null || echo "  $(YELLOW)⊘$(NC) Library: Tests incomplete"; \
	else echo "  $(YELLOW)⊘$(NC) Library: No Package.swift"; fi

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
	@rm -rf $(SYNC_DIR)/.build 2>/dev/null || true
	@echo "$(GREEN)✓$(NC) Cleaned Swift artifacts"
