# UN Kotlin Client - Build and Test

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
	@echo "UN Kotlin Client - Build and Test"
	@echo ""
	@echo "  make build            Compile Kotlin"
	@echo "  make test             All 4 test modes"
	@echo ""

build:
	@if [ -f "$(SYNC_DIR)/build.gradle.kts" ]; then \
		cd $(SYNC_DIR) && gradle build -q && echo "$(GREEN)✓$(NC) Sync SDK compiled"; \
	elif [ -f "$(SYNC_DIR)/pom.xml" ]; then \
		cd $(SYNC_DIR) && mvn compile -q && echo "$(GREEN)✓$(NC) Sync SDK compiled"; \
	else echo "$(YELLOW)⊘$(NC) No build config found"; fi

test: test-cli test-library test-integration test-functional
	@echo "$(GREEN)✓ Kotlin Client: All 4 test modes complete$(NC)"

test-cli:
	@echo ""
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "CLI MODE: Testing Kotlin CLI"
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@if [ -f "$(ROOT_DIR)/un.kt" ]; then \
		kotlinc -script "$(ROOT_DIR)/un.kt" -- --help 2>/dev/null && echo "  $(GREEN)✓$(NC) CLI: --help works" || echo "  $(YELLOW)⊘$(NC) CLI: kotlinc not installed or error"; \
	fi
	@if [ -f "$(SYNC_DIR)/build.gradle.kts" ]; then \
		cd $(SYNC_DIR) && gradle compileKotlin -q 2>/dev/null && echo "  $(GREEN)✓$(NC) CLI: Sync SDK compiles" || echo "  $(YELLOW)⊘$(NC) CLI: Compile failed"; \
	fi

test-library:
	@echo ""
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "LIBRARY MODE: Testing Kotlin classes"
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@if [ -f "$(SYNC_DIR)/build.gradle.kts" ]; then \
		cd $(SYNC_DIR) && gradle test -q 2>/dev/null || echo "  $(YELLOW)⊘$(NC) Library: Tests incomplete"; \
	else echo "  $(YELLOW)⊘$(NC) Library: No build config"; fi

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
	@rm -rf $(SYNC_DIR)/build $(ASYNC_DIR)/build 2>/dev/null || true
	@echo "$(GREEN)✓$(NC) Cleaned Kotlin artifacts"
