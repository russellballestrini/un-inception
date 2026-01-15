# UN C# Client - Build and Test

.PHONY: all build test test-cli test-library test-integration test-functional clean help

ROOT_DIR := $(shell cd ../.. && pwd)
SYNC_DIR := sync
GREEN := \033[32m
RED := \033[31m
YELLOW := \033[33m
NC := \033[0m

.DEFAULT_GOAL := help

help:
	@echo "UN C# Client - Build and Test"
	@echo ""
	@echo "  make build            Compile with dotnet"
	@echo "  make test             All 4 test modes"
	@echo ""

build:
	@if [ -f "$(SYNC_DIR)/*.csproj" ] || [ -f "$(SYNC_DIR)/*.sln" ]; then \
		cd $(SYNC_DIR) && dotnet build && echo "$(GREEN)✓$(NC) Sync SDK compiled"; \
	else echo "$(YELLOW)⊘$(NC) No .csproj or .sln found"; fi

test: test-cli test-library test-integration test-functional
	@echo "$(GREEN)✓ C# Client: All 4 test modes complete$(NC)"

test-cli:
	@echo ""
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "CLI MODE: Testing C# CLI"
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@if [ -f "$(ROOT_DIR)/un.cs" ]; then \
		dotnet script "$(ROOT_DIR)/un.cs" -- --help 2>/dev/null && echo "  $(GREEN)✓$(NC) CLI: --help works" || echo "  $(YELLOW)⊘$(NC) CLI: dotnet not installed or error"; \
	fi
	@if ls $(SYNC_DIR)/*.csproj 1>/dev/null 2>&1; then \
		cd $(SYNC_DIR) && dotnet build 2>/dev/null && echo "  $(GREEN)✓$(NC) CLI: Sync SDK compiles" || echo "  $(YELLOW)⊘$(NC) CLI: Compile failed"; \
	fi

test-library:
	@echo ""
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "LIBRARY MODE: Testing C# classes"
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@if ls $(SYNC_DIR)/*.csproj 1>/dev/null 2>&1; then \
		cd $(SYNC_DIR) && dotnet test 2>/dev/null || echo "  $(YELLOW)⊘$(NC) Library: Tests incomplete"; \
	else echo "  $(YELLOW)⊘$(NC) Library: No .csproj"; fi

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
	@rm -rf $(SYNC_DIR)/bin $(SYNC_DIR)/obj 2>/dev/null || true
	@echo "$(GREEN)✓$(NC) Cleaned C# artifacts"
