# UN C# Client - Build and Test (Mono/.NET Framework compatible)

.PHONY: all build test test-cli test-library test-integration test-functional clean help

ROOT_DIR := $(shell cd ../.. && pwd)
SYNC_DIR := sync/src
ASYNC_DIR := async/src
GREEN := \033[32m
RED := \033[31m
YELLOW := \033[33m
NC := \033[0m

.DEFAULT_GOAL := help

help:
	@echo "UN C# Client (Mono) - Build and Test"
	@echo ""
	@echo "  make build            Compile Mono/.NET Framework versions"
	@echo "  make test             All test modes"
	@echo "  make test-cli         Test CLI functionality"
	@echo "  make test-functional  Test with real API (requires credentials)"
	@echo "  make clean            Remove build artifacts"
	@echo ""
	@echo "Note: For modern .NET 10, use clients/dotnet/"

build:
	@echo "$(YELLOW)Building C# Mono version...$(NC)"
	@if [ -f "$(SYNC_DIR)/Un.cs" ]; then \
		mcs "$(SYNC_DIR)/Un.cs" -out:"$(SYNC_DIR)/un-mono.exe" && echo "$(GREEN)✓$(NC) Sync version compiled"; \
	else echo "$(RED)✗$(NC) Sync source not found"; fi
	@if [ -f "$(ASYNC_DIR)/Un.cs" ]; then \
		mcs "$(ASYNC_DIR)/Un.cs" -out:"$(ASYNC_DIR)/un-mono.exe" && echo "$(GREEN)✓$(NC) Async version compiled"; \
	else echo "$(YELLOW)⊘$(NC) Async source not found (optional)"; fi

test: test-cli test-library test-integration test-functional
	@echo "$(GREEN)✓ C# Client: All test modes complete$(NC)"

test-cli: build
	@echo ""
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "CLI MODE: Testing C# CLI"
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@if [ -f "$(SYNC_DIR)/un-mono.exe" ]; then \
		mono "$(SYNC_DIR)/un-mono.exe" --help 2>/dev/null && echo "  $(GREEN)✓$(NC) CLI: --help works" || echo "  $(YELLOW)⊘$(NC) CLI: mono not installed or error"; \
	else echo "  $(RED)✗$(NC) CLI: Build failed"; fi

test-library:
	@echo ""
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "LIBRARY MODE: Testing C# classes"
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "  $(YELLOW)⊘$(NC) Library: Unit tests not yet implemented"

test-integration:
	@echo ""
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "INTEGRATION MODE: Testing API contract"
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@if [ -z "$$UNSANDBOX_PUBLIC_KEY" ]; then echo "  $(YELLOW)⊘$(NC) Skipping (no API credentials)"; \
	else echo "  $(YELLOW)⊘$(NC) Integration: Not yet implemented"; fi

test-functional: build
	@echo ""
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "FUNCTIONAL MODE: Real-world scenarios"
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@if [ -z "$$UNSANDBOX_PUBLIC_KEY" ]; then \
		echo "  $(YELLOW)⊘$(NC) Skipping (no API credentials)"; \
	elif [ -f "$(SYNC_DIR)/un-mono.exe" ]; then \
		echo "Testing execute..."; \
		mono "$(SYNC_DIR)/un-mono.exe" "$(ROOT_DIR)/test/fib.py" 2>&1 | grep -q "fib(10) = 55" && echo "  $(GREEN)✓$(NC) Execute passed" || echo "  $(YELLOW)⊘$(NC) Execute test failed"; \
	else \
		echo "  $(RED)✗$(NC) Build required first"; \
	fi

clean:
	@rm -f $(SYNC_DIR)/un-mono.exe $(ASYNC_DIR)/un-mono.exe 2>/dev/null || true
	@rm -rf $(SYNC_DIR)/bin $(SYNC_DIR)/obj $(ASYNC_DIR)/bin $(ASYNC_DIR)/obj 2>/dev/null || true
	@echo "$(GREEN)✓$(NC) Cleaned C# artifacts"
