# UN CLI - .NET 10 Implementation (sync and async)

.PHONY: build build-sync build-async run clean test test-sync test-async help

SYNC_DIR := sync/src
ASYNC_DIR := async/src

GREEN := \033[32m
YELLOW := \033[33m
NC := \033[0m

build: build-sync build-async

build-sync:
	@echo "$(YELLOW)Building .NET 10 sync version...$(NC)"
	cd $(SYNC_DIR) && dotnet build -c Release
	@echo "$(GREEN)✓ Sync build complete$(NC)"

build-async:
	@echo "$(YELLOW)Building .NET 10 async version...$(NC)"
	cd $(ASYNC_DIR) && dotnet build -c Release
	@echo "$(GREEN)✓ Async build complete$(NC)"

run-sync:
	cd $(SYNC_DIR) && dotnet run --

run-async:
	cd $(ASYNC_DIR) && dotnet run --

clean:
	cd $(SYNC_DIR) && dotnet clean 2>/dev/null || true
	cd $(ASYNC_DIR) && dotnet clean 2>/dev/null || true
	rm -rf $(SYNC_DIR)/bin $(SYNC_DIR)/obj
	rm -rf $(ASYNC_DIR)/bin $(ASYNC_DIR)/obj

test: test-sync test-async
	@echo "$(GREEN)✓ .NET 10: All tests complete$(NC)"

test-sync: build-sync
	@echo ""
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "Testing .NET 10 SYNC version"
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "Testing --help..."
	cd $(SYNC_DIR) && dotnet run -- --help
	@echo ""
	@echo "Testing --version..."
	cd $(SYNC_DIR) && dotnet run -- --version
	@echo "$(GREEN)✓ Sync tests passed$(NC)"

test-async: build-async
	@echo ""
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "Testing .NET 10 ASYNC version"
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "Testing --help..."
	cd $(ASYNC_DIR) && dotnet run -- --help
	@echo ""
	@echo "Testing --version..."
	cd $(ASYNC_DIR) && dotnet run -- --version
	@echo "$(GREEN)✓ Async tests passed$(NC)"

test-cli: build
	@echo "CLI tests require UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY"
	@if [ -n "$$UNSANDBOX_PUBLIC_KEY" ]; then \
		echo "Testing sync key validation..."; \
		cd $(SYNC_DIR) && dotnet run -- key; \
		echo "Testing async key validation..."; \
		cd $(ASYNC_DIR) && dotnet run -- key; \
	else \
		echo "$(YELLOW)⊘ Skipping (no API credentials)$(NC)"; \
	fi

test-functional: build
	@if [ -z "$$UNSANDBOX_PUBLIC_KEY" ]; then \
		echo "$(YELLOW)⊘ Skipping functional tests (no API credentials)$(NC)"; \
	else \
		echo "Testing sync execute..."; \
		cd $(SYNC_DIR) && dotnet run -- ../../test/fib.py 2>&1 | grep -q "fib(10) = 55" && echo "$(GREEN)✓ Sync execute passed$(NC)"; \
		echo "Testing async execute..."; \
		cd $(ASYNC_DIR) && dotnet run -- ../../test/fib.py 2>&1 | grep -q "fib(10) = 55" && echo "$(GREEN)✓ Async execute passed$(NC)"; \
	fi

help:
	@echo "UN CLI (.NET 10) Makefile"
	@echo ""
	@echo "Targets:"
	@echo "  build          Build both sync and async versions"
	@echo "  build-sync     Build sync version only"
	@echo "  build-async    Build async version only"
	@echo "  run-sync       Run sync CLI"
	@echo "  run-async      Run async CLI"
	@echo "  clean          Clean build artifacts"
	@echo "  test           Run basic tests (both versions)"
	@echo "  test-sync      Test sync version"
	@echo "  test-async     Test async version"
	@echo "  test-cli       Run CLI tests (requires API keys)"
	@echo "  test-functional Run functional tests (requires API keys)"
	@echo "  help           Show this help"
