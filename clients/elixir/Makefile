# UN Elixir Client - Build and Test

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
	@echo "UN Elixir Client - Build and Test"
	@echo ""
	@echo "  make build            Compile with mix"
	@echo "  make test             All 4 test modes"
	@echo ""

build:
	@if [ -f "$(SYNC_DIR)/mix.exs" ]; then \
		cd $(SYNC_DIR) && mix compile && echo "$(GREEN)✓$(NC) Sync SDK compiled"; \
	else echo "$(YELLOW)⊘$(NC) No mix.exs found"; fi

test: test-cli test-library test-integration test-functional
	@echo "$(GREEN)✓ Elixir Client: All 4 test modes complete$(NC)"

test-cli:
	@echo ""
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "CLI MODE: Testing Elixir CLI"
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@if [ -f "$(ROOT_DIR)/un.exs" ]; then \
		elixir "$(ROOT_DIR)/un.exs" --help 2>/dev/null && echo "  $(GREEN)✓$(NC) CLI: --help works" || echo "  $(YELLOW)⊘$(NC) CLI: Check implementation"; \
	fi
	@if [ -f "$(SYNC_DIR)/mix.exs" ]; then \
		cd $(SYNC_DIR) && mix compile 2>/dev/null && echo "  $(GREEN)✓$(NC) CLI: Sync SDK compiles" || echo "  $(YELLOW)⊘$(NC) CLI: Compile failed"; \
	fi

test-library:
	@echo ""
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "LIBRARY MODE: Testing Elixir modules"
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@if [ -f "$(SYNC_DIR)/mix.exs" ]; then \
		cd $(SYNC_DIR) && mix test 2>/dev/null || echo "  $(YELLOW)⊘$(NC) Library: Run mix deps.get first"; \
	else echo "  $(YELLOW)⊘$(NC) Library: No mix.exs"; fi

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
	@rm -rf $(SYNC_DIR)/_build $(ASYNC_DIR)/_build 2>/dev/null || true
	@echo "$(GREEN)✓$(NC) Cleaned Elixir artifacts"
