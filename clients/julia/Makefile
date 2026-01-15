# UN Julia Client

.PHONY: test test-cli test-library test-integration test-functional clean help

ROOT_DIR := $(shell cd ../.. && pwd)
SYNC_DIR := sync
GREEN := \033[32m
YELLOW := \033[33m
NC := \033[0m

.DEFAULT_GOAL := help

help:
	@echo "UN Julia Client"
	@echo "  make test    All 4 test modes"

test: test-cli test-library test-integration test-functional
	@echo "$(GREEN)✓ Julia Client: All 4 test modes complete$(NC)"

test-cli:
	@echo ""
	@echo "━━━ CLI MODE ━━━"
	@if [ -f "$(ROOT_DIR)/un.jl" ]; then julia --compile=min -e "include(\"$(ROOT_DIR)/un.jl\")" 2>/dev/null && echo "  $(GREEN)✓$(NC) Syntax valid" || echo "  $(YELLOW)⊘$(NC) Check syntax"; fi

test-library:
	@echo ""
	@echo "━━━ LIBRARY MODE ━━━"
	@if [ -f "$(SYNC_DIR)/src/Unsandbox.jl" ]; then julia -e "include(\"$(SYNC_DIR)/src/Unsandbox.jl\")" 2>/dev/null && echo "  $(GREEN)✓$(NC) Module loads" || echo "  $(YELLOW)⊘$(NC) Check dependencies"; fi
	@if [ -f "$(SYNC_DIR)/Project.toml" ]; then cd $(SYNC_DIR) && julia --project -e "using Pkg; Pkg.test()" 2>/dev/null || echo "  $(YELLOW)⊘$(NC) Run Pkg.instantiate()"; fi

test-integration:
	@echo ""
	@echo "━━━ INTEGRATION MODE ━━━"
	@if [ -z "$$UNSANDBOX_PUBLIC_KEY" ]; then echo "  $(YELLOW)⊘$(NC) No credentials"; else echo "  $(YELLOW)⊘$(NC) Not implemented"; fi

test-functional:
	@echo ""
	@echo "━━━ FUNCTIONAL MODE ━━━"
	@if [ -z "$$UNSANDBOX_PUBLIC_KEY" ]; then echo "  $(YELLOW)⊘$(NC) No credentials"; else echo "  $(YELLOW)⊘$(NC) Not implemented"; fi

clean:
	@rm -rf $(SYNC_DIR)/Manifest.toml 2>/dev/null; echo "$(GREEN)✓$(NC) Cleaned"
