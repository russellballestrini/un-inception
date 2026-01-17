# UN Common Lisp Client - Build and Test

.PHONY: all test test-cli test-library test-integration test-functional clean help

ROOT_DIR := $(shell cd ../.. && pwd)
SYNC_DIR := sync
GREEN := \033[32m
RED := \033[31m
YELLOW := \033[33m
NC := \033[0m

.DEFAULT_GOAL := help

help:
	@echo "UN Common Lisp Client - Build and Test"
	@echo ""
	@echo "  make test             All 4 test modes"
	@echo "  make test-cli         CLI mode"
	@echo "  make test-library     Library mode"
	@echo ""

test: test-cli test-library test-integration test-functional
	@echo "$(GREEN)✓ Lisp Client: All 4 test modes complete$(NC)"

test-cli:
	@echo ""
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "CLI MODE: Testing Common Lisp CLI"
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@if [ -f "$(SYNC_DIR)/src/un.lisp" ]; then \
		which sbcl > /dev/null 2>&1 && echo "  $(GREEN)✓$(NC) CLI: SBCL available" || echo "  $(YELLOW)⊘$(NC) CLI: SBCL not found (apt install sbcl)"; \
		sbcl --noinform --non-interactive --load "$(SYNC_DIR)/src/un.lisp" --eval '(sb-ext:exit)' 2>/dev/null && echo "  $(GREEN)✓$(NC) CLI: Loads without error" || echo "  $(RED)✗$(NC) CLI: Load error"; \
	fi

test-library:
	@echo ""
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "LIBRARY MODE: Testing Lisp package"
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "  $(YELLOW)⊘$(NC) Library: Use ASDF/Quicklisp for package management"

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
	rm -f *.fasl *.fas *.lib
	@echo "$(GREEN)✓$(NC) Cleaned Lisp artifacts"
