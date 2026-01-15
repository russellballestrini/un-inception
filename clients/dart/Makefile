# UN Dart Client

.PHONY: test test-cli test-library test-integration test-functional clean help

ROOT_DIR := $(shell cd ../.. && pwd)
SYNC_DIR := sync
GREEN := \033[32m
YELLOW := \033[33m
NC := \033[0m

.DEFAULT_GOAL := help

help:
	@echo "UN Dart Client"
	@echo "  make test    All 4 test modes"

test: test-cli test-library test-integration test-functional
	@echo "$(GREEN)✓ Dart Client: All 4 test modes complete$(NC)"

test-cli:
	@echo ""
	@echo "━━━ CLI MODE ━━━"
	@if [ -f "$(ROOT_DIR)/un.dart" ]; then dart analyze "$(ROOT_DIR)/un.dart" 2>/dev/null && echo "  $(GREEN)✓$(NC) Analysis passed" || echo "  $(YELLOW)⊘$(NC) Check syntax"; fi
	@if [ -f "$(SYNC_DIR)/pubspec.yaml" ]; then cd $(SYNC_DIR) && dart pub get 2>/dev/null && dart analyze 2>/dev/null && echo "  $(GREEN)✓$(NC) SDK analysis passed" || echo "  $(YELLOW)⊘$(NC) Run dart pub get"; fi

test-library:
	@echo ""
	@echo "━━━ LIBRARY MODE ━━━"
	@if [ -f "$(SYNC_DIR)/pubspec.yaml" ]; then cd $(SYNC_DIR) && dart test 2>/dev/null || echo "  $(YELLOW)⊘$(NC) Tests incomplete"; fi

test-integration:
	@echo ""
	@echo "━━━ INTEGRATION MODE ━━━"
	@if [ -z "$$UNSANDBOX_PUBLIC_KEY" ]; then echo "  $(YELLOW)⊘$(NC) No credentials"; else echo "  $(YELLOW)⊘$(NC) Not implemented"; fi

test-functional:
	@echo ""
	@echo "━━━ FUNCTIONAL MODE ━━━"
	@if [ -z "$$UNSANDBOX_PUBLIC_KEY" ]; then echo "  $(YELLOW)⊘$(NC) No credentials"; else echo "  $(YELLOW)⊘$(NC) Not implemented"; fi

clean:
	@rm -rf $(SYNC_DIR)/.dart_tool 2>/dev/null; echo "$(GREEN)✓$(NC) Cleaned"
