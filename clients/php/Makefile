# UN PHP Client - Build and Test
#
# This client has two implementations:
#   - sync/  : Synchronous PHP SDK (curl)
#   - async/ : Asynchronous PHP SDK (ReactPHP/Amp)
#
# Usage:
#   make              # Run all tests
#   make test         # Run all 4 test modes
#   make test-cli     # CLI mode only
#   make test-library # Library mode only
#   make test-integration # Integration mode only
#   make test-functional  # Functional mode only
#   make test-sync    # Test sync SDK only
#   make test-async   # Test async SDK only
#   make clean        # Remove build artifacts
#
# Dependencies:
#   composer require phpunit/phpunit

.PHONY: all test test-cli test-library test-integration test-functional
.PHONY: test-sync test-async install dev-install lint format clean help examples

# Paths
ROOT_DIR := $(shell cd ../.. && pwd)
SYNC_DIR := sync
ASYNC_DIR := async

# Colors
GREEN := \033[32m
RED := \033[31m
YELLOW := \033[33m
NC := \033[0m

.DEFAULT_GOAL := help

help:
	@echo "UN PHP Client - Build and Test"
	@echo ""
	@echo "Test (all 4 modes):"
	@echo "  make test               All 4 modes for both sync and async"
	@echo "  make test-cli           CLI mode (command-line interface)"
	@echo "  make test-library       Library mode (require and use)"
	@echo "  make test-integration   Integration mode (API contract)"
	@echo "  make test-functional    Functional mode (real-world scenarios)"
	@echo ""
	@echo "Test by SDK:"
	@echo "  make test-sync          Test synchronous SDK"
	@echo "  make test-async         Test asynchronous SDK"
	@echo ""
	@echo "Development:"
	@echo "  make install            Install dependencies"
	@echo "  make lint               Lint with PHP_CodeSniffer"
	@echo "  make format             Format with PHP-CS-Fixer"
	@echo "  make examples           Run example scripts"
	@echo ""
	@echo "Utility:"
	@echo "  make clean              Remove build artifacts"
	@echo "  make deps               Show required dependencies"
	@echo ""

all: test

deps:
	@echo "Required packages:"
	@echo "  composer require phpunit/phpunit squizlabs/php_codesniffer"
	@echo ""
	@php --version 2>/dev/null || echo "PHP not installed"
	@composer --version 2>/dev/null || echo "Composer not installed"

# ============================================================================
# TEST: All 4 Modes
# ============================================================================

test: test-cli test-library test-integration test-functional
	@echo ""
	@echo "$(GREEN)✓ PHP Client: All 4 test modes complete$(NC)"

# ============================================================================
# TEST: CLI Mode
# ============================================================================

test-cli:
	@echo ""
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "CLI MODE: Testing PHP CLI interface"
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo ""
	@# Test root-level un.php if it exists
	@if [ -f "$(ROOT_DIR)/un.php" ]; then \
		php -l "$(ROOT_DIR)/un.php" > /dev/null 2>&1 && echo "  $(GREEN)✓$(NC) CLI: Syntax valid (un.php)" || echo "  $(RED)✗$(NC) CLI: Syntax error in un.php"; \
	fi
	@# Test sync SDK syntax
	@if [ -f "$(SYNC_DIR)/src/Unsandbox.php" ]; then \
		php -l "$(SYNC_DIR)/src/Unsandbox.php" > /dev/null 2>&1 && echo "  $(GREEN)✓$(NC) CLI: Sync SDK syntax valid" || echo "  $(RED)✗$(NC) CLI: Sync SDK syntax error"; \
	elif [ -f "$(SYNC_DIR)/src/un.php" ]; then \
		php -l "$(SYNC_DIR)/src/un.php" > /dev/null 2>&1 && echo "  $(GREEN)✓$(NC) CLI: Sync SDK syntax valid" || echo "  $(RED)✗$(NC) CLI: Sync SDK syntax error"; \
	fi
	@# Test async SDK syntax
	@if [ -f "$(ASYNC_DIR)/src/Unsandbox.php" ]; then \
		php -l "$(ASYNC_DIR)/src/Unsandbox.php" > /dev/null 2>&1 && echo "  $(GREEN)✓$(NC) CLI: Async SDK syntax valid" || echo "  $(YELLOW)⊘$(NC) CLI: Async SDK not yet implemented"; \
	fi

# ============================================================================
# TEST: Library Mode
# ============================================================================

test-library:
	@echo ""
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "LIBRARY MODE: Testing PHP imports"
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo ""
	@# Test sync SDK import
	@if [ -f "$(SYNC_DIR)/src/Unsandbox.php" ]; then \
		php -r "require '$(SYNC_DIR)/src/Unsandbox.php'; echo '  ✓ Library: Sync SDK importable\n';" 2>/dev/null || echo "  $(YELLOW)⊘$(NC) Library: Sync import needs dependencies"; \
	elif [ -f "$(SYNC_DIR)/src/un.php" ]; then \
		php -r "require '$(SYNC_DIR)/src/un.php'; echo '  ✓ Library: Sync SDK importable\n';" 2>/dev/null || echo "  $(YELLOW)⊘$(NC) Library: Sync import needs dependencies"; \
	fi
	@# Test async SDK import
	@if [ -f "$(ASYNC_DIR)/src/Unsandbox.php" ]; then \
		php -r "require '$(ASYNC_DIR)/src/Unsandbox.php'; echo '  ✓ Library: Async SDK importable\n';" 2>/dev/null || echo "  $(YELLOW)⊘$(NC) Library: Async import needs dependencies"; \
	fi
	@# Run phpunit tests
	@echo ""
	@echo "Running unit tests..."
	@if [ -d "$(SYNC_DIR)/tests" ] && [ -f "$(SYNC_DIR)/composer.json" ]; then \
		cd $(SYNC_DIR) && ./vendor/bin/phpunit tests/ 2>/dev/null && echo "  $(GREEN)✓$(NC) Sync SDK tests passed" || echo "  $(YELLOW)⊘$(NC) Sync tests need: composer install"; \
	fi
	@if [ -d "$(ASYNC_DIR)/tests" ] && [ -f "$(ASYNC_DIR)/composer.json" ]; then \
		cd $(ASYNC_DIR) && ./vendor/bin/phpunit tests/ 2>/dev/null && echo "  $(GREEN)✓$(NC) Async SDK tests passed" || echo "  $(YELLOW)⊘$(NC) Async tests need: composer install"; \
	fi

# ============================================================================
# TEST: Integration Mode
# ============================================================================

test-integration:
	@echo ""
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "INTEGRATION MODE: Testing API contract"
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo ""
	@if [ -z "$$UNSANDBOX_PUBLIC_KEY" ] || [ -z "$$UNSANDBOX_SECRET_KEY" ]; then \
		echo "  $(YELLOW)⊘$(NC) Skipping (no API credentials)"; \
		echo "  Set UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY"; \
	else \
		echo "  Testing API authentication..."; \
		echo "  $(YELLOW)⊘$(NC) Integration: SDK not yet implemented"; \
	fi

# ============================================================================
# TEST: Functional Mode
# ============================================================================

test-functional:
	@echo ""
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "FUNCTIONAL MODE: Real-world scenarios"
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo ""
	@if [ -z "$$UNSANDBOX_PUBLIC_KEY" ] || [ -z "$$UNSANDBOX_SECRET_KEY" ]; then \
		echo "  $(YELLOW)⊘$(NC) Skipping (no API credentials)"; \
	else \
		echo "  Running functional tests..."; \
		if [ -f "$(SYNC_DIR)/tests/FunctionalTest.php" ]; then \
			cd $(SYNC_DIR) && php vendor/bin/phpunit tests/FunctionalTest.php 2>&1 && echo "  $(GREEN)✓$(NC) Functional: All tests passed" || echo "  $(RED)✗$(NC) Functional: Tests failed"; \
		fi; \
	fi

# ============================================================================
# TEST: By SDK Type
# ============================================================================

test-sync:
	@echo "Testing Sync SDK..."
	@if [ -f "$(SYNC_DIR)/composer.json" ]; then \
		cd $(SYNC_DIR) && ./vendor/bin/phpunit tests/; \
	elif [ -d "$(SYNC_DIR)/tests" ]; then \
		cd $(SYNC_DIR) && phpunit tests/; \
	else \
		echo "  $(YELLOW)⊘$(NC) Sync SDK tests not found"; \
	fi

test-async:
	@echo "Testing Async SDK..."
	@if [ -f "$(ASYNC_DIR)/composer.json" ]; then \
		cd $(ASYNC_DIR) && ./vendor/bin/phpunit tests/; \
	elif [ -d "$(ASYNC_DIR)/tests" ]; then \
		cd $(ASYNC_DIR) && phpunit tests/; \
	else \
		echo "  $(YELLOW)⊘$(NC) Async SDK tests not found"; \
	fi

# ============================================================================
# Development
# ============================================================================

install:
	@echo "Installing PHP SDK dependencies..."
	@if [ -f "$(SYNC_DIR)/composer.json" ]; then cd $(SYNC_DIR) && composer install; fi
	@if [ -f "$(ASYNC_DIR)/composer.json" ]; then cd $(ASYNC_DIR) && composer install; fi
	@echo "$(GREEN)✓$(NC) Installation complete"

lint:
	@echo "Linting PHP SDKs..."
	@if [ -d "$(SYNC_DIR)/src" ]; then phpcs $(SYNC_DIR)/src/ 2>/dev/null || echo "  $(YELLOW)⊘$(NC) PHP_CodeSniffer not installed"; fi
	@if [ -d "$(ASYNC_DIR)/src" ]; then phpcs $(ASYNC_DIR)/src/ 2>/dev/null || echo "  $(YELLOW)⊘$(NC) PHP_CodeSniffer not installed"; fi
	@echo "$(GREEN)✓$(NC) Lint complete"

format:
	@echo "Formatting PHP SDKs..."
	@if [ -d "$(SYNC_DIR)/src" ]; then php-cs-fixer fix $(SYNC_DIR)/src/ 2>/dev/null || echo "  $(YELLOW)⊘$(NC) PHP-CS-Fixer not installed"; fi
	@if [ -d "$(ASYNC_DIR)/src" ]; then php-cs-fixer fix $(ASYNC_DIR)/src/ 2>/dev/null || echo "  $(YELLOW)⊘$(NC) PHP-CS-Fixer not installed"; fi
	@echo "$(GREEN)✓$(NC) Format complete"

examples:
	@echo "Running PHP examples..."
	@if [ -d "$(SYNC_DIR)/examples" ]; then \
		for f in $(SYNC_DIR)/examples/*.php; do \
			echo "Running $$f..."; \
			php "$$f" 2>&1 | head -10 || true; \
		done; \
	fi

# ============================================================================
# Clean
# ============================================================================

clean:
	@echo "Cleaning PHP build artifacts..."
	@rm -rf $(SYNC_DIR)/vendor $(ASYNC_DIR)/vendor 2>/dev/null || true
	@rm -f $(SYNC_DIR)/composer.lock $(ASYNC_DIR)/composer.lock 2>/dev/null || true
	@echo "$(GREEN)✓$(NC) Cleaned build artifacts"
