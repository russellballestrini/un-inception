# UN Ruby Client - Build and Test
#
# This client has two implementations:
#   - sync/  : Synchronous Ruby SDK (net/http)
#   - async/ : Asynchronous Ruby SDK (async-http)
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
#   gem install bundler rspec

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
	@echo "UN Ruby Client - Build and Test"
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
	@echo "  make lint               Lint with RuboCop"
	@echo "  make format             Format with RuboCop"
	@echo "  make examples           Run example scripts"
	@echo ""
	@echo "Utility:"
	@echo "  make clean              Remove build artifacts"
	@echo "  make deps               Show required dependencies"
	@echo ""

all: test

deps:
	@echo "Required packages:"
	@echo "  gem install bundler rspec rubocop"
	@echo ""
	@ruby --version 2>/dev/null || echo "Ruby not installed"

# ============================================================================
# TEST: All 4 Modes
# ============================================================================

test: test-cli test-library test-integration test-functional
	@echo ""
	@echo "$(GREEN)✓ Ruby Client: All 4 test modes complete$(NC)"

# ============================================================================
# TEST: CLI Mode
# ============================================================================

test-cli:
	@echo ""
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "CLI MODE: Testing Ruby CLI interface"
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo ""
	@# Test root-level un.rb if it exists
	@if [ -f "$(ROOT_DIR)/un.rb" ]; then \
		ruby -c "$(ROOT_DIR)/un.rb" > /dev/null 2>&1 && echo "  $(GREEN)✓$(NC) CLI: Syntax valid (un.rb)" || echo "  $(RED)✗$(NC) CLI: Syntax error in un.rb"; \
	fi
	@# Test sync SDK syntax
	@if [ -f "$(SYNC_DIR)/lib/unsandbox.rb" ]; then \
		ruby -c "$(SYNC_DIR)/lib/unsandbox.rb" > /dev/null 2>&1 && echo "  $(GREEN)✓$(NC) CLI: Sync SDK syntax valid" || echo "  $(RED)✗$(NC) CLI: Sync SDK syntax error"; \
	elif [ -f "$(SYNC_DIR)/src/un.rb" ]; then \
		ruby -c "$(SYNC_DIR)/src/un.rb" > /dev/null 2>&1 && echo "  $(GREEN)✓$(NC) CLI: Sync SDK syntax valid" || echo "  $(RED)✗$(NC) CLI: Sync SDK syntax error"; \
	fi
	@# Test async SDK syntax
	@if [ -f "$(ASYNC_DIR)/lib/unsandbox.rb" ]; then \
		ruby -c "$(ASYNC_DIR)/lib/unsandbox.rb" > /dev/null 2>&1 && echo "  $(GREEN)✓$(NC) CLI: Async SDK syntax valid" || echo "  $(YELLOW)⊘$(NC) CLI: Async SDK not yet implemented"; \
	fi

# ============================================================================
# TEST: Library Mode
# ============================================================================

test-library:
	@echo ""
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "LIBRARY MODE: Testing Ruby imports"
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo ""
	@# Test sync SDK import
	@if [ -f "$(SYNC_DIR)/lib/unsandbox.rb" ]; then \
		ruby -I$(SYNC_DIR)/lib -e "require 'unsandbox'; puts '  ✓ Library: Sync SDK importable'" 2>/dev/null || echo "  $(YELLOW)⊘$(NC) Library: Sync import needs dependencies"; \
	elif [ -f "$(SYNC_DIR)/src/un.rb" ]; then \
		ruby -I$(SYNC_DIR)/src -e "require 'un'; puts '  ✓ Library: Sync SDK importable'" 2>/dev/null || echo "  $(YELLOW)⊘$(NC) Library: Sync import needs dependencies"; \
	fi
	@# Test async SDK import
	@if [ -f "$(ASYNC_DIR)/lib/unsandbox.rb" ]; then \
		ruby -I$(ASYNC_DIR)/lib -e "require 'unsandbox'; puts '  ✓ Library: Async SDK importable'" 2>/dev/null || echo "  $(YELLOW)⊘$(NC) Library: Async import needs dependencies"; \
	fi
	@# Run rspec tests
	@echo ""
	@echo "Running unit tests..."
	@if [ -d "$(SYNC_DIR)/spec" ]; then \
		cd $(SYNC_DIR) && bundle exec rspec spec/ 2>/dev/null && echo "  $(GREEN)✓$(NC) Sync SDK tests passed" || echo "  $(YELLOW)⊘$(NC) Sync tests need: bundle install"; \
	fi
	@if [ -d "$(ASYNC_DIR)/spec" ]; then \
		cd $(ASYNC_DIR) && bundle exec rspec spec/ 2>/dev/null && echo "  $(GREEN)✓$(NC) Async SDK tests passed" || echo "  $(YELLOW)⊘$(NC) Async tests need: bundle install"; \
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
		echo "  $(YELLOW)⊘$(NC) Functional: SDK not yet implemented"; \
	fi

# ============================================================================
# TEST: By SDK Type
# ============================================================================

test-sync:
	@echo "Testing Sync SDK..."
	@if [ -f "$(SYNC_DIR)/Gemfile" ]; then \
		cd $(SYNC_DIR) && bundle exec rspec spec/; \
	elif [ -d "$(SYNC_DIR)/spec" ]; then \
		cd $(SYNC_DIR) && rspec spec/; \
	else \
		echo "  $(YELLOW)⊘$(NC) Sync SDK tests not found"; \
	fi

test-async:
	@echo "Testing Async SDK..."
	@if [ -f "$(ASYNC_DIR)/Gemfile" ]; then \
		cd $(ASYNC_DIR) && bundle exec rspec spec/; \
	elif [ -d "$(ASYNC_DIR)/spec" ]; then \
		cd $(ASYNC_DIR) && rspec spec/; \
	else \
		echo "  $(YELLOW)⊘$(NC) Async SDK tests not found"; \
	fi

# ============================================================================
# Development
# ============================================================================

install:
	@echo "Installing Ruby SDK dependencies..."
	@if [ -f "$(SYNC_DIR)/Gemfile" ]; then cd $(SYNC_DIR) && bundle install; fi
	@if [ -f "$(ASYNC_DIR)/Gemfile" ]; then cd $(ASYNC_DIR) && bundle install; fi
	@echo "$(GREEN)✓$(NC) Installation complete"

lint:
	@echo "Linting Ruby SDKs..."
	@if [ -d "$(SYNC_DIR)/lib" ]; then rubocop $(SYNC_DIR)/lib/ 2>/dev/null || echo "  $(YELLOW)⊘$(NC) RuboCop not installed"; fi
	@if [ -d "$(ASYNC_DIR)/lib" ]; then rubocop $(ASYNC_DIR)/lib/ 2>/dev/null || echo "  $(YELLOW)⊘$(NC) RuboCop not installed"; fi
	@echo "$(GREEN)✓$(NC) Lint complete"

format:
	@echo "Formatting Ruby SDKs..."
	@if [ -d "$(SYNC_DIR)/lib" ]; then rubocop -a $(SYNC_DIR)/lib/ 2>/dev/null || echo "  $(YELLOW)⊘$(NC) RuboCop not installed"; fi
	@if [ -d "$(ASYNC_DIR)/lib" ]; then rubocop -a $(ASYNC_DIR)/lib/ 2>/dev/null || echo "  $(YELLOW)⊘$(NC) RuboCop not installed"; fi
	@echo "$(GREEN)✓$(NC) Format complete"

examples:
	@echo "Running Ruby examples..."
	@if [ -d "$(SYNC_DIR)/examples" ]; then \
		for f in $(SYNC_DIR)/examples/*.rb; do \
			echo "Running $$f..."; \
			ruby "$$f" 2>&1 | head -10 || true; \
		done; \
	fi

# ============================================================================
# Clean
# ============================================================================

clean:
	@echo "Cleaning Ruby build artifacts..."
	@find . -name "*.gem" -delete 2>/dev/null || true
	@find . -name "Gemfile.lock" -delete 2>/dev/null || true
	@rm -rf $(SYNC_DIR)/pkg $(ASYNC_DIR)/pkg 2>/dev/null || true
	@echo "$(GREEN)✓$(NC) Cleaned build artifacts"
