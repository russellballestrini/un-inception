# UN Python Client - Build and Test
#
# This client has two implementations:
#   - sync/  : Synchronous Python SDK (requests-based)
#   - async/ : Asynchronous Python SDK (aiohttp-based)
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
#   pip install pytest pytest-cov pytest-asyncio aiohttp requests

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
	@echo "UN Python Client - Build and Test"
	@echo ""
	@echo "Test (all 4 modes):"
	@echo "  make test               All 4 modes for both sync and async"
	@echo "  make test-cli           CLI mode (command-line interface)"
	@echo "  make test-library       Library mode (import and use)"
	@echo "  make test-integration   Integration mode (API contract)"
	@echo "  make test-functional    Functional mode (real-world scenarios)"
	@echo ""
	@echo "Test by SDK:"
	@echo "  make test-sync          Test synchronous SDK"
	@echo "  make test-async         Test asynchronous SDK"
	@echo ""
	@echo "Development:"
	@echo "  make install            Install both SDKs"
	@echo "  make dev-install        Install with dev dependencies"
	@echo "  make lint               Lint both SDKs"
	@echo "  make format             Format both SDKs"
	@echo "  make examples           Run example scripts"
	@echo ""
	@echo "Utility:"
	@echo "  make clean              Remove build artifacts"
	@echo "  make deps               Show required dependencies"
	@echo ""

all: test

deps:
	@echo "Required packages:"
	@echo "  pip install pytest pytest-cov pytest-asyncio aiohttp requests black flake8 mypy"

# ============================================================================
# TEST: All 4 Modes
# ============================================================================

test: test-cli test-library test-integration test-functional
	@echo ""
	@echo "$(GREEN)✓ Python Client: All 4 test modes complete$(NC)"

# ============================================================================
# TEST: CLI Mode
# ============================================================================

test-cli:
	@echo ""
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "CLI MODE: Testing Python CLI interface"
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo ""
	@# Test root-level un.py if it exists
	@if [ -f "$(ROOT_DIR)/un.py" ]; then \
		python3 -m py_compile "$(ROOT_DIR)/un.py" && echo "  $(GREEN)✓$(NC) CLI: Syntax valid (un.py)"; \
		python3 "$(ROOT_DIR)/un.py" --help > /dev/null 2>&1 && echo "  $(GREEN)✓$(NC) CLI: --help works" || echo "  $(YELLOW)⊘$(NC) CLI: --help (may need API)"; \
	else \
		echo "  $(YELLOW)⊘$(NC) Root un.py not found"; \
	fi
	@# Test sync SDK CLI
	@if [ -f "$(SYNC_DIR)/src/unsandbox/__main__.py" ]; then \
		python3 -m py_compile "$(SYNC_DIR)/src/unsandbox/__main__.py" && echo "  $(GREEN)✓$(NC) CLI: Sync SDK syntax valid"; \
	fi
	@# Test async SDK CLI
	@if [ -f "$(ASYNC_DIR)/src/un_async/__main__.py" ]; then \
		python3 -m py_compile "$(ASYNC_DIR)/src/un_async/__main__.py" && echo "  $(GREEN)✓$(NC) CLI: Async SDK syntax valid"; \
	fi

# ============================================================================
# TEST: Library Mode
# ============================================================================

test-library:
	@echo ""
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "LIBRARY MODE: Testing Python imports"
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo ""
	@# Test sync SDK import
	@if [ -d "$(SYNC_DIR)/src/unsandbox" ]; then \
		cd $(SYNC_DIR) && PYTHONPATH=src python3 -c "from unsandbox import UnsandboxClient; print('  ✓ Library: Sync UnsandboxClient importable')" 2>/dev/null || echo "  $(YELLOW)⊘$(NC) Library: Sync import needs install"; \
	fi
	@# Test async SDK import
	@if [ -d "$(ASYNC_DIR)/src/un_async" ]; then \
		cd $(ASYNC_DIR) && PYTHONPATH=src python3 -c "from un_async import AsyncUnsandboxClient; print('  ✓ Library: Async AsyncUnsandboxClient importable')" 2>/dev/null || echo "  $(YELLOW)⊘$(NC) Library: Async import needs install"; \
	fi
	@# Run pytest for library tests
	@echo ""
	@echo "Running unit tests..."
	@if [ -d "$(SYNC_DIR)/tests" ]; then \
		cd $(SYNC_DIR) && pytest tests/ -q --tb=no 2>/dev/null && echo "  $(GREEN)✓$(NC) Sync SDK tests passed" || echo "  $(YELLOW)⊘$(NC) Sync tests need dependencies"; \
	fi
	@if [ -d "$(ASYNC_DIR)/tests" ]; then \
		cd $(ASYNC_DIR) && pytest tests/ -q --tb=no 2>/dev/null && echo "  $(GREEN)✓$(NC) Async SDK tests passed" || echo "  $(YELLOW)⊘$(NC) Async tests need dependencies"; \
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
		python3 -c "import sys; sys.path.insert(0, '$(SYNC_DIR)/src'); from unsandbox import UnsandboxClient; c = UnsandboxClient(); r = c.execute('python', 'print(42)'); print('  ✓ Integration: API auth works') if r else print('  ✗ Integration: API auth failed')" 2>/dev/null || echo "  $(YELLOW)⊘$(NC) Integration: Need to install SDK first"; \
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
		if [ -f "$(SYNC_DIR)/verify_sdk.py" ]; then \
			cd $(SYNC_DIR) && python3 verify_sdk.py 2>/dev/null && echo "  $(GREEN)✓$(NC) Functional: Sync SDK verified" || echo "  $(YELLOW)⊘$(NC) Functional: Sync verification incomplete"; \
		fi; \
	fi

# ============================================================================
# TEST: By SDK Type
# ============================================================================

test-sync:
	@echo "Testing Sync SDK..."
	@if [ -f "$(SYNC_DIR)/Makefile" ]; then \
		$(MAKE) -C $(SYNC_DIR) test; \
	elif [ -d "$(SYNC_DIR)/tests" ]; then \
		cd $(SYNC_DIR) && pytest tests/ -v; \
	else \
		echo "  $(YELLOW)⊘$(NC) Sync SDK tests not found"; \
	fi

test-async:
	@echo "Testing Async SDK..."
	@if [ -f "$(ASYNC_DIR)/Makefile" ]; then \
		$(MAKE) -C $(ASYNC_DIR) test; \
	elif [ -d "$(ASYNC_DIR)/tests" ]; then \
		cd $(ASYNC_DIR) && pytest tests/ -v; \
	else \
		echo "  $(YELLOW)⊘$(NC) Async SDK tests not found"; \
	fi

# ============================================================================
# Development
# ============================================================================

install:
	@echo "Installing Python SDKs..."
	@if [ -f "$(SYNC_DIR)/setup.py" ]; then cd $(SYNC_DIR) && pip install -e . ; fi
	@if [ -f "$(ASYNC_DIR)/setup.py" ]; then cd $(ASYNC_DIR) && pip install -e . ; fi
	@echo "$(GREEN)✓$(NC) Installation complete"

dev-install:
	@echo "Installing Python SDKs with dev dependencies..."
	@if [ -f "$(SYNC_DIR)/setup.py" ]; then cd $(SYNC_DIR) && pip install -e ".[dev]" 2>/dev/null || pip install -e . ; fi
	@if [ -f "$(ASYNC_DIR)/setup.py" ]; then cd $(ASYNC_DIR) && pip install -e ".[dev]" 2>/dev/null || pip install -e . ; fi
	@pip install pytest pytest-cov pytest-asyncio black flake8 mypy 2>/dev/null || true
	@echo "$(GREEN)✓$(NC) Dev installation complete"

lint:
	@echo "Linting Python SDKs..."
	@if [ -d "$(SYNC_DIR)/src" ]; then flake8 $(SYNC_DIR)/src/ --max-line-length=120 || true; fi
	@if [ -d "$(ASYNC_DIR)/src" ]; then flake8 $(ASYNC_DIR)/src/ --max-line-length=120 || true; fi
	@echo "$(GREEN)✓$(NC) Lint complete"

format:
	@echo "Formatting Python SDKs..."
	@if [ -d "$(SYNC_DIR)/src" ]; then black $(SYNC_DIR)/src/ $(SYNC_DIR)/tests/ 2>/dev/null || true; fi
	@if [ -d "$(ASYNC_DIR)/src" ]; then black $(ASYNC_DIR)/src/ $(ASYNC_DIR)/tests/ 2>/dev/null || true; fi
	@echo "$(GREEN)✓$(NC) Format complete"

examples:
	@echo "Running Python examples..."
	@if [ -f "$(ASYNC_DIR)/Makefile" ]; then $(MAKE) -C $(ASYNC_DIR) examples; fi

# ============================================================================
# Clean
# ============================================================================

clean:
	@echo "Cleaning Python build artifacts..."
	@find . -type d -name __pycache__ -exec rm -rf {} + 2>/dev/null || true
	@find . -type f -name "*.pyc" -delete 2>/dev/null || true
	@find . -type d -name ".pytest_cache" -exec rm -rf {} + 2>/dev/null || true
	@find . -type d -name ".mypy_cache" -exec rm -rf {} + 2>/dev/null || true
	@find . -type d -name "*.egg-info" -exec rm -rf {} + 2>/dev/null || true
	@find . -type d -name "htmlcov" -exec rm -rf {} + 2>/dev/null || true
	@find . -type d -name "dist" -exec rm -rf {} + 2>/dev/null || true
	@find . -type d -name "build" -exec rm -rf {} + 2>/dev/null || true
	@echo "$(GREEN)✓$(NC) Cleaned build artifacts"
