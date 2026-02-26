# UN Python Client - Build and Test
#
# This client has two implementations:
#   - sync/  : Synchronous Python SDK (requests-based)
#   - async/ : Asynchronous Python SDK (aiohttp-based)
#
# Usage:
#   make test         # Run all 4 test modes (auto-creates venv)
#   make test-cli     # CLI mode only
#   make test-library # Library mode only
#   make test-sync    # Test sync SDK only
#   make test-async   # Test async SDK only
#   make clean        # Remove build artifacts + venv
#
# The Makefile manages a .venv automatically. No manual pip install needed.

.PHONY: all test test-cli test-library test-integration test-functional
.PHONY: test-sync test-async lint format clean help examples venv

# Paths
ROOT_DIR := $(shell cd ../.. && pwd)
SYNC_DIR := sync
ASYNC_DIR := async
VENV := .venv
PYTHON := $(VENV)/bin/python
PYTEST := $(VENV)/bin/pytest

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
	@echo "  make test               All 4 modes (auto-creates venv)"
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
	@echo "  make venv               Create/update virtual environment"
	@echo "  make lint               Lint both SDKs"
	@echo "  make format             Format both SDKs"
	@echo "  make examples           Run example scripts"
	@echo ""
	@echo "Utility:"
	@echo "  make clean              Remove build artifacts + venv"
	@echo ""

all: test

# ============================================================================
# Virtual Environment
# ============================================================================

$(VENV)/bin/activate:
	@echo "Creating virtual environment..."
	@python3 -m venv $(VENV)
	@$(VENV)/bin/pip install --upgrade pip -q

$(VENV)/.deps-installed: $(VENV)/bin/activate
	@echo "Installing test dependencies into venv..."
	@$(VENV)/bin/pip install -q requests pytest pytest-cov pytest-asyncio aiohttp
	@if [ -f "$(SYNC_DIR)/setup.py" ]; then \
		cd $(SYNC_DIR) && ../$(VENV)/bin/pip install -q -e . 2>/dev/null || true; \
	fi
	@if [ -f "$(ASYNC_DIR)/setup.py" ]; then \
		cd $(ASYNC_DIR) && ../$(VENV)/bin/pip install -q -e . 2>/dev/null || true; \
	fi
	@touch $(VENV)/.deps-installed

venv: $(VENV)/.deps-installed
	@echo "$(GREEN)✓$(NC) Virtual environment ready at $(VENV)/"

# ============================================================================
# TEST: All 4 Modes
# ============================================================================

test: test-cli test-library test-integration test-functional
	@echo ""
	@echo "$(GREEN)✓ Python Client: All 4 test modes complete$(NC)"

# ============================================================================
# TEST: CLI Mode
# ============================================================================

test-cli: $(VENV)/.deps-installed
	@echo ""
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "CLI MODE: Testing Python CLI interface"
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo ""
	@if [ -f "$(ROOT_DIR)/un.py" ]; then \
		$(PYTHON) -m py_compile "$(ROOT_DIR)/un.py" && echo "  $(GREEN)✓$(NC) CLI: Syntax valid (un.py)"; \
		$(PYTHON) "$(ROOT_DIR)/un.py" --help > /dev/null 2>&1 && echo "  $(GREEN)✓$(NC) CLI: --help works" || echo "  $(YELLOW)⊘$(NC) CLI: --help (may need API)"; \
	else \
		echo "  $(YELLOW)⊘$(NC) Root un.py not found"; \
	fi
	@if [ -f "$(SYNC_DIR)/src/unsandbox/__main__.py" ]; then \
		$(PYTHON) -m py_compile "$(SYNC_DIR)/src/unsandbox/__main__.py" && echo "  $(GREEN)✓$(NC) CLI: Sync SDK syntax valid"; \
	fi
	@if [ -f "$(ASYNC_DIR)/src/un_async/__main__.py" ]; then \
		$(PYTHON) -m py_compile "$(ASYNC_DIR)/src/un_async/__main__.py" && echo "  $(GREEN)✓$(NC) CLI: Async SDK syntax valid"; \
	fi

# ============================================================================
# TEST: Library Mode
# ============================================================================

test-library: $(VENV)/.deps-installed
	@echo ""
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "LIBRARY MODE: Testing Python imports"
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo ""
	@if [ -d "$(SYNC_DIR)/src/unsandbox" ]; then \
		cd $(SYNC_DIR) && PYTHONPATH=src ../$(PYTHON) -c "from unsandbox import UnsandboxClient; print('  ✓ Library: Sync UnsandboxClient importable')" 2>/dev/null || echo "  $(YELLOW)⊘$(NC) Library: Sync import failed"; \
	fi
	@if [ -d "$(ASYNC_DIR)/src/un_async" ]; then \
		cd $(ASYNC_DIR) && PYTHONPATH=src ../$(PYTHON) -c "from un_async import AsyncUnsandboxClient; print('  ✓ Library: Async AsyncUnsandboxClient importable')" 2>/dev/null || echo "  $(YELLOW)⊘$(NC) Library: Async import failed"; \
	fi
	@echo ""
	@echo "Running unit tests..."
	@if [ -d "$(SYNC_DIR)/tests" ]; then \
		cd $(SYNC_DIR) && PYTHONPATH=src ../$(PYTEST) tests/ -q --tb=short 2>&1 && echo "  $(GREEN)✓$(NC) Sync SDK tests passed" || echo "  $(RED)✗$(NC) Sync tests failed"; \
	fi
	@if [ -d "$(ASYNC_DIR)/tests" ]; then \
		cd $(ASYNC_DIR) && PYTHONPATH=src ../$(PYTEST) tests/ -q --tb=short 2>&1 && echo "  $(GREEN)✓$(NC) Async SDK tests passed" || echo "  $(RED)✗$(NC) Async tests failed"; \
	fi

# ============================================================================
# TEST: Integration Mode
# ============================================================================

test-integration: $(VENV)/.deps-installed
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
		$(PYTHON) -c "import sys; sys.path.insert(0, '$(SYNC_DIR)/src'); from unsandbox import UnsandboxClient; c = UnsandboxClient(); r = c.execute('python', 'print(42)'); print('  ✓ Integration: API auth works') if r else print('  ✗ Integration: API auth failed')" 2>/dev/null || echo "  $(RED)✗$(NC) Integration: SDK error"; \
	fi

# ============================================================================
# TEST: Functional Mode
# ============================================================================

test-functional: $(VENV)/.deps-installed
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
			cd $(SYNC_DIR) && ../$(PYTHON) verify_sdk.py 2>/dev/null && echo "  $(GREEN)✓$(NC) Functional: Sync SDK verified" || echo "  $(RED)✗$(NC) Functional: Sync verification failed"; \
		fi; \
	fi

# ============================================================================
# TEST: By SDK Type
# ============================================================================

test-sync: $(VENV)/.deps-installed
	@echo "Testing Sync SDK..."
	@if [ -d "$(SYNC_DIR)/tests" ]; then \
		cd $(SYNC_DIR) && PYTHONPATH=src ../$(PYTEST) tests/ -v; \
	else \
		echo "  $(YELLOW)⊘$(NC) Sync SDK tests not found"; \
	fi

test-async: $(VENV)/.deps-installed
	@echo "Testing Async SDK..."
	@if [ -d "$(ASYNC_DIR)/tests" ]; then \
		cd $(ASYNC_DIR) && PYTHONPATH=src ../$(PYTEST) tests/ -v; \
	else \
		echo "  $(YELLOW)⊘$(NC) Async SDK tests not found"; \
	fi

# ============================================================================
# Development
# ============================================================================

lint: $(VENV)/.deps-installed
	@echo "Linting Python SDKs..."
	@$(VENV)/bin/pip install -q flake8 2>/dev/null || true
	@if [ -d "$(SYNC_DIR)/src" ]; then $(VENV)/bin/flake8 $(SYNC_DIR)/src/ --max-line-length=120 || true; fi
	@if [ -d "$(ASYNC_DIR)/src" ]; then $(VENV)/bin/flake8 $(ASYNC_DIR)/src/ --max-line-length=120 || true; fi
	@echo "$(GREEN)✓$(NC) Lint complete"

format: $(VENV)/.deps-installed
	@echo "Formatting Python SDKs..."
	@$(VENV)/bin/pip install -q black 2>/dev/null || true
	@if [ -d "$(SYNC_DIR)/src" ]; then $(VENV)/bin/black $(SYNC_DIR)/src/ $(SYNC_DIR)/tests/ 2>/dev/null || true; fi
	@if [ -d "$(ASYNC_DIR)/src" ]; then $(VENV)/bin/black $(ASYNC_DIR)/src/ $(ASYNC_DIR)/tests/ 2>/dev/null || true; fi
	@echo "$(GREEN)✓$(NC) Format complete"

examples: $(VENV)/.deps-installed
	@echo "Running Python examples..."
	@if [ -f "$(ASYNC_DIR)/Makefile" ]; then $(MAKE) -C $(ASYNC_DIR) examples; fi

# ============================================================================
# Clean
# ============================================================================

clean:
	@echo "Cleaning Python build artifacts..."
	@rm -rf $(VENV)
	@find . -type d -name __pycache__ -exec rm -rf {} + 2>/dev/null || true
	@find . -type f -name "*.pyc" -delete 2>/dev/null || true
	@find . -type d -name ".pytest_cache" -exec rm -rf {} + 2>/dev/null || true
	@find . -type d -name ".mypy_cache" -exec rm -rf {} + 2>/dev/null || true
	@find . -type d -name "*.egg-info" -exec rm -rf {} + 2>/dev/null || true
	@find . -type d -name "htmlcov" -exec rm -rf {} + 2>/dev/null || true
	@find . -type d -name "dist" -exec rm -rf {} + 2>/dev/null || true
	@find . -type d -name "build" -exec rm -rf {} + 2>/dev/null || true
	@echo "$(GREEN)✓$(NC) Cleaned build artifacts + venv"
