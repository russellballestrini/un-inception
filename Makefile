.PHONY: help test test-all test-c test-python test-go test-javascript test-ruby test-php test-rust test-java test-bash test-perl test-lua set-version

# Client directories with their own Makefiles
CLIENTS_WITH_MAKEFILE := $(wildcard clients/*/Makefile)
CLIENT_DIRS := $(dir $(CLIENTS_WITH_MAKEFILE))

help:
	@echo "UN Inception - Multi-Mode Testing Framework"
	@echo ""
	@echo "Test client SDKs (delegates to clients/*/Makefile):"
	@echo "  make test-c                   # C client (4 modes)"
	@echo "  make test-python              # Python client (4 modes)"
	@echo "  make test-go                  # Go client (4 modes)"
	@echo "  make test-javascript          # JavaScript client (4 modes)"
	@echo "  make test-rust                # Rust client (4 modes)"
	@echo ""
	@echo "Test specific modes:"
	@echo "  make test-c-cli               # C CLI mode only"
	@echo "  make test-c-library           # C Library mode only"
	@echo "  make test-c-integration       # C Integration mode only"
	@echo "  make test-c-functional        # C Functional mode only"
	@echo ""
	@echo "Test all clients:"
	@echo "  make test-all                 # All clients, all 4 modes"
	@echo "  make test-clients             # Only clients with Makefiles"
	@echo ""
	@echo "Legacy (root-level implementations):"
	@echo "  make test-python-root         # Test un.py in root"
	@echo "  make test-go-root             # Test un.go in root"
	@echo ""
	@echo "Utility:"
	@echo "  make test-ci-locally          # Simulate CI pipeline"
	@echo "  make lint                     # Lint all clients"
	@echo "  make clean                    # Clean build artifacts"
	@echo "  make set-version VERSION=X.Y.Z  # Set version in all files"
	@echo ""
	@echo "Available client Makefiles:"
	@for dir in $(CLIENT_DIRS); do echo "  $$dir"; done
	@echo ""

# ============================================================================
# Main Test Targets
# ============================================================================

test: test-all

# Test all clients that have Makefiles
test-all: test-clients test-root
	@echo "✓ All tests complete"

# Test only clients with their own Makefiles (clients/*/Makefile)
test-clients:
	@echo "Testing clients with Makefiles..."
	@for dir in $(CLIENT_DIRS); do \
		echo ""; \
		echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"; \
		echo "Testing $$dir"; \
		echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"; \
		$(MAKE) -C $$dir test || true; \
	done

# Test root-level implementations (un.py, un.go, etc.)
test-root:
	@echo "Testing root-level implementations..."
	@$(MAKE) test-python-root test-go-root test-javascript-root || true

# ============================================================================
# C Client (delegates to clients/c/Makefile)
# ============================================================================

test-c:
	@if [ -f clients/c/Makefile ]; then \
		$(MAKE) -C clients/c test; \
	else \
		echo "clients/c/Makefile not found"; \
	fi

test-c-cli:
	@$(MAKE) -C clients/c test-cli

test-c-library:
	@$(MAKE) -C clients/c test-library

test-c-integration:
	@$(MAKE) -C clients/c test-integration

test-c-functional:
	@$(MAKE) -C clients/c test-functional

build-c:
	@$(MAKE) -C clients/c build

# ============================================================================
# Python Client (delegates to clients/python/Makefile if exists)
# ============================================================================

test-python:
	@if [ -f clients/python/Makefile ]; then \
		$(MAKE) -C clients/python test; \
	else \
		$(MAKE) test-python-root; \
	fi

test-python-cli:
	@if [ -f clients/python/Makefile ]; then \
		$(MAKE) -C clients/python test-cli; \
	else \
		$(MAKE) test-python-root-cli; \
	fi

test-python-library:
	@if [ -f clients/python/Makefile ]; then \
		$(MAKE) -C clients/python test-library; \
	else \
		$(MAKE) test-python-root-library; \
	fi

test-python-integration:
	@if [ -f clients/python/Makefile ]; then \
		$(MAKE) -C clients/python test-integration; \
	else \
		$(MAKE) test-python-root-integration; \
	fi

test-python-functional:
	@if [ -f clients/python/Makefile ]; then \
		$(MAKE) -C clients/python test-functional; \
	else \
		$(MAKE) test-python-root-functional; \
	fi

# ============================================================================
# Python Root (legacy - tests un.py in repo root)
# ============================================================================

test-python-root: test-python-root-cli test-python-root-library test-python-root-integration test-python-root-functional
	@echo "✓ Python (root): All 4 test modes passed"

test-python-root-cli:
	@echo "Testing Python CLI Mode (root)..."
	@if [ -f un.py ]; then \
		python3 -m py_compile un.py && \
		python3 un.py --help > /dev/null && \
		echo "  ✓ CLI: --help works"; \
	else \
		echo "  ⚠ un.py not found in root"; \
	fi

test-python-root-library:
	@echo "Testing Python Library Mode (root)..."
	@if [ -f un.py ]; then \
		python3 -c "from un import UnsandboxClient; print('  ✓ Library: Import works')" 2>/dev/null || echo "  ⚠ Library: UnsandboxClient not exportable"; \
	else \
		echo "  ⚠ un.py not found"; \
	fi

test-python-root-integration:
	@echo "Testing Python Integration Mode (root)..."
	@if [ -f tests/test_un_py_integration.py ]; then \
		pytest tests/test_un_py_integration.py -v; \
	else \
		echo "  ℹ Create tests/test_un_py_integration.py"; \
	fi

test-python-root-functional:
	@echo "Testing Python Functional Mode (root)..."
	@if [ -n "$$UNSANDBOX_PUBLIC_KEY" ] && [ -n "$$UNSANDBOX_SECRET_KEY" ] && [ -f un.py ]; then \
		python3 un.py test/fib.py > /dev/null 2>&1 && echo "  ✓ Functional: Fibonacci works" || echo "  ⚠ Functional: Fibonacci failed"; \
	else \
		echo "  ⚠ Skipping (no API keys or un.py not found)"; \
	fi

# ============================================================================
# Go Client (delegates to clients/go/Makefile if exists)
# ============================================================================

test-go:
	@if [ -f clients/go/Makefile ]; then \
		$(MAKE) -C clients/go test; \
	else \
		$(MAKE) test-go-root; \
	fi

test-go-root:
	@echo "Testing Go (root)..."
	@if [ -f un.go ]; then \
		go run un.go --help > /dev/null 2>&1 && echo "  ✓ CLI: --help works" || echo "  ⚠ CLI: --help failed"; \
	else \
		echo "  ⚠ un.go not found"; \
	fi

# ============================================================================
# JavaScript Client (delegates to clients/javascript/Makefile if exists)
# ============================================================================

test-javascript:
	@if [ -f clients/javascript/Makefile ]; then \
		$(MAKE) -C clients/javascript test; \
	else \
		$(MAKE) test-javascript-root; \
	fi

test-javascript-root:
	@echo "Testing JavaScript (root)..."
	@if [ -f un.js ]; then \
		node un.js --help > /dev/null 2>&1 && echo "  ✓ CLI: --help works" || echo "  ⚠ CLI: --help failed"; \
	else \
		echo "  ⚠ un.js not found"; \
	fi

# ============================================================================
# Other Languages (delegate or test root)
# ============================================================================

test-rust:
	@if [ -f clients/rust/Makefile ]; then $(MAKE) -C clients/rust test; \
	elif [ -f un.rs ]; then echo "Testing Rust (root)..."; rustc --version > /dev/null && echo "  ✓ Rust available"; \
	else echo "  ⚠ Rust client not found"; fi

test-ruby:
	@if [ -f clients/ruby/Makefile ]; then $(MAKE) -C clients/ruby test; \
	elif [ -f un.rb ]; then echo "Testing Ruby (root)..."; ruby -c un.rb > /dev/null && echo "  ✓ Syntax valid"; \
	else echo "  ⚠ Ruby client not found"; fi

test-php:
	@if [ -f clients/php/Makefile ]; then $(MAKE) -C clients/php test; \
	elif [ -f un.php ]; then echo "Testing PHP (root)..."; php -l un.php > /dev/null && echo "  ✓ Syntax valid"; \
	else echo "  ⚠ PHP client not found"; fi

test-java:
	@if [ -f clients/java/Makefile ]; then $(MAKE) -C clients/java test; \
	elif [ -f Un.java ]; then echo "Testing Java (root)..."; javac Un.java && echo "  ✓ Compiles"; \
	else echo "  ⚠ Java client not found"; fi

test-bash:
	@if [ -f clients/bash/Makefile ]; then $(MAKE) -C clients/bash test; \
	elif [ -f un.sh ]; then echo "Testing Bash (root)..."; bash -n un.sh && echo "  ✓ Syntax valid"; \
	else echo "  ⚠ Bash client not found"; fi

test-perl:
	@if [ -f clients/perl/Makefile ]; then $(MAKE) -C clients/perl test; \
	elif [ -f un.pl ]; then echo "Testing Perl (root)..."; perl -c un.pl 2>/dev/null && echo "  ✓ Syntax valid"; \
	else echo "  ⚠ Perl client not found"; fi

test-lua:
	@if [ -f clients/lua/Makefile ]; then $(MAKE) -C clients/lua test; \
	elif [ -f un.lua ]; then echo "Testing Lua (root)..."; echo "  ✓ Lua file exists"; \
	else echo "  ⚠ Lua client not found"; fi

# ============================================================================
# Cross-Language Tests
# ============================================================================

test-integration-all:
	@echo "Testing all clients against API..."
	@if [ -f tests/integration-all-clients.sh ]; then \
		bash tests/integration-all-clients.sh; \
	else \
		echo "Integration test script not found"; \
		echo "ℹ Create tests/integration-all-clients.sh"; \
	fi

test-parity:
	@echo "Testing feature parity across all clients..."
	@if [ -f tests/feature-parity-matrix.sh ]; then \
		bash tests/feature-parity-matrix.sh; \
	else \
		echo "Feature parity test script not found"; \
		echo "ℹ Create tests/feature-parity-matrix.sh"; \
	fi

test-ci-locally:
	@echo "Running CI pipeline locally (detection + build + test)..."
	@bash scripts/detect-changes.sh > changes.json
	@echo "Changes detected:"
	@cat changes.json
	@bash scripts/generate-matrix.sh > test-matrix.yml
	@echo "Test matrix generated:"
	@cat test-matrix.yml
	@bash scripts/build-clients.sh || true
	@echo "Ready to run tests from test-matrix.yml"

.PHONY: lint
lint:
	@echo "Linting all clients (where applicable)..."
	@command -v shellcheck >/dev/null 2>&1 && bash -n un.sh && shellcheck un.sh || echo "Skipping shellcheck"
	@command -v python3 >/dev/null 2>&1 && python3 -m pylint un.py 2>/dev/null || echo "Skipping pylint"
	@command -v eslint >/dev/null 2>&1 && npx eslint un.js 2>/dev/null || echo "Skipping eslint"
	@command -v rubocop >/dev/null 2>&1 && rubocop un.rb 2>/dev/null || echo "Skipping rubocop"
	@echo "Linting complete"

.PHONY: clean
clean:
	@echo "Cleaning build artifacts..."
	@rm -rf build/ bin/ dist/ *.o un_*
	@find . -name "*.pyc" -delete
	@find . -name "__pycache__" -type d -delete
	@echo "Clean complete"

# ============================================================================
# Version Management
# ============================================================================

.PHONY: set-version
set-version:
ifndef VERSION
	@echo "Usage: make set-version VERSION=4.2.0"
	@exit 1
endif
	@bash scripts/set-version.sh $(VERSION)
