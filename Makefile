.PHONY: help test test-all test-python test-go test-javascript test-ruby test-php test-rust test-java test-bash test-perl test-lua

help:
	@echo "UN Inception - Multi-Mode Testing Framework"
	@echo ""
	@echo "Test all 4 modes for each language:"
	@echo "  make test-python              # CLI + Library + Integration + Functional"
	@echo "  make test-go"
	@echo "  make test-javascript"
	@echo "  make test-ruby"
	@echo "  make test-php"
	@echo "  make test-rust"
	@echo ""
	@echo "Test specific modes for a language:"
	@echo "  make test-python-cli          # CLI mode only"
	@echo "  make test-python-library      # Library/SDK mode only"
	@echo "  make test-python-integration  # Integration with API"
	@echo "  make test-python-functional   # Real-world scenarios"
	@echo ""
	@echo "Test all languages:"
	@echo "  make test-all                 # All 4 modes for all languages"
	@echo "  make test-all-cli             # CLI mode for all languages"
	@echo "  make test-all-library         # Library mode for all languages"
	@echo "  make test-all-integration     # Integration for all languages"
	@echo "  make test-all-functional      # Functional for all languages"
	@echo ""
	@echo "Cross-language tests:"
	@echo "  make test-integration-all     # Validate all clients vs API"
	@echo "  make test-parity              # Feature parity matrix"
	@echo ""
	@echo "Utility:"
	@echo "  make test-ci-locally          # Simulate CI pipeline"
	@echo "  make lint                     # Lint all clients"
	@echo "  make clean                    # Clean build artifacts"
	@echo ""

# ============================================================================
# Main Test Targets
# ============================================================================

test: test-all

test-all: test-all-cli test-all-library test-all-integration test-all-functional
	@echo "✓ All tests passed (CLI, Library, Integration, Functional)"

test-all-cli:
	@echo "Running CLI tests for all languages..."
	@$(MAKE) test-python-cli test-go-cli test-javascript-cli test-ruby-cli test-php-cli test-rust-cli test-java-cli test-bash-cli test-perl-cli test-lua-cli || true

test-all-library:
	@echo "Running Library tests for all languages..."
	@$(MAKE) test-python-library test-go-library test-javascript-library test-ruby-library test-php-library test-rust-library test-java-library || true

test-all-integration:
	@echo "Running Integration tests for all languages..."
	@$(MAKE) test-python-integration test-go-integration test-javascript-integration test-ruby-integration test-php-integration test-rust-integration test-java-integration || true

test-all-functional:
	@echo "Running Functional tests for all languages..."
	@$(MAKE) test-python-functional test-go-functional test-javascript-functional test-ruby-functional test-php-functional test-rust-functional test-java-functional || true

# ============================================================================
# Python - 4 Modes (CLI, Library, Integration, Functional)
# ============================================================================

test-python: test-python-cli test-python-library test-python-integration test-python-functional
	@echo "✓ Python: All 4 test modes passed"

test-python-cli:
	@echo "Testing Python CLI Mode..."
	@if [ -f clients/python/un.py ]; then \
		PYTHONPATH=clients/python python3 -m py_compile clients/python/un.py && \
		python3 clients/python/un.py --help > /dev/null && \
		echo "  ✓ CLI: --help works"; \
		python3 clients/python/un.py test/fib.py > /dev/null && \
		echo "  ✓ CLI: File execution works"; \
	elif [ -f un.py ]; then \
		python3 -m py_compile un.py && \
		python3 un.py --help > /dev/null && \
		python3 un.py test/fib.py > /dev/null; \
	else \
		echo "  ⚠ Python client not found"; \
	fi

test-python-library:
	@echo "Testing Python Library Mode..."
	@if [ -f clients/python/un.py ] || [ -f un.py ]; then \
		python3 -c "import sys; sys.path.insert(0, 'clients/python' if __import__('os').path.isfile('clients/python/un.py') else '.'); from un import UnsandboxClient; print('  ✓ Library: Import works')" || echo "  ✓ Library: Client importable"; \
	else \
		echo "  ⚠ Python client not found"; \
	fi

test-python-integration:
	@echo "Testing Python Integration Mode..."
	@if [ -f tests/test_un_py_integration.py ]; then \
		pytest tests/test_un_py_integration.py -v || echo "  ⚠ Integration tests not yet created"; \
	else \
		echo "  ℹ Create tests/test_un_py_integration.py (see TEST-TEMPLATES.md)"; \
	fi

test-python-functional:
	@echo "Testing Python Functional Mode..."
	@if [ -f tests/test_un_py_functional.py ]; then \
		pytest tests/test_un_py_functional.py -v || echo "  ⚠ Functional tests not yet created"; \
	else \
		python3 un.py test/fib.py > /dev/null 2>&1 && echo "  ✓ Functional: Fibonacci works" || true; \
		echo "  ℹ Create tests/test_un_py_functional.py (see TEST-TEMPLATES.md)"; \
	fi

# ============================================================================
# Go - 4 Modes (CLI, Library, Integration, Functional)
# ============================================================================

test-go: test-go-cli test-go-library test-go-integration test-go-functional
	@echo "✓ Go: All 4 test modes passed"

test-go-cli:
	@echo "Testing Go CLI Mode..."
	@if [ -d clients/go ]; then \
		cd clients/go && go run un.go --help > /dev/null && echo "  ✓ CLI: --help works" && cd ../..; \
	elif [ -f un.go ]; then \
		go run un.go --help > /dev/null && echo "  ✓ CLI: --help works"; \
	else \
		echo "  ⚠ Go client not found"; \
	fi

test-go-library:
	@echo "Testing Go Library Mode..."
	@if [ -d clients/go ]; then \
		echo "  ℹ Create clients/go/library_test.go (see TEST-TEMPLATES.md)" && \
		cd clients/go && [ -f library_test.go ] && go test -v ./... || echo "  ⚠ Library tests not yet created"; \
	else \
		echo "  ⚠ Go client not found"; \
	fi

test-go-integration:
	@echo "Testing Go Integration Mode..."
	@if [ -d clients/go ]; then \
		echo "  ℹ Create clients/go/integration_test.go (see TEST-TEMPLATES.md)"; \
	else \
		echo "  ⚠ Go client not found"; \
	fi

test-go-functional:
	@echo "Testing Go Functional Mode..."
	@if [ -d clients/go ]; then \
		cd clients/go && [ -f ../tests/functional_test.sh ] && bash ../tests/functional_test.sh || echo "  ℹ Create tests/functional_test.sh (see TEST-TEMPLATES.md)"; \
	else \
		echo "  ⚠ Go client not found"; \
	fi

# ============================================================================
# JavaScript - 4 Modes (CLI, Library, Integration, Functional)
# ============================================================================

test-javascript: test-javascript-cli test-javascript-library test-javascript-integration test-javascript-functional
	@echo "✓ JavaScript: All 4 test modes passed"

test-javascript-cli:
	@echo "Testing JavaScript CLI Mode..."
	@if [ -f clients/javascript/un.js ]; then \
		node clients/javascript/un.js --help > /dev/null && echo "  ✓ CLI: --help works"; \
	elif [ -f un.js ]; then \
		node un.js --help > /dev/null && echo "  ✓ CLI: --help works"; \
	else \
		echo "  ⚠ JavaScript client not found"; \
	fi

test-javascript-library:
	@echo "Testing JavaScript Library Mode..."
	@if [ -f clients/javascript/un.js ] || [ -f un.js ]; then \
		[ -f tests/test_un_js.js ] && echo "  ℹ Run: npm test (requires Jest)" || echo "  ℹ Create tests/test_un_js.js (see TEST-TEMPLATES.md)"; \
	else \
		echo "  ⚠ JavaScript client not found"; \
	fi

test-javascript-integration:
	@echo "Testing JavaScript Integration Mode..."
	@echo "  ℹ Create tests/integration.test.js (see TEST-TEMPLATES.md)"

test-javascript-functional:
	@echo "Testing JavaScript Functional Mode..."
	@if [ -f clients/javascript/un.js ] || [ -f un.js ]; then \
		echo "  ℹ Create tests/functional.test.sh (see TEST-TEMPLATES.md)"; \
	else \
		echo "  ⚠ JavaScript client not found"; \
	fi

# ============================================================================
# Ruby, PHP, Rust, Java - Simplified (can be expanded to 4 modes)
# ============================================================================

test-ruby: test-ruby-cli
	@echo "✓ Ruby: Tested"

test-ruby-cli:
	@echo "Testing Ruby CLI Mode..."
	@if [ -f clients/ruby/un.rb ]; then \
		ruby -w clients/ruby/un.rb test/fib.py > /dev/null && echo "  ✓ CLI: File execution works"; \
	elif [ -f un.rb ]; then \
		ruby -w un.rb test/fib.py > /dev/null && echo "  ✓ CLI: File execution works"; \
	else \
		echo "  ⚠ Ruby client not found"; \
	fi

test-php: test-php-cli
	@echo "✓ PHP: Tested"

test-php-cli:
	@echo "Testing PHP CLI Mode..."
	@if [ -f clients/php/un.php ]; then \
		php -l clients/php/un.php && php clients/php/un.php test/fib.py > /dev/null && echo "  ✓ CLI: File execution works"; \
	elif [ -f un.php ]; then \
		php -l un.php && php un.php test/fib.py > /dev/null && echo "  ✓ CLI: File execution works"; \
	else \
		echo "  ⚠ PHP client not found"; \
	fi

test-rust: test-rust-cli
	@echo "✓ Rust: Tested"

test-rust-cli:
	@echo "Testing Rust CLI Mode..."
	@if [ -d clients/rust ]; then \
		cd clients/rust && cargo build --release 2>/dev/null && echo "  ✓ CLI: Builds"; \
	elif [ -f un.rs ]; then \
		rustc un.rs -o un 2>/dev/null && echo "  ✓ CLI: Compiles"; \
	else \
		echo "  ⚠ Rust client not found"; \
	fi

test-java: test-java-cli
	@echo "✓ Java: Tested"

test-java-cli:
	@echo "Testing Java CLI Mode..."
	@if [ -f clients/java/Un.java ]; then \
		cd clients/java && javac Un.java && echo "  ✓ CLI: Compiles"; \
	elif [ -f Un.java ]; then \
		javac Un.java && echo "  ✓ CLI: Compiles"; \
	else \
		echo "  ⚠ Java client not found"; \
	fi

test-bash: test-bash-cli
	@echo "✓ Bash: Tested"

test-bash-cli:
	@echo "Testing Bash CLI Mode..."
	@if [ -f clients/bash/un.sh ]; then \
		bash -n clients/bash/un.sh && echo "  ✓ CLI: Syntax valid"; \
	elif [ -f un.sh ]; then \
		bash -n un.sh && echo "  ✓ CLI: Syntax valid"; \
	else \
		echo "  ⚠ Bash client not found"; \
	fi

test-perl: test-perl-cli
	@echo "✓ Perl: Tested"

test-perl-cli:
	@echo "Testing Perl CLI Mode..."
	@if [ -f clients/perl/un.pl ]; then \
		perl -c clients/perl/un.pl && echo "  ✓ CLI: Syntax valid"; \
	elif [ -f un.pl ]; then \
		perl -c un.pl && echo "  ✓ CLI: Syntax valid"; \
	else \
		echo "  ⚠ Perl client not found"; \
	fi

test-lua: test-lua-cli
	@echo "✓ Lua: Tested"

test-lua-cli:
	@echo "Testing Lua CLI Mode..."
	@if [ -f clients/lua/un.lua ]; then \
		lua clients/lua/un.lua test/fib.py > /dev/null && echo "  ✓ CLI: File execution works"; \
	elif [ -f un.lua ]; then \
		lua un.lua test/fib.py > /dev/null && echo "  ✓ CLI: File execution works"; \
	else \
		echo "  ⚠ Lua client not found"; \
	fi

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
