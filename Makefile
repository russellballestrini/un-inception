.PHONY: help test test-all test-python test-go test-javascript test-ruby test-php test-rust test-java test-bash test-perl test-lua

help:
	@echo "UN Inception Testing Targets"
	@echo ""
	@echo "Usage: make [target]"
	@echo ""
	@echo "Test Targets:"
	@echo "  test-all              Run all available tests"
	@echo "  test-python           Test Python client (un.py)"
	@echo "  test-go               Test Go client (un.go)"
	@echo "  test-javascript       Test JavaScript client (un.js)"
	@echo "  test-ruby             Test Ruby client (un.rb)"
	@echo "  test-php              Test PHP client (un.php)"
	@echo "  test-rust             Test Rust client (un.rs)"
	@echo "  test-java             Test Java client (Un.java)"
	@echo "  test-bash             Test Bash client (un.sh)"
	@echo "  test-perl             Test Perl client (un.pl)"
	@echo "  test-lua              Test Lua client (un.lua)"
	@echo ""
	@echo "Integration Tests:"
	@echo "  test-integration      Test all clients against API"
	@echo ""
	@echo "Examples:"
	@echo "  make test-python      # Test only Python"
	@echo "  make test-all         # Test everything"
	@echo ""

test: test-all

test-all:
	@echo "Running all available tests..."
	./tests/run_all_tests.sh

test-python:
	@echo "Testing Python client (clients/python/un.py)..."
	@if [ -f clients/python/un.py ]; then \
		python3 -m py_compile clients/python/un.py && \
		python3 clients/python/un.py test/fib.py && \
		[ -f tests/test_un_py.py ] && pytest tests/test_un_py.py -v || true; \
	elif [ -f un.py ]; then \
		python3 -m py_compile un.py && \
		python3 un.py test/fib.py && \
		[ -f tests/test_un_py.py ] && pytest tests/test_un_py.py -v || true; \
	else \
		echo "ERROR: Python client not found"; \
		exit 1; \
	fi

test-go:
	@echo "Testing Go client (clients/go/un.go)..."
	@if [ -d clients/go ]; then \
		cd clients/go && \
		go mod tidy 2>/dev/null || true && \
		go test -v && \
		go run un.go ../../test/fib.py; \
	elif [ -f un.go ]; then \
		go run un.go test/fib.py; \
	else \
		echo "Go client not found"; \
		exit 1; \
	fi

test-javascript:
	@echo "Testing JavaScript client (clients/javascript/un.js)..."
	@if [ -f clients/javascript/un.js ]; then \
		node clients/javascript/un.js test/fib.py && \
		[ -f tests/test_un_js.js ] && node tests/test_un_js.js || true; \
	elif [ -f un.js ]; then \
		node un.js test/fib.py && \
		[ -f tests/test_un_js.js ] && node tests/test_un_js.js || true; \
	else \
		echo "JavaScript client not found"; \
		exit 1; \
	fi

test-ruby:
	@echo "Testing Ruby client (clients/ruby/un.rb)..."
	@if [ -f clients/ruby/un.rb ]; then \
		ruby -w clients/ruby/un.rb test/fib.py && \
		[ -f tests/test_un_rb.rb ] && ruby tests/test_un_rb.rb || true; \
	elif [ -f un.rb ]; then \
		ruby -w un.rb test/fib.py; \
	else \
		echo "Ruby client not found"; \
		exit 1; \
	fi

test-php:
	@echo "Testing PHP client (clients/php/un.php)..."
	@if [ -f clients/php/un.php ]; then \
		php -l clients/php/un.php && \
		php clients/php/un.php test/fib.py; \
	elif [ -f un.php ]; then \
		php -l un.php && \
		php un.php test/fib.py; \
	else \
		echo "PHP client not found"; \
		exit 1; \
	fi

test-rust:
	@echo "Testing Rust client (clients/rust/un.rs)..."
	@if [ -d clients/rust ]; then \
		cd clients/rust && \
		cargo test --release && \
		cargo run --release -- ../../test/fib.py; \
	elif [ -f un.rs ]; then \
		rustc un.rs -o un && \
		./un test/fib.py; \
	else \
		echo "Rust client not found"; \
		exit 1; \
	fi

test-java:
	@echo "Testing Java client (clients/java/Un.java)..."
	@if [ -f clients/java/Un.java ]; then \
		cd clients/java && \
		javac Un.java && \
		java -cp . Un ../../test/fib.py; \
	elif [ -f Un.java ]; then \
		javac Un.java && \
		java -cp . Un test/fib.py; \
	else \
		echo "Java client not found"; \
		exit 1; \
	fi

test-bash:
	@echo "Testing Bash client (clients/bash/un.sh)..."
	@if [ -f clients/bash/un.sh ]; then \
		bash -n clients/bash/un.sh && \
		bash clients/bash/un.sh test/fib.py; \
	elif [ -f un.sh ]; then \
		bash -n un.sh && \
		bash un.sh test/fib.py; \
	else \
		echo "Bash client not found"; \
		exit 1; \
	fi

test-perl:
	@echo "Testing Perl client (clients/perl/un.pl)..."
	@if [ -f clients/perl/un.pl ]; then \
		perl -c clients/perl/un.pl && \
		perl clients/perl/un.pl test/fib.py; \
	elif [ -f un.pl ]; then \
		perl -c un.pl && \
		perl un.pl test/fib.py; \
	else \
		echo "Perl client not found"; \
		exit 1; \
	fi

test-lua:
	@echo "Testing Lua client (clients/lua/un.lua)..."
	@if [ -f clients/lua/un.lua ]; then \
		lua clients/lua/un.lua test/fib.py; \
	elif [ -f un.lua ]; then \
		lua un.lua test/fib.py; \
	else \
		echo "Lua client not found"; \
		exit 1; \
	fi

test-integration:
	@echo "Testing all clients against API..."
	@if [ -f tests/integration-all-clients.sh ]; then \
		bash tests/integration-all-clients.sh; \
	else \
		echo "Integration test script not found"; \
		exit 1; \
	fi

test-parity:
	@echo "Testing feature parity across all clients..."
	@if [ -f tests/feature-parity-matrix.sh ]; then \
		bash tests/feature-parity-matrix.sh; \
	else \
		echo "Feature parity test script not found"; \
		exit 1; \
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
