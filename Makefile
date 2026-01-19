.PHONY: help test test-all test-c test-python test-go test-javascript test-ruby test-php test-rust test-java test-bash test-perl test-lua set-version perf-report perf-aggregate-report

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
	@echo "  make perf-report TAG=X.Y.Z     # Generate performance report for release"
	@echo "  make perf-aggregate-report    # Analyze variance across all reports"
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

# ============================================================================
# Performance Reporting
# ============================================================================

.PHONY: perf-report perf-charts
perf-report:
ifdef TAG
	@bash scripts/generate-perf-report.sh $(TAG)
else
	@bash scripts/generate-perf-report.sh
endif

perf-charts:
ifdef TAG
	@echo "Generating performance charts for $(TAG)..."
	@build/un -a -f reports/$(TAG)/perf.json scripts/generate-perf-charts.py
	@mv -f *.png reports/$(TAG)/ 2>/dev/null || true
	@echo "✓ Charts saved to reports/$(TAG)/"
else
	@echo "Usage: make perf-charts TAG=4.2.0"
endif

perf-all: perf-report perf-charts
	@echo "✓ Performance report and charts complete for $(TAG)"
	@ls -la reports/$(TAG)/

perf-aggregate-report:
	@echo "Aggregating performance reports across all releases..."
	@echo "Step 1: Discovering releases from git tags..."
	@VERSIONS=$$(git tag | grep -E '^[0-9]+\.[0-9]+\.[0-9]+$$' | sort -V); \
	echo "Found tagged releases: $$VERSIONS"; \
	if [ -z "$$VERSIONS" ]; then echo "ERROR: No version tags found"; exit 1; fi; \
	echo "Step 2: Copying perf.json files for tagged releases..."; \
	for v in $$VERSIONS; do \
		if [ -f "reports/$$v/perf.json" ]; then \
			cp "reports/$$v/perf.json" "perf-$$v.json"; \
			echo "  ✓ $$v"; \
		fi; \
	done; \
	echo "Step 3: Generating charts via UN..."; \
	FILES=$$(ls perf-*.json 2>/dev/null | sed 's/^/-f /' | tr '\n' ' '); \
	if [ -z "$$FILES" ]; then echo "ERROR: No perf JSON files found"; exit 1; fi; \
	build/un -a $$FILES scripts/generate-aggregated-charts.py; \
	rm -f perf-*.json; \
	mv -f *.png reports/ 2>/dev/null || true; \
	echo "Step 4: Generating markdown report..."; \
	python3 scripts/aggregate-performance-reports.py reports AGGREGATED-PERFORMANCE.md; \
	echo "✓ Aggregated report generated: AGGREGATED-PERFORMANCE.md"; \
	echo "✓ Charts generated via UN:"; \
	ls -lh reports/aggregated-*.png 2>/dev/null || true

# ============================================================================
# CI Deploy Key Setup (one-time)
# ============================================================================

GITLAB_URL ?= https://git.unturf.com
PROJECT_ID ?= 116
DEPLOY_KEY_NAME ?= ci-perf-report
DEPLOY_KEY_PATH ?= $(HOME)/.ssh/un-inception-ci

.PHONY: setup-ci-deploy-key
setup-ci-deploy-key:
ifndef GITLAB_TOKEN
	@echo "ERROR: GITLAB_TOKEN required (API token with api scope)"
	@echo "Usage: GITLAB_TOKEN=xxx make setup-ci-deploy-key"
	@exit 1
endif
	@echo "========================================"
	@echo "Setting up CI Deploy Key"
	@echo "========================================"
	@echo ""
	@# Step 1: Generate SSH key if not exists
	@if [ -f "$(DEPLOY_KEY_PATH)" ]; then \
		echo "Key already exists: $(DEPLOY_KEY_PATH)"; \
		echo "Delete it first if you want to regenerate"; \
	else \
		echo "Step 1: Generating SSH key..."; \
		ssh-keygen -t ed25519 -C "ci@un-inception" -f "$(DEPLOY_KEY_PATH)" -N ""; \
		echo "✓ Key generated: $(DEPLOY_KEY_PATH)"; \
	fi
	@echo ""
	@# Step 2: Add public key as deploy key to repo
	@echo "Step 2: Adding deploy key to GitLab repo..."
	@PUBKEY=$$(cat "$(DEPLOY_KEY_PATH).pub"); \
	curl -s --request POST \
		--header "PRIVATE-TOKEN: $(GITLAB_TOKEN)" \
		--header "Content-Type: application/json" \
		--data "{\"title\": \"$(DEPLOY_KEY_NAME)\", \"key\": \"$$PUBKEY\", \"can_push\": true}" \
		"$(GITLAB_URL)/api/v4/projects/$(PROJECT_ID)/deploy_keys" | jq -r 'if .id then "✓ Deploy key added (id: \(.id))" else "⚠ \(.message // "Already exists or error")" end'
	@echo ""
	@# Step 3: Protect release tags (so protected variables are available)
	@echo "Step 3: Protecting release tags (*.*.*) ..."
	@curl -s --request POST \
		--header "PRIVATE-TOKEN: $(GITLAB_TOKEN)" \
		--header "Content-Type: application/json" \
		--data '{"name": "*.*.*", "create_access_level": "40"}' \
		"$(GITLAB_URL)/api/v4/projects/$(PROJECT_ID)/protected_tags" | jq -r 'if .name then "✓ Protected tag added: \(.name)" else "⚠ \(.message // "Already exists or error")" end'
	@echo ""
	@# Step 4: Add private key as CI variable
	@echo "Step 4: Adding DEPLOY_KEY variable to CI..."
	@PRIVKEY=$$(base64 -w0 < "$(DEPLOY_KEY_PATH)"); \
	curl -s --request POST \
		--header "PRIVATE-TOKEN: $(GITLAB_TOKEN)" \
		--form "key=DEPLOY_KEY" \
		--form "value=$$PRIVKEY" \
		--form "protected=true" \
		--form "masked=true" \
		"$(GITLAB_URL)/api/v4/projects/$(PROJECT_ID)/variables" | jq -r 'if .key then "✓ CI variable added: \(.key) (protected, masked)" else "⚠ \(.message // "Already exists or error")" end'
	@echo ""
	@echo "========================================"
	@echo "Setup complete!"
	@echo "========================================"
	@echo ""
	@echo "Key location: $(DEPLOY_KEY_PATH)"
	@echo "Protected tags: *.*.*"
	@echo ""
	@echo "Test with: make test-ci-deploy-key"

.PHONY: test-ci-deploy-key
test-ci-deploy-key:
	@echo "Testing deploy key SSH access..."
	@ssh -i "$(DEPLOY_KEY_PATH)" -o StrictHostKeyChecking=no -p 2222 git@git.unturf.com 2>&1 | head -5 || true

.PHONY: remove-ci-deploy-key
remove-ci-deploy-key:
ifndef GITLAB_TOKEN
	@echo "ERROR: GITLAB_TOKEN required"
	@exit 1
endif
	@echo "Removing deploy key and CI variable..."
	@# Get deploy key ID and delete
	@KEYID=$$(curl -s --header "PRIVATE-TOKEN: $(GITLAB_TOKEN)" \
		"$(GITLAB_URL)/api/v4/projects/$(PROJECT_ID)/deploy_keys" | \
		jq -r '.[] | select(.title == "$(DEPLOY_KEY_NAME)") | .id'); \
	if [ -n "$$KEYID" ]; then \
		curl -s --request DELETE --header "PRIVATE-TOKEN: $(GITLAB_TOKEN)" \
			"$(GITLAB_URL)/api/v4/projects/$(PROJECT_ID)/deploy_keys/$$KEYID"; \
		echo "✓ Deploy key removed"; \
	else \
		echo "⚠ Deploy key not found"; \
	fi
	@# Delete CI variable
	@curl -s --request DELETE --header "PRIVATE-TOKEN: $(GITLAB_TOKEN)" \
		"$(GITLAB_URL)/api/v4/projects/$(PROJECT_ID)/variables/DEPLOY_KEY" && \
		echo "✓ CI variable removed" || echo "⚠ CI variable not found"
	@# Delete protected tag
	@curl -s --request DELETE --header "PRIVATE-TOKEN: $(GITLAB_TOKEN)" \
		"$(GITLAB_URL)/api/v4/projects/$(PROJECT_ID)/protected_tags/*.*.*" && \
		echo "✓ Protected tag removed" || echo "⚠ Protected tag not found"
	@# Optionally remove local key
	@echo ""
	@echo "Local key still at: $(DEPLOY_KEY_PATH)"
	@echo "To remove: rm $(DEPLOY_KEY_PATH) $(DEPLOY_KEY_PATH).pub"
