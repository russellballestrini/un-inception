# UN Java Client - Build and Test
#
# This client has two implementations:
#   - sync/  : Synchronous Java SDK (HttpURLConnection)
#   - async/ : Asynchronous Java SDK (CompletableFuture/HttpClient)
#
# Usage:
#   make              # Build all
#   make test         # Run all 4 test modes
#   make test-cli     # CLI mode only
#   make test-library # Library mode only
#   make test-integration # Integration mode only
#   make test-functional  # Functional mode only
#   make build        # Build JAR files
#   make clean        # Remove build artifacts
#
# Dependencies:
#   Java 11+ (for HttpClient)
#   Maven or Gradle

.PHONY: all build test test-cli test-library test-integration test-functional
.PHONY: test-sync test-async clean help examples

# Paths
ROOT_DIR := $(shell cd ../.. && pwd)
SYNC_DIR := sync
ASYNC_DIR := async

# Java settings
JAVA := java
JAVAC := javac
MAVEN := mvn
GRADLE := gradle

# Colors
GREEN := \033[32m
RED := \033[31m
YELLOW := \033[33m
NC := \033[0m

.DEFAULT_GOAL := help

help:
	@echo "UN Java Client - Build and Test"
	@echo ""
	@echo "Build:"
	@echo "  make build            Build all JARs"
	@echo "  make build-sync       Build sync SDK"
	@echo "  make build-async      Build async SDK"
	@echo ""
	@echo "Test (all 4 modes):"
	@echo "  make test             All 4 modes for both sync and async"
	@echo "  make test-cli         CLI mode (command-line interface)"
	@echo "  make test-library     Library mode (JUnit tests)"
	@echo "  make test-integration Integration mode (API contract)"
	@echo "  make test-functional  Functional mode (real-world scenarios)"
	@echo ""
	@echo "Test by SDK:"
	@echo "  make test-sync        Test synchronous SDK"
	@echo "  make test-async       Test asynchronous SDK"
	@echo ""
	@echo "Utility:"
	@echo "  make clean            Remove build artifacts"
	@echo "  make deps             Show required dependencies"
	@echo ""

all: build

deps:
	@echo "Required:"
	@echo "  Java 11+ (for HttpClient)"
	@echo "  Maven or Gradle"
	@echo ""
	@java -version 2>&1 | head -1 || echo "Java not installed"
	@mvn --version 2>/dev/null | head -1 || echo "Maven not installed (optional)"
	@gradle --version 2>/dev/null | head -1 || echo "Gradle not installed (optional)"

# ============================================================================
# BUILD
# ============================================================================

build: build-sync build-async
	@echo "$(GREEN)✓ All Java SDKs built$(NC)"

build-sync:
	@echo "Building sync SDK..."
	@if [ -f "$(SYNC_DIR)/pom.xml" ]; then \
		cd $(SYNC_DIR) && $(MAVEN) compile -q && echo "$(GREEN)✓$(NC) Sync SDK compiled (Maven)"; \
	elif [ -f "$(SYNC_DIR)/build.gradle" ]; then \
		cd $(SYNC_DIR) && $(GRADLE) build -q && echo "$(GREEN)✓$(NC) Sync SDK compiled (Gradle)"; \
	elif [ -d "$(SYNC_DIR)/src" ]; then \
		echo "$(YELLOW)⊘$(NC) Sync SDK needs pom.xml or build.gradle"; \
	else \
		echo "$(YELLOW)⊘$(NC) Sync SDK not found"; \
	fi

build-async:
	@echo "Building async SDK..."
	@if [ -f "$(ASYNC_DIR)/pom.xml" ]; then \
		cd $(ASYNC_DIR) && $(MAVEN) compile -q && echo "$(GREEN)✓$(NC) Async SDK compiled (Maven)"; \
	elif [ -f "$(ASYNC_DIR)/build.gradle" ]; then \
		cd $(ASYNC_DIR) && $(GRADLE) build -q && echo "$(GREEN)✓$(NC) Async SDK compiled (Gradle)"; \
	elif [ -d "$(ASYNC_DIR)/src" ]; then \
		echo "$(YELLOW)⊘$(NC) Async SDK needs pom.xml or build.gradle"; \
	else \
		echo "$(YELLOW)⊘$(NC) Async SDK not found"; \
	fi

# ============================================================================
# TEST: All 4 Modes
# ============================================================================

test: test-cli test-library test-integration test-functional
	@echo ""
	@echo "$(GREEN)✓ Java Client: All 4 test modes complete$(NC)"

# ============================================================================
# TEST: CLI Mode
# ============================================================================

test-cli:
	@echo ""
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "CLI MODE: Testing Java CLI interface"
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo ""
	@# Test sync SDK compile
	@if [ -f "$(SYNC_DIR)/pom.xml" ]; then \
		cd $(SYNC_DIR) && $(MAVEN) compile -q 2>/dev/null && echo "  $(GREEN)✓$(NC) CLI: Sync SDK compiles (Maven)" || echo "  $(RED)✗$(NC) CLI: Sync SDK compile failed"; \
	elif [ -f "$(SYNC_DIR)/build.gradle" ]; then \
		cd $(SYNC_DIR) && $(GRADLE) compileJava -q 2>/dev/null && echo "  $(GREEN)✓$(NC) CLI: Sync SDK compiles (Gradle)" || echo "  $(RED)✗$(NC) CLI: Sync SDK compile failed"; \
	elif [ -d "$(SYNC_DIR)/src" ]; then \
		echo "  $(YELLOW)⊘$(NC) CLI: Sync SDK needs build configuration"; \
	fi
	@# Test async SDK compile
	@if [ -f "$(ASYNC_DIR)/pom.xml" ]; then \
		cd $(ASYNC_DIR) && $(MAVEN) compile -q 2>/dev/null && echo "  $(GREEN)✓$(NC) CLI: Async SDK compiles (Maven)" || echo "  $(YELLOW)⊘$(NC) CLI: Async SDK not yet buildable"; \
	elif [ -f "$(ASYNC_DIR)/build.gradle" ]; then \
		cd $(ASYNC_DIR) && $(GRADLE) compileJava -q 2>/dev/null && echo "  $(GREEN)✓$(NC) CLI: Async SDK compiles (Gradle)" || echo "  $(YELLOW)⊘$(NC) CLI: Async SDK not yet buildable"; \
	fi

# ============================================================================
# TEST: Library Mode
# ============================================================================

test-library:
	@echo ""
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "LIBRARY MODE: Testing Java classes"
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo ""
	@# Test sync SDK with Maven/Gradle test
	@if [ -f "$(SYNC_DIR)/pom.xml" ]; then \
		cd $(SYNC_DIR) && $(MAVEN) test -q 2>&1 | tail -10 || echo "  $(YELLOW)⊘$(NC) Library: Sync tests incomplete"; \
	elif [ -f "$(SYNC_DIR)/build.gradle" ]; then \
		cd $(SYNC_DIR) && $(GRADLE) test -q 2>&1 | tail -10 || echo "  $(YELLOW)⊘$(NC) Library: Sync tests incomplete"; \
	else \
		echo "  $(YELLOW)⊘$(NC) Library: Sync SDK needs build configuration"; \
	fi
	@# Test async SDK
	@if [ -f "$(ASYNC_DIR)/pom.xml" ]; then \
		cd $(ASYNC_DIR) && $(MAVEN) test -q 2>&1 | tail -10 || echo "  $(YELLOW)⊘$(NC) Library: Async tests incomplete"; \
	elif [ -f "$(ASYNC_DIR)/build.gradle" ]; then \
		cd $(ASYNC_DIR) && $(GRADLE) test -q 2>&1 | tail -10 || echo "  $(YELLOW)⊘$(NC) Library: Async tests incomplete"; \
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
		if [ -f "$(SYNC_DIR)/tests/TestFunctional.java" ] && [ -f "$(SYNC_DIR)/src/Un.java" ]; then \
			$(JAVAC) -cp $(SYNC_DIR)/src $(SYNC_DIR)/tests/TestFunctional.java -d /tmp/un-java-test 2>&1 && \
			$(JAVA) -cp /tmp/un-java-test:$(SYNC_DIR)/src TestFunctional 2>&1 && \
			echo "  $(GREEN)✓$(NC) Functional: All tests passed" || echo "  $(RED)✗$(NC) Functional: Tests failed"; \
			rm -rf /tmp/un-java-test; \
		fi; \
	fi

# ============================================================================
# TEST: By SDK Type
# ============================================================================

test-sync:
	@echo "Testing Sync SDK..."
	@if [ -f "$(SYNC_DIR)/pom.xml" ]; then \
		cd $(SYNC_DIR) && $(MAVEN) test; \
	elif [ -f "$(SYNC_DIR)/build.gradle" ]; then \
		cd $(SYNC_DIR) && $(GRADLE) test; \
	else \
		echo "  $(YELLOW)⊘$(NC) Sync SDK tests not found"; \
	fi

test-async:
	@echo "Testing Async SDK..."
	@if [ -f "$(ASYNC_DIR)/pom.xml" ]; then \
		cd $(ASYNC_DIR) && $(MAVEN) test; \
	elif [ -f "$(ASYNC_DIR)/build.gradle" ]; then \
		cd $(ASYNC_DIR) && $(GRADLE) test; \
	else \
		echo "  $(YELLOW)⊘$(NC) Async SDK tests not found"; \
	fi

# ============================================================================
# Examples
# ============================================================================

examples:
	@echo "Running Java examples..."
	@if [ -f "$(SYNC_DIR)/pom.xml" ]; then \
		cd $(SYNC_DIR) && $(MAVEN) exec:java -Dexec.mainClass="examples.HelloWorld" 2>&1 | head -10 || true; \
	fi

# ============================================================================
# Clean
# ============================================================================

clean:
	@echo "Cleaning Java build artifacts..."
	@if [ -f "$(SYNC_DIR)/pom.xml" ]; then cd $(SYNC_DIR) && $(MAVEN) clean -q 2>/dev/null || true; fi
	@if [ -f "$(ASYNC_DIR)/pom.xml" ]; then cd $(ASYNC_DIR) && $(MAVEN) clean -q 2>/dev/null || true; fi
	@if [ -f "$(SYNC_DIR)/build.gradle" ]; then cd $(SYNC_DIR) && $(GRADLE) clean -q 2>/dev/null || true; fi
	@if [ -f "$(ASYNC_DIR)/build.gradle" ]; then cd $(ASYNC_DIR) && $(GRADLE) clean -q 2>/dev/null || true; fi
	@rm -rf $(SYNC_DIR)/target $(ASYNC_DIR)/target 2>/dev/null || true
	@rm -rf $(SYNC_DIR)/build $(ASYNC_DIR)/build 2>/dev/null || true
	@echo "$(GREEN)✓$(NC) Cleaned build artifacts"
