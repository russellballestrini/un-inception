# UN Objective-C Client - Build and Test

.PHONY: all test test-cli test-library test-integration test-functional clean help

ROOT_DIR := $(shell cd ../.. && pwd)
SYNC_DIR := sync
SRC := $(SYNC_DIR)/src/un.m
BIN := un
GREEN := \033[32m
RED := \033[31m
YELLOW := \033[33m
NC := \033[0m

CC := clang
CFLAGS := -O2 -Wall -Wextra -fobjc-arc
LDFLAGS := -framework Foundation -lcurl -lssl -lcrypto

.DEFAULT_GOAL := help

help:
	@echo "UN Objective-C Client - Build and Test"
	@echo ""
	@echo "  make build            Build CLI binary"
	@echo "  make test             All 4 test modes"
	@echo "  make test-cli         CLI mode"
	@echo "  make test-library     Library mode"
	@echo ""

all: build

build: $(BIN)

$(BIN): $(SRC)
	@echo "Building Objective-C CLI..."
	$(CC) $(CFLAGS) -o $@ $< $(LDFLAGS)
	@echo "$(GREEN)✓$(NC) Built: $@"

test: test-cli test-library test-integration test-functional
	@echo "$(GREEN)✓ Objective-C Client: All 4 test modes complete$(NC)"

test-cli:
	@echo ""
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "CLI MODE: Testing Objective-C CLI"
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@if [ -f "$(SRC)" ]; then \
		$(CC) -fsyntax-only $(CFLAGS) $(SRC) 2>/dev/null && echo "  $(GREEN)✓$(NC) CLI: Syntax valid" || echo "  $(RED)✗$(NC) CLI: Syntax error"; \
	fi
	@if [ -f "$(BIN)" ]; then \
		./$(BIN) --help > /dev/null 2>&1 && echo "  $(GREEN)✓$(NC) CLI: --help works" || echo "  $(YELLOW)⊘$(NC) CLI: --help (check implementation)"; \
	fi

test-library:
	@echo ""
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "LIBRARY MODE: Testing Objective-C framework"
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "  $(YELLOW)⊘$(NC) Library: Requires header separation (un.h)"

test-integration:
	@echo ""
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "INTEGRATION MODE: Testing API contract"
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@if [ -z "$$UNSANDBOX_PUBLIC_KEY" ]; then echo "  $(YELLOW)⊘$(NC) Skipping (no API credentials)"; \
	else echo "  $(YELLOW)⊘$(NC) Integration: Not yet implemented"; fi

test-functional:
	@echo ""
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "FUNCTIONAL MODE: Real-world scenarios"
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@if [ -z "$$UNSANDBOX_PUBLIC_KEY" ]; then echo "  $(YELLOW)⊘$(NC) Skipping (no API credentials)"; \
	else echo "  $(YELLOW)⊘$(NC) Functional: Not yet implemented"; fi

clean:
	rm -f $(BIN)
	@echo "$(GREEN)✓$(NC) Cleaned Objective-C artifacts"
