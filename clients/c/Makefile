# UN C Client - Build and Test
#
# Usage:
#   make              # Build un binary
#   make test         # Run all 4 test modes
#   make test-cli     # CLI mode only
#   make test-library # Library mode only
#   make test-integration # Integration mode only
#   make test-functional  # Functional mode only
#   make clean        # Remove build artifacts
#
# Dependencies:
#   apt install build-essential libcurl4-openssl-dev libwebsockets-dev libssl-dev

.PHONY: all build test test-cli test-library test-integration test-functional clean help examples

# Paths
ROOT_DIR := $(shell cd ../.. && pwd)
SRC_DIR := src
SRC := $(SRC_DIR)/un.c
HEADER := $(SRC_DIR)/un.h
BIN := un
TEST_DIR := tests
EXAMPLES_DIR := examples

# Compiler settings
CC := gcc
CFLAGS := -O2 -Wall -Wextra -I$(SRC_DIR)
CFLAGS_LIB := $(CFLAGS) -DUNSANDBOX_LIBRARY
LDFLAGS := -lcurl -lwebsockets -lssl -lcrypto

# Colors
GREEN := \033[32m
RED := \033[31m
YELLOW := \033[33m
NC := \033[0m

.DEFAULT_GOAL := build

help:
	@echo "UN C Client - Build and Test"
	@echo ""
	@echo "Build:"
	@echo "  make              Build library and examples"
	@echo "  make build        Same as above"
	@echo "  make lib          Build just the library"
	@echo "  make examples     Build example programs"
	@echo ""
	@echo "Test:"
	@echo "  make test         Run all tests"
	@echo "  make test-library Test library functions"
	@echo ""
	@echo "Utility:"
	@echo "  make clean        Remove build artifacts"
	@echo "  make deps         Show required dependencies"
	@echo ""

all: build

build: lib examples

lib: $(SRC) $(HEADER)
	@echo "$(GREEN)✓$(NC) Library files ready:"
	@echo "  - $(SRC_DIR)/un.c (implementation)"
	@echo "  - $(SRC_DIR)/un.h (header)"

examples: $(EXAMPLES_DIR)/hello_world $(EXAMPLES_DIR)/fibonacci
	@echo "$(GREEN)✓$(NC) Examples built"

$(EXAMPLES_DIR)/hello_world: $(EXAMPLES_DIR)/hello_world.c $(SRC) $(HEADER)
	@mkdir -p $(EXAMPLES_DIR)
	@echo "Building hello_world example..."
	$(CC) $(CFLAGS_LIB) -o $@ $< $(SRC) $(LDFLAGS)
	@echo "$(GREEN)✓$(NC) Built: $@"

$(EXAMPLES_DIR)/fibonacci: $(EXAMPLES_DIR)/fibonacci.c $(SRC) $(HEADER)
	@mkdir -p $(EXAMPLES_DIR)
	@echo "Building fibonacci example..."
	$(CC) $(CFLAGS_LIB) -o $@ $< $(SRC) $(LDFLAGS)
	@echo "$(GREEN)✓$(NC) Built: $@"

deps:
	@echo "Required packages:"
	@echo "  apt install build-essential libcurl4-openssl-dev libwebsockets-dev libssl-dev"

# ============================================================================
# TEST
# ============================================================================

test: build $(TEST_DIR)/test_library
	@echo ""
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "LIBRARY MODE: Testing un.c functions"
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo ""
	@./$(TEST_DIR)/test_library

test-library: test

$(TEST_DIR)/test_library: $(TEST_DIR)/test_library.c $(SRC) $(HEADER)
	@mkdir -p $(TEST_DIR)
	@echo "Building test suite..."
	$(CC) $(CFLAGS_LIB) -o $@ $< $(SRC) $(LDFLAGS)
	@echo "$(GREEN)✓$(NC) Test binary ready"

# ============================================================================
# Clean
# ============================================================================

clean:
	rm -f $(TEST_DIR)/test_library
	rm -f $(EXAMPLES_DIR)/hello_world
	rm -f $(EXAMPLES_DIR)/fibonacci
	rm -f $(EXAMPLES_DIR)/*.o
	rm -f $(TEST_DIR)/*.o
	@echo "$(GREEN)✓$(NC) Cleaned build artifacts"
