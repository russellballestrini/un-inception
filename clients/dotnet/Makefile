# UN CLI - .NET 10 Implementation

.PHONY: build run clean test help

BUILD_DIR := sync/src
BINARY := sync/src/bin/Release/net10.0/un

build:
	cd $(BUILD_DIR) && dotnet build -c Release

run:
	cd $(BUILD_DIR) && dotnet run --

clean:
	cd $(BUILD_DIR) && dotnet clean
	rm -rf $(BUILD_DIR)/bin $(BUILD_DIR)/obj

test: build
	@echo "Testing --help..."
	cd $(BUILD_DIR) && dotnet run -- --help
	@echo ""
	@echo "Testing --version..."
	cd $(BUILD_DIR) && dotnet run -- --version

test-cli: build
	@echo "CLI tests require UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY"
	cd $(BUILD_DIR) && dotnet run -- key

help:
	@echo "UN CLI (.NET 10) Makefile"
	@echo ""
	@echo "Targets:"
	@echo "  build     Build the CLI"
	@echo "  run       Run the CLI"
	@echo "  clean     Clean build artifacts"
	@echo "  test      Run basic tests"
	@echo "  test-cli  Run CLI tests (requires API keys)"
	@echo "  help      Show this help"
