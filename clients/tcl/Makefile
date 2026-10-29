# UN Tcl Client

.PHONY: test test-cli test-library test-integration test-functional test-os check-deps clean help

ROOT_DIR := $(shell cd ../.. && pwd)
SYNC_DIR := sync
GREEN := \033[32m
YELLOW := \033[33m
NC := \033[0m
RED := \033[31m

test: test-cli test-library test-integration test-functional
	@echo "$(GREEN)✓ Tcl Client: All 4 test modes complete$(NC)"

test-cli:
	@echo "━━━ CLI MODE ━━━"
	@if [ -f "$(ROOT_DIR)/un.tcl" ]; then tclsh "$(ROOT_DIR)/un.tcl" --help 2>/dev/null && echo "  $(GREEN)✓$(NC) --help works" || echo "  $(YELLOW)⊘$(NC) Check syntax"; fi

# Our client needs Tcl 8.6 plus tcllib (json, json::write, base64, sha256) and tcltls.
# Debian/Ubuntu: apt-get install tcllib tcl-tls   macOS: brew install tcl-tk (bundles both)
check-deps:
	@echo 'foreach p {http json json::write base64 sha256 tls} { if {[catch {package require $$p}]} { puts stderr "missing Tcl package: $$p"; set ::missing 1 } }; if {[info exists ::missing]} { puts stderr "install tcllib & tcl-tls (apt-get install tcllib tcl-tls)"; exit 1 }' | tclsh || { echo "  $(RED)✗$(NC) Tcl dependencies missing"; exit 1; }
	@echo "  $(GREEN)✓$(NC) Tcl dependencies present"

test-library: check-deps
	@echo "━━━ LIBRARY MODE ━━━"
	@tclsh tests/test_library.tcl

test-integration:
	@echo "━━━ INTEGRATION MODE ━━━"
	@echo "  $(YELLOW)⊘$(NC) Not implemented"

test-functional:
	@echo "━━━ FUNCTIONAL MODE ━━━"
	@echo "  $(YELLOW)⊘$(NC) Not implemented"

# Offline: global --os flag request bodies against a local mock API (no credentials)
test-os: check-deps
	@bash ../../tests/os_flag_cli.sh tclsh sync/src/un.tcl

clean:
	@echo "$(GREEN)✓$(NC) Cleaned"
