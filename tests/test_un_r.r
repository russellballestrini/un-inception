#!/usr/bin/env Rscript
# Comprehensive tests for un.r (R UN CLI Inception implementation)
# Run with: Rscript test_un_r.r

# Color codes
GREEN <- "\033[32m"
RED <- "\033[31m"
BLUE <- "\033[34m"
RESET <- "\033[0m"

# Test counters
passed <- 0
failed <- 0

# Extension to language mapping (from un.r)
ext_map <- list(
    ".jl" = "julia",
    ".r" = "r",
    ".cr" = "crystal",
    ".f90" = "fortran",
    ".cob" = "cobol",
    ".pro" = "prolog",
    ".forth" = "forth",
    ".4th" = "forth",
    ".py" = "python",
    ".js" = "javascript",
    ".rb" = "ruby",
    ".go" = "go",
    ".rs" = "rust",
    ".c" = "c",
    ".cpp" = "cpp",
    ".java" = "java",
    ".sh" = "bash"
)

detect_language <- function(filename) {
    ext <- tolower(sub(".*(\\..*?)$", "\\1", filename))
    lang <- ext_map[[ext]]
    if (is.null(lang)) {
        return("unknown")
    }
    return(lang)
}

print_test <- function(name, result) {
    if (result) {
        cat(sprintf("%s✓ PASS%s: %s\n", GREEN, RESET, name))
        passed <<- passed + 1
    } else {
        cat(sprintf("%s✗ FAIL%s: %s\n", RED, RESET, name))
        failed <<- failed + 1
    }
}

cat(sprintf("\n%s========================================%s\n", BLUE, RESET))
cat(sprintf("%sUN CLI Inception Tests - R%s\n", BLUE, RESET))
cat(sprintf("%s========================================%s\n\n", BLUE, RESET))

# Test 1: Extension detection tests
cat(sprintf("%sTest Suite 1: Extension Detection%s\n", BLUE, RESET))
print_test("Detect .jl as julia", detect_language("test.jl") == "julia")
print_test("Detect .r as r", detect_language("test.r") == "r")
print_test("Detect .cr as crystal", detect_language("test.cr") == "crystal")
print_test("Detect .f90 as fortran", detect_language("test.f90") == "fortran")
print_test("Detect .cob as cobol", detect_language("test.cob") == "cobol")
print_test("Detect .pro as prolog", detect_language("test.pro") == "prolog")
print_test("Detect .forth as forth", detect_language("test.forth") == "forth")
print_test("Detect .4th as forth", detect_language("test.4th") == "forth")
print_test("Detect .py as python", detect_language("test.py") == "python")
print_test("Detect .rs as rust", detect_language("test.rs") == "rust")
print_test("Detect unknown extension", detect_language("test.xyz") == "unknown")

# Test 2: API Integration Test
cat(sprintf("\n%sTest Suite 2: API Integration%s\n", BLUE, RESET))
api_key <- Sys.getenv("UNSANDBOX_API_KEY")
if (api_key == "") {
    cat(sprintf("%sℹ SKIP%s: API integration test (UNSANDBOX_API_KEY not set)\n", BLUE, RESET))
} else {
    tryCatch({
        library(httr)
        library(jsonlite)

        url <- "https://api.unsandbox.com/execute"
        headers <- add_headers(
            `Content-Type` = "application/json",
            `Authorization` = paste("Bearer", api_key)
        )
        body <- toJSON(list(
            language = "python",
            code = "print('Hello from test')"
        ), auto_unbox = TRUE)

        response <- POST(url, headers, body = body, encode = "raw")
        result <- fromJSON(content(response, "text", encoding = "UTF-8"))

        api_works <- !is.null(result$stdout) && grepl("Hello from test", result$stdout)
        print_test("API endpoint reachable and functional", api_works)
    }, error = function(e) {
        print_test("API endpoint reachable and functional", FALSE)
        cat(sprintf("  Error: %s\n", e$message))
    })
}

# Test 3: End-to-end functional test
cat(sprintf("\n%sTest Suite 3: End-to-End Functional Test%s\n", BLUE, RESET))
if (api_key == "") {
    cat(sprintf("%sℹ SKIP%s: E2E test (UNSANDBOX_API_KEY not set)\n", BLUE, RESET))
} else {
    fib_file <- "../../test/fib.r"
    if (!file.exists(fib_file)) {
        fib_file <- "/home/fox/git/unsandbox.com/cli/test/fib.r"
    }

    if (file.exists(fib_file)) {
        tryCatch({
            un_script <- "../un.r"
            if (!file.exists(un_script)) {
                un_script <- "/home/fox/git/unsandbox.com/cli/inception/un.r"
            }

            result <- system2("Rscript", args = c(un_script, fib_file),
                            stdout = TRUE, stderr = TRUE)
            result_str <- paste(result, collapse = "\n")

            has_fib10 <- grepl("fib\\(10\\) = 55", result_str)
            has_fib5 <- grepl("fib\\(5\\) = 5", result_str)
            has_fib0 <- grepl("fib\\(0\\) = 0", result_str)

            print_test("E2E: fib.r produces fib(10) = 55", has_fib10)
            print_test("E2E: fib.r produces fib(5) = 5", has_fib5)
            print_test("E2E: fib.r produces fib(0) = 0", has_fib0)
        }, error = function(e) {
            print_test("E2E: fib.r execution", FALSE)
            cat(sprintf("  Error: %s\n", e$message))
        })
    } else {
        cat(sprintf("%sℹ SKIP%s: E2E test (fib.r not found at expected location)\n", BLUE, RESET))
    }
}

# Test 4: Error handling tests
cat(sprintf("\n%sTest Suite 4: Error Handling%s\n", BLUE, RESET))
print_test("Unknown extension returns 'unknown'", detect_language("file.unknown") == "unknown")
print_test("Case insensitive detection", detect_language("TEST.R") == "r")
print_test("Multiple dots in filename", detect_language("my.test.py") == "python")

# Print summary
cat(sprintf("\n%s========================================%s\n", BLUE, RESET))
cat(sprintf("%sTest Summary%s\n", BLUE, RESET))
cat(sprintf("%s========================================%s\n", BLUE, RESET))
cat(sprintf("%sPassed: %d%s\n", GREEN, passed, RESET))
cat(sprintf("%sFailed: %d%s\n", RED, failed, RESET))
cat(sprintf("%sTotal:  %d%s\n", BLUE, passed + failed, RESET))

if (failed > 0) {
    cat(sprintf("\n%sTESTS FAILED%s\n", RED, RESET))
    quit(status = 1)
} else {
    cat(sprintf("\n%sALL TESTS PASSED%s\n", GREEN, RESET))
    quit(status = 0)
}
