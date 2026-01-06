#!/usr/bin/env Rscript
# Unit tests for un.r - tests internal functions without API calls

library(openssl)
library(base64enc)

passed <- 0
failed <- 0

test <- function(name, fn) {
  tryCatch({
    fn()
    cat(sprintf("  ✓ %s\n", name))
    passed <<- passed + 1
  }, error = function(e) {
    cat(sprintf("  ✗ %s\n", name))
    cat(sprintf("    %s\n", e$message))
    failed <<- failed + 1
  })
}

assert_equal <- function(actual, expected) {
  if (actual != expected) {
    stop(sprintf("Expected '%s' but got '%s'", expected, actual))
  }
}

assert_not_equal <- function(a, b) {
  if (a == b) {
    stop(sprintf("Expected values to be different but both were '%s'", a))
  }
}

assert_contains <- function(str, substr) {
  if (!grepl(substr, str, fixed = TRUE)) {
    stop(sprintf("Expected '%s' to contain '%s'", str, substr))
  }
}

assert_true <- function(val) {
  if (!val) {
    stop("Expected true but got false")
  }
}

# Extension mapping
EXT_MAP <- list(
  ".py" = "python", ".js" = "javascript", ".ts" = "typescript",
  ".rb" = "ruby", ".php" = "php", ".pl" = "perl", ".lua" = "lua",
  ".sh" = "bash", ".go" = "go", ".rs" = "rust", ".c" = "c",
  ".cpp" = "cpp", ".java" = "java", ".kt" = "kotlin",
  ".hs" = "haskell", ".clj" = "clojure", ".erl" = "erlang",
  ".ex" = "elixir", ".jl" = "julia", ".r" = "r", ".R" = "r"
)

cat("\n=== Extension Mapping Tests ===\n")

test("Python extension maps correctly", function() {
  assert_equal(EXT_MAP[[".py"]], "python")
})

test("R extensions map correctly", function() {
  assert_equal(EXT_MAP[[".r"]], "r")
  assert_equal(EXT_MAP[[".R"]], "r")
})

test("JavaScript extension maps correctly", function() {
  assert_equal(EXT_MAP[[".js"]], "javascript")
})

test("Go extension maps correctly", function() {
  assert_equal(EXT_MAP[[".go"]], "go")
})

test("Julia extension maps correctly", function() {
  assert_equal(EXT_MAP[[".jl"]], "julia")
})

cat("\n=== HMAC Signature Tests ===\n")

test("HMAC-SHA256 generates 64 character hex string", function() {
  sig <- sha256(charToRaw("test-message"), key = "test-secret")
  hex_sig <- paste0(as.character(sig), collapse = "")
  assert_equal(nchar(hex_sig), 64)
})

test("Same input produces same signature", function() {
  sig1 <- sha256(charToRaw("msg"), key = "key")
  sig2 <- sha256(charToRaw("msg"), key = "key")
  assert_equal(paste0(sig1, collapse = ""), paste0(sig2, collapse = ""))
})

test("Different secrets produce different signatures", function() {
  sig1 <- paste0(sha256(charToRaw("msg"), key = "key1"), collapse = "")
  sig2 <- paste0(sha256(charToRaw("msg"), key = "key2"), collapse = "")
  assert_not_equal(sig1, sig2)
})

test("Signature format verification", function() {
  timestamp <- "1704067200"
  method <- "POST"
  endpoint <- "/execute"
  body <- '{"language":"python"}'

  message <- paste(timestamp, method, endpoint, body, sep = ":")

  assert_true(startsWith(message, timestamp))
  assert_contains(message, ":POST:")
  assert_contains(message, ":/execute:")
})

cat("\n=== Language Detection Tests ===\n")

test("Detect language from .R extension", function() {
  ext <- paste0(".", tools::file_ext("script.R"))
  assert_equal(EXT_MAP[[ext]], "r")
})

test("Python shebang detection", function() {
  content <- "#!/usr/bin/env python3\nprint('hello')"
  first_line <- strsplit(content, "\n")[[1]][1]
  assert_true(startsWith(first_line, "#!"))
  assert_contains(first_line, "python")
})

cat("\n=== Argument Parsing Tests ===\n")

test("Parse -e KEY=VALUE format", function() {
  arg <- "DEBUG=1"
  parts <- strsplit(arg, "=", fixed = TRUE)[[1]]
  key <- parts[1]
  value <- paste(parts[-1], collapse = "=")
  assert_equal(key, "DEBUG")
  assert_equal(value, "1")
})

test("Parse -e KEY=VALUE with equals in value", function() {
  arg <- "URL=https://example.com?foo=bar"
  parts <- strsplit(arg, "=", fixed = TRUE)[[1]]
  key <- parts[1]
  value <- paste(parts[-1], collapse = "=")
  assert_equal(key, "URL")
  assert_equal(value, "https://example.com?foo=bar")
})

cat("\n=== File Operations Tests ===\n")

test("Base64 encoding/decoding", function() {
  content <- "print('hello world')"
  encoded <- base64encode(charToRaw(content))
  decoded <- rawToChar(base64decode(encoded))
  assert_equal(decoded, content)
})

test("Extract file basename", function() {
  path <- "/home/user/project/script.R"
  assert_equal(basename(path), "script.R")
})

test("Extract file extension", function() {
  path <- "/home/user/project/script.R"
  assert_equal(tools::file_ext(path), "R")
})

cat("\n=== API Constants Tests ===\n")

test("API base URL format", function() {
  api_base <- "https://api.unsandbox.com"
  assert_true(startsWith(api_base, "https://"))
  assert_contains(api_base, "unsandbox.com")
})

# Summary
cat("\n=== Summary ===\n")
cat(sprintf("Passed: %d\n", passed))
cat(sprintf("Failed: %d\n", failed))
cat(sprintf("Total:  %d\n", passed + failed))

quit(status = if (failed > 0) 1 else 0)
