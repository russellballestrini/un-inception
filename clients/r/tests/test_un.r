#!/usr/bin/env Rscript
# Test suite for R Unsandbox SDK
# Run: Rscript tests/test_un.r

# Load the SDK
source(file.path(dirname(sys.frame(1)$ofile), "..", "sync", "src", "un.r"))

# Test helper
test_that <- function(description, test_expr) {
    result <- tryCatch({
        test_expr
        TRUE
    }, error = function(e) {
        FALSE
    })
    status <- if (result) "\033[32mPASS\033[0m" else "\033[31mFAIL\033[0m"
    cat(sprintf("[%s] %s\n", status, description))
    return(result)
}

all_passed <- TRUE

# Test Constants
cat("\n=== Constants ===\n")
all_passed <- all_passed && test_that("API_BASE is correct", {
    stopifnot(API_BASE == "https://api.unsandbox.com")
})

all_passed <- all_passed && test_that("PORTAL_BASE is correct", {
    stopifnot(PORTAL_BASE == "https://unsandbox.com")
})

# Test Language Detection
cat("\n=== Language Detection ===\n")
all_passed <- all_passed && test_that("detect Python", {
    stopifnot(detect_language("test.py") == "python")
})

all_passed <- all_passed && test_that("detect JavaScript", {
    stopifnot(detect_language("test.js") == "javascript")
})

all_passed <- all_passed && test_that("detect Ruby", {
    stopifnot(detect_language("test.rb") == "ruby")
})

all_passed <- all_passed && test_that("detect Julia", {
    stopifnot(detect_language("test.jl") == "julia")
})

all_passed <- all_passed && test_that("detect R", {
    stopifnot(detect_language("test.r") == "r")
})

all_passed <- all_passed && test_that("detect unknown", {
    stopifnot(detect_language("test.xyz") == "unknown")
})

# Test HMAC signing
cat("\n=== HMAC Signing ===\n")
all_passed <- all_passed && test_that("hmac_sign returns 64 char hex", {
    sig <- hmac_sign("secret", "message")
    stopifnot(is.character(sig))
    stopifnot(nchar(sig) == 64)
})

all_passed <- all_passed && test_that("hmac_sign is deterministic", {
    sig1 <- hmac_sign("secret", "message")
    sig2 <- hmac_sign("secret", "message")
    stopifnot(sig1 == sig2)
})

# Test Version
cat("\n=== Version ===\n")
all_passed <- all_passed && test_that("sdk_version returns string", {
    v <- sdk_version()
    stopifnot(is.character(v))
    stopifnot(nchar(v) > 0)
})

# Test Library Functions Exist
cat("\n=== Library Function Existence ===\n")

# Execution functions
all_passed <- all_passed && test_that("execute function exists", {
    stopifnot(exists("execute") && is.function(execute))
})
all_passed <- all_passed && test_that("execute_async function exists", {
    stopifnot(exists("execute_async") && is.function(execute_async))
})
all_passed <- all_passed && test_that("get_job function exists", {
    stopifnot(exists("get_job") && is.function(get_job))
})
all_passed <- all_passed && test_that("wait function exists", {
    stopifnot(exists("wait") && is.function(wait))
})
all_passed <- all_passed && test_that("cancel_job function exists", {
    stopifnot(exists("cancel_job") && is.function(cancel_job))
})
all_passed <- all_passed && test_that("list_jobs function exists", {
    stopifnot(exists("list_jobs") && is.function(list_jobs))
})
all_passed <- all_passed && test_that("languages function exists", {
    stopifnot(exists("languages") && is.function(languages))
})

# Session functions
all_passed <- all_passed && test_that("session_list function exists", {
    stopifnot(exists("session_list") && is.function(session_list))
})
all_passed <- all_passed && test_that("session_get function exists", {
    stopifnot(exists("session_get") && is.function(session_get))
})
all_passed <- all_passed && test_that("session_create function exists", {
    stopifnot(exists("session_create") && is.function(session_create))
})
all_passed <- all_passed && test_that("session_destroy function exists", {
    stopifnot(exists("session_destroy") && is.function(session_destroy))
})
all_passed <- all_passed && test_that("session_freeze function exists", {
    stopifnot(exists("session_freeze") && is.function(session_freeze))
})
all_passed <- all_passed && test_that("session_unfreeze function exists", {
    stopifnot(exists("session_unfreeze") && is.function(session_unfreeze))
})
all_passed <- all_passed && test_that("session_execute function exists", {
    stopifnot(exists("session_execute") && is.function(session_execute))
})

# Service functions
all_passed <- all_passed && test_that("service_list function exists", {
    stopifnot(exists("service_list") && is.function(service_list))
})
all_passed <- all_passed && test_that("service_get function exists", {
    stopifnot(exists("service_get") && is.function(service_get))
})
all_passed <- all_passed && test_that("service_create function exists", {
    stopifnot(exists("service_create") && is.function(service_create))
})
all_passed <- all_passed && test_that("service_destroy function exists", {
    stopifnot(exists("service_destroy") && is.function(service_destroy))
})
all_passed <- all_passed && test_that("service_env_get function exists", {
    stopifnot(exists("service_env_get") && is.function(service_env_get))
})
all_passed <- all_passed && test_that("service_env_set function exists", {
    stopifnot(exists("service_env_set") && is.function(service_env_set))
})
all_passed <- all_passed && test_that("service_env_delete function exists", {
    stopifnot(exists("service_env_delete") && is.function(service_env_delete))
})

# Snapshot functions
all_passed <- all_passed && test_that("snapshot_list function exists", {
    stopifnot(exists("snapshot_list") && is.function(snapshot_list))
})
all_passed <- all_passed && test_that("snapshot_get function exists", {
    stopifnot(exists("snapshot_get") && is.function(snapshot_get))
})
all_passed <- all_passed && test_that("snapshot_session function exists", {
    stopifnot(exists("snapshot_session") && is.function(snapshot_session))
})
all_passed <- all_passed && test_that("snapshot_service function exists", {
    stopifnot(exists("snapshot_service") && is.function(snapshot_service))
})
all_passed <- all_passed && test_that("snapshot_restore function exists", {
    stopifnot(exists("snapshot_restore") && is.function(snapshot_restore))
})
all_passed <- all_passed && test_that("snapshot_delete function exists", {
    stopifnot(exists("snapshot_delete") && is.function(snapshot_delete))
})
all_passed <- all_passed && test_that("snapshot_clone function exists", {
    stopifnot(exists("snapshot_clone") && is.function(snapshot_clone))
})

# Image functions
all_passed <- all_passed && test_that("image_list function exists", {
    stopifnot(exists("image_list") && is.function(image_list))
})
all_passed <- all_passed && test_that("image_get function exists", {
    stopifnot(exists("image_get") && is.function(image_get))
})
all_passed <- all_passed && test_that("image_publish function exists", {
    stopifnot(exists("image_publish") && is.function(image_publish))
})
all_passed <- all_passed && test_that("image_delete function exists", {
    stopifnot(exists("image_delete") && is.function(image_delete))
})
all_passed <- all_passed && test_that("image_spawn function exists", {
    stopifnot(exists("image_spawn") && is.function(image_spawn))
})
all_passed <- all_passed && test_that("image_clone function exists", {
    stopifnot(exists("image_clone") && is.function(image_clone))
})

# Utility functions
all_passed <- all_passed && test_that("validate_keys function exists", {
    stopifnot(exists("validate_keys") && is.function(validate_keys))
})
all_passed <- all_passed && test_that("health_check function exists", {
    stopifnot(exists("health_check") && is.function(health_check))
})
all_passed <- all_passed && test_that("logs_fetch function exists", {
    stopifnot(exists("logs_fetch") && is.function(logs_fetch))
})

# Summary
cat("\n=== Summary ===\n")
if (all_passed) {
    cat("\033[32mAll tests passed!\033[0m\n")
    quit(status = 0)
} else {
    cat("\033[31mSome tests failed!\033[0m\n")
    quit(status = 1)
}
