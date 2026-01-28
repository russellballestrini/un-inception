#!/usr/bin/env Rscript
# PUBLIC DOMAIN - NO LICENSE, NO WARRANTY
#
# This is free public domain software for the public good of a permacomputer hosted
# at permacomputer.com - an always-on computer by the people, for the people. One
# which is durable, easy to repair, and distributed like tap water for machine
# learning intelligence.
#
# The permacomputer is community-owned infrastructure optimized around four values:
#
#   TRUTH    - First principles, math & science, open source code freely distributed
#   FREEDOM  - Voluntary partnerships, freedom from tyranny & corporate control
#   HARMONY  - Minimal waste, self-renewing systems with diverse thriving connections
#   LOVE     - Be yourself without hurting others, cooperation through natural law
#
# This software contributes to that vision by enabling code execution across 42+
# programming languages through a unified interface, accessible to all. Code is
# seeds to sprout on any abandoned technology.
#
# Learn more: https://www.permacomputer.com
#
# Anyone is free to copy, modify, publish, use, compile, sell, or distribute this
# software, either in source code form or as a compiled binary, for any purpose,
# commercial or non-commercial, and by any means.
#
# NO WARRANTY. THE SOFTWARE IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY KIND.
#
# That said, our permacomputer's digital membrane stratum continuously runs unit,
# integration, and functional tests on all of it's own software - with our
# permacomputer monitoring itself, repairing itself, with minimal human in the
# loop guidance. Our agents do their best.
#
# Copyright 2025 TimeHexOn & foxhop & russell@unturf
# https://www.timehexon.com
# https://www.foxhop.net
# https://www.unturf.com/software

#!/usr/bin/env Rscript

#' @title Unsandbox R SDK
#' @description R client library and CLI for the Unsandbox code execution platform.
#' Provides both a programmatic API for library usage and a command-line interface.
#' @details
#' The Unsandbox SDK enables secure code execution across 42+ programming languages
#' through a unified interface. It supports synchronous and asynchronous execution,
#' job management, session handling, and persistent services.
#'
#' Authentication uses HMAC-SHA256 signatures with the format:
#' \code{HMAC(secret_key, "timestamp:METHOD:path:body")}
#'
#' Credentials are loaded in priority order:
#' \enumerate{
#'   \item Function arguments (public_key, secret_key)
#'   \item Environment variables (UNSANDBOX_PUBLIC_KEY, UNSANDBOX_SECRET_KEY)
#'   \item Accounts file (~/.unsandbox/accounts.csv)
#' }
#' @name unsandbox
#' @docType package
NULL

library(httr)
library(jsonlite)
library(digest)

# Extension to language mapping
ext_map <- list(
    ".jl" = "julia", ".r" = "r", ".cr" = "crystal",
    ".f90" = "fortran", ".cob" = "cobol", ".pro" = "prolog",
    ".forth" = "forth", ".4th" = "forth", ".py" = "python",
    ".js" = "javascript", ".ts" = "typescript", ".rb" = "ruby",
    ".php" = "php", ".pl" = "perl", ".lua" = "lua", ".sh" = "bash",
    ".go" = "go", ".rs" = "rust", ".c" = "c", ".cpp" = "cpp",
    ".cc" = "cpp", ".cxx" = "cpp", ".java" = "java", ".kt" = "kotlin",
    ".cs" = "csharp", ".fs" = "fsharp", ".hs" = "haskell",
    ".ml" = "ocaml", ".clj" = "clojure", ".scm" = "scheme",
    ".lisp" = "commonlisp", ".erl" = "erlang", ".ex" = "elixir",
    ".exs" = "elixir", ".d" = "d", ".nim" = "nim", ".zig" = "zig",
    ".v" = "v", ".dart" = "dart", ".groovy" = "groovy",
    ".scala" = "scala", ".tcl" = "tcl", ".raku" = "raku", ".m" = "objc"
)

# ANSI color codes
BLUE <- "\033[34m"
RED <- "\033[31m"
GREEN <- "\033[32m"
YELLOW <- "\033[33m"
RESET <- "\033[0m"

#' @title API Base URL
#' @description Base URL for the Unsandbox API
#' @export
API_BASE <- "https://api.unsandbox.com"

#' @title Portal Base URL
#' @description Base URL for the Unsandbox web portal
#' @export
PORTAL_BASE <- "https://unsandbox.com"

#' @title Languages Cache TTL
#' @description Cache time-to-live in seconds (1 hour)
#' @export
LANGUAGES_CACHE_TTL <- 3600

MAX_ENV_CONTENT_SIZE <- 65536

# =============================================================================
# Credential Management
# =============================================================================

#' Get Credentials
#'
#' Retrieves API credentials from multiple sources in priority order:
#' arguments, environment variables, or accounts file.
#'
#' @param public_key Optional public key override
#' @param secret_key Optional secret key override
#' @return A list with public_key and secret_key
#' @export
#' @examples
#' \dontrun{
#' creds <- get_credentials()
#' creds <- get_credentials(public_key = "unsb-pk-xxxx", secret_key = "unsb-sk-xxxx")
#' }
get_credentials <- function(public_key = NULL, secret_key = NULL) {
    # Priority 1: Function arguments
    if (!is.null(public_key) && !is.null(secret_key)) {
        return(list(public_key = public_key, secret_key = secret_key))
    }

    # Priority 2: Environment variables
    env_public <- Sys.getenv("UNSANDBOX_PUBLIC_KEY")
    env_secret <- Sys.getenv("UNSANDBOX_SECRET_KEY")
    if (env_public != "" && env_secret != "") {
        return(list(public_key = env_public, secret_key = env_secret))
    }

    # Priority 3: Accounts file
    accounts_file <- file.path(Sys.getenv("HOME"), ".unsandbox", "accounts.csv")
    if (file.exists(accounts_file)) {
        lines <- readLines(accounts_file, warn = FALSE)
        for (line in lines) {
            parts <- strsplit(trimws(line), ",")[[1]]
            if (length(parts) >= 2) {
                return(list(public_key = parts[1], secret_key = parts[2]))
            }
        }
    }

    # Fallback to legacy UNSANDBOX_API_KEY
    legacy_key <- Sys.getenv("UNSANDBOX_API_KEY")
    if (legacy_key != "") {
        return(list(public_key = legacy_key, secret_key = ""))
    }

    stop("No credentials found. Set UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY environment variables or provide as arguments.")
}

# =============================================================================
# Internal API Functions
# =============================================================================

detect_language <- function(filename) {
    ext <- tolower(sub(".*(\\..*)$", "\\1", filename))
    lang <- ext_map[[ext]]
    if (is.null(lang)) {
        return("unknown")
    }
    return(lang)
}

get_api_keys <- function(args_key = NULL) {
    public_key <- Sys.getenv("UNSANDBOX_PUBLIC_KEY")
    secret_key <- Sys.getenv("UNSANDBOX_SECRET_KEY")

    # Fallback to old UNSANDBOX_API_KEY for backwards compat
    if (public_key == "" && Sys.getenv("UNSANDBOX_API_KEY") != "") {
        public_key <- Sys.getenv("UNSANDBOX_API_KEY")
        secret_key <- ""
    }

    if (public_key == "") {
        cat(sprintf("%sError: UNSANDBOX_PUBLIC_KEY or UNSANDBOX_API_KEY not set%s\n", RED, RESET), file = stderr())
        quit(status = 1)
    }
    return(list(public_key = public_key, secret_key = secret_key))
}

check_clock_drift <- function(response_text) {
    response_lower <- tolower(response_text)
    has_timestamp <- grepl("timestamp", response_lower, fixed = TRUE)
    has_401 <- grepl("401", response_lower, fixed = TRUE)
    has_expired <- grepl("expired", response_lower, fixed = TRUE)
    has_invalid <- grepl("invalid", response_lower, fixed = TRUE)
    has_error <- has_401 || has_expired || has_invalid

    if (has_timestamp && has_error) {
        cat(sprintf("%sError: Request timestamp expired (must be within 5 minutes of server time)%s\n", RED, RESET), file = stderr())
        cat(sprintf("%sYour computer's clock may have drifted.\n", YELLOW), file = stderr())
        cat("Check your system time and sync with NTP if needed:\n", file = stderr())
        cat("  Linux:   sudo ntpdate -s time.nist.gov\n", file = stderr())
        cat("  macOS:   sudo sntp -sS time.apple.com\n", file = stderr())
        cat(sprintf("  Windows: w32tm /resync%s\n", RESET), file = stderr())
        quit(status = 1)
    }
}

#' Compute HMAC-SHA256 Signature
#'
#' Computes the HMAC-SHA256 signature for API authentication.
#'
#' @param secret_key The secret key
#' @param message The message to sign (timestamp:METHOD:path:body)
#' @return Hexadecimal signature string
#' @keywords internal
compute_signature <- function(secret_key, message) {
    return(hmac(message, secret_key, algo = "sha256"))
}

#' Build Authentication Headers
#'
#' Constructs HTTP headers with HMAC authentication.
#'
#' @param method HTTP method (GET, POST, etc.)
#' @param endpoint API endpoint path
#' @param body Request body (empty string if none)
#' @param public_key Public API key
#' @param secret_key Secret API key
#' @return httr headers object
#' @keywords internal
build_auth_headers <- function(method, endpoint, body, public_key, secret_key) {
    if (secret_key != "") {
        timestamp <- as.integer(Sys.time())
        sig_input <- paste0(timestamp, ":", method, ":", endpoint, ":", body)
        signature <- compute_signature(secret_key, sig_input)
        return(add_headers(
            `Content-Type` = "application/json",
            `Authorization` = paste("Bearer", public_key),
            `X-Timestamp` = as.character(timestamp),
            `X-Signature` = signature
        ))
    } else {
        return(add_headers(
            `Content-Type` = "application/json",
            `Authorization` = paste("Bearer", public_key)
        ))
    }
}

api_request <- function(endpoint, public_key, secret_key, method = "GET", data = NULL) {
    url <- paste0(API_BASE, endpoint)

    body_content <- ""
    if (!is.null(data)) {
        body_content <- toJSON(data, auto_unbox = TRUE)
    }

    headers <- build_auth_headers(method, endpoint, body_content, public_key, secret_key)

    tryCatch({
        if (method == "GET") {
            response <- GET(url, headers, timeout(300))
        } else if (method == "POST") {
            response <- POST(url, headers, body = body_content, encode = "raw", timeout(300))
        } else if (method == "DELETE") {
            response <- DELETE(url, headers, timeout(300))
        } else if (method == "PATCH") {
            response <- PATCH(url, headers, body = body_content, encode = "raw", timeout(300))
        } else {
            stop(paste("Unsupported method:", method))
        }

        response_text <- content(response, "text", encoding = "UTF-8")
        check_clock_drift(response_text)
        result <- fromJSON(response_text)
        return(result)
    }, error = function(e) {
        cat(sprintf("%sError: Request failed: %s%s\n", RED, e$message, RESET), file = stderr())
        quit(status = 1)
    })
}

api_request_text <- function(endpoint, public_key, secret_key, body) {
    url <- paste0(API_BASE, endpoint)
    headers <- add_headers(
        `Content-Type` = "text/plain",
        `Authorization` = paste("Bearer", public_key)
    )

    # Add HMAC signature if secret_key is present
    if (secret_key != "") {
        timestamp <- as.integer(Sys.time())
        sig_input <- paste0(timestamp, ":PUT:", endpoint, ":", body)
        signature <- hmac(sig_input, secret_key, algo = "sha256")
        headers <- add_headers(
            `Content-Type` = "text/plain",
            `Authorization` = paste("Bearer", public_key),
            `X-Timestamp` = as.character(timestamp),
            `X-Signature` = signature
        )
    }

    tryCatch({
        response <- PUT(url, headers, body = body, encode = "raw", timeout(300))
        status_code <- status_code(response)
        return(status_code >= 200 && status_code < 300)
    }, error = function(e) {
        return(FALSE)
    })
}

# =============================================================================
# Library API Functions
# =============================================================================

#' Execute Code Synchronously
#'
#' Executes code in a specified language and waits for completion.
#'
#' @param code The source code to execute
#' @param language The programming language (e.g., "python", "javascript")
#' @param env Named list of environment variables (optional)
#' @param input_files List of input files with filename and content_base64 (optional)
#' @param network Network mode: "zerotrust" (default) or "semitrusted"
#' @param timeout Maximum execution time in seconds (optional)
#' @param public_key API public key (optional, uses credentials chain)
#' @param secret_key API secret key (optional, uses credentials chain)
#' @return A list containing stdout, stderr, exit_code, and optionally artifacts
#' @export
#' @examples
#' \dontrun{
#' result <- execute("print('Hello, World!')", "python")
#' cat(result$stdout)
#'
#' result <- execute("console.log(process.env.NAME)", "javascript",
#'                   env = list(NAME = "Alice"))
#' }
execute <- function(code, language, env = NULL, input_files = NULL,
                    network = "zerotrust", timeout = NULL,
                    public_key = NULL, secret_key = NULL) {
    creds <- get_credentials(public_key, secret_key)

    payload <- list(language = language, code = code)
    if (!is.null(env)) payload$env <- env
    if (!is.null(input_files)) payload$input_files <- input_files
    if (network != "zerotrust") payload$network <- network
    if (!is.null(timeout)) payload$timeout <- timeout

    result <- api_request("/execute", creds$public_key, creds$secret_key,
                          method = "POST", data = payload)
    return(result)
}

#' Execute Code Asynchronously
#'
#' Submits code for execution and returns immediately with a job ID.
#' Use \code{get_job} or \code{wait} to retrieve results.
#'
#' @param code The source code to execute
#' @param language The programming language
#' @param env Named list of environment variables (optional)
#' @param input_files List of input files (optional)
#' @param network Network mode: "zerotrust" or "semitrusted"
#' @param public_key API public key (optional)
#' @param secret_key API secret key (optional)
#' @return A list containing job_id for tracking the execution
#' @export
#' @examples
#' \dontrun{
#' job <- execute_async("import time; time.sleep(10); print('Done')", "python")
#' result <- wait(job$job_id)
#' }
execute_async <- function(code, language, env = NULL, input_files = NULL,
                          network = "zerotrust",
                          public_key = NULL, secret_key = NULL) {
    creds <- get_credentials(public_key, secret_key)

    payload <- list(language = language, code = code, async = TRUE)
    if (!is.null(env)) payload$env <- env
    if (!is.null(input_files)) payload$input_files <- input_files
    if (network != "zerotrust") payload$network <- network

    result <- api_request("/execute", creds$public_key, creds$secret_key,
                          method = "POST", data = payload)
    return(result)
}

#' Get Job Status
#'
#' Retrieves the current status and results of an asynchronous job.
#'
#' @param job_id The job ID returned by execute_async
#' @param public_key API public key (optional)
#' @param secret_key API secret key (optional)
#' @return A list containing status, and if completed: stdout, stderr, exit_code
#' @export
#' @examples
#' \dontrun{
#' job <- execute_async("print('Hello')", "python")
#' status <- get_job(job$job_id)
#' if (status$status == "completed") {
#'     cat(status$stdout)
#' }
#' }
get_job <- function(job_id, public_key = NULL, secret_key = NULL) {
    creds <- get_credentials(public_key, secret_key)
    result <- api_request(paste0("/jobs/", job_id), creds$public_key, creds$secret_key)
    return(result)
}

#' Wait for Job Completion
#'
#' Polls a job until it completes or times out.
#'
#' @param job_id The job ID to wait for
#' @param poll_interval Seconds between status checks (default: 1)
#' @param max_wait Maximum seconds to wait (default: 300)
#' @param public_key API public key (optional)
#' @param secret_key API secret key (optional)
#' @return The completed job result
#' @export
#' @examples
#' \dontrun{
#' job <- execute_async("import time; time.sleep(5); print('Done')", "python")
#' result <- wait(job$job_id, poll_interval = 2)
#' }
wait <- function(job_id, poll_interval = 1, max_wait = 300,
                 public_key = NULL, secret_key = NULL) {
    creds <- get_credentials(public_key, secret_key)
    start_time <- Sys.time()

    repeat {
        result <- get_job(job_id, creds$public_key, creds$secret_key)

        if (!is.null(result$status) && result$status %in% c("completed", "failed", "timeout")) {
            return(result)
        }

        elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
        if (elapsed >= max_wait) {
            stop(paste("Job", job_id, "did not complete within", max_wait, "seconds"))
        }

        Sys.sleep(poll_interval)
    }
}

#' Cancel a Job
#'
#' Cancels a running asynchronous job.
#'
#' @param job_id The job ID to cancel
#' @param public_key API public key (optional)
#' @param secret_key API secret key (optional)
#' @return A list with cancellation status
#' @export
#' @examples
#' \dontrun{
#' job <- execute_async("import time; time.sleep(60)", "python")
#' cancel_job(job$job_id)
#' }
cancel_job <- function(job_id, public_key = NULL, secret_key = NULL) {
    creds <- get_credentials(public_key, secret_key)
    result <- api_request(paste0("/jobs/", job_id), creds$public_key, creds$secret_key,
                          method = "DELETE")
    return(result)
}

#' List Jobs
#'
#' Lists recent jobs for the authenticated account.
#'
#' @param status Filter by status (optional): "pending", "running", "completed", "failed"
#' @param limit Maximum number of jobs to return (default: 50)
#' @param public_key API public key (optional)
#' @param secret_key API secret key (optional)
#' @return A list containing jobs array
#' @export
#' @examples
#' \dontrun{
#' jobs <- list_jobs()
#' running <- list_jobs(status = "running")
#' }
list_jobs <- function(status = NULL, limit = 50, public_key = NULL, secret_key = NULL) {
    creds <- get_credentials(public_key, secret_key)
    endpoint <- paste0("/jobs?limit=", limit)
    if (!is.null(status)) endpoint <- paste0(endpoint, "&status=", status)
    result <- api_request(endpoint, creds$public_key, creds$secret_key)
    return(result)
}

#' Run Code from File
#'
#' Convenience function to execute code from a file with auto-detected language.
#'
#' @param filepath Path to the source file
#' @param env Named list of environment variables (optional)
#' @param network Network mode (optional)
#' @param public_key API public key (optional)
#' @param secret_key API secret key (optional)
#' @return Execution result
#' @export
#' @examples
#' \dontrun{
#' result <- run("script.py")
#' result <- run("app.js", env = list(NODE_ENV = "production"))
#' }
run <- function(filepath, env = NULL, network = "zerotrust",
                public_key = NULL, secret_key = NULL) {
    if (!file.exists(filepath)) {
        stop(paste("File not found:", filepath))
    }

    language <- detect_language(filepath)
    if (language == "unknown") {
        stop(paste("Cannot detect language for:", filepath))
    }

    code <- paste(readLines(filepath, warn = FALSE), collapse = "\n")
    return(execute(code, language, env = env, network = network,
                   public_key = public_key, secret_key = secret_key))
}

#' Run Code from File Asynchronously
#'
#' Convenience function to execute code from a file asynchronously.
#'
#' @param filepath Path to the source file
#' @param env Named list of environment variables (optional)
#' @param network Network mode (optional)
#' @param public_key API public key (optional)
#' @param secret_key API secret key (optional)
#' @return A list containing job_id
#' @export
#' @examples
#' \dontrun{
#' job <- run_async("long_script.py")
#' result <- wait(job$job_id)
#' }
run_async <- function(filepath, env = NULL, network = "zerotrust",
                      public_key = NULL, secret_key = NULL) {
    if (!file.exists(filepath)) {
        stop(paste("File not found:", filepath))
    }

    language <- detect_language(filepath)
    if (language == "unknown") {
        stop(paste("Cannot detect language for:", filepath))
    }

    code <- paste(readLines(filepath, warn = FALSE), collapse = "\n")
    return(execute_async(code, language, env = env, network = network,
                         public_key = public_key, secret_key = secret_key))
}

#' Get Container Image Information
#'
#' Retrieves information about the execution environment image.
#'
#' @param public_key API public key (optional)
#' @param secret_key API secret key (optional)
#' @return A list containing image version and installed packages
#' @export
#' @examples
#' \dontrun{
#' info <- image()
#' cat("Image version:", info$version)
#' }
image <- function(public_key = NULL, secret_key = NULL) {
    creds <- get_credentials(public_key, secret_key)
    result <- api_request("/image", creds$public_key, creds$secret_key)
    return(result)
}

#' List Supported Languages
#'
#' Retrieves the list of supported programming languages.
#'
#' @param public_key API public key (optional)
#' @param secret_key API secret key (optional)
#' @return A list containing supported languages with their details
#' @export
#' @examples
#' \dontrun{
#' langs <- languages()
#' print(names(langs$languages))
#' }
languages <- function(public_key = NULL, secret_key = NULL) {
    creds <- get_credentials(public_key, secret_key)
    result <- api_request("/languages", creds$public_key, creds$secret_key)
    return(result)
}

# =============================================================================
# Client Class (R6)
# =============================================================================

#' Unsandbox Client Class
#'
#' An R6 class providing an object-oriented interface to the Unsandbox API.
#' Stores credentials for reuse across multiple API calls.
#'
#' @description
#' The Client class provides a convenient way to interact with the Unsandbox API
#' when making multiple calls. It stores credentials and provides methods for
#' all API operations.
#'
#' @export
#' @examples
#' \dontrun{
#' # Create client with environment credentials
#' client <- Client$new()
#'
#' # Create client with explicit credentials
#' client <- Client$new(public_key = "unsb-pk-xxxx", secret_key = "unsb-sk-xxxx")
#'
#' # Execute code
#' result <- client$execute("print('Hello')", "python")
#'
#' # Async execution
#' job <- client$execute_async("import time; time.sleep(10)", "python")
#' result <- client$wait(job$job_id)
#' }
Client <- NULL

# Only create if R6 is available
if (requireNamespace("R6", quietly = TRUE)) {
    Client <- R6::R6Class("Client",
        public = list(
            #' @field public_key The API public key
            public_key = NULL,
            #' @field secret_key The API secret key
            secret_key = NULL,

            #' @description
            #' Create a new Unsandbox client
            #' @param public_key Optional public key (uses credential chain if not provided)
            #' @param secret_key Optional secret key (uses credential chain if not provided)
            initialize = function(public_key = NULL, secret_key = NULL) {
                creds <- get_credentials(public_key, secret_key)
                self$public_key <- creds$public_key
                self$secret_key <- creds$secret_key
            },

            #' @description Execute code synchronously
            #' @param code Source code to execute
            #' @param language Programming language
            #' @param env Environment variables
            #' @param input_files Input files
            #' @param network Network mode
            #' @param timeout Execution timeout
            execute = function(code, language, env = NULL, input_files = NULL,
                               network = "zerotrust", timeout = NULL) {
                execute(code, language, env, input_files, network, timeout,
                        self$public_key, self$secret_key)
            },

            #' @description Execute code asynchronously
            #' @param code Source code to execute
            #' @param language Programming language
            #' @param env Environment variables
            #' @param input_files Input files
            #' @param network Network mode
            execute_async = function(code, language, env = NULL, input_files = NULL,
                                     network = "zerotrust") {
                execute_async(code, language, env, input_files, network,
                              self$public_key, self$secret_key)
            },

            #' @description Get job status
            #' @param job_id Job ID
            get_job = function(job_id) {
                get_job(job_id, self$public_key, self$secret_key)
            },

            #' @description Wait for job completion
            #' @param job_id Job ID
            #' @param poll_interval Poll interval in seconds
            #' @param max_wait Maximum wait time
            wait = function(job_id, poll_interval = 1, max_wait = 300) {
                wait(job_id, poll_interval, max_wait, self$public_key, self$secret_key)
            },

            #' @description Cancel a job
            #' @param job_id Job ID
            cancel_job = function(job_id) {
                cancel_job(job_id, self$public_key, self$secret_key)
            },

            #' @description List jobs
            #' @param status Filter by status
            #' @param limit Maximum number of jobs
            list_jobs = function(status = NULL, limit = 50) {
                list_jobs(status, limit, self$public_key, self$secret_key)
            },

            #' @description Run code from file
            #' @param filepath Path to source file
            #' @param env Environment variables
            #' @param network Network mode
            run = function(filepath, env = NULL, network = "zerotrust") {
                run(filepath, env, network, self$public_key, self$secret_key)
            },

            #' @description Run code from file asynchronously
            #' @param filepath Path to source file
            #' @param env Environment variables
            #' @param network Network mode
            run_async = function(filepath, env = NULL, network = "zerotrust") {
                run_async(filepath, env, network, self$public_key, self$secret_key)
            },

            #' @description Get image information
            image = function() {
                image(self$public_key, self$secret_key)
            },

            #' @description List supported languages
            languages = function() {
                languages(self$public_key, self$secret_key)
            }
        )
    )
}

# =============================================================================
# CLI Helper Functions
# =============================================================================

read_env_file <- function(path) {
    if (!file.exists(path)) {
        cat(sprintf("%sError: Env file not found: %s%s\n", RED, path, RESET), file = stderr())
        quit(status = 1)
    }
    return(paste(readLines(path, warn = FALSE), collapse = "\n"))
}

build_env_content <- function(envs, env_file) {
    lines <- c()
    if (!is.null(envs)) {
        lines <- c(lines, envs)
    }
    if (!is.null(env_file) && env_file != "") {
        content <- read_env_file(env_file)
        for (line in strsplit(content, "\n")[[1]]) {
            trimmed <- trimws(line)
            if (nchar(trimmed) > 0 && !startsWith(trimmed, "#")) {
                lines <- c(lines, trimmed)
            }
        }
    }
    return(paste(lines, collapse = "\n"))
}

cmd_service_env <- function(args) {
    keys <- get_api_keys(args$api_key)
    public_key <- keys$public_key
    secret_key <- keys$secret_key

    action <- args$env_action
    target <- args$env_target

    if (action == "status") {
        if (is.null(target) || target == "") {
            cat(sprintf("%sError: service env status requires service ID%s\n", RED, RESET), file = stderr())
            quit(status = 1)
        }
        result <- api_request(paste0("/services/", target, "/env"), public_key, secret_key)
        if (!is.null(result$has_vault) && result$has_vault) {
            cat(sprintf("%sVault: configured%s\n", GREEN, RESET))
            if (!is.null(result$env_count)) {
                cat(sprintf("Variables: %s\n", result$env_count))
            }
            if (!is.null(result$updated_at)) {
                cat(sprintf("Updated: %s\n", result$updated_at))
            }
        } else {
            cat(sprintf("%sVault: not configured%s\n", YELLOW, RESET))
        }
        return()
    }

    if (action == "set") {
        if (is.null(target) || target == "") {
            cat(sprintf("%sError: service env set requires service ID%s\n", RED, RESET), file = stderr())
            quit(status = 1)
        }
        if ((is.null(args$svc_envs) || length(args$svc_envs) == 0) && (is.null(args$svc_env_file) || args$svc_env_file == "")) {
            cat(sprintf("%sError: service env set requires -e or --env-file%s\n", RED, RESET), file = stderr())
            quit(status = 1)
        }
        env_content <- build_env_content(args$svc_envs, args$svc_env_file)
        if (nchar(env_content) > MAX_ENV_CONTENT_SIZE) {
            cat(sprintf("%sError: Env content exceeds maximum size of 64KB%s\n", RED, RESET), file = stderr())
            quit(status = 1)
        }
        if (api_request_text(paste0("/services/", target, "/env"), public_key, secret_key, env_content)) {
            cat(sprintf("%sVault updated for service %s%s\n", GREEN, target, RESET))
        } else {
            cat(sprintf("%sError: Failed to update vault%s\n", RED, RESET), file = stderr())
            quit(status = 1)
        }
        return()
    }

    if (action == "export") {
        if (is.null(target) || target == "") {
            cat(sprintf("%sError: service env export requires service ID%s\n", RED, RESET), file = stderr())
            quit(status = 1)
        }
        result <- api_request(paste0("/services/", target, "/env/export"), public_key, secret_key, method = "POST", data = list())
        if (!is.null(result$content)) {
            cat(result$content)
        }
        return()
    }

    if (action == "delete") {
        if (is.null(target) || target == "") {
            cat(sprintf("%sError: service env delete requires service ID%s\n", RED, RESET), file = stderr())
            quit(status = 1)
        }
        result <- api_request(paste0("/services/", target, "/env"), public_key, secret_key, method = "DELETE")
        cat(sprintf("%sVault deleted for service %s%s\n", GREEN, target, RESET))
        return()
    }

    cat(sprintf("%sError: Unknown env action: %s%s\n", RED, action, RESET), file = stderr())
    cat("Usage: un.r service env <status|set|export|delete> <service_id>\n", file = stderr())
    quit(status = 1)
}

cmd_execute <- function(args) {
    keys <- get_api_keys(args$api_key)
    public_key <- keys$public_key
    secret_key <- keys$secret_key

    filename <- args$source_file
    if (!file.exists(filename)) {
        cat(sprintf("%sError: File not found: %s%s\n", RED, filename, RESET), file = stderr())
        quit(status = 1)
    }

    language <- detect_language(filename)
    if (language == "unknown") {
        cat(sprintf("%sError: Cannot detect language for %s%s\n", RED, filename, RESET), file = stderr())
        quit(status = 1)
    }

    code <- paste(readLines(filename, warn = FALSE), collapse = "\n")

    # Build request payload
    payload <- list(language = language, code = code)

    # Add environment variables
    if (!is.null(args$env)) {
        env_vars <- list()
        for (e in args$env) {
            if (grepl("=", e)) {
                parts <- strsplit(e, "=", fixed = TRUE)[[1]]
                k <- parts[1]
                v <- paste(parts[-1], collapse = "=")
                env_vars[[k]] <- v
            }
        }
        if (length(env_vars) > 0) {
            payload$env <- env_vars
        }
    }

    # Add input files
    if (!is.null(args$files)) {
        input_files <- list()
        for (filepath in args$files) {
            if (!file.exists(filepath)) {
                cat(sprintf("%sError: Input file not found: %s%s\n", RED, filepath, RESET), file = stderr())
                quit(status = 1)
            }
            content <- base64enc::base64encode(filepath)
            input_files[[length(input_files) + 1]] <- list(
                filename = basename(filepath),
                content_base64 = content
            )
        }
        if (length(input_files) > 0) {
            payload$input_files <- input_files
        }
    }

    # Add options
    if (!is.null(args$artifacts) && args$artifacts) {
        payload$return_artifacts <- TRUE
    }
    if (!is.null(args$network)) {
        payload$network <- args$network
    }

    # Execute
    result <- api_request("/execute", public_key, secret_key, method = "POST", data = payload)

    # Print output
    if (!is.null(result$stdout) && result$stdout != "") {
        cat(BLUE, result$stdout, RESET, sep = "")
    }
    if (!is.null(result$stderr) && result$stderr != "") {
        cat(RED, result$stderr, RESET, sep = "")
    }

    # Save artifacts
    if (!is.null(args$artifacts) && args$artifacts && !is.null(result$artifacts)) {
        out_dir <- if (!is.null(args$output_dir)) args$output_dir else "."
        dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
        for (artifact in result$artifacts) {
            filename <- if (!is.null(artifact$filename)) artifact$filename else "artifact"
            content <- base64enc::base64decode(what = artifact$content_base64)
            path <- file.path(out_dir, filename)
            writeBin(content, path)
            Sys.chmod(path, mode = "0755")
            cat(sprintf("%sSaved: %s%s\n", GREEN, path, RESET), file = stderr())
        }
    }

    exit_code <- if (!is.null(result$exit_code)) result$exit_code else 0
    quit(status = exit_code)
}

cmd_session <- function(args) {
    keys <- get_api_keys(args$api_key)
    public_key <- keys$public_key
    secret_key <- keys$secret_key

    if (!is.null(args$list) && args$list) {
        result <- api_request("/sessions", public_key, secret_key)
        sessions <- if (!is.null(result$sessions)) result$sessions else list()
        if (length(sessions) == 0) {
            cat("No active sessions\n")
        } else {
            cat(sprintf("%-40s %-10s %-10s %s\n", "ID", "Shell", "Status", "Created"))
            for (s in sessions) {
                cat(sprintf("%-40s %-10s %-10s %s\n",
                    if (!is.null(s$id)) s$id else "N/A",
                    if (!is.null(s$shell)) s$shell else "N/A",
                    if (!is.null(s$status)) s$status else "N/A",
                    if (!is.null(s$created_at)) s$created_at else "N/A"))
            }
        }
        return()
    }

    if (!is.null(args$kill)) {
        result <- api_request(paste0("/sessions/", args$kill), public_key, secret_key, method = "DELETE")
        cat(sprintf("%sSession terminated: %s%s\n", GREEN, args$kill, RESET))
        return()
    }

    if (!is.null(args$snapshot_id)) {
        payload <- list()
        if (!is.null(args$snapshot_name)) {
            payload$name <- args$snapshot_name
        }
        if (!is.null(args$hot) && args$hot) {
            payload$hot <- TRUE
        }

        cat(sprintf("%sCreating snapshot of session %s...%s\n", YELLOW, args$snapshot_id, RESET), file = stderr())
        result <- api_request(paste0("/sessions/", args$snapshot_id, "/snapshot"), public_key, secret_key, method = "POST", data = payload)
        cat(sprintf("%sSnapshot created successfully%s\n", GREEN, RESET))
        cat(sprintf("Snapshot ID: %s\n", if (!is.null(result$id)) result$id else "N/A"))
        return()
    }

    if (!is.null(args$restore_id)) {
        # --restore takes snapshot ID directly, calls /snapshots/:id/restore
        cat(sprintf("%sRestoring from snapshot %s...%s\n", YELLOW, args$restore_id, RESET), file = stderr())
        result <- api_request(paste0("/snapshots/", args$restore_id, "/restore"), public_key, secret_key, method = "POST", data = list())
        cat(sprintf("%sSession restored from snapshot%s\n", GREEN, RESET))
        return()
    }

    # Create new session
    payload <- list(shell = "bash")

    if (!is.null(args$network)) {
        payload$network <- args$network
    }

    # Add input files
    if (!is.null(args$files)) {
        input_files <- list()
        for (filepath in args$files) {
            if (!file.exists(filepath)) {
                cat(sprintf("%sError: Input file not found: %s%s\n", RED, filepath, RESET), file = stderr())
                quit(status = 1)
            }
            content <- base64enc::base64encode(filepath)
            input_files[[length(input_files) + 1]] <- list(
                filename = basename(filepath),
                content_base64 = content
            )
        }
        if (length(input_files) > 0) {
            payload$input_files <- input_files
        }
    }

    cat(sprintf("%sCreating session...%s\n", YELLOW, RESET))
    result <- api_request("/sessions", public_key, secret_key, method = "POST", data = payload)
    cat(sprintf("%sSession created: %s%s\n", GREEN, if (!is.null(result$id)) result$id else "N/A", RESET))
    cat(sprintf("%s(Interactive sessions require WebSocket - use un2 for full support)%s\n", YELLOW, RESET))
}

#' Get Languages Cache Path
#'
#' Returns the path to the languages cache file.
#'
#' @return Path to ~/.unsandbox/languages.json
#' @keywords internal
get_languages_cache_path <- function() {
    home <- Sys.getenv("HOME")
    if (home == "") {
        home <- "."
    }
    return(file.path(home, ".unsandbox", "languages.json"))
}

#' Load Languages Cache
#'
#' Loads languages from the cache file if it exists and is not expired.
#'
#' @return Vector of language names, or NULL if cache is missing/expired
#' @keywords internal
load_languages_cache <- function() {
    cache_path <- get_languages_cache_path()

    if (!file.exists(cache_path)) {
        return(NULL)
    }

    tryCatch({
        content <- paste(readLines(cache_path, warn = FALSE), collapse = "\n")
        data <- fromJSON(content)
        timestamp <- data$timestamp
        now <- as.integer(Sys.time())

        if (!is.null(timestamp) && (now - timestamp) < LANGUAGES_CACHE_TTL) {
            return(data$languages)
        } else {
            return(NULL)
        }
    }, error = function(e) {
        return(NULL)
    })
}

#' Save Languages Cache
#'
#' Saves languages to the cache file.
#'
#' @param languages Vector of language names
#' @keywords internal
save_languages_cache <- function(languages) {
    cache_path <- get_languages_cache_path()
    cache_dir <- dirname(cache_path)

    # Ensure directory exists
    if (!dir.exists(cache_dir)) {
        dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    }

    timestamp <- as.integer(Sys.time())
    data <- list(languages = languages, timestamp = timestamp)

    tryCatch({
        writeLines(toJSON(data, auto_unbox = TRUE), cache_path)
    }, error = function(e) {
        # Ignore write errors
    })
}

cmd_languages <- function(args) {
    # Try to load from cache first
    langs <- load_languages_cache()

    if (is.null(langs)) {
        # Cache miss or expired, fetch from API
        keys <- get_api_keys(args$api_key)
        public_key <- keys$public_key
        secret_key <- keys$secret_key

        result <- api_request("/languages", public_key, secret_key)
        langs <- result$languages
        save_languages_cache(langs)
    }

    if (!is.null(args$json_output) && args$json_output) {
        # JSON output - print as JSON array
        cat(toJSON(langs, auto_unbox = TRUE), "\n")
    } else {
        # Default output - one language per line
        for (lang in langs) {
            cat(lang, "\n")
        }
    }
}

cmd_key <- function(args) {
    keys <- get_api_keys(args$api_key)
    public_key <- keys$public_key
    secret_key <- keys$secret_key

    if (!is.null(args$extend) && args$extend) {
        # First validate to get public_key
        url <- paste0(PORTAL_BASE, "/keys/validate")
        headers <- add_headers(
            `Content-Type` = "application/json",
            `Authorization` = paste("Bearer", public_key)
        )

        # Add HMAC signature if secret_key is present
        if (secret_key != "") {
            timestamp <- as.integer(Sys.time())
            sig_input <- paste0(timestamp, ":POST:/keys/validate:")
            signature <- hmac(sig_input, secret_key, algo = "sha256")
            headers <- add_headers(
                `Content-Type` = "application/json",
                `Authorization` = paste("Bearer", public_key),
                `X-Timestamp` = as.character(timestamp),
                `X-Signature` = signature
            )
        }

        tryCatch({
            response <- POST(url, headers, encode = "json", timeout(10))
            response_text <- content(response, "text", encoding = "UTF-8")
            check_clock_drift(response_text)
            result <- fromJSON(response_text)

            if (!is.null(result$public_key)) {
                extend_url <- paste0(PORTAL_BASE, "/keys/extend?pk=", result$public_key)
                cat(sprintf("Opening: %s\n", extend_url))
                system(sprintf("xdg-open '%s' 2>/dev/null || open '%s' 2>/dev/null || start '%s'", extend_url, extend_url, extend_url))
            } else {
                cat(sprintf("%sError: Could not retrieve public key%s\n", RED, RESET), file = stderr())
                quit(status = 1)
            }
        }, error = function(e) {
            cat(sprintf("%sError: Request failed: %s%s\n", RED, e$message, RESET), file = stderr())
            quit(status = 1)
        })
        return()
    }

    # Validate key
    url <- paste0(PORTAL_BASE, "/keys/validate")
    headers <- add_headers(
        `Content-Type` = "application/json",
        `Authorization` = paste("Bearer", public_key)
    )

    # Add HMAC signature if secret_key is present
    if (secret_key != "") {
        timestamp <- as.integer(Sys.time())
        sig_input <- paste0(timestamp, ":POST:/keys/validate:")
        signature <- hmac(sig_input, secret_key, algo = "sha256")
        headers <- add_headers(
            `Content-Type` = "application/json",
            `Authorization` = paste("Bearer", public_key),
            `X-Timestamp` = as.character(timestamp),
            `X-Signature` = signature
        )
    }

    tryCatch({
        response <- POST(url, headers, encode = "json", timeout(10))
        response_text <- content(response, "text", encoding = "UTF-8")
        check_clock_drift(response_text)
        result <- fromJSON(response_text)

        status <- if (!is.null(result$status)) result$status else "Unknown"

        if (status == "valid") {
            cat(sprintf("%sValid%s\n", GREEN, RESET))
            cat(sprintf("Public Key: %s\n", if (!is.null(result$public_key)) result$public_key else "N/A"))
            cat(sprintf("Tier: %s\n", if (!is.null(result$tier)) result$tier else "N/A"))
            if (!is.null(result$expires_at)) {
                cat(sprintf("Expires: %s\n", result$expires_at))
            }
        } else if (status == "expired") {
            cat(sprintf("%sExpired%s\n", RED, RESET))
            cat(sprintf("Public Key: %s\n", if (!is.null(result$public_key)) result$public_key else "N/A"))
            cat(sprintf("Tier: %s\n", if (!is.null(result$tier)) result$tier else "N/A"))
            if (!is.null(result$expires_at)) {
                cat(sprintf("Expired: %s\n", result$expires_at))
            }
            cat(sprintf("%sTo renew: Visit https://unsandbox.com/keys/extend%s\n", YELLOW, RESET))
        } else {
            cat(sprintf("%sInvalid%s\n", RED, RESET))
        }
    }, error = function(e) {
        cat(sprintf("%sError: Request failed: %s%s\n", RED, e$message, RESET), file = stderr())
        quit(status = 1)
    })
}

cmd_snapshot <- function(args) {
    keys <- get_api_keys(args$api_key)
    public_key <- keys$public_key
    secret_key <- keys$secret_key

    if (!is.null(args$list) && args$list) {
        result <- api_request("/snapshots", public_key, secret_key)
        snapshots <- if (!is.null(result$snapshots)) result$snapshots else list()
        if (length(snapshots) == 0) {
            cat("No snapshots found\n")
        } else {
            cat(sprintf("%-40s %-20s %-12s %-30s %s\n", "ID", "Name", "Type", "Source ID", "Size"))
            for (s in snapshots) {
                cat(sprintf("%-40s %-20s %-12s %-30s %s\n",
                    if (!is.null(s$id)) s$id else "N/A",
                    if (!is.null(s$name)) s$name else "-",
                    if (!is.null(s$source_type)) s$source_type else "N/A",
                    if (!is.null(s$source_id)) s$source_id else "N/A",
                    if (!is.null(s$size)) s$size else "N/A"))
            }
        }
        return()
    }

    if (!is.null(args$info)) {
        result <- api_request(paste0("/snapshots/", args$info), public_key, secret_key)
        cat(sprintf("%sSnapshot Details%s\n\n", BLUE, RESET))
        cat(sprintf("Snapshot ID: %s\n", if (!is.null(result$id)) result$id else "N/A"))
        cat(sprintf("Name: %s\n", if (!is.null(result$name)) result$name else "-"))
        cat(sprintf("Source Type: %s\n", if (!is.null(result$source_type)) result$source_type else "N/A"))
        cat(sprintf("Source ID: %s\n", if (!is.null(result$source_id)) result$source_id else "N/A"))
        cat(sprintf("Size: %s\n", if (!is.null(result$size)) result$size else "N/A"))
        cat(sprintf("Created: %s\n", if (!is.null(result$created_at)) result$created_at else "N/A"))
        return()
    }

    if (!is.null(args$delete)) {
        result <- api_request(paste0("/snapshots/", args$delete), public_key, secret_key, method = "DELETE")
        cat(sprintf("%sSnapshot deleted successfully%s\n", GREEN, RESET))
        return()
    }

    if (!is.null(args$clone)) {
        if (is.null(args$type)) {
            cat(sprintf("%sError: --type required for --clone (session or service)%s\n", RED, RESET), file = stderr())
            quit(status = 1)
        }
        if (!(args$type %in% c("session", "service"))) {
            cat(sprintf("%sError: --type must be 'session' or 'service'%s\n", RED, RESET), file = stderr())
            quit(status = 1)
        }

        payload <- list(type = args$type)
        if (!is.null(args$clone_name)) {
            payload$name <- args$clone_name
        }
        if (!is.null(args$shell)) {
            payload$shell <- args$shell
        }
        if (!is.null(args$ports)) {
            ports_vec <- as.integer(strsplit(args$ports, ",")[[1]])
            payload$ports <- ports_vec
        }

        result <- api_request(paste0("/snapshots/", args$clone, "/clone"), public_key, secret_key, method = "POST", data = payload)

        if (args$type == "session") {
            cat(sprintf("%sSession created from snapshot%s\n", GREEN, RESET))
            cat(sprintf("Session ID: %s\n", if (!is.null(result$id)) result$id else "N/A"))
        } else {
            cat(sprintf("%sService created from snapshot%s\n", GREEN, RESET))
            cat(sprintf("Service ID: %s\n", if (!is.null(result$id)) result$id else "N/A"))
        }
        return()
    }

    cat(sprintf("%sError: Specify --list, --info ID, --delete ID, or --clone ID --type TYPE%s\n", RED, RESET), file = stderr())
    quit(status = 1)
}

cmd_image <- function(args) {
    keys <- get_api_keys(args$api_key)
    public_key <- keys$public_key
    secret_key <- keys$secret_key

    if (!is.null(args$list) && args$list) {
        result <- api_request("/images", public_key, secret_key)
        images <- if (!is.null(result$images)) result$images else list()
        if (length(images) == 0) {
            cat("No images found\n")
        } else {
            cat(sprintf("%-40s %-20s %-12s %s\n", "ID", "Name", "Visibility", "Created"))
            for (img in images) {
                cat(sprintf("%-40s %-20s %-12s %s\n",
                    if (!is.null(img$id)) img$id else "N/A",
                    if (!is.null(img$name)) img$name else "-",
                    if (!is.null(img$visibility)) img$visibility else "N/A",
                    if (!is.null(img$created_at)) img$created_at else "N/A"))
            }
        }
        return()
    }

    if (!is.null(args$image_info)) {
        result <- api_request(paste0("/images/", args$image_info), public_key, secret_key)
        cat(sprintf("%sImage Details%s\n\n", BLUE, RESET))
        cat(sprintf("Image ID: %s\n", if (!is.null(result$id)) result$id else "N/A"))
        cat(sprintf("Name: %s\n", if (!is.null(result$name)) result$name else "-"))
        cat(sprintf("Visibility: %s\n", if (!is.null(result$visibility)) result$visibility else "N/A"))
        cat(sprintf("Created: %s\n", if (!is.null(result$created_at)) result$created_at else "N/A"))
        return()
    }

    if (!is.null(args$image_delete)) {
        result <- api_request(paste0("/images/", args$image_delete), public_key, secret_key, method = "DELETE")
        cat(sprintf("%sImage deleted successfully%s\n", GREEN, RESET))
        return()
    }

    if (!is.null(args$image_lock)) {
        result <- api_request(paste0("/images/", args$image_lock, "/lock"), public_key, secret_key, method = "POST", data = list())
        cat(sprintf("%sImage locked successfully%s\n", GREEN, RESET))
        return()
    }

    if (!is.null(args$image_unlock)) {
        result <- api_request(paste0("/images/", args$image_unlock, "/unlock"), public_key, secret_key, method = "POST", data = list())
        cat(sprintf("%sImage unlocked successfully%s\n", GREEN, RESET))
        return()
    }

    if (!is.null(args$image_publish)) {
        if (is.null(args$source_type)) {
            cat(sprintf("%sError: --source-type required for --publish (service or snapshot)%s\n", RED, RESET), file = stderr())
            quit(status = 1)
        }
        payload <- list(source_type = args$source_type, source_id = args$image_publish)
        if (!is.null(args$name)) {
            payload$name <- args$name
        }
        result <- api_request("/images/publish", public_key, secret_key, method = "POST", data = payload)
        cat(sprintf("%sImage published successfully%s\n", GREEN, RESET))
        cat(sprintf("Image ID: %s\n", if (!is.null(result$id)) result$id else "N/A"))
        return()
    }

    if (!is.null(args$image_visibility)) {
        if (is.null(args$visibility_mode)) {
            cat(sprintf("%sError: visibility mode required (private, unlisted, or public)%s\n", RED, RESET), file = stderr())
            quit(status = 1)
        }
        payload <- list(visibility = args$visibility_mode)
        result <- api_request(paste0("/images/", args$image_visibility, "/visibility"), public_key, secret_key, method = "POST", data = payload)
        cat(sprintf("%sImage visibility set to %s%s\n", GREEN, args$visibility_mode, RESET))
        return()
    }

    if (!is.null(args$image_spawn)) {
        payload <- list()
        if (!is.null(args$name)) {
            payload$name <- args$name
        }
        if (!is.null(args$ports)) {
            ports_vec <- as.integer(strsplit(args$ports, ",")[[1]])
            payload$ports <- ports_vec
        }
        result <- api_request(paste0("/images/", args$image_spawn, "/spawn"), public_key, secret_key, method = "POST", data = payload)
        cat(sprintf("%sService spawned from image%s\n", GREEN, RESET))
        cat(sprintf("Service ID: %s\n", if (!is.null(result$id)) result$id else "N/A"))
        return()
    }

    if (!is.null(args$image_clone)) {
        payload <- list()
        if (!is.null(args$name)) {
            payload$name <- args$name
        }
        result <- api_request(paste0("/images/", args$image_clone, "/clone"), public_key, secret_key, method = "POST", data = payload)
        cat(sprintf("%sImage cloned successfully%s\n", GREEN, RESET))
        cat(sprintf("Image ID: %s\n", if (!is.null(result$id)) result$id else "N/A"))
        return()
    }

    cat(sprintf("%sError: Specify --list, --info ID, --delete ID, --lock ID, --unlock ID, --publish ID, --visibility ID MODE, --spawn ID, or --clone ID%s\n", RED, RESET), file = stderr())
    quit(status = 1)
}

cmd_service <- function(args) {
    # Handle env subcommand
    if (!is.null(args$env_action) && args$env_action != "") {
        cmd_service_env(args)
        return()
    }

    keys <- get_api_keys(args$api_key)
    public_key <- keys$public_key
    secret_key <- keys$secret_key

    if (!is.null(args$list) && args$list) {
        result <- api_request("/services", public_key, secret_key)
        services <- if (!is.null(result$services)) result$services else list()
        if (length(services) == 0) {
            cat("No services\n")
        } else {
            cat(sprintf("%-20s %-15s %-10s %-15s %s\n", "ID", "Name", "Status", "Ports", "Domains"))
            for (s in services) {
                ports <- if (!is.null(s$ports)) paste(s$ports, collapse = ",") else ""
                domains <- if (!is.null(s$domains)) paste(s$domains, collapse = ",") else ""
                cat(sprintf("%-20s %-15s %-10s %-15s %s\n",
                    if (!is.null(s$id)) s$id else "N/A",
                    if (!is.null(s$name)) s$name else "N/A",
                    if (!is.null(s$status)) s$status else "N/A",
                    ports, domains))
            }
        }
        return()
    }

    if (!is.null(args$info)) {
        result <- api_request(paste0("/services/", args$info), public_key, secret_key)
        cat(toJSON(result, pretty = TRUE, auto_unbox = TRUE), "\n")
        return()
    }

    if (!is.null(args$logs)) {
        result <- api_request(paste0("/services/", args$logs, "/logs"), public_key, secret_key)
        cat(if (!is.null(result$logs)) result$logs else "", "\n")
        return()
    }

    if (!is.null(args$sleep)) {
        result <- api_request(paste0("/services/", args$sleep, "/freeze"), public_key, secret_key, method = "POST")
        cat(sprintf("%sService frozen: %s%s\n", GREEN, args$sleep, RESET))
        return()
    }

    if (!is.null(args$wake)) {
        result <- api_request(paste0("/services/", args$wake, "/unfreeze"), public_key, secret_key, method = "POST")
        cat(sprintf("%sService unfreezing: %s%s\n", GREEN, args$wake, RESET))
        return()
    }

    if (!is.null(args$destroy)) {
        result <- api_request(paste0("/services/", args$destroy), public_key, secret_key, method = "DELETE")
        cat(sprintf("%sService destroyed: %s%s\n", GREEN, args$destroy, RESET))
        return()
    }

    if (!is.null(args$resize)) {
        if (is.null(args$vcpu) || args$vcpu < 1 || args$vcpu > 8) {
            cat(sprintf("%sError: --resize requires --vcpu N (1-8)%s\n", RED, RESET), file = stderr())
            quit(status = 1)
        }
        payload <- list(vcpu = args$vcpu)
        result <- api_request(paste0("/services/", args$resize), public_key, secret_key, method = "PATCH", data = payload)
        ram <- args$vcpu * 2
        cat(sprintf("%sService resized to %d vCPU, %d GB RAM%s\n", GREEN, args$vcpu, ram, RESET))
        return()
    }

    if (!is.null(args$set_unfreeze_on_demand)) {
        if (is.null(args$set_unfreeze_on_demand_val)) {
            cat(sprintf("%sError: --set-unfreeze-on-demand requires true/false or 1/0%s\n", RED, RESET), file = stderr())
            quit(status = 1)
        }
        enabled <- args$set_unfreeze_on_demand_val %in% c("true", "1")
        payload <- list(unfreeze_on_demand = enabled)
        result <- api_request(paste0("/services/", args$set_unfreeze_on_demand), public_key, secret_key, method = "PATCH", data = payload)
        status_msg <- if (enabled) "enabled" else "disabled"
        cat(sprintf("%sUnfreeze-on-demand %s for service: %s%s\n", GREEN, status_msg, args$set_unfreeze_on_demand, RESET))
        return()
    }

    if (!is.null(args$snapshot_svc)) {
        payload <- list()
        if (!is.null(args$snapshot_name)) {
            payload$name <- args$snapshot_name
        }
        if (!is.null(args$hot) && args$hot) {
            payload$hot <- TRUE
        }

        cat(sprintf("%sCreating snapshot of service %s...%s\n", YELLOW, args$snapshot_svc, RESET), file = stderr())
        result <- api_request(paste0("/services/", args$snapshot_svc, "/snapshot"), public_key, secret_key, method = "POST", data = payload)
        cat(sprintf("%sSnapshot created successfully%s\n", GREEN, RESET))
        cat(sprintf("Snapshot ID: %s\n", if (!is.null(result$id)) result$id else "N/A"))
        return()
    }

    if (!is.null(args$restore_svc)) {
        # --restore takes snapshot ID directly, calls /snapshots/:id/restore
        cat(sprintf("%sRestoring from snapshot %s...%s\n", YELLOW, args$restore_svc, RESET), file = stderr())
        result <- api_request(paste0("/snapshots/", args$restore_svc, "/restore"), public_key, secret_key, method = "POST", data = list())
        cat(sprintf("%sService restored from snapshot%s\n", GREEN, RESET))
        return()
    }

    if (!is.null(args$dump_bootstrap)) {
        cat(sprintf("Fetching bootstrap script from %s...\n", args$dump_bootstrap), file = stderr())
        payload <- list(command = "cat /tmp/bootstrap.sh")
        result <- api_request(paste0("/services/", args$dump_bootstrap, "/execute"), public_key, secret_key, method = "POST", data = payload)

        if (!is.null(result$stdout) && result$stdout != "") {
            bootstrap <- result$stdout
            if (!is.null(args$dump_file)) {
                # Write to file
                tryCatch({
                    writeLines(bootstrap, args$dump_file)
                    Sys.chmod(args$dump_file, mode = "0755")
                    cat(sprintf("Bootstrap saved to %s\n", args$dump_file))
                }, error = function(e) {
                    cat(sprintf("%sError: Could not write to %s: %s%s\n", RED, args$dump_file, e$message, RESET), file = stderr())
                    quit(status = 1)
                })
            } else {
                # Print to stdout
                cat(bootstrap)
            }
        } else {
            cat(sprintf("%sError: Failed to fetch bootstrap (service not running or no bootstrap file)%s\n", RED, RESET), file = stderr())
            quit(status = 1)
        }
        return()
    }

    if (!is.null(args$name)) {
        payload <- list(name = args$name)

        if (!is.null(args$ports)) {
            ports_vec <- as.integer(strsplit(args$ports, ",")[[1]])
            payload$ports <- ports_vec
        }

        if (!is.null(args$domains)) {
            domains_vec <- strsplit(args$domains, ",")[[1]]
            payload$domains <- domains_vec
        }

        if (!is.null(args$type)) {
            payload$service_type <- args$type
        }

        if (!is.null(args$bootstrap)) {
            payload$bootstrap <- args$bootstrap
        }

        if (!is.null(args$bootstrap_file)) {
            if (file.exists(args$bootstrap_file)) {
                payload$bootstrap_content <- paste(readLines(args$bootstrap_file, warn = FALSE), collapse = "\n")
            } else {
                cat(sprintf("%sError: Bootstrap file not found: %s%s\n", RED, args$bootstrap_file, RESET), file = stderr())
                quit(status = 1)
            }
        }

        if (!is.null(args$network)) {
            payload$network <- args$network
        }

        if (!is.null(args$vcpu)) {
            payload$vcpu <- args$vcpu
        }

        if (!is.null(args$unfreeze_on_demand) && args$unfreeze_on_demand) {
            payload$unfreeze_on_demand <- TRUE
        }

        # Add input files
        if (!is.null(args$files)) {
            input_files <- list()
            for (filepath in args$files) {
                if (!file.exists(filepath)) {
                    cat(sprintf("%sError: Input file not found: %s%s\n", RED, filepath, RESET), file = stderr())
                    quit(status = 1)
                }
                content <- base64enc::base64encode(filepath)
                input_files[[length(input_files) + 1]] <- list(
                    filename = basename(filepath),
                    content_base64 = content
                )
            }
            if (length(input_files) > 0) {
                payload$input_files <- input_files
            }
        }

        result <- api_request("/services", public_key, secret_key, method = "POST", data = payload)
        cat(sprintf("%sService created: %s%s\n", GREEN, if (!is.null(result$id)) result$id else "N/A", RESET))
        cat(sprintf("Name: %s\n", if (!is.null(result$name)) result$name else "N/A"))
        if (!is.null(result$url)) {
            cat(sprintf("URL: %s\n", result$url))
        }

        # Auto-set vault if -e or --env-file provided
        if ((!is.null(args$svc_envs) && length(args$svc_envs) > 0) || (!is.null(args$svc_env_file) && args$svc_env_file != "")) {
            service_id <- result$id
            if (!is.null(service_id)) {
                env_content <- build_env_content(args$svc_envs, args$svc_env_file)
                if (api_request_text(paste0("/services/", service_id, "/env"), public_key, secret_key, env_content)) {
                    cat(sprintf("%sVault configured for service %s%s\n", GREEN, service_id, RESET))
                } else {
                    cat(sprintf("%sWarning: Failed to set vault%s\n", YELLOW, RESET), file = stderr())
                }
            }
        }
        return()
    }

    cat(sprintf("%sError: Use --name to create, or --list, --info, --logs, --freeze, --unfreeze, --destroy%s\n", RED, RESET), file = stderr())
    quit(status = 1)
}

parse_args <- function() {
    args <- commandArgs(trailingOnly = TRUE)

    result <- list(
        source_file = NULL,
        api_key = NULL,
        network = NULL,
        env = NULL,
        files = NULL,
        artifacts = FALSE,
        output_dir = NULL,
        command = NULL,
        list = FALSE,
        kill = NULL,
        snapshot_id = NULL,
        snapshot_svc = NULL,
        restore_id = NULL,
        restore_svc = NULL,
        from_snapshot = NULL,
        snapshot_name = NULL,
        hot = FALSE,
        info = NULL,
        logs = NULL,
        sleep = NULL,
        wake = NULL,
        destroy = NULL,
        resize = NULL,
        delete = NULL,
        clone = NULL,
        clone_name = NULL,
        shell = NULL,
        dump_bootstrap = NULL,
        dump_file = NULL,
        set_unfreeze_on_demand = NULL,
        set_unfreeze_on_demand_val = NULL,
        unfreeze_on_demand = FALSE,
        name = NULL,
        ports = NULL,
        domains = NULL,
        type = NULL,
        bootstrap = NULL,
        bootstrap_file = NULL,
        vcpu = NULL,
        extend = FALSE,
        svc_envs = NULL,
        svc_env_file = NULL,
        env_action = NULL,
        env_target = NULL,
        json_output = FALSE,
        image_info = NULL,
        image_delete = NULL,
        image_lock = NULL,
        image_unlock = NULL,
        image_publish = NULL,
        source_type = NULL,
        image_visibility = NULL,
        visibility_mode = NULL,
        image_spawn = NULL,
        image_clone = NULL
    )

    i <- 1
    while (i <= length(args)) {
        arg <- args[i]

        if (arg == "session") {
            result$command <- "session"
            i <- i + 1
        } else if (arg == "service") {
            result$command <- "service"
            i <- i + 1
            # Check for env subcommand
            if (i <= length(args) && args[i] == "env") {
                i <- i + 1
                if (i <= length(args)) {
                    result$env_action <- args[i]
                    i <- i + 1
                }
                if (i <= length(args) && !startsWith(args[i], "-")) {
                    result$env_target <- args[i]
                    i <- i + 1
                }
            }
        } else if (arg == "key") {
            result$command <- "key"
            i <- i + 1
        } else if (arg == "languages") {
            result$command <- "languages"
            i <- i + 1
        } else if (arg == "--json") {
            result$json_output <- TRUE
            i <- i + 1
        } else if (arg == "snapshot") {
            result$command <- "snapshot"
            i <- i + 1
        } else if (arg == "image") {
            result$command <- "image"
            i <- i + 1
        } else if (arg %in% c("-k", "--api-key")) {
            i <- i + 1
            result$api_key <- args[i]
            i <- i + 1
        } else if (arg %in% c("-n", "--network")) {
            i <- i + 1
            result$network <- args[i]
            i <- i + 1
        } else if (arg %in% c("-e", "--env")) {
            i <- i + 1
            if (!is.null(result$command) && result$command == "service") {
                result$svc_envs <- c(result$svc_envs, args[i])
            } else {
                result$env <- c(result$env, args[i])
            }
            i <- i + 1
        } else if (arg == "--env-file") {
            i <- i + 1
            result$svc_env_file <- args[i]
            i <- i + 1
        } else if (arg %in% c("-f", "--files")) {
            i <- i + 1
            result$files <- c(result$files, args[i])
            i <- i + 1
        } else if (arg %in% c("-a", "--artifacts")) {
            result$artifacts <- TRUE
            i <- i + 1
        } else if (arg %in% c("-o", "--output-dir")) {
            i <- i + 1
            result$output_dir <- args[i]
            i <- i + 1
        } else if (arg %in% c("-l", "--list")) {
            result$list <- TRUE
            i <- i + 1
        } else if (arg == "--kill") {
            i <- i + 1
            result$kill <- args[i]
            i <- i + 1
        } else if (arg == "--info") {
            i <- i + 1
            if (!is.null(result$command) && result$command == "image") {
                result$image_info <- args[i]
            } else {
                result$info <- args[i]
            }
            i <- i + 1
        } else if (arg == "--logs") {
            i <- i + 1
            result$logs <- args[i]
            i <- i + 1
        } else if (arg == "--freeze") {
            i <- i + 1
            result$sleep <- args[i]
            i <- i + 1
        } else if (arg == "--unfreeze") {
            i <- i + 1
            result$wake <- args[i]
            i <- i + 1
        } else if (arg == "--destroy") {
            i <- i + 1
            result$destroy <- args[i]
            i <- i + 1
        } else if (arg == "--resize") {
            i <- i + 1
            result$resize <- args[i]
            i <- i + 1
        } else if (arg == "--dump-bootstrap") {
            i <- i + 1
            result$dump_bootstrap <- args[i]
            i <- i + 1
        } else if (arg == "--dump-file") {
            i <- i + 1
            result$dump_file <- args[i]
            i <- i + 1
        } else if (arg == "--set-unfreeze-on-demand") {
            i <- i + 1
            result$set_unfreeze_on_demand <- args[i]
            i <- i + 1
            if (i <= length(args)) {
                result$set_unfreeze_on_demand_val <- args[i]
                i <- i + 1
            }
        } else if (arg == "--unfreeze-on-demand") {
            result$unfreeze_on_demand <- TRUE
            i <- i + 1
        } else if (arg == "--name") {
            i <- i + 1
            result$name <- args[i]
            i <- i + 1
        } else if (arg == "--ports") {
            i <- i + 1
            result$ports <- args[i]
            i <- i + 1
        } else if (arg == "--domains") {
            i <- i + 1
            result$domains <- args[i]
            i <- i + 1
        } else if (arg == "--type") {
            i <- i + 1
            result$type <- args[i]
            i <- i + 1
        } else if (arg == "--bootstrap") {
            i <- i + 1
            result$bootstrap <- args[i]
            i <- i + 1
        } else if (arg == "--bootstrap-file") {
            i <- i + 1
            result$bootstrap_file <- args[i]
            i <- i + 1
        } else if (arg %in% c("-v", "--vcpu")) {
            i <- i + 1
            result$vcpu <- as.integer(args[i])
            i <- i + 1
        } else if (arg == "--snapshot") {
            i <- i + 1
            if (result$command == "session") {
                result$snapshot_id <- args[i]
            } else if (result$command == "service") {
                result$snapshot_svc <- args[i]
            }
            i <- i + 1
        } else if (arg == "--restore") {
            i <- i + 1
            if (result$command == "session") {
                result$restore_id <- args[i]
            } else if (result$command == "service") {
                result$restore_svc <- args[i]
            }
            i <- i + 1
        } else if (arg == "--from") {
            i <- i + 1
            result$from_snapshot <- args[i]
            i <- i + 1
        } else if (arg == "--snapshot-name") {
            i <- i + 1
            result$snapshot_name <- args[i]
            i <- i + 1
        } else if (arg == "--hot") {
            result$hot <- TRUE
            i <- i + 1
        } else if (arg == "--delete") {
            i <- i + 1
            if (!is.null(result$command) && result$command == "image") {
                result$image_delete <- args[i]
            } else {
                result$delete <- args[i]
            }
            i <- i + 1
        } else if (arg == "--lock") {
            i <- i + 1
            result$image_lock <- args[i]
            i <- i + 1
        } else if (arg == "--unlock") {
            i <- i + 1
            result$image_unlock <- args[i]
            i <- i + 1
        } else if (arg == "--clone") {
            i <- i + 1
            if (!is.null(result$command) && result$command == "image") {
                result$image_clone <- args[i]
            } else {
                result$clone <- args[i]
            }
            i <- i + 1
        } else if (arg == "--shell") {
            i <- i + 1
            result$shell <- args[i]
            i <- i + 1
        } else if (arg == "--extend") {
            result$extend <- TRUE
            i <- i + 1
        } else if (arg == "--source-type") {
            i <- i + 1
            result$source_type <- args[i]
            i <- i + 1
        } else if (arg == "--visibility") {
            i <- i + 1
            result$image_visibility <- args[i]
            i <- i + 1
            if (i <= length(args) && !startsWith(args[i], "-")) {
                result$visibility_mode <- args[i]
                i <- i + 1
            }
        } else if (arg == "--publish") {
            i <- i + 1
            result$image_publish <- args[i]
            i <- i + 1
        } else if (arg == "--spawn") {
            i <- i + 1
            result$image_spawn <- args[i]
            i <- i + 1
        } else if (!startsWith(arg, "-")) {
            result$source_file <- arg
            i <- i + 1
        } else {
            cat(sprintf("Unknown option: %s\n", arg), file = stderr())
            cat("Usage: un.r [options] <source_file>\n", file = stderr())
            cat("       un.r session [options]\n", file = stderr())
            cat("       un.r service [options]\n", file = stderr())
            cat("       un.r service env <action> <service_id> [options]\n", file = stderr())
            cat("       un.r snapshot [options]\n", file = stderr())
            cat("       un.r key [options]\n", file = stderr())
            cat("\nService env commands:\n", file = stderr())
            cat("  env status <id>     Show vault status\n", file = stderr())
            cat("  env set <id>        Set vault (-e KEY=VALUE or --env-file FILE)\n", file = stderr())
            cat("  env export <id>     Export vault contents\n", file = stderr())
            cat("  env delete <id>     Delete vault\n", file = stderr())
            quit(status = 1)
        }
    }

    return(result)
}

main <- function() {
    args <- parse_args()

    if (!is.null(args$command) && args$command == "session") {
        cmd_session(args)
    } else if (!is.null(args$command) && args$command == "service") {
        cmd_service(args)
    } else if (!is.null(args$command) && args$command == "snapshot") {
        cmd_snapshot(args)
    } else if (!is.null(args$command) && args$command == "image") {
        cmd_image(args)
    } else if (!is.null(args$command) && args$command == "key") {
        cmd_key(args)
    } else if (!is.null(args$command) && args$command == "languages") {
        cmd_languages(args)
    } else if (!is.null(args$source_file)) {
        cmd_execute(args)
    } else {
        cat("Usage: un.r [options] <source_file>\n", file = stderr())
        cat("       un.r session [options]\n", file = stderr())
        cat("       un.r service [options]\n", file = stderr())
        cat("       un.r service env <action> <service_id> [options]\n", file = stderr())
        cat("       un.r snapshot [options]\n", file = stderr())
        cat("       un.r image [options]\n", file = stderr())
        cat("       un.r key [options]\n", file = stderr())
        cat("       un.r languages [--json]\n", file = stderr())
        cat("\nLanguages options:\n", file = stderr())
        cat("  --json              Output as JSON array\n", file = stderr())
        cat("\nService env commands:\n", file = stderr())
        cat("  env status <id>     Show vault status\n", file = stderr())
        cat("  env set <id>        Set vault (-e KEY=VALUE or --env-file FILE)\n", file = stderr())
        cat("  env export <id>     Export vault contents\n", file = stderr())
        cat("  env delete <id>     Delete vault\n", file = stderr())
        cat("\nImage commands:\n", file = stderr())
        cat("  --list              List all images\n", file = stderr())
        cat("  --info ID           Get image details\n", file = stderr())
        cat("  --delete ID         Delete an image\n", file = stderr())
        cat("  --lock ID           Lock image to prevent deletion\n", file = stderr())
        cat("  --unlock ID         Unlock image\n", file = stderr())
        cat("  --publish ID        Publish image from service/snapshot\n", file = stderr())
        cat("  --source-type TYPE  Source type: service or snapshot\n", file = stderr())
        cat("  --visibility ID MODE  Set visibility: private, unlisted, public\n", file = stderr())
        cat("  --spawn ID          Spawn new service from image\n", file = stderr())
        cat("  --clone ID          Clone an image\n", file = stderr())
        cat("  --name NAME         Name for spawned service or cloned image\n", file = stderr())
        cat("  --ports PORTS       Ports for spawned service\n", file = stderr())
        quit(status = 1)
    }
}

# Only run main if executed as a script (not when sourced as a library)
if (!interactive() && identical(environment(), globalenv())) {
    main()
}
