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

API_BASE <- "https://api.unsandbox.com"
PORTAL_BASE <- "https://unsandbox.com"
MAX_ENV_CONTENT_SIZE <- 65536

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

api_request <- function(endpoint, public_key, secret_key, method = "GET", data = NULL) {
    url <- paste0(API_BASE, endpoint)
    headers <- add_headers(
        `Content-Type` = "application/json",
        `Authorization` = paste("Bearer", public_key)
    )

    body_content <- ""
    if (!is.null(data)) {
        body_content <- toJSON(data, auto_unbox = TRUE)
    }

    # Add HMAC signature if secret_key is present
    if (secret_key != "") {
        timestamp <- as.integer(Sys.time())
        sig_input <- paste0(timestamp, ":", method, ":", endpoint, ":", body_content)
        signature <- hmac(sig_input, secret_key, algo = "sha256")
        headers <- add_headers(
            `Content-Type` = "application/json",
            `Authorization` = paste("Bearer", public_key),
            `X-Timestamp` = as.character(timestamp),
            `X-Signature` = signature
        )
    }

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
        env_target = NULL
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
        } else if (arg == "snapshot") {
            result$command <- "snapshot"
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
            result$info <- args[i]
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
            result$delete <- args[i]
            i <- i + 1
        } else if (arg == "--clone") {
            i <- i + 1
            result$clone <- args[i]
            i <- i + 1
        } else if (arg == "--shell") {
            i <- i + 1
            result$shell <- args[i]
            i <- i + 1
        } else if (arg == "--extend") {
            result$extend <- TRUE
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
    } else if (!is.null(args$command) && args$command == "key") {
        cmd_key(args)
    } else if (!is.null(args$source_file)) {
        cmd_execute(args)
    } else {
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

main()
