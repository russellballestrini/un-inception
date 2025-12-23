# PUBLIC DOMAIN - NO LICENSE, NO WARRANTY
#
# This is free public domain software for the public good of a permacomputer hosted
# at permacomputer.com - an always-on computer by the people, for the people. One
# which is durable, easy to repair, and distributed like tap water for machine
# learning intelligence.
#
# The permacomputer is community-owned infrastructure optimized around four values:
#
#   TRUTH    - Source code must be open source & freely distributed
#   FREEDOM  - Voluntary participation without corporate control
#   HARMONY  - Systems operating with minimal waste that self-renew
#   LOVE     - Individual rights protected while fostering cooperation
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

detect_language <- function(filename) {
    ext <- tolower(sub(".*(\\..*)$", "\\1", filename))
    lang <- ext_map[[ext]]
    if (is.null(lang)) {
        return("unknown")
    }
    return(lang)
}

get_api_key <- function(args_key = NULL) {
    key <- if (!is.null(args_key)) args_key else Sys.getenv("UNSANDBOX_API_KEY")
    if (key == "") {
        cat(sprintf("%sError: UNSANDBOX_API_KEY not set%s\n", RED, RESET), file = stderr())
        quit(status = 1)
    }
    return(key)
}

api_request <- function(endpoint, api_key, method = "GET", data = NULL) {
    url <- paste0(API_BASE, endpoint)
    headers <- add_headers(
        `Content-Type` = "application/json",
        `Authorization` = paste("Bearer", api_key)
    )

    tryCatch({
        if (method == "GET") {
            response <- GET(url, headers, timeout(300))
        } else if (method == "POST") {
            body <- if (!is.null(data)) toJSON(data, auto_unbox = TRUE) else ""
            response <- POST(url, headers, body = body, encode = "raw", timeout(300))
        } else if (method == "DELETE") {
            response <- DELETE(url, headers, timeout(300))
        } else {
            stop(paste("Unsupported method:", method))
        }

        result <- fromJSON(content(response, "text", encoding = "UTF-8"))
        return(result)
    }, error = function(e) {
        cat(sprintf("%sError: Request failed: %s%s\n", RED, e$message, RESET), file = stderr())
        quit(status = 1)
    })
}

cmd_execute <- function(args) {
    api_key <- get_api_key(args$api_key)

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
    result <- api_request("/execute", api_key, method = "POST", data = payload)

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
    api_key <- get_api_key(args$api_key)

    if (!is.null(args$list) && args$list) {
        result <- api_request("/sessions", api_key)
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
        result <- api_request(paste0("/sessions/", args$kill), api_key, method = "DELETE")
        cat(sprintf("%sSession terminated: %s%s\n", GREEN, args$kill, RESET))
        return()
    }

    cat(sprintf("%sError: Use --list or --kill%s\n", RED, RESET), file = stderr())
    quit(status = 1)
}

cmd_service <- function(args) {
    api_key <- get_api_key(args$api_key)

    if (!is.null(args$list) && args$list) {
        result <- api_request("/services", api_key)
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
        result <- api_request(paste0("/services/", args$info), api_key)
        cat(toJSON(result, pretty = TRUE, auto_unbox = TRUE), "\n")
        return()
    }

    if (!is.null(args$logs)) {
        result <- api_request(paste0("/services/", args$logs, "/logs"), api_key)
        cat(if (!is.null(result$logs)) result$logs else "", "\n")
        return()
    }

    if (!is.null(args$sleep)) {
        result <- api_request(paste0("/services/", args$sleep, "/sleep"), api_key, method = "POST")
        cat(sprintf("%sService sleeping: %s%s\n", GREEN, args$sleep, RESET))
        return()
    }

    if (!is.null(args$wake)) {
        result <- api_request(paste0("/services/", args$wake, "/wake"), api_key, method = "POST")
        cat(sprintf("%sService waking: %s%s\n", GREEN, args$wake, RESET))
        return()
    }

    if (!is.null(args$destroy)) {
        result <- api_request(paste0("/services/", args$destroy), api_key, method = "DELETE")
        cat(sprintf("%sService destroyed: %s%s\n", GREEN, args$destroy, RESET))
        return()
    }

    cat(sprintf("%sError: Use --list, --info, --logs, --sleep, --wake, or --destroy%s\n", RED, RESET), file = stderr())
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
        info = NULL,
        logs = NULL,
        sleep = NULL,
        wake = NULL,
        destroy = NULL
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
            result$env <- c(result$env, args[i])
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
        } else if (arg == "--sleep") {
            i <- i + 1
            result$sleep <- args[i]
            i <- i + 1
        } else if (arg == "--wake") {
            i <- i + 1
            result$wake <- args[i]
            i <- i + 1
        } else if (arg == "--destroy") {
            i <- i + 1
            result$destroy <- args[i]
            i <- i + 1
        } else if (!startsWith(arg, "-")) {
            result$source_file <- arg
            i <- i + 1
        } else {
            cat(sprintf("Unknown argument: %s\n", arg), file = stderr())
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
    } else if (!is.null(args$source_file)) {
        cmd_execute(args)
    } else {
        cat("Usage: un.r [options] <source_file>\n", file = stderr())
        cat("       un.r session [options]\n", file = stderr())
        cat("       un.r service [options]\n", file = stderr())
        quit(status = 1)
    }
}

main()
