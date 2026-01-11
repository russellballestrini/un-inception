#!/usr/bin/env tclsh
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

# unsandbox CLI - TCL implementation
# Full-featured CLI matching un.c/un.py capabilities

package require http
package require json
package require tls
package require base64
package require sha256

# Register https support
::http::register https 443 ::tls::socket

set API_BASE "https://api.unsandbox.com"
set PORTAL_BASE "https://unsandbox.com"
set BLUE "\033\[34m"
set RED "\033\[31m"
set GREEN "\033\[32m"
set YELLOW "\033\[33m"
set RESET "\033\[0m"

# Extension to language mapping
array set EXT_MAP {
    .py python .js javascript .ts typescript
    .rb ruby .php php .pl perl .lua lua
    .sh bash .go go .rs rust .c c
    .cpp cpp .cc cpp .cxx cpp
    .java java .kt kotlin .cs csharp .fs fsharp
    .hs haskell .ml ocaml .clj clojure .scm scheme
    .lisp commonlisp .erl erlang .ex elixir .exs elixir
    .jl julia .r r .R r .cr crystal
    .d d .nim nim .zig zig .v vlang
    .dart dart .groovy groovy .scala scala
    .f90 fortran .f95 fortran .cob cobol
    .pro prolog .forth forth .4th forth
    .tcl tcl .raku raku .m objc
}

proc get_api_keys {} {
    set public_key ""
    set secret_key ""

    if {[info exists ::env(UNSANDBOX_PUBLIC_KEY)]} {
        set public_key $::env(UNSANDBOX_PUBLIC_KEY)
    }
    if {[info exists ::env(UNSANDBOX_SECRET_KEY)]} {
        set secret_key $::env(UNSANDBOX_SECRET_KEY)
    }

    # Fallback to old UNSANDBOX_API_KEY for backwards compat
    if {$public_key eq "" && [info exists ::env(UNSANDBOX_API_KEY)]} {
        set public_key $::env(UNSANDBOX_API_KEY)
        set secret_key ""
    }

    if {$public_key eq ""} {
        puts stderr "${::RED}Error: UNSANDBOX_PUBLIC_KEY or UNSANDBOX_API_KEY not set${::RESET}"
        exit 1
    }
    return [list $public_key $secret_key]
}

proc detect_language {filename} {
    set ext [file extension $filename]
    if {[info exists ::EXT_MAP($ext)]} {
        return $::EXT_MAP($ext)
    }

    # Try reading shebang
    if {[catch {open $filename r} fp] == 0} {
        set first_line [gets $fp]
        close $fp
        if {[string match "#!*" $first_line]} {
            if {[string match "*python*" $first_line]} { return "python" }
            if {[string match "*node*" $first_line]} { return "javascript" }
            if {[string match "*ruby*" $first_line]} { return "ruby" }
            if {[string match "*perl*" $first_line]} { return "perl" }
            if {[string match "*bash*" $first_line] || [string match "*/sh*" $first_line]} { return "bash" }
        }
    }

    puts stderr "${::RED}Error: Cannot detect language for $filename${::RESET}"
    exit 1
}

proc api_request {endpoint method data public_key secret_key} {
    set url "${::API_BASE}${endpoint}"
    set headers [list Authorization "Bearer $public_key" Content-Type "application/json"]

    set json_data ""
    if {$method ne "GET" && $method ne "DELETE" && [llength $data] > 0} {
        set json_data [::json::write object {*}$data]
    }

    # Add HMAC signature if secret_key is present
    if {$secret_key ne ""} {
        set timestamp [clock seconds]
        set sig_input "${timestamp}:${method}:${endpoint}:${json_data}"
        set signature [::sha2::hmac -hex -key $secret_key $sig_input]
        lappend headers X-Timestamp $timestamp
        lappend headers X-Signature $signature
    }

    if {$method eq "GET"} {
        set token [::http::geturl $url -headers $headers -timeout 300000]
    } elseif {$method eq "DELETE"} {
        set token [::http::geturl $url -method DELETE -headers $headers -timeout 300000]
    } else {
        set token [::http::geturl $url -method $method -headers $headers -query $json_data -timeout 300000]
    }

    set status [::http::status $token]
    set ncode [::http::ncode $token]
    set body [::http::data $token]
    ::http::cleanup $token

    if {$status ne "ok" || ($ncode != 200 && $ncode != 201)} {
        if {$ncode == 401 && [string match -nocase "*timestamp*" $body]} {
            puts stderr "${::RED}Error: Request timestamp expired (must be within 5 minutes of server time)${::RESET}"
            puts stderr "${::YELLOW}Your computer's clock may have drifted.${::RESET}"
            puts stderr "Check your system time and sync with NTP if needed:"
            puts stderr "  Linux:   sudo ntpdate -s time.nist.gov"
            puts stderr "  macOS:   sudo sntp -sS time.apple.com"
            puts stderr "  Windows: w32tm /resync"
        } else {
            puts stderr "${::RED}Error: HTTP $ncode${::RESET}"
            puts stderr $body
        }
        exit 1
    }

    return [::json::json2dict $body]
}

proc api_request_text {endpoint method body public_key secret_key} {
    set url "${::API_BASE}${endpoint}"
    set headers [list Authorization "Bearer $public_key" Content-Type "text/plain"]

    # Add HMAC signature if secret_key is present
    if {$secret_key ne ""} {
        set timestamp [clock seconds]
        set sig_input "${timestamp}:${method}:${endpoint}:${body}"
        set signature [::sha2::hmac -hex -key $secret_key $sig_input]
        lappend headers X-Timestamp $timestamp
        lappend headers X-Signature $signature
    }

    set token [::http::geturl $url -method $method -headers $headers -query $body -timeout 300000]
    set status [::http::status $token]
    set ncode [::http::ncode $token]
    set response [::http::data $token]
    ::http::cleanup $token

    return [list $ncode $response]
}

proc read_env_file {path} {
    if {![file exists $path]} {
        puts stderr "${::RED}Error: Env file not found: $path${::RESET}"
        exit 1
    }
    set fp [open $path r]
    set content [read $fp]
    close $fp
    return $content
}

proc build_env_content {envs env_file} {
    set lines [list]

    # Add from -e flags
    foreach env $envs {
        lappend lines $env
    }

    # Add from --env-file
    if {$env_file ne ""} {
        set content [read_env_file $env_file]
        foreach line [split $content "\n"] {
            set line [string trim $line]
            if {$line ne "" && [string index $line 0] ne "#"} {
                lappend lines $line
            }
        }
    }

    return [join $lines "\n"]
}

set MAX_ENV_CONTENT_SIZE 65536

proc service_env_status {service_id public_key secret_key} {
    return [api_request "/services/$service_id/env" "GET" {} $public_key $secret_key]
}

proc service_env_set {service_id env_content public_key secret_key} {
    if {[string length $env_content] > $::MAX_ENV_CONTENT_SIZE} {
        puts stderr "${::RED}Error: Env content exceeds maximum size of 64KB${::RESET}"
        return 0
    }

    lassign [api_request_text "/services/$service_id/env" "PUT" $env_content $public_key $secret_key] ncode response
    if {$ncode == 200 || $ncode == 201} {
        return 1
    }
    return 0
}

proc service_env_export {service_id public_key secret_key} {
    return [api_request "/services/$service_id/env/export" "POST" {} $public_key $secret_key]
}

proc service_env_delete {service_id public_key secret_key} {
    if {[catch {api_request "/services/$service_id/env" "DELETE" {} $public_key $secret_key}]} {
        return 0
    }
    return 1
}

proc cmd_service_env {action target envs env_file public_key secret_key} {
    switch -exact -- $action {
        status {
            if {$target eq ""} {
                puts stderr "${::RED}Error: service env status requires service ID${::RESET}"
                exit 1
            }
            set result [service_env_status $target $public_key $secret_key]
            if {[dict exists $result has_vault] && [dict get $result has_vault]} {
                puts "${::GREEN}Vault: configured${::RESET}"
                if {[dict exists $result env_count]} {
                    puts "Variables: [dict get $result env_count]"
                }
                if {[dict exists $result updated_at]} {
                    puts "Updated: [dict get $result updated_at]"
                }
            } else {
                puts "${::YELLOW}Vault: not configured${::RESET}"
            }
        }
        set {
            if {$target eq ""} {
                puts stderr "${::RED}Error: service env set requires service ID${::RESET}"
                exit 1
            }
            if {[llength $envs] == 0 && $env_file eq ""} {
                puts stderr "${::RED}Error: service env set requires -e or --env-file${::RESET}"
                exit 1
            }
            set env_content [build_env_content $envs $env_file]
            if {[service_env_set $target $env_content $public_key $secret_key]} {
                puts "${::GREEN}Vault updated for service $target${::RESET}"
            } else {
                puts stderr "${::RED}Error: Failed to update vault${::RESET}"
                exit 1
            }
        }
        export {
            if {$target eq ""} {
                puts stderr "${::RED}Error: service env export requires service ID${::RESET}"
                exit 1
            }
            set result [service_env_export $target $public_key $secret_key]
            if {[dict exists $result content]} {
                puts -nonewline [dict get $result content]
            }
        }
        delete {
            if {$target eq ""} {
                puts stderr "${::RED}Error: service env delete requires service ID${::RESET}"
                exit 1
            }
            if {[service_env_delete $target $public_key $secret_key]} {
                puts "${::GREEN}Vault deleted for service $target${::RESET}"
            } else {
                puts stderr "${::RED}Error: Failed to delete vault${::RESET}"
                exit 1
            }
        }
        default {
            puts stderr "${::RED}Error: Unknown env action: $action${::RESET}"
            puts stderr "Usage: un.tcl service env <status|set|export|delete> <service_id>"
            exit 1
        }
    }
}

proc cmd_execute {args} {
    lassign [get_api_keys] public_key secret_key
    set source_file ""
    set env_vars [dict create]
    set input_files [list]
    set artifacts 0
    set output_dir "."
    set network ""
    set vcpu 0

    # Parse arguments
    for {set i 0} {$i < [llength $args]} {incr i} {
        set arg [lindex $args $i]
        switch -exact -- $arg {
            -e {
                incr i
                set env_spec [lindex $args $i]
                if {[regexp {^([^=]+)=(.*)$} $env_spec -> key value]} {
                    dict set env_vars $key $value
                }
            }
            -f {
                incr i
                lappend input_files [lindex $args $i]
            }
            -a {
                set artifacts 1
            }
            -o {
                incr i
                set output_dir [lindex $args $i]
            }
            -n {
                incr i
                set network [lindex $args $i]
            }
            -v {
                incr i
                set vcpu [lindex $args $i]
            }
            default {
                set source_file $arg
            }
        }
    }

    if {$source_file eq ""} {
        puts stderr "Usage: un.tcl \[options\] <source_file>"
        exit 1
    }

    if {![file exists $source_file]} {
        puts stderr "${::RED}Error: File not found: $source_file${::RESET}"
        exit 1
    }

    # Read source file
    set fp [open $source_file r]
    set code [read $fp]
    close $fp

    set language [detect_language $source_file]

    # Build request payload
    set payload [list language [::json::write string $language] code [::json::write string $code]]

    # Add environment variables
    if {[dict size $env_vars] > 0} {
        set env_json [list]
        dict for {key value} $env_vars {
            lappend env_json $key [::json::write string $value]
        }
        lappend payload env [::json::write object {*}$env_json]
    }

    # Add input files
    if {[llength $input_files] > 0} {
        set files_json [list]
        foreach filepath $input_files {
            if {![file exists $filepath]} {
                puts stderr "${::RED}Error: Input file not found: $filepath${::RESET}"
                exit 1
            }
            set fp [open $filepath rb]
            set content [read $fp]
            close $fp
            set b64_content [::base64::encode $content]
            lappend files_json [::json::write object \
                filename [::json::write string [file tail $filepath]] \
                content_base64 [::json::write string $b64_content]]
        }
        lappend payload input_files [::json::write array {*}$files_json]
    }

    # Add options
    if {$artifacts} {
        lappend payload return_artifacts [::json::write string true]
    }
    if {$network ne ""} {
        lappend payload network [::json::write string $network]
    }
    if {$vcpu > 0} {
        lappend payload vcpu $vcpu
    }

    # Execute
    set result [api_request "/execute" "POST" $payload $public_key $secret_key]

    # Print output
    if {[dict exists $result stdout]} {
        set stdout_text [dict get $result stdout]
        if {$stdout_text ne ""} {
            puts -nonewline "${::BLUE}${stdout_text}${::RESET}"
        }
    }
    if {[dict exists $result stderr]} {
        set stderr_text [dict get $result stderr]
        if {$stderr_text ne ""} {
            puts -nonewline stderr "${::RED}${stderr_text}${::RESET}"
        }
    }

    # Save artifacts
    if {$artifacts && [dict exists $result artifacts]} {
        file mkdir $output_dir
        foreach artifact [dict get $result artifacts] {
            set filename [dict get $artifact filename]
            set content [::base64::decode [dict get $artifact content_base64]]
            set path [file join $output_dir $filename]
            set fp [open $path wb]
            puts -nonewline $fp $content
            close $fp
            file attributes $path -permissions 0755
            puts stderr "${::GREEN}Saved: $path${::RESET}"
        }
    }

    set exit_code 0
    if {[dict exists $result exit_code]} {
        set exit_code [dict get $result exit_code]
    }
    exit $exit_code
}

proc cmd_session {args} {
    lassign [get_api_keys] public_key secret_key
    set list_mode 0
    set kill_id ""
    set shell ""
    set network ""
    set vcpu 0
    set input_files [list]

    # Parse arguments
    for {set i 0} {$i < [llength $args]} {incr i} {
        set arg [lindex $args $i]
        switch -exact -- $arg {
            --list {
                set list_mode 1
            }
            --kill {
                incr i
                set kill_id [lindex $args $i]
            }
            --shell {
                incr i
                set shell [lindex $args $i]
            }
            -n {
                incr i
                set network [lindex $args $i]
            }
            -v {
                incr i
                set vcpu [lindex $args $i]
            }
            -f {
                incr i
                lappend input_files [lindex $args $i]
            }
            default {
                if {[string index $arg 0] eq "-"} {
                    puts stderr "${::RED}Unknown option: $arg${::RESET}"
                    puts stderr "Usage: un.tcl session \[options\]"
                    exit 1
                }
            }
        }
    }

    if {$list_mode} {
        set result [api_request "/sessions" "GET" {} $public_key $secret_key]
        set sessions [dict get $result sessions]
        if {[llength $sessions] == 0} {
            puts "No active sessions"
        } else {
            puts [format "%-40s %-10s %-10s %s" "ID" "Shell" "Status" "Created"]
            foreach s $sessions {
                puts [format "%-40s %-10s %-10s %s" \
                    [dict get $s id] \
                    [dict get $s shell] \
                    [dict get $s status] \
                    [dict get $s created_at]]
            }
        }
        return
    }

    if {$kill_id ne ""} {
        api_request "/sessions/$kill_id" "DELETE" {} $public_key $secret_key
        puts "${::GREEN}Session terminated: $kill_id${::RESET}"
        return
    }

    # Create new session
    set payload [list]
    if {$shell ne ""} {
        lappend payload shell [::json::write string $shell]
    } else {
        lappend payload shell [::json::write string "bash"]
    }
    if {$network ne ""} {
        lappend payload network [::json::write string $network]
    }
    if {$vcpu > 0} {
        lappend payload vcpu $vcpu
    }

    # Add input files
    if {[llength $input_files] > 0} {
        set files_json [list]
        foreach filepath $input_files {
            if {![file exists $filepath]} {
                puts stderr "${::RED}Error: Input file not found: $filepath${::RESET}"
                exit 1
            }
            set fp [open $filepath rb]
            set content [read $fp]
            close $fp
            set b64_content [::base64::encode $content]
            lappend files_json [::json::write object \
                filename [::json::write string [file tail $filepath]] \
                content_base64 [::json::write string $b64_content]]
        }
        lappend payload input_files [::json::write array {*}$files_json]
    }

    puts "${::YELLOW}Creating session...${::RESET}"
    set result [api_request "/sessions" "POST" $payload $public_key $secret_key]
    puts "${::GREEN}Session created: [dict get $result id]${::RESET}"
    puts "${::YELLOW}(Interactive sessions require WebSocket - use un2 for full support)${::RESET}"
}

proc cmd_key {args} {
    lassign [get_api_keys] public_key secret_key
    set extend_mode 0

    # Parse arguments
    for {set i 0} {$i < [llength $args]} {incr i} {
        set arg [lindex $args $i]
        switch -exact -- $arg {
            --extend {
                set extend_mode 1
            }
        }
    }

    # POST to /keys/validate with Bearer auth
    set url "${::PORTAL_BASE}/keys/validate"
    set headers [list Authorization "Bearer $public_key" Content-Type "application/json"]

    # Add HMAC signature if secret_key is present
    if {$secret_key ne ""} {
        set timestamp [clock seconds]
        set sig_input "${timestamp}:POST:/keys/validate:"
        set signature [::sha2::hmac -hex -key $secret_key $sig_input]
        lappend headers X-Timestamp $timestamp
        lappend headers X-Signature $signature
    }

    set token [::http::geturl $url -method POST -headers $headers -timeout 30000]
    set status [::http::status $token]
    set ncode [::http::ncode $token]
    set body [::http::data $token]
    ::http::cleanup $token

    if {$status ne "ok"} {
        puts stderr "${::RED}Error: Failed to connect to validation endpoint${::RESET}"
        exit 1
    }

    if {$ncode == 401 || $ncode == 403} {
        puts "${::RED}Invalid${::RESET}"
        puts "Status: Invalid API key"
        exit 1
    }

    if {$ncode != 200} {
        puts stderr "${::RED}Error: HTTP $ncode${::RESET}"
        puts stderr $body
        exit 1
    }

    set result [::json::json2dict $body]
    set key_status [dict get $result status]
    set public_key [dict get $result public_key]
    set tier [dict get $result tier]

    if {$key_status eq "valid"} {
        puts "${::GREEN}Valid${::RESET}"
        puts "Public Key: $public_key"
        puts "Tier: $tier"

        if {[dict exists $result expires_at]} {
            set expires_at [dict get $result expires_at]
            puts "Expires: $expires_at"
        }

        if {$extend_mode} {
            set extend_url "${::PORTAL_BASE}/keys/extend?pk=${public_key}"
            puts "${::YELLOW}Opening browser to extend key...${::RESET}"
            exec xdg-open $extend_url &
        }
    } elseif {$key_status eq "expired"} {
        puts "${::RED}Expired${::RESET}"
        puts "Public Key: $public_key"
        puts "Tier: $tier"

        if {[dict exists $result expired_at]} {
            set expired_at [dict get $result expired_at]
            puts "Expired: $expired_at"
        }

        puts "${::YELLOW}To renew: Visit ${::PORTAL_BASE}/keys/extend${::RESET}"

        if {$extend_mode} {
            set extend_url "${::PORTAL_BASE}/keys/extend?pk=${public_key}"
            puts "${::YELLOW}Opening browser to extend key...${::RESET}"
            exec xdg-open $extend_url &
        }
    } else {
        puts "${::RED}Invalid${::RESET}"
        puts "Status: Unknown key status"
        exit 1
    }
}

proc cmd_service {args} {
    lassign [get_api_keys] public_key secret_key
    set list_mode 0
    set info_id ""
    set logs_id ""
    set sleep_id ""
    set wake_id ""
    set destroy_id ""
    set resize_id ""
    set dump_bootstrap_id ""
    set dump_file ""
    set name ""
    set ports ""
    set service_type ""
    set bootstrap ""
    set bootstrap_file ""
    set network ""
    set vcpu 0
    set input_files [list]
    set envs [list]
    set env_file ""
    set env_action ""
    set env_target ""

    # Parse arguments
    for {set i 0} {$i < [llength $args]} {incr i} {
        set arg [lindex $args $i]
        switch -exact -- $arg {
            env {
                # Parse: env <action> [target]
                if {$i + 1 < [llength $args]} {
                    set next [lindex $args [expr {$i + 1}]]
                    if {[string index $next 0] ne "-"} {
                        incr i
                        set env_action $next
                        if {$i + 1 < [llength $args]} {
                            set next2 [lindex $args [expr {$i + 1}]]
                            if {[string index $next2 0] ne "-"} {
                                incr i
                                set env_target $next2
                            }
                        }
                    }
                }
            }
            --list {
                set list_mode 1
            }
            --info {
                incr i
                set info_id [lindex $args $i]
            }
            --logs {
                incr i
                set logs_id [lindex $args $i]
            }
            --freeze {
                incr i
                set sleep_id [lindex $args $i]
            }
            --unfreeze {
                incr i
                set wake_id [lindex $args $i]
            }
            --destroy {
                incr i
                set destroy_id [lindex $args $i]
            }
            --resize {
                incr i
                set resize_id [lindex $args $i]
            }
            --dump-bootstrap {
                incr i
                set dump_bootstrap_id [lindex $args $i]
            }
            --dump-file {
                incr i
                set dump_file [lindex $args $i]
            }
            --name {
                incr i
                set name [lindex $args $i]
            }
            --ports {
                incr i
                set ports [lindex $args $i]
            }
            --type {
                incr i
                set service_type [lindex $args $i]
            }
            --bootstrap {
                incr i
                set bootstrap [lindex $args $i]
            }
            --bootstrap-file {
                incr i
                set bootstrap_file [lindex $args $i]
            }
            -n {
                incr i
                set network [lindex $args $i]
            }
            -v {
                incr i
                set vcpu [lindex $args $i]
            }
            -f {
                incr i
                lappend input_files [lindex $args $i]
            }
            -e {
                incr i
                lappend envs [lindex $args $i]
            }
            --env-file {
                incr i
                set env_file [lindex $args $i]
            }
        }
    }

    # Handle env subcommand
    if {$env_action ne ""} {
        cmd_service_env $env_action $env_target $envs $env_file $public_key $secret_key
        return
    }

    if {$list_mode} {
        set result [api_request "/services" "GET" {} $public_key $secret_key]
        set services [dict get $result services]
        if {[llength $services] == 0} {
            puts "No services"
        } else {
            puts [format "%-20s %-15s %-10s %-15s %s" "ID" "Name" "Status" "Ports" "Domains"]
            foreach s $services {
                set port_list [dict get $s ports]
                set domain_list [dict get $s domains]
                puts [format "%-20s %-15s %-10s %-15s %s" \
                    [dict get $s id] \
                    [dict get $s name] \
                    [dict get $s status] \
                    [join $port_list ","] \
                    [join $domain_list ","]]
            }
        }
        return
    }

    if {$info_id ne ""} {
        set result [api_request "/services/$info_id" "GET" {} $public_key $secret_key]
        puts [::json::write object {*}[dict_to_json_list $result]]
        return
    }

    if {$logs_id ne ""} {
        set result [api_request "/services/$logs_id/logs" "GET" {} $public_key $secret_key]
        puts [dict get $result logs]
        return
    }

    if {$sleep_id ne ""} {
        api_request "/services/$sleep_id/sleep" "POST" {} $public_key $secret_key
        puts "${::GREEN}Service sleeping: $sleep_id${::RESET}"
        return
    }

    if {$wake_id ne ""} {
        api_request "/services/$wake_id/wake" "POST" {} $public_key $secret_key
        puts "${::GREEN}Service waking: $wake_id${::RESET}"
        return
    }

    if {$destroy_id ne ""} {
        api_request "/services/$destroy_id" "DELETE" {} $public_key $secret_key
        puts "${::GREEN}Service destroyed: $destroy_id${::RESET}"
        return
    }

    if {$resize_id ne ""} {
        if {$vcpu < 1 || $vcpu > 8} {
            puts stderr "${::RED}Error: --resize requires --vcpu N (1-8)${::RESET}"
            exit 1
        }
        set payload [list vcpu $vcpu]
        api_request "/services/$resize_id" "PATCH" $payload $public_key $secret_key
        set ram [expr {$vcpu * 2}]
        puts "${::GREEN}Service resized to $vcpu vCPU, $ram GB RAM${::RESET}"
        return
    }

    if {$dump_bootstrap_id ne ""} {
        puts stderr "Fetching bootstrap script from $dump_bootstrap_id..."
        set payload [list command [::json::write string "cat /tmp/bootstrap.sh"]]
        set result [api_request "/services/$dump_bootstrap_id/execute" "POST" $payload $public_key $secret_key]

        if {[dict exists $result stdout] && [dict get $result stdout] ne ""} {
            set bootstrap [dict get $result stdout]
            if {$dump_file ne ""} {
                # Write to file
                set fp [open $dump_file w]
                puts -nonewline $fp $bootstrap
                close $fp
                file attributes $dump_file -permissions 0755
                puts "Bootstrap saved to $dump_file"
            } else {
                # Print to stdout
                puts -nonewline $bootstrap
            }
        } else {
            puts stderr "${::RED}Error: Failed to fetch bootstrap (service not running or no bootstrap file)${::RESET}"
            exit 1
        }
        return
    }

    # Create new service
    if {$name ne ""} {
        set payload [list name [::json::write string $name]]

        if {$ports ne ""} {
            set port_list [split $ports ","]
            set port_json [list]
            foreach p $port_list {
                lappend port_json $p
            }
            lappend payload ports [::json::write array {*}$port_json]
        }

        if {$service_type ne ""} {
            lappend payload service_type [::json::write string $service_type]
        }

        if {$bootstrap ne ""} {
            lappend payload bootstrap [::json::write string $bootstrap]
        }

        if {$bootstrap_file ne ""} {
            if {[file exists $bootstrap_file]} {
                set fp [open $bootstrap_file r]
                set bootstrap_content [read $fp]
                close $fp
                lappend payload bootstrap_content [::json::write string $bootstrap_content]
            } else {
                puts stderr "${::RED}Error: Bootstrap file not found: $bootstrap_file${::RESET}"
                exit 1
            }
        }

        if {$network ne ""} {
            lappend payload network [::json::write string $network]
        }
        if {$vcpu > 0} {
            lappend payload vcpu $vcpu
        }

        # Add input files
        if {[llength $input_files] > 0} {
            set files_json [list]
            foreach filepath $input_files {
                if {![file exists $filepath]} {
                    puts stderr "${::RED}Error: Input file not found: $filepath${::RESET}"
                    exit 1
                }
                set fp [open $filepath rb]
                set content [read $fp]
                close $fp
                set b64_content [::base64::encode $content]
                lappend files_json [::json::write object \
                    filename [::json::write string [file tail $filepath]] \
                    content_base64 [::json::write string $b64_content]]
            }
            lappend payload input_files [::json::write array {*}$files_json]
        }

        set result [api_request "/services" "POST" $payload $public_key $secret_key]
        set service_id [dict get $result id]
        puts "${::GREEN}Service created: $service_id${::RESET}"
        puts "Name: [dict get $result name]"
        if {[dict exists $result url]} {
            puts "URL: [dict get $result url]"
        }

        # Auto-set vault if env vars were provided
        if {[llength $envs] > 0 || $env_file ne ""} {
            set env_content [build_env_content $envs $env_file]
            if {$env_content ne ""} {
                if {[service_env_set $service_id $env_content $public_key $secret_key]} {
                    puts "${::GREEN}Vault configured with environment variables${::RESET}"
                } else {
                    puts stderr "${::YELLOW}Warning: Failed to set vault${::RESET}"
                }
            }
        }
        return
    }

    puts stderr "${::RED}Error: Specify --name to create a service, or use --list, --info, etc.${::RESET}"
    exit 1
}

proc main {argv} {
    if {[llength $argv] == 0} {
        puts stderr "Usage: un.tcl \[options\] <source_file>"
        puts stderr "       un.tcl session \[options\]"
        puts stderr "       un.tcl service \[options\]"
        puts stderr "       un.tcl service env <action> <service_id> \[options\]"
        puts stderr "       un.tcl key \[--extend\]"
        puts stderr ""
        puts stderr "Service env commands:"
        puts stderr "  env status ID     Check vault status"
        puts stderr "  env set ID        Set vault (use -e or --env-file)"
        puts stderr "  env export ID     Export vault contents"
        puts stderr "  env delete ID     Delete vault"
        puts stderr ""
        puts stderr "Service vault options:"
        puts stderr "  -e KEY=VALUE      Set vault env var (with --name or env set)"
        puts stderr "  --env-file FILE   Load vault vars from file"
        exit 1
    }

    set first_arg [lindex $argv 0]

    if {$first_arg eq "session"} {
        cmd_session [lrange $argv 1 end]
    } elseif {$first_arg eq "service"} {
        cmd_service [lrange $argv 1 end]
    } elseif {$first_arg eq "key"} {
        cmd_key [lrange $argv 1 end]
    } else {
        cmd_execute $argv
    }
}

main $argv
