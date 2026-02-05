#!/usr/bin/env tclsh
# PUBLIC DOMAIN - NO LICENSE, NO WARRANTY
#
# unsandbox.com TCL SDK (Synchronous)
# Full API with execution, sessions, services, snapshots, and images.
#
# Library Usage:
#     source un.tcl
#     set result [Un::execute "python" "print(42)"]
#     puts [dict get $result stdout]
#
# CLI Usage:
#     tclsh un.tcl script.py
#     tclsh un.tcl -s python 'print(42)'
#     tclsh un.tcl session --list
#     tclsh un.tcl service --list
#
# Copyright 2025 TimeHexOn & foxhop & russell@unturf

package require http
package require json
package require tls
package require base64
package require sha256

# Register https support
::http::register https 443 ::tls::socket

namespace eval Un {
    variable VERSION "4.2.50"
    variable API_BASE "https://api.unsandbox.com"
    variable PORTAL_BASE "https://unsandbox.com"
    variable LANGUAGES_CACHE_TTL 3600
    variable LANGUAGES_CACHE_FILE [file join $::env(HOME) ".unsandbox" "languages.json"]
    variable LAST_ERROR ""

    # Colors
    variable BLUE "\033\[34m"
    variable RED "\033\[31m"
    variable GREEN "\033\[32m"
    variable YELLOW "\033\[33m"
    variable RESET "\033\[0m"

    # Extension to language mapping
    variable EXT_MAP
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

    # ============================================================================
    # Utility Functions
    # ============================================================================

    proc version {} {
        variable VERSION
        return $VERSION
    }

    proc last_error {} {
        variable LAST_ERROR
        return $LAST_ERROR
    }

    proc set_error {msg} {
        variable LAST_ERROR
        set LAST_ERROR $msg
    }

    proc detect_language {filename} {
        variable EXT_MAP
        if {$filename eq ""} { return "" }
        set ext [file extension $filename]
        if {[info exists EXT_MAP($ext)]} {
            return $EXT_MAP($ext)
        }
        return ""
    }

    proc hmac_sign {secret message} {
        if {$secret eq "" || $message eq ""} { return "" }
        return [::sha2::hmac -hex -key $secret $message]
    }

    proc health_check {} {
        variable API_BASE
        if {[catch {
            set token [::http::geturl "$API_BASE/health" -timeout 5000]
            set ncode [::http::ncode $token]
            ::http::cleanup $token
            return [expr {$ncode == 200}]
        }]} {
            return 0
        }
    }

    # ============================================================================
    # Credential Management
    # ============================================================================

    proc load_accounts_csv {path} {
        if {![file exists $path]} { return "" }
        if {[catch {open $path r} fp]} { return "" }
        set line [gets $fp]
        close $fp
        if {[string index $line 0] eq "#"} { return "" }
        return $line
    }

    proc get_credentials {{public_key ""} {secret_key ""}} {
        # Tier 1: Arguments
        if {$public_key ne "" && $secret_key ne ""} {
            return [list $public_key $secret_key]
        }

        # Tier 2: Environment
        if {[info exists ::env(UNSANDBOX_PUBLIC_KEY)] && [info exists ::env(UNSANDBOX_SECRET_KEY)]} {
            return [list $::env(UNSANDBOX_PUBLIC_KEY) $::env(UNSANDBOX_SECRET_KEY)]
        }

        # Legacy fallback
        if {[info exists ::env(UNSANDBOX_API_KEY)]} {
            return [list $::env(UNSANDBOX_API_KEY) ""]
        }

        # Tier 3: Home directory
        set creds [load_accounts_csv [file join $::env(HOME) ".unsandbox" "accounts.csv"]]
        if {$creds ne ""} {
            set parts [split $creds ","]
            return [list [lindex $parts 0] [lindex $parts 1]]
        }

        # Tier 4: Local directory
        set creds [load_accounts_csv "./accounts.csv"]
        if {$creds ne ""} {
            set parts [split $creds ","]
            return [list [lindex $parts 0] [lindex $parts 1]]
        }

        set_error "No credentials found"
        error "No credentials found"
    }

    # ============================================================================
    # API Communication
    # ============================================================================

    proc api_request {endpoint method body {public_key ""} {secret_key ""} {extra_headers {}} {content_type "application/json"}} {
        variable API_BASE

        lassign [get_credentials $public_key $secret_key] pk sk

        set headers [list Authorization "Bearer $pk" Content-Type $content_type]

        # Add HMAC signature if secret key exists
        if {$sk ne ""} {
            set timestamp [clock seconds]
            set sig_input "${timestamp}:${method}:${endpoint}:${body}"
            set signature [hmac_sign $sk $sig_input]
            lappend headers X-Timestamp $timestamp
            lappend headers X-Signature $signature
        }

        # Add extra headers
        foreach {k v} $extra_headers {
            lappend headers $k $v
        }

        set url "${API_BASE}${endpoint}"

        if {$method eq "GET"} {
            set token [::http::geturl $url -headers $headers -timeout 300000]
        } elseif {$method eq "DELETE"} {
            set token [::http::geturl $url -method DELETE -headers $headers -timeout 300000]
        } else {
            set token [::http::geturl $url -method $method -headers $headers -query $body -timeout 300000]
        }

        set ncode [::http::ncode $token]
        set response [::http::data $token]
        ::http::cleanup $token

        return [list $ncode $response]
    }

    proc api_request_json {endpoint method body {public_key ""} {secret_key ""}} {
        lassign [api_request $endpoint $method $body $public_key $secret_key] ncode response
        if {$ncode >= 200 && $ncode < 300} {
            if {[catch {::json::json2dict $response} result]} {
                return $response
            }
            return $result
        }
        set_error "API error ($ncode): $response"
        error "API error ($ncode)"
    }

    proc api_request_with_sudo {endpoint method body {public_key ""} {secret_key ""}} {
        variable YELLOW RED GREEN RESET

        lassign [api_request $endpoint $method $body $public_key $secret_key] ncode response

        # Handle 428 - Sudo OTP required
        if {$ncode == 428} {
            set challenge_id ""
            if {[catch {::json::json2dict $response} resp_dict] == 0} {
                if {[dict exists $resp_dict challenge_id]} {
                    set challenge_id [dict get $resp_dict challenge_id]
                }
            }

            puts stderr "${YELLOW}Confirmation required. Check your email for a one-time code.${RESET}"
            puts -nonewline stderr "Enter OTP: "
            flush stderr

            gets stdin otp
            set otp [string trim $otp]

            if {$otp eq ""} {
                set_error "Operation cancelled"
                error "Operation cancelled"
            }

            # Retry with sudo headers
            set extra_headers [list X-Sudo-OTP $otp]
            if {$challenge_id ne ""} {
                lappend extra_headers X-Sudo-Challenge $challenge_id
            }

            lassign [api_request $endpoint $method $body $public_key $secret_key $extra_headers] ncode response
        }

        if {$ncode >= 200 && $ncode < 300} {
            if {[catch {::json::json2dict $response} result]} {
                return $response
            }
            return $result
        }
        set_error "API error ($ncode)"
        error "API error ($ncode)"
    }

    # ============================================================================
    # Execution Functions (8)
    # ============================================================================

    proc execute {language code {network_mode "zerotrust"} {public_key ""} {secret_key ""}} {
        set body [::json::write object \
            language [::json::write string $language] \
            code [::json::write string $code] \
            network_mode [::json::write string $network_mode] \
            ttl 60]
        return [api_request_json "/execute" "POST" $body $public_key $secret_key]
    }

    proc execute_async {language code {network_mode "zerotrust"} {public_key ""} {secret_key ""}} {
        set body [::json::write object \
            language [::json::write string $language] \
            code [::json::write string $code] \
            network_mode [::json::write string $network_mode] \
            ttl 300]
        return [api_request_json "/execute/async" "POST" $body $public_key $secret_key]
    }

    proc wait_job {job_id {public_key ""} {secret_key ""}} {
        set delays {300 450 700 900 650 1600 2000}
        for {set i 0} {$i < 120} {incr i} {
            set job [get_job $job_id $public_key $secret_key]
            set status [dict get $job status]
            if {$status eq "completed"} { return $job }
            if {$status eq "failed"} {
                set_error "Job failed"
                error "Job failed"
            }
            set delay [lindex $delays [expr {$i % 7}]]
            after $delay
        }
        set_error "Max polls exceeded"
        error "Max polls exceeded"
    }

    proc get_job {job_id {public_key ""} {secret_key ""}} {
        return [api_request_json "/jobs/$job_id" "GET" "" $public_key $secret_key]
    }

    proc cancel_job {job_id {public_key ""} {secret_key ""}} {
        return [api_request_json "/jobs/$job_id" "DELETE" "" $public_key $secret_key]
    }

    proc list_jobs {{public_key ""} {secret_key ""}} {
        return [api_request_json "/jobs" "GET" "" $public_key $secret_key]
    }

    proc get_languages {{public_key ""} {secret_key ""}} {
        variable LANGUAGES_CACHE_FILE LANGUAGES_CACHE_TTL

        # Check cache
        if {[file exists $LANGUAGES_CACHE_FILE]} {
            set mtime [file mtime $LANGUAGES_CACHE_FILE]
            set age [expr {[clock seconds] - $mtime}]
            if {$age < $LANGUAGES_CACHE_TTL} {
                if {[catch {open $LANGUAGES_CACHE_FILE r} fp] == 0} {
                    set content [read $fp]
                    close $fp
                    if {[catch {::json::json2dict $content} cache]} {
                        return [dict get $cache languages]
                    }
                }
            }
        }

        # Fetch from API
        set result [api_request_json "/languages" "GET" "" $public_key $secret_key]

        # Save to cache
        file mkdir [file dirname $LANGUAGES_CACHE_FILE]
        set cache_json [::json::write object \
            languages [dict get $result languages] \
            timestamp [clock seconds]]
        set fp [open $LANGUAGES_CACHE_FILE w]
        puts -nonewline $fp $cache_json
        close $fp

        return [dict get $result languages]
    }

    # ============================================================================
    # Session Functions (9)
    # ============================================================================

    proc session_list {{public_key ""} {secret_key ""}} {
        return [api_request_json "/sessions" "GET" "" $public_key $secret_key]
    }

    proc session_get {session_id {public_key ""} {secret_key ""}} {
        return [api_request_json "/sessions/$session_id" "GET" "" $public_key $secret_key]
    }

    proc session_create {{shell "bash"} {network ""} {vcpu ""} {public_key ""} {secret_key ""}} {
        set body [::json::write object shell [::json::write string $shell]]
        if {$network ne ""} {
            set body [string trimright $body "\}"]
            append body ",\"network\":\"$network\"\}"
        }
        if {$vcpu ne ""} {
            set body [string trimright $body "\}"]
            append body ",\"vcpu\":$vcpu\}"
        }
        return [api_request_json "/sessions" "POST" $body $public_key $secret_key]
    }

    proc session_destroy {session_id {public_key ""} {secret_key ""}} {
        return [api_request_json "/sessions/$session_id" "DELETE" "" $public_key $secret_key]
    }

    proc session_freeze {session_id {public_key ""} {secret_key ""}} {
        return [api_request_json "/sessions/$session_id/freeze" "POST" "\{\}" $public_key $secret_key]
    }

    proc session_unfreeze {session_id {public_key ""} {secret_key ""}} {
        return [api_request_json "/sessions/$session_id/unfreeze" "POST" "\{\}" $public_key $secret_key]
    }

    proc session_boost {session_id {vcpu 2} {public_key ""} {secret_key ""}} {
        set body "\{\"vcpu\":$vcpu\}"
        return [api_request_json "/sessions/$session_id/boost" "POST" $body $public_key $secret_key]
    }

    proc session_unboost {session_id {public_key ""} {secret_key ""}} {
        return [api_request_json "/sessions/$session_id/unboost" "POST" "\{\}" $public_key $secret_key]
    }

    proc session_execute {session_id command {public_key ""} {secret_key ""}} {
        set body [::json::write object command [::json::write string $command]]
        return [api_request_json "/sessions/$session_id/execute" "POST" $body $public_key $secret_key]
    }

    # ============================================================================
    # Service Functions (17)
    # ============================================================================

    proc service_list {{public_key ""} {secret_key ""}} {
        return [api_request_json "/services" "GET" "" $public_key $secret_key]
    }

    proc service_get {service_id {public_key ""} {secret_key ""}} {
        return [api_request_json "/services/$service_id" "GET" "" $public_key $secret_key]
    }

    proc service_create {name {ports ""} {bootstrap ""} {public_key ""} {secret_key ""}} {
        set body [::json::write object name [::json::write string $name]]
        if {$ports ne ""} {
            set body [string trimright $body "\}"]
            append body ",\"ports\":\[$ports\]\}"
        }
        if {$bootstrap ne ""} {
            set body [string trimright $body "\}"]
            append body ",\"bootstrap\":\"[string map {\" \\\" \\ \\\\} $bootstrap]\"\}"
        }
        return [api_request_json "/services" "POST" $body $public_key $secret_key]
    }

    proc service_destroy {service_id {public_key ""} {secret_key ""}} {
        return [api_request_with_sudo "/services/$service_id" "DELETE" "" $public_key $secret_key]
    }

    proc service_freeze {service_id {public_key ""} {secret_key ""}} {
        return [api_request_json "/services/$service_id/freeze" "POST" "\{\}" $public_key $secret_key]
    }

    proc service_unfreeze {service_id {public_key ""} {secret_key ""}} {
        return [api_request_json "/services/$service_id/unfreeze" "POST" "\{\}" $public_key $secret_key]
    }

    proc service_lock {service_id {public_key ""} {secret_key ""}} {
        return [api_request_json "/services/$service_id/lock" "POST" "\{\}" $public_key $secret_key]
    }

    proc service_unlock {service_id {public_key ""} {secret_key ""}} {
        return [api_request_with_sudo "/services/$service_id/unlock" "POST" "\{\}" $public_key $secret_key]
    }

    proc service_set_unfreeze_on_demand {service_id enabled {public_key ""} {secret_key ""}} {
        set body "\{\"unfreeze_on_demand\":$enabled\}"
        return [api_request_json "/services/$service_id" "PATCH" $body $public_key $secret_key]
    }

    proc service_redeploy {service_id {bootstrap ""} {public_key ""} {secret_key ""}} {
        set body "\{\}"
        if {$bootstrap ne ""} {
            set body [::json::write object bootstrap [::json::write string $bootstrap]]
        }
        return [api_request_json "/services/$service_id/redeploy" "POST" $body $public_key $secret_key]
    }

    proc service_logs {service_id {lines ""} {public_key ""} {secret_key ""}} {
        set endpoint "/services/$service_id/logs"
        if {$lines ne ""} {
            append endpoint "?lines=$lines"
        }
        return [api_request_json $endpoint "GET" "" $public_key $secret_key]
    }

    proc service_execute {service_id command {public_key ""} {secret_key ""}} {
        set body [::json::write object command [::json::write string $command]]
        return [api_request_json "/services/$service_id/execute" "POST" $body $public_key $secret_key]
    }

    proc service_env_get {service_id {public_key ""} {secret_key ""}} {
        return [api_request_json "/services/$service_id/env" "GET" "" $public_key $secret_key]
    }

    proc service_env_set {service_id env_content {public_key ""} {secret_key ""}} {
        lassign [api_request "/services/$service_id/env" "PUT" $env_content $public_key $secret_key {} "text/plain"] ncode response
        return [expr {$ncode >= 200 && $ncode < 300}]
    }

    proc service_env_delete {service_id {public_key ""} {secret_key ""}} {
        return [api_request_json "/services/$service_id/env" "DELETE" "" $public_key $secret_key]
    }

    proc service_env_export {service_id {public_key ""} {secret_key ""}} {
        return [api_request_json "/services/$service_id/env/export" "POST" "\{\}" $public_key $secret_key]
    }

    proc service_resize {service_id vcpu {public_key ""} {secret_key ""}} {
        set body "\{\"vcpu\":$vcpu\}"
        return [api_request_json "/services/$service_id" "PATCH" $body $public_key $secret_key]
    }

    # ============================================================================
    # Snapshot Functions (9)
    # ============================================================================

    proc snapshot_list {{public_key ""} {secret_key ""}} {
        return [api_request_json "/snapshots" "GET" "" $public_key $secret_key]
    }

    proc snapshot_get {snapshot_id {public_key ""} {secret_key ""}} {
        return [api_request_json "/snapshots/$snapshot_id" "GET" "" $public_key $secret_key]
    }

    proc snapshot_session {session_id {name ""} {hot false} {public_key ""} {secret_key ""}} {
        set body "\{\}"
        if {$name ne "" || $hot} {
            set parts [list]
            if {$name ne ""} { lappend parts "\"name\":\"$name\"" }
            if {$hot} { lappend parts "\"hot\":true" }
            set body "\{[join $parts ","]\}"
        }
        return [api_request_json "/sessions/$session_id/snapshot" "POST" $body $public_key $secret_key]
    }

    proc snapshot_service {service_id {name ""} {hot false} {public_key ""} {secret_key ""}} {
        set body "\{\}"
        if {$name ne "" || $hot} {
            set parts [list]
            if {$name ne ""} { lappend parts "\"name\":\"$name\"" }
            if {$hot} { lappend parts "\"hot\":true" }
            set body "\{[join $parts ","]\}"
        }
        return [api_request_json "/services/$service_id/snapshot" "POST" $body $public_key $secret_key]
    }

    proc snapshot_restore {snapshot_id {public_key ""} {secret_key ""}} {
        return [api_request_json "/snapshots/$snapshot_id/restore" "POST" "\{\}" $public_key $secret_key]
    }

    proc snapshot_delete {snapshot_id {public_key ""} {secret_key ""}} {
        return [api_request_with_sudo "/snapshots/$snapshot_id" "DELETE" "" $public_key $secret_key]
    }

    proc snapshot_lock {snapshot_id {public_key ""} {secret_key ""}} {
        return [api_request_json "/snapshots/$snapshot_id/lock" "POST" "\{\}" $public_key $secret_key]
    }

    proc snapshot_unlock {snapshot_id {public_key ""} {secret_key ""}} {
        return [api_request_with_sudo "/snapshots/$snapshot_id/unlock" "POST" "\{\}" $public_key $secret_key]
    }

    proc snapshot_clone {snapshot_id {clone_type "session"} {name ""} {public_key ""} {secret_key ""}} {
        set parts [list "\"clone_type\":\"$clone_type\""]
        if {$name ne ""} { lappend parts "\"name\":\"$name\"" }
        set body "\{[join $parts ","]\}"
        return [api_request_json "/snapshots/$snapshot_id/clone" "POST" $body $public_key $secret_key]
    }

    # ============================================================================
    # Image Functions (13)
    # ============================================================================

    proc image_list {{filter ""} {public_key ""} {secret_key ""}} {
        set endpoint "/images"
        if {$filter ne ""} {
            append endpoint "?filter=$filter"
        }
        return [api_request_json $endpoint "GET" "" $public_key $secret_key]
    }

    proc image_get {image_id {public_key ""} {secret_key ""}} {
        return [api_request_json "/images/$image_id" "GET" "" $public_key $secret_key]
    }

    proc image_publish {source_type source_id {name ""} {public_key ""} {secret_key ""}} {
        set parts [list "\"source_type\":\"$source_type\"" "\"source_id\":\"$source_id\""]
        if {$name ne ""} { lappend parts "\"name\":\"$name\"" }
        set body "\{[join $parts ","]\}"
        return [api_request_json "/images/publish" "POST" $body $public_key $secret_key]
    }

    proc image_delete {image_id {public_key ""} {secret_key ""}} {
        return [api_request_with_sudo "/images/$image_id" "DELETE" "" $public_key $secret_key]
    }

    proc image_lock {image_id {public_key ""} {secret_key ""}} {
        return [api_request_json "/images/$image_id/lock" "POST" "\{\}" $public_key $secret_key]
    }

    proc image_unlock {image_id {public_key ""} {secret_key ""}} {
        return [api_request_with_sudo "/images/$image_id/unlock" "POST" "\{\}" $public_key $secret_key]
    }

    proc image_set_visibility {image_id visibility {public_key ""} {secret_key ""}} {
        set body "\{\"visibility\":\"$visibility\"\}"
        return [api_request_json "/images/$image_id/visibility" "POST" $body $public_key $secret_key]
    }

    proc image_grant_access {image_id trusted_key {public_key ""} {secret_key ""}} {
        set body "\{\"api_key\":\"$trusted_key\"\}"
        return [api_request_json "/images/$image_id/access" "POST" $body $public_key $secret_key]
    }

    proc image_revoke_access {image_id trusted_key {public_key ""} {secret_key ""}} {
        return [api_request_json "/images/$image_id/access/$trusted_key" "DELETE" "" $public_key $secret_key]
    }

    proc image_list_trusted {image_id {public_key ""} {secret_key ""}} {
        return [api_request_json "/images/$image_id/access" "GET" "" $public_key $secret_key]
    }

    proc image_transfer {image_id to_key {public_key ""} {secret_key ""}} {
        set body "\{\"to_api_key\":\"$to_key\"\}"
        return [api_request_json "/images/$image_id/transfer" "POST" $body $public_key $secret_key]
    }

    proc image_spawn {image_id {name ""} {ports ""} {public_key ""} {secret_key ""}} {
        set parts [list]
        if {$name ne ""} { lappend parts "\"name\":\"$name\"" }
        if {$ports ne ""} { lappend parts "\"ports\":\[$ports\]" }
        set body "\{[join $parts ","]\}"
        return [api_request_json "/images/$image_id/spawn" "POST" $body $public_key $secret_key]
    }

    proc image_clone {image_id {name ""} {public_key ""} {secret_key ""}} {
        set body "\{\}"
        if {$name ne ""} {
            set body "\{\"name\":\"$name\"\}"
        }
        return [api_request_json "/images/$image_id/clone" "POST" $body $public_key $secret_key]
    }

    # ============================================================================
    # PaaS Logs Functions (2)
    # ============================================================================

    proc logs_fetch {{source "all"} {lines 100} {since "1h"} {grep ""} {public_key ""} {secret_key ""}} {
        set parts [list "\"source\":\"$source\"" "\"lines\":$lines" "\"since\":\"$since\""]
        if {$grep ne ""} { lappend parts "\"grep\":\"$grep\"" }
        set body "\{[join $parts ","]\}"
        return [api_request_json "/paas/logs" "POST" $body $public_key $secret_key]
    }

    proc logs_stream {args} {
        set_error "logs_stream requires async support"
        error "logs_stream requires async support"
    }

    # ============================================================================
    # Key Validation
    # ============================================================================

    proc validate_keys {{public_key ""} {secret_key ""}} {
        variable PORTAL_BASE

        lassign [get_credentials $public_key $secret_key] pk sk

        set headers [list Authorization "Bearer $pk" Content-Type "application/json"]

        if {$sk ne ""} {
            set timestamp [clock seconds]
            set sig_input "${timestamp}:POST:/keys/validate:"
            set signature [hmac_sign $sk $sig_input]
            lappend headers X-Timestamp $timestamp
            lappend headers X-Signature $signature
        }

        set token [::http::geturl "$PORTAL_BASE/keys/validate" -method POST -headers $headers -timeout 30000]
        set ncode [::http::ncode $token]
        set response [::http::data $token]
        ::http::cleanup $token

        if {$ncode == 200} {
            return [::json::json2dict $response]
        }
        set_error "Key validation failed ($ncode)"
        error "Key validation failed"
    }

    # ============================================================================
    # CLI Commands
    # ============================================================================

    proc cmd_languages {args} {
        variable BLUE RESET
        set json_output 0
        foreach arg $args {
            if {$arg eq "--json"} { set json_output 1 }
        }

        set langs [get_languages]

        if {$json_output} {
            puts $langs
        } else {
            foreach lang $langs {
                puts $lang
            }
        }
    }

    proc cmd_key {args} {
        variable GREEN RED YELLOW PORTAL_BASE RESET

        set extend 0
        foreach arg $args {
            if {$arg eq "--extend"} { set extend 1 }
        }

        set result [validate_keys]
        set pk [dict get $result public_key]

        if {$extend && $pk ne ""} {
            set url "$PORTAL_BASE/keys/extend?pk=$pk"
            puts "${YELLOW}Opening browser to extend key...${RESET}"
            catch {exec xdg-open $url &}
            return
        }

        if {[dict exists $result expired] && [dict get $result expired]} {
            puts "${RED}Expired${RESET}"
            puts "Public Key: $pk"
            puts "Tier: [dict get $result tier]"
            puts "${YELLOW}To renew: Visit $PORTAL_BASE/keys/extend${RESET}"
            exit 1
        }

        puts "${GREEN}Valid${RESET}"
        puts "Public Key: $pk"
        puts "Tier: [dict get $result tier]"
        puts "Status: [dict get $result status]"
        if {[dict exists $result expires_at]} {
            puts "Expires: [dict get $result expires_at]"
        }
        if {[dict exists $result time_remaining]} {
            puts "Time Remaining: [dict get $result time_remaining]"
        }
    }

    proc cmd_session {args} {
        variable GREEN RESET
        set action ""
        set target ""

        for {set i 0} {$i < [llength $args]} {incr i} {
            set arg [lindex $args $i]
            switch -exact -- $arg {
                --list - -l { set action "list" }
                --info { set action "info"; incr i; set target [lindex $args $i] }
                --kill { set action "kill"; incr i; set target [lindex $args $i] }
                --freeze { set action "freeze"; incr i; set target [lindex $args $i] }
                --unfreeze { set action "unfreeze"; incr i; set target [lindex $args $i] }
            }
        }

        switch -exact -- $action {
            list {
                set result [session_list]
                if {[dict exists $result sessions]} {
                    foreach s [dict get $result sessions] {
                        puts "[dict get $s id]\t[dict get $s shell]\t[dict get $s status]\t[dict get $s created_at]"
                    }
                }
            }
            info { puts [session_get $target] }
            kill {
                session_destroy $target
                puts "${GREEN}Session terminated: $target${RESET}"
            }
            freeze {
                session_freeze $target
                puts "${GREEN}Session frozen: $target${RESET}"
            }
            unfreeze {
                session_unfreeze $target
                puts "${GREEN}Session unfreezing: $target${RESET}"
            }
            default {
                puts stderr "Usage: un.tcl session --list|--info ID|--kill ID|--freeze ID|--unfreeze ID"
                exit 1
            }
        }
    }

    proc cmd_service {args} {
        variable GREEN RESET
        set action ""
        set target ""
        set name ""
        set ports ""

        for {set i 0} {$i < [llength $args]} {incr i} {
            set arg [lindex $args $i]
            switch -exact -- $arg {
                --list - -l { set action "list" }
                --info { set action "info"; incr i; set target [lindex $args $i] }
                --destroy { set action "destroy"; incr i; set target [lindex $args $i] }
                --freeze { set action "freeze"; incr i; set target [lindex $args $i] }
                --unfreeze { set action "unfreeze"; incr i; set target [lindex $args $i] }
                --lock { set action "lock"; incr i; set target [lindex $args $i] }
                --unlock { set action "unlock"; incr i; set target [lindex $args $i] }
                --logs { set action "logs"; incr i; set target [lindex $args $i] }
                --name { incr i; set name [lindex $args $i] }
                --ports { incr i; set ports [lindex $args $i] }
            }
        }

        switch -exact -- $action {
            list {
                set result [service_list]
                if {[dict exists $result services]} {
                    foreach s [dict get $result services] {
                        puts "[dict get $s id]\t[dict get $s name]\t[dict get $s status]"
                    }
                }
            }
            info { puts [service_get $target] }
            destroy {
                service_destroy $target
                puts "${GREEN}Service destroyed: $target${RESET}"
            }
            freeze {
                service_freeze $target
                puts "${GREEN}Service frozen: $target${RESET}"
            }
            unfreeze {
                service_unfreeze $target
                puts "${GREEN}Service unfreezing: $target${RESET}"
            }
            lock {
                service_lock $target
                puts "${GREEN}Service locked: $target${RESET}"
            }
            unlock {
                service_unlock $target
                puts "${GREEN}Service unlocked: $target${RESET}"
            }
            logs {
                set result [service_logs $target]
                if {[dict exists $result logs]} {
                    puts [dict get $result logs]
                }
            }
            default {
                if {$name ne ""} {
                    set result [service_create $name $ports ""]
                    puts "${GREEN}Service created${RESET}"
                    puts $result
                } else {
                    puts stderr "Usage: un.tcl service --list|--info ID|--destroy ID|--name NAME"
                    exit 1
                }
            }
        }
    }

    proc cmd_snapshot {args} {
        variable GREEN RESET
        set action ""
        set target ""

        for {set i 0} {$i < [llength $args]} {incr i} {
            set arg [lindex $args $i]
            switch -exact -- $arg {
                --list - -l { set action "list" }
                --info { set action "info"; incr i; set target [lindex $args $i] }
                --delete { set action "delete"; incr i; set target [lindex $args $i] }
                --restore { set action "restore"; incr i; set target [lindex $args $i] }
                --lock { set action "lock"; incr i; set target [lindex $args $i] }
                --unlock { set action "unlock"; incr i; set target [lindex $args $i] }
            }
        }

        switch -exact -- $action {
            list {
                set result [snapshot_list]
                if {[dict exists $result snapshots]} {
                    foreach s [dict get $result snapshots] {
                        puts "[dict get $s id]\t[dict get $s name]\t[dict get $s type]"
                    }
                }
            }
            info { puts [snapshot_get $target] }
            delete {
                snapshot_delete $target
                puts "${GREEN}Snapshot deleted: $target${RESET}"
            }
            restore {
                snapshot_restore $target
                puts "${GREEN}Snapshot restored${RESET}"
            }
            lock {
                snapshot_lock $target
                puts "${GREEN}Snapshot locked: $target${RESET}"
            }
            unlock {
                snapshot_unlock $target
                puts "${GREEN}Snapshot unlocked: $target${RESET}"
            }
            default {
                puts stderr "Usage: un.tcl snapshot --list|--info ID|--delete ID|--restore ID"
                exit 1
            }
        }
    }

    proc cmd_image {args} {
        variable GREEN RESET
        set action ""
        set target ""
        set source_type ""
        set visibility_mode ""
        set name ""
        set ports ""

        for {set i 0} {$i < [llength $args]} {incr i} {
            set arg [lindex $args $i]
            switch -exact -- $arg {
                --list - -l { set action "list" }
                --info { set action "info"; incr i; set target [lindex $args $i] }
                --delete { set action "delete"; incr i; set target [lindex $args $i] }
                --lock { set action "lock"; incr i; set target [lindex $args $i] }
                --unlock { set action "unlock"; incr i; set target [lindex $args $i] }
                --publish { set action "publish"; incr i; set target [lindex $args $i] }
                --source-type { incr i; set source_type [lindex $args $i] }
                --visibility {
                    set action "visibility"
                    incr i; set target [lindex $args $i]
                    incr i; set visibility_mode [lindex $args $i]
                }
                --spawn { set action "spawn"; incr i; set target [lindex $args $i] }
                --clone { set action "clone"; incr i; set target [lindex $args $i] }
                --name { incr i; set name [lindex $args $i] }
                --ports { incr i; set ports [lindex $args $i] }
            }
        }

        switch -exact -- $action {
            list {
                set result [image_list]
                if {[dict exists $result images]} {
                    foreach img [dict get $result images] {
                        set img_name "-"
                        if {[dict exists $img name]} { set img_name [dict get $img name] }
                        puts "[dict get $img id]\t$img_name\t[dict get $img visibility]"
                    }
                }
            }
            info { puts [image_get $target] }
            delete {
                image_delete $target
                puts "${GREEN}Image deleted: $target${RESET}"
            }
            lock {
                image_lock $target
                puts "${GREEN}Image locked: $target${RESET}"
            }
            unlock {
                image_unlock $target
                puts "${GREEN}Image unlocked: $target${RESET}"
            }
            publish {
                if {$source_type eq ""} {
                    puts stderr "Error: --source-type required"
                    exit 1
                }
                set result [image_publish $source_type $target $name]
                puts "${GREEN}Image published${RESET}"
                puts $result
            }
            visibility {
                image_set_visibility $target $visibility_mode
                puts "${GREEN}Visibility set to $visibility_mode${RESET}"
            }
            spawn {
                set result [image_spawn $target $name $ports]
                puts "${GREEN}Service spawned from image${RESET}"
                puts $result
            }
            clone {
                set result [image_clone $target $name]
                puts "${GREEN}Image cloned${RESET}"
                puts $result
            }
            default {
                puts stderr "Usage: un.tcl image --list|--info ID|--delete ID|--publish ID|--spawn ID|--clone ID"
                exit 1
            }
        }
    }

    proc run_file {file} {
        variable BLUE RED RESET

        if {![file exists $file]} {
            puts stderr "${RED}Error: File not found: $file${RESET}"
            exit 1
        }

        set fp [open $file r]
        set code [read $fp]
        close $fp

        set lang [detect_language $file]
        if {$lang eq ""} {
            puts stderr "${RED}Error: Cannot detect language${RESET}"
            exit 1
        }

        set result [execute $lang $code]

        if {[dict exists $result stdout]} {
            puts -nonewline "${BLUE}[dict get $result stdout]${RESET}"
        }
        if {[dict exists $result stderr]} {
            puts -nonewline stderr "${RED}[dict get $result stderr]${RESET}"
        }

        set exit_code 0
        if {[dict exists $result exit_code]} {
            set exit_code [dict get $result exit_code]
        }
        exit $exit_code
    }

    proc show_help {} {
        puts {Unsandbox CLI - Execute code in secure sandboxes

Usage:
  tclsh un.tcl [options] <source_file>
  tclsh un.tcl -s <language> '<code>'
  tclsh un.tcl session [options]
  tclsh un.tcl service [options]
  tclsh un.tcl snapshot [options]
  tclsh un.tcl image [options]
  tclsh un.tcl languages [--json]
  tclsh un.tcl key [--extend]

Commands:
  languages     List available programming languages
  key           Validate API key
  session       Manage interactive sessions
  service       Manage persistent services
  snapshot      Manage snapshots
  image         Manage images

Environment:
  UNSANDBOX_PUBLIC_KEY  API public key
  UNSANDBOX_SECRET_KEY  API secret key}
    }

    proc main {argv} {
        variable BLUE RED RESET

        if {[llength $argv] == 0} {
            show_help
            exit 1
        }

        set first_arg [lindex $argv 0]

        switch -exact -- $first_arg {
            languages { cmd_languages {*}[lrange $argv 1 end] }
            key { cmd_key {*}[lrange $argv 1 end] }
            session { cmd_session {*}[lrange $argv 1 end] }
            service { cmd_service {*}[lrange $argv 1 end] }
            snapshot { cmd_snapshot {*}[lrange $argv 1 end] }
            image { cmd_image {*}[lrange $argv 1 end] }
            --help - -h { show_help }
            -s {
                set lang [lindex $argv 1]
                set code [lindex $argv 2]
                if {$lang eq "" || $code eq ""} {
                    puts stderr "${RED}Error: -s requires language and code${RESET}"
                    exit 1
                }
                set result [execute $lang $code]
                if {[dict exists $result stdout]} {
                    puts -nonewline [dict get $result stdout]
                }
                if {[dict exists $result stderr]} {
                    puts -nonewline stderr [dict get $result stderr]
                }
                set exit_code 0
                if {[dict exists $result exit_code]} {
                    set exit_code [dict get $result exit_code]
                }
                exit $exit_code
            }
            default {
                run_file $first_arg
            }
        }
    }
}

# CLI entry point
if {[info script] eq $argv0} {
    Un::main $argv
}
