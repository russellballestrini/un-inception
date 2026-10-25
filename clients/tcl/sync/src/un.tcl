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
    variable API_BASE [string trimright [expr {[info exists ::env(UNSANDBOX_API_URL)] && $::env(UNSANDBOX_API_URL) ne "" ? $::env(UNSANDBOX_API_URL) : "https://api.unsandbox.com"}] /]
    variable PORTAL_BASE [string trimright [expr {[info exists ::env(UNSANDBOX_PORTAL_URL)] && $::env(UNSANDBOX_PORTAL_URL) ne "" ? $::env(UNSANDBOX_PORTAL_URL) : "https://unsandbox.com"}] /]
    variable LANGUAGES_CACHE_TTL 3600
    variable LANGUAGES_CACHE_FILE [file join $::env(HOME) ".unsandbox" "languages.json"]
    variable LAST_ERROR ""
    variable ACCOUNT_INDEX -1

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

    proc load_accounts_csv {path {row 0}} {
        if {![file exists $path]} { return "" }
        if {[catch {open $path r} fp]} { return "" }
        set n 0
        set result ""
        while {[gets $fp line] >= 0} {
            if {[string index $line 0] eq "#" || [string trim $line] eq ""} { continue }
            if {$n == $row} { set result $line; break }
            incr n
        }
        close $fp
        return $result
    }

    proc default_account_row {} {
        if {[info exists ::env(UNSANDBOX_ACCOUNT)] && [string is integer -strict $::env(UNSANDBOX_ACCOUNT)]} {
            return $::env(UNSANDBOX_ACCOUNT)
        }
        return 0
    }

    # The accounts.csv our credentials came from, or "" (arguments / env vars).
    # Mirrors get_credentials' tiers.
    proc credentials_csv_path {{public_key ""} {secret_key ""}} {
        variable ACCOUNT_INDEX
        if {$public_key ne "" && $secret_key ne ""} { return "" }
        set row $ACCOUNT_INDEX
        if {$row < 0} {
            if {([info exists ::env(UNSANDBOX_PUBLIC_KEY)] && [info exists ::env(UNSANDBOX_SECRET_KEY)]) || [info exists ::env(UNSANDBOX_API_KEY)]} {
                return ""
            }
            set row [default_account_row]
        }
        foreach path [list [file join $::env(HOME) ".unsandbox" "accounts.csv"] "./accounts.csv"] {
            if {[load_accounts_csv $path $row] ne ""} { return $path }
        }
        return ""
    }

    # Replace our secret (2nd field) on the line whose 1st field equals pk.
    # Every other line, field, comment and our order stay as they are.
    # Temp file 0600 in the same directory, then rename.
    proc rewrite_accounts_csv {path pk new_secret} {
        if {[catch {open $path r} fp]} { return 0 }
        fconfigure $fp -translation binary
        set lines [split [read $fp] "\n"]
        close $fp
        set changed 0
        for {set i 0} {$i < [llength $lines]} {incr i} {
            set line [lindex $lines $i]
            if {[string index [string trimleft $line] 0] eq "#"} { continue }
            set fields [split $line ","]
            if {[llength $fields] >= 2 && [string trim [lindex $fields 0]] eq $pk} {
                set cr [expr {[llength $fields] == 2 && [string index $line end] eq "\r" ? "\r" : ""}]
                lset fields 1 "$new_secret$cr"
                lset lines $i [join $fields ","]
                set changed 1
                break
            }
        }
        if {!$changed} { return 0 }
        set tmp [file join [file dirname $path] ".[file tail $path].[pid].tmp"]
        if {[catch {
            set out [open $tmp {WRONLY CREAT TRUNC} 0600]
            fconfigure $out -translation binary
            puts -nonewline $out [join $lines "\n"]
            close $out
            file attributes $tmp -permissions 0600
            file rename -force $tmp $path
        }]} {
            catch {file delete $tmp}
            return 0
        }
        return 1
    }

    # POST /keys/self/rotate-secret, body {}, signed. Returns {ncode response}.
    proc rotate_secret_request {pk sk {otp ""} {challenge ""}} {
        variable API_BASE
        set endpoint "/keys/self/rotate-secret"
        set body "{}"
        set timestamp [clock seconds]
        set headers [list Authorization "Bearer $pk" \
            X-Timestamp $timestamp \
            X-Signature [hmac_sign $sk "${timestamp}:POST:${endpoint}:${body}"]]
        if {$otp ne ""} {
            lappend headers X-Sudo-OTP $otp X-Sudo-Challenge $challenge
        }
        if {[catch {
            set token [::http::geturl "${API_BASE}${endpoint}" -method POST -headers $headers \
                -type application/json -query $body -timeout 120000]
        } err]} {
            return [list 0 "{\"error\": \"[string map {\\ \\\\ \" \\\"} $err]\"}"]
        }
        set ncode [::http::ncode $token]
        set response [::http::data $token]
        ::http::cleanup $token
        return [list $ncode $response]
    }

    proc json_field {response key} {
        if {[catch {::json::json2dict $response} d] || [catch {dict exists $d $key} has] || !$has} {
            return ""
        }
        return [dict get $d $key]
    }

    # un key --rotate [--otp CODE]: rotate this key's secret (see CLI_SPEC.md).
    proc cmd_key_rotate {otp_given otp} {
        variable RED; variable RESET
        if {[catch {get_credentials} creds]} {
            puts stderr "${RED}Error: no credentials found${RESET}"
            exit 3
        }
        lassign $creds pk sk
        if {$sk eq ""} {
            puts stderr "${RED}Error: no credentials found${RESET}"
            exit 3
        }
        set dir [file join $::env(HOME) ".unsandbox"]
        set challenge_file [file join $dir "rotate-[string range [::sha2::sha256 -hex $pk] 0 15].challenge"]

        if {$otp_given} {
            if {[catch {open $challenge_file r} fp]} {
                puts stderr "${RED}Error: no rotation in progress; run: un key --rotate${RESET}"
                exit 2
            }
            set challenge [string trim [read $fp]]
            close $fp
            lassign [rotate_secret_request $pk $sk [string trim $otp] $challenge] ncode response
            catch {file delete $challenge_file}
        } else {
            lassign [rotate_secret_request $pk $sk] ncode response
            if {$ncode == 428} {
                set challenge [json_field $response challenge_id]
                if {[catch {exec test -t 0 <@ stdin}] == 0} {
                    puts stderr "One-time code emailed to the key owner."
                    puts -nonewline stderr "Code: "
                    flush stderr
                    if {[gets stdin code] < 0} { set code "" }
                    set code [string trim $code]
                    if {$code eq ""} {
                        puts stderr "${RED}Error: cancelled${RESET}"
                        exit 1
                    }
                    lassign [rotate_secret_request $pk $sk $code $challenge] ncode response
                } else {
                    if {![file isdirectory $dir]} { file mkdir $dir }
                    file attributes $dir -permissions 0700
                    set fp [open $challenge_file {WRONLY CREAT TRUNC} 0600]
                    puts -nonewline $fp $challenge
                    close $fp
                    file attributes $challenge_file -permissions 0600
                    puts "One-time code emailed to the key owner. Finish with: un key --rotate --otp CODE"
                    return
                }
            }
        }

        if {$ncode != 200} {
            if {$ncode == 403 && [json_field $response error] eq "email_required"} {
                puts stderr "${RED}Error: this key has no email; add one or use POST /keys/recover${RESET}"
                exit 3
            }
            if {$ncode == 401} {
                puts stderr "${RED}Error: authentication failed${RESET}"
                exit 3
            }
            set msg [json_field $response message]
            if {$msg eq ""} { set msg [json_field $response error] }
            if {$msg eq ""} { set msg "HTTP $ncode" }
            puts stderr "${RED}Error: ${msg}${RESET}"
            exit 4
        }
        set new_secret [json_field $response secret]
        if {$new_secret eq ""} {
            puts stderr "${RED}Error: API returned no secret${RESET}"
            exit 4
        }

        set csv_path [credentials_csv_path]
        if {$csv_path ne "" && [rewrite_accounts_csv $csv_path $pk $new_secret]} {
            puts "Secret rotated. Updated $csv_path (public key $pk)."
        } else {
            puts "New secret (shown once, store it now): $new_secret"
            puts "Warning: UNSANDBOX_SECRET_KEY / -k still hold the old, now invalid, secret."
        }
    }

    proc get_credentials {{public_key ""} {secret_key ""} {account_index -1}} {
        variable ACCOUNT_INDEX

        # Tier 1: Arguments
        if {$public_key ne "" && $secret_key ne ""} {
            return [list $public_key $secret_key]
        }

        # Tier 2: --account N flag → bypass env vars, load CSV row N directly
        set ai $account_index
        if {$ai < 0} { set ai $ACCOUNT_INDEX }
        if {$ai >= 0} {
            set creds [load_accounts_csv [file join $::env(HOME) ".unsandbox" "accounts.csv"] $ai]
            if {$creds eq ""} { set creds [load_accounts_csv "./accounts.csv" $ai] }
            if {$creds ne ""} {
                set parts [split $creds ","]
                return [list [lindex $parts 0] [lindex $parts 1]]
            }
            set_error "Account index $ai not found in accounts.csv"
            error "Account index $ai not found in accounts.csv"
        }

        # Tier 3: Environment
        if {[info exists ::env(UNSANDBOX_PUBLIC_KEY)] && [info exists ::env(UNSANDBOX_SECRET_KEY)]} {
            return [list $::env(UNSANDBOX_PUBLIC_KEY) $::env(UNSANDBOX_SECRET_KEY)]
        }

        # Legacy fallback
        if {[info exists ::env(UNSANDBOX_API_KEY)]} {
            return [list $::env(UNSANDBOX_API_KEY) ""]
        }

        # Tier 4: Home directory (row UNSANDBOX_ACCOUNT, default 0)
        set row [default_account_row]
        set creds [load_accounts_csv [file join $::env(HOME) ".unsandbox" "accounts.csv"] $row]
        if {$creds ne ""} {
            set parts [split $creds ","]
            return [list [lindex $parts 0] [lindex $parts 1]]
        }

        # Tier 5: Local directory
        set creds [load_accounts_csv "./accounts.csv" $row]
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
        variable YELLOW; variable RED; variable GREEN; variable RESET

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
        variable LANGUAGES_CACHE_FILE; variable LANGUAGES_CACHE_TTL

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

    # Platform logs live on the portal: GET $PORTAL_BASE/logs/{source}[/stream]
    # (partner keys with log access). HMAC covers the path only: "ts:GET:/logs/api:".

    proc uri_escape {v} {
        set out ""
        foreach b [split [encoding convertto utf-8 $v] ""] {
            if {[string match {[A-Za-z0-9._~-]} $b]} {
                append out $b
            } else {
                binary scan $b cu n
                append out [format "%%%02X" $n]
            }
        }
        return $out
    }

    proc logs_query {args} {
        set q {}
        foreach {k v} $args {
            if {$v ne ""} { lappend q "$k=[uri_escape $v]" }
        }
        if {[llength $q] == 0} { return "" }
        return "?[join $q &]"
    }

    proc portal_headers {path public_key secret_key} {
        lassign [get_credentials $public_key $secret_key] pk sk
        set headers [list Authorization "Bearer $pk"]
        if {$sk ne ""} {
            set timestamp [clock seconds]
            lappend headers X-Timestamp $timestamp X-Signature [hmac_sign $sk "${timestamp}:GET:${path}:"]
        }
        return $headers
    }

    # Raw portal fetch: returns {ncode body}; raises on network errors.
    proc logs_fetch_raw {source lines since grep level public_key secret_key} {
        variable PORTAL_BASE
        set path "/logs/$source"
        set headers [portal_headers $path $public_key $secret_key]
        set url "${PORTAL_BASE}${path}[logs_query lines $lines since $since grep $grep level $level]"
        set token [::http::geturl $url -headers $headers -timeout 120000 -binary 1]
        set status [::http::status $token]
        set ncode [::http::ncode $token]
        set body [encoding convertfrom utf-8 [::http::data $token]]
        ::http::cleanup $token
        if {$status ne "ok"} { error "request $status" }
        return [list $ncode $body]
    }

    # Library: fetch logs; returns the decoded dict (raises on non-200).
    proc logs_fetch {{source "all"} {lines 100} {since "5m"} {grep ""} {public_key ""} {secret_key ""} {level ""}} {
        lassign [logs_fetch_raw $source $lines $since $grep $level $public_key $secret_key] ncode body
        if {$ncode != 200} {
            set_error "API error ($ncode): $body"
            error "API error ($ncode): $body"
        }
        return [::json::json2dict $body]
    }

    proc print_log_row {src line} {
        if {$src ne ""} {
            puts [format "\033\[36m%-12s\033\[0m %s" $src $line]
        } else {
            puts $line
        }
        flush stdout
    }

    proc log_cursor_path {} { return [file join $::env(HOME) ".unsandbox" "paas_log_cursor"] }

    proc read_log_cursor {} {
        if {[catch {set fh [open [log_cursor_path] r]}]} { return "" }
        set ts [string trim [gets $fh]]
        close $fh
        if {![string is digit -strict $ts] || $ts <= 0} { return "" }
        set ago [expr {[clock seconds] - $ts}]
        if {$ago < 5} { set ago 5 }
        return "${ago}s"
    }

    proc write_log_cursor {} {
        set dir [file join $::env(HOME) ".unsandbox"]
        catch {
            if {![file isdirectory $dir]} { file mkdir $dir; file attributes $dir -permissions 0700 }
            set fh [open [log_cursor_path] w]
            puts $fh [clock seconds]
            close $fh
        }
    }

    # SSE body handler: reads what arrived, emits complete events.
    proc stream_handler {cb sock token} {
        variable stream_buf
        variable stream_last
        variable stream_seen
        set data [read $sock]
        set stream_seen [clock seconds]
        if {[::http::ncode $token] != 200} { return [string length $data] }
        append stream_buf $data
        set stream_buf [string map {"\r\n" "\n"} $stream_buf]
        while {[set i [string first "\n\n" $stream_buf]] >= 0} {
            set event [string range $stream_buf 0 [expr {$i - 1}]]
            set stream_buf [string range $stream_buf [expr {$i + 2}] end]
            foreach line [split [encoding convertfrom utf-8 $event] "\n"] {
                if {![string match "data: *" $line]} continue
                set payload [string range $line 6 end]
                if {$payload eq ":keepalive"} continue
                set stream_last [clock seconds]
                if {![catch {::json::json2dict $payload} ev] && [string index [string trimleft $payload] 0] eq "\{"} {
                    if {[dict exists $ev line]} {
                        set src [expr {[dict exists $ev source] ? [dict get $ev source] : ""}]
                        {*}$cb $src [dict get $ev line]
                    }
                } else {
                    {*}$cb "" $payload
                }
            }
        }
        return [string length $data]
    }

    proc stream_watchdog {token} {
        variable stream_seen
        variable stream_watch
        if {[clock seconds] - $stream_seen > 120} {
            ::http::reset $token timeout
            return
        }
        set stream_watch [after 5000 [list ::Un::stream_watchdog $token]]
    }

    # Library + CLI: stream /logs/{source}/stream (SSE), reconnecting on drops
    # (2 s backoff doubling to 30 s, since=<gap>s). Returns 0 on stop, 1 on error.
    proc logs_stream {{source "all"} {grep ""} {level ""} {public_key ""} {secret_key ""} {callback ::Un::print_log_row}} {
        variable PORTAL_BASE
        variable stream_buf
        variable stream_last 0
        variable stream_seen
        variable stream_watch
        set path "/logs/$source/stream"
        puts stderr "Streaming $source logs... (Ctrl+C to stop)"
        set delay 2; set reconnect 0
        while {1} {
            set since ""
            if {$reconnect && $stream_last > 0} {
                set gap [expr {[clock seconds] - $stream_last + 5}]
                if {$gap < 10} { set gap 10 }
                set since "${gap}s"
            }
            set url "${PORTAL_BASE}${path}[logs_query grep $grep level $level since $since]"
            set headers [portal_headers $path $public_key $secret_key]
            lappend headers Accept text/event-stream
            set stream_buf ""
            set stream_seen [clock seconds]
            set started [clock seconds]
            # HTTP/1.0 so the stream arrives unchunked, closed by the server.
            if {[catch {::http::geturl $url -headers $headers -protocol 1.0 -binary 1 \
                    -handler [list ::Un::stream_handler $callback] -command ::Un::stream_done} token]} {
                puts stderr "Error: Stream error: $token"
                return 1
            }
            set stream_watch [after 5000 [list ::Un::stream_watchdog $token]]
            ::http::wait $token
            after cancel $stream_watch
            set status [::http::status $token]
            set ncode [::http::ncode $token]
            ::http::cleanup $token
            if {[clock seconds] - $started > 30} { set delay 2 }
            if {$ncode == 401 || $ncode == 403} {
                puts stderr "Error: HTTP $ncode (auth failed)"
                return 1
            }
            if {$ncode eq "" && $status eq "error"} {
                puts stderr "Error: Stream error: connection failed"
                return 1
            }
            if {$ncode == 200 || ($ncode ne "" && $ncode >= 500) || $status in {eof timeout error}} {
                after [expr {$delay * 1000}]
                set delay [expr {min($delay * 2, 30)}]
                set reconnect 1
                continue
            }
            break
        }
        write_log_cursor
        puts stderr "\nStream stopped."
        return 0
    }

    proc stream_done {token} {}

    proc paas_logs_help {} {
        set p "tclsh un.tcl"
        puts stderr "Usage: $p paas logs \[options\]

Fetch or stream PaaS production logs.

Sources:
  --all              All sources (default)
  --api              API server logs
  --portal           Portal server logs
  --pool NODE        Pool node logs (e.g., cammy, ai)
  --source SOURCE    Source string (api, portal, pool/cammy, all)

Options:
  --lines N, -n N    Number of lines (default: 100, max: 10000)
  --since TIME       Time window: 1m, 5m, 15m, 1h, 6h, 1d (default: 5m)
  --grep PATTERN     Filter log lines by pattern
  --level LVL, -l    Min log level: debug, info, notice, warning, err, crit
  --follow, -f       Follow logs in real-time (Ctrl+C to stop)
  --json             Output raw JSON response
  -p KEY             Public key
  -k KEY             Secret key
  -h                 Show this help

Examples:
  $p paas logs                             # last 100 lines from all sources
  $p paas logs --api --lines 500           # last 500 API log lines
  $p paas logs --portal --grep error       # portal logs matching 'error'
  $p paas logs --pool cammy --since 1h     # cammy pool logs from last hour
  $p paas logs -l warning                  # warnings and above from all sources
  $p paas logs --follow                    # follow all logs in real-time
  $p paas logs --follow -l warning         # follow warnings+ in real-time

Requires a partner API key with log access enabled."
    }

    proc cmd_paas {args} {
        variable ACCOUNT_INDEX
        if {[llength $args] == 0} {
            puts stderr "Usage: tclsh un.tcl paas <command> \[options\]\n\nCommands:\n  logs    Fetch or stream production logs"
            puts stderr "\nRun 'tclsh un.tcl paas <command> -h' for help on a specific command."
            return 1
        }
        set sub [lindex $args 0]
        if {$sub ne "logs"} {
            puts stderr "Error: Unknown paas command '$sub'\nRun 'tclsh un.tcl paas' for available commands."
            return 1
        }
        set source all; set lines 100; set since ""; set since_explicit 0
        set grep ""; set level ""; set follow 0; set json 0; set help 0
        set pk ""; set sk ""
        set n [llength $args]
        for {set i 1} {$i < $n} {incr i} {
            set a [lindex $args $i]
            set has [expr {$i + 1 < $n}]
            set next [lindex $args [expr {$i + 1}]]
            if {$a eq "-p" && $has} { set pk $next; incr i
            } elseif {$a eq "-k" && $has} { set sk $next; incr i
            } elseif {$a eq "--account" && $has} { set ACCOUNT_INDEX $next; incr i
            } elseif {$a eq "--source" && $has} { set source $next; incr i
            } elseif {$a eq "--all"} { set source all
            } elseif {$a eq "--api"} { set source api
            } elseif {$a eq "--portal"} { set source portal
            } elseif {$a eq "--pool" && $has} { set source "pool/$next"; incr i
            } elseif {($a eq "--lines" || $a eq "-n") && $has} {
                incr i
                if {![regexp {^\s*([-+]?[0-9]+)} $next -> v]} { set v 0 }
                scan $v %d v
                set lines [expr {max(1, min(10000, $v))}]
            } elseif {$a eq "--since" && $has} { set since $next; set since_explicit 1; incr i
            } elseif {$a eq "--grep" && $has} { set grep $next; incr i
            } elseif {($a eq "--level" || $a eq "-l") && $has} { set level $next; incr i
            } elseif {$a eq "--follow" || $a eq "-f"} { set follow 1
            } elseif {$a eq "--json"} { set json 1
            } elseif {$a eq "-h" || $a eq "--help"} { set help 1
            }
        }
        if {$help} { paas_logs_help; return 0 }
        if {[catch {get_credentials $pk $sk} creds] || [lindex $creds 0] eq ""} {
            puts stderr "Error: API credentials required."
            puts stderr "  Set UNSANDBOX_PUBLIC_KEY + UNSANDBOX_SECRET_KEY env vars, or"
            puts stderr "  Use -p PUBLIC_KEY -k SECRET_KEY flags, or"
            puts stderr "  Create ~/.unsandbox/accounts.csv with: public_key,secret_key"
            return 1
        }
        if {!$since_explicit} {
            set since [read_log_cursor]
            if {$since eq ""} { set since 5m }
        }
        if {$follow} { return [logs_stream $source $grep $level $pk $sk] }

        if {[catch {logs_fetch_raw $source $lines $since $grep $level $pk $sk} r]} {
            puts stderr "Error: Network error fetching logs: $r"
            return 1
        }
        lassign $r ncode body
        if {$ncode != 200} {
            set err ""
            catch { set err [dict get [::json::json2dict $body] error] }
            puts stderr "Error: HTTP $ncode[expr {$err ne "" ? " — $err" : ""}]"
            return 1
        }
        write_log_cursor
        if {$json} { puts $body; return 0 }
        if {[catch {::json::json2dict $body} data] || ![dict exists $data lines]} {
            puts stderr "Error: Invalid response format"
            return 1
        }
        set is_all [dict exists $data sources]
        foreach l [dict get $data lines] {
            if {$is_all} {
                set src [expr {[dict exists $l source] ? [dict get $l source] : ""}]
                set ln [expr {[dict exists $l line] ? [dict get $l line] : ""}]
                print_log_row $src $ln
            } else {
                puts $l
            }
        }
        return 0
    }

    # ============================================================================
    # Key Validation
    # ============================================================================

    proc validate_keys {{public_key ""} {secret_key ""}} {
        lassign [validate_keys_raw $public_key $secret_key] ncode response
        if {$ncode == 200} {
            return [::json::json2dict $response]
        }
        set_error "Key validation failed ($ncode)"
        error "Key validation failed"
    }

    # POST /keys/validate; returns {http_code body} whatever the status.
    proc validate_keys_raw {{public_key ""} {secret_key ""}} {
        variable API_BASE

        lassign [get_credentials $public_key $secret_key] pk sk

        set headers [list Authorization "Bearer $pk" Content-Type "application/json"]

        if {$sk ne ""} {
            set timestamp [clock seconds]
            set sig_input "${timestamp}:POST:/keys/validate:"
            set signature [hmac_sign $sk $sig_input]
            lappend headers X-Timestamp $timestamp
            lappend headers X-Signature $signature
        }

        set token [::http::geturl "$API_BASE/keys/validate" -method POST -headers $headers -timeout 30000]
        set ncode [::http::ncode $token]
        set response [::http::data $token]
        ::http::cleanup $token
        return [list $ncode $response]
    }

    # ============================================================================
    # CLI Commands
    # ============================================================================

    proc cmd_languages {args} {
        variable BLUE; variable RESET
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
        variable GREEN; variable RED; variable YELLOW; variable PORTAL_BASE; variable RESET

        set extend 0
        set rotate 0
        set otp_given 0
        set otp ""
        for {set i 0} {$i < [llength $args]} {incr i} {
            set arg [lindex $args $i]
            if {$arg eq "--extend"} { set extend 1 }
            if {$arg eq "--rotate"} { set rotate 1 }
            if {$arg eq "--otp" && $i + 1 < [llength $args]} {
                set otp_given 1
                set otp [lindex $args [incr i]]
            }
        }

        if {$rotate} {
            cmd_key_rotate $otp_given $otp
            return
        }

        if {[catch {get_credentials} creds]} {
            puts stderr "${RED}Error: no credentials found${RESET}"
            exit 3
        }
        set pk [lindex $creds 0]

        if {$extend} {
            set url "$PORTAL_BASE/keys/extend?pk=$pk"
            puts "${YELLOW}Opening browser to extend key...${RESET}"
            catch {exec xdg-open $url &}
            return
        }

        if {[catch {validate_keys_raw} raw]} {
            puts stderr "${RED}Error: $raw${RESET}"
            exit 1
        }
        lassign $raw ncode response
        set result {}
        catch {set result [::json::json2dict $response]}

        if {$ncode == 200} {
            if {[dict exists $result status]} {
                set state [dict get $result status]
            } elseif {[dict exists $result valid] && [dict get $result valid]} {
                set state active
            } else {
                set state invalid
            }
            set reason "status $state"
        } elseif {$ncode == 401} {
            set reason unauthorized
            if {[dict exists $result error]} { set reason [dict get $result error] }
            switch -exact -- $reason {
                :expired - expired { set state expired }
                :suspended - suspended { set state suspended }
                default { set state invalid }
            }
        } else {
            set msg ""
            if {[dict exists $result error]} { set msg " [dict get $result error]" } elseif {[dict exists $result message]} { set msg " [dict get $result message]" }
            puts stderr "${RED}Error: HTTP $ncode$msg${RESET}"
            exit 4
        }

        switch -exact -- $state {
            active {}
            expired {
                puts "${RED}Expired${RESET}"
                puts "Public Key: $pk"
                puts "${YELLOW}To renew: Visit $PORTAL_BASE/keys/extend${RESET}"
                exit 3
            }
            suspended {
                puts "${RED}Suspended${RESET}"
                puts "Public Key: $pk"
                exit 3
            }
            default {
                puts "${RED}Invalid${RESET}"
                puts "Public Key: $pk"
                puts "Reason: $reason"
                exit 3
            }
        }

        set tier "N/A"; set status "N/A"; set expires "N/A"; set remaining "N/A"
        if {[dict exists $result tier]} { set tier [dict get $result tier] }
        if {[dict exists $result status]} { set status [dict get $result status] }
        if {[dict exists $result valid_through] && [string is double -strict [dict get $result valid_through]]} {
            set secs [expr {wide([dict get $result valid_through]) / 1000}]
            set expires [clock format $secs -format "%Y-%m-%dT%H:%M:%SZ" -gmt 1]
            set left [expr {$secs - [clock seconds]}]
            if {$left <= 0} {
                set remaining "expired"
            } else {
                set remaining "[expr {$left / 86400}]d [expr {$left % 86400 / 3600}]h [expr {$left % 3600 / 60}]m"
            }
        }

        puts "${GREEN}Valid${RESET}"
        puts "Public Key: $pk"
        puts "Tier: $tier"
        puts "Status: $status"
        puts "Expires: $expires"
        puts "Time Remaining: $remaining"
    }

    proc cmd_session {args} {
        variable GREEN; variable RESET
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
        variable GREEN; variable RESET
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
        variable GREEN; variable RESET
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
        variable GREEN; variable RESET
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
        variable BLUE; variable RED; variable RESET

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
  tclsh un.tcl key --rotate [--otp CODE]

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
        variable BLUE; variable RED; variable RESET; variable ACCOUNT_INDEX

        # Pre-scan for --account N; strip it from argv before dispatch
        set filtered {}
        for {set i 0} {$i < [llength $argv]} {incr i} {
            set a [lindex $argv $i]
            if {$a eq "--account"} {
                incr i
                set ACCOUNT_INDEX [lindex $argv $i]
            } else {
                lappend filtered $a
            }
        }
        set argv $filtered

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
            paas { exit [cmd_paas {*}[lrange $argv 1 end]] }
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
