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

#!/usr/bin/env tclsh

# unsandbox CLI - TCL implementation
# Full-featured CLI matching un.c/un.py capabilities

package require http
package require json
package require tls
package require base64

# Register https support
::http::register https 443 ::tls::socket

set API_BASE "https://api.unsandbox.com"
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

proc get_api_key {} {
    if {[info exists ::env(UNSANDBOX_API_KEY)]} {
        return $::env(UNSANDBOX_API_KEY)
    }
    puts stderr "${::RED}Error: UNSANDBOX_API_KEY not set${::RESET}"
    exit 1
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

proc api_request {endpoint method data api_key} {
    set url "${::API_BASE}${endpoint}"
    set headers [list Authorization "Bearer $api_key" Content-Type "application/json"]

    if {$method eq "GET"} {
        set token [::http::geturl $url -headers $headers -timeout 300000]
    } elseif {$method eq "DELETE"} {
        set token [::http::geturl $url -method DELETE -headers $headers -timeout 300000]
    } else {
        set json_data [::json::write object {*}$data]
        set token [::http::geturl $url -method $method -headers $headers -query $json_data -timeout 300000]
    }

    set status [::http::status $token]
    set ncode [::http::ncode $token]
    set body [::http::data $token]
    ::http::cleanup $token

    if {$status ne "ok" || ($ncode != 200 && $ncode != 201)} {
        puts stderr "${::RED}Error: HTTP $ncode${::RESET}"
        puts stderr $body
        exit 1
    }

    return [::json::json2dict $body]
}

proc cmd_execute {args} {
    set api_key [get_api_key]
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
    set result [api_request "/execute" "POST" $payload $api_key]

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
    set api_key [get_api_key]
    set list_mode 0
    set kill_id ""
    set shell ""
    set network ""
    set vcpu 0

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
        }
    }

    if {$list_mode} {
        set result [api_request "/sessions" "GET" {} $api_key]
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
        api_request "/sessions/$kill_id" "DELETE" {} $api_key
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

    puts "${::YELLOW}Creating session...${::RESET}"
    set result [api_request "/sessions" "POST" $payload $api_key]
    puts "${::GREEN}Session created: [dict get $result id]${::RESET}"
    puts "${::YELLOW}(Interactive sessions require WebSocket - use un2 for full support)${::RESET}"
}

proc cmd_service {args} {
    set api_key [get_api_key]
    set list_mode 0
    set info_id ""
    set logs_id ""
    set sleep_id ""
    set wake_id ""
    set destroy_id ""
    set name ""
    set ports ""
    set bootstrap ""
    set network ""
    set vcpu 0

    # Parse arguments
    for {set i 0} {$i < [llength $args]} {incr i} {
        set arg [lindex $args $i]
        switch -exact -- $arg {
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
            --sleep {
                incr i
                set sleep_id [lindex $args $i]
            }
            --wake {
                incr i
                set wake_id [lindex $args $i]
            }
            --destroy {
                incr i
                set destroy_id [lindex $args $i]
            }
            --name {
                incr i
                set name [lindex $args $i]
            }
            --ports {
                incr i
                set ports [lindex $args $i]
            }
            --bootstrap {
                incr i
                set bootstrap [lindex $args $i]
            }
            -n {
                incr i
                set network [lindex $args $i]
            }
            -v {
                incr i
                set vcpu [lindex $args $i]
            }
        }
    }

    if {$list_mode} {
        set result [api_request "/services" "GET" {} $api_key]
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
        set result [api_request "/services/$info_id" "GET" {} $api_key]
        puts [::json::write object {*}[dict_to_json_list $result]]
        return
    }

    if {$logs_id ne ""} {
        set result [api_request "/services/$logs_id/logs" "GET" {} $api_key]
        puts [dict get $result logs]
        return
    }

    if {$sleep_id ne ""} {
        api_request "/services/$sleep_id/sleep" "POST" {} $api_key
        puts "${::GREEN}Service sleeping: $sleep_id${::RESET}"
        return
    }

    if {$wake_id ne ""} {
        api_request "/services/$wake_id/wake" "POST" {} $api_key
        puts "${::GREEN}Service waking: $wake_id${::RESET}"
        return
    }

    if {$destroy_id ne ""} {
        api_request "/services/$destroy_id" "DELETE" {} $api_key
        puts "${::GREEN}Service destroyed: $destroy_id${::RESET}"
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

        if {$bootstrap ne ""} {
            # Check if bootstrap is a file
            if {[file exists $bootstrap]} {
                set fp [open $bootstrap r]
                set bootstrap_content [read $fp]
                close $fp
                lappend payload bootstrap [::json::write string $bootstrap_content]
            } else {
                lappend payload bootstrap [::json::write string $bootstrap]
            }
        }

        if {$network ne ""} {
            lappend payload network [::json::write string $network]
        }
        if {$vcpu > 0} {
            lappend payload vcpu $vcpu
        }

        set result [api_request "/services" "POST" $payload $api_key]
        puts "${::GREEN}Service created: [dict get $result id]${::RESET}"
        puts "Name: [dict get $result name]"
        if {[dict exists $result url]} {
            puts "URL: [dict get $result url]"
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
        exit 1
    }

    set first_arg [lindex $argv 0]

    if {$first_arg eq "session"} {
        cmd_session [lrange $argv 1 end]
    } elseif {$first_arg eq "service"} {
        cmd_service [lrange $argv 1 end]
    } else {
        cmd_execute $argv
    }
}

main $argv
