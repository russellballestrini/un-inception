      * PUBLIC DOMAIN - NO LICENSE, NO WARRANTY
      *
      * This is free public domain software for the public good of a permacomputer hosted
      * at permacomputer.com - an always-on computer by the people, for the people. One
      * which is durable, easy to repair, and distributed like tap water for machine
      * learning intelligence.
      *
      * The permacomputer is community-owned infrastructure optimized around four values:
      *
      *   TRUTH    - First principles, math & science, open source code freely distributed
      *   FREEDOM  - Voluntary partnerships, freedom from tyranny & corporate control
      *   HARMONY  - Minimal waste, self-renewing systems with diverse thriving connections
      *   LOVE     - Be yourself without hurting others, cooperation through natural law
      *
      * This software contributes to that vision by enabling code execution across 42+
      * programming languages through a unified interface, accessible to all. Code is
      * seeds to sprout on any abandoned technology.
      *
      * Learn more: https://www.permacomputer.com
      *
      * Anyone is free to copy, modify, publish, use, compile, sell, or distribute this
      * software, either in source code form or as a compiled binary, for any purpose,
      * commercial or non-commercial, and by any means.
      *
      * NO WARRANTY. THE SOFTWARE IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY KIND.
      *
      * That said, our permacomputer's digital membrane stratum continuously runs unit,
      * integration, and functional tests on all of it's own software - with our
      * permacomputer monitoring itself, repairing itself, with minimal human in the
      * loop guidance. Our agents do their best.
      *
      * Copyright 2025 TimeHexOn & foxhop & russell@unturf
      * https://www.timehexon.com
      * https://www.foxhop.net
      * https://www.unturf.com/software


       IDENTIFICATION DIVISION.
       PROGRAM-ID. UNSANDBOX-CLI.
       AUTHOR. UNSANDBOX.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SOURCE-FILE ASSIGN TO WS-FILENAME
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  SOURCE-FILE.
       01  SOURCE-LINE         PIC X(1024).

       WORKING-STORAGE SECTION.
       01  WS-FILENAME         PIC X(256).
       01  WS-FILE-STATUS      PIC XX.
       01  WS-API-KEY          PIC X(256).
       01  WS-PUBLIC-KEY       PIC X(256).
       01  WS-SECRET-KEY       PIC X(256).
       01  WS-LANGUAGE         PIC X(32).
       01  WS-EXTENSION        PIC X(16).
       01  WS-CURL-CMD         PIC X(4096).
       01  WS-EXIT-CODE        PIC 9(4) VALUE 0.
       01  WS-DOT-POS          PIC 9(4) VALUE 0.
       01  WS-LEN              PIC 9(4) VALUE 0.
       01  WS-I                PIC 9(4) VALUE 0.
       01  WS-ARG1             PIC X(256).
       01  WS-ARG2             PIC X(256).
       01  WS-ARG3             PIC X(256).
       01  WS-COMMAND          PIC X(32).
       01  WS-OPERATION        PIC X(32).
       01  WS-ID               PIC X(256).
       01  WS-NAME             PIC X(256).
       01  WS-PORTS            PIC X(256).
       01  WS-DOMAINS          PIC X(256).
       01  WS-SERVICE-TYPE     PIC X(64).
       01  WS-BOOTSTRAP        PIC X(2048).
       01  WS-PORTAL-BASE      PIC X(256) VALUE
           "https://unsandbox.com".
       01  WS-EXTEND-FLAG      PIC X(8).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
      * Get command line argument (first argument)
           ACCEPT WS-ARG1 FROM COMMAND-LINE.

           IF WS-ARG1 = SPACES
               DISPLAY "Usage: un.cob <source_file>" UPON SYSERR
               DISPLAY "       un.cob session [options]" UPON SYSERR
               DISPLAY "       un.cob service [options]" UPON SYSERR
               MOVE 1 TO RETURN-CODE
               STOP RUN
           END-IF.

      * Check for subcommands
           IF WS-ARG1 = "session"
               PERFORM HANDLE-SESSION
               STOP RUN
           END-IF.

           IF WS-ARG1 = "service"
               PERFORM HANDLE-SERVICE
               STOP RUN
           END-IF.

           IF WS-ARG1 = "key"
               PERFORM HANDLE-KEY
               STOP RUN
           END-IF.

      * Default: execute command
           MOVE WS-ARG1 TO WS-FILENAME.
           PERFORM HANDLE-EXECUTE.
           STOP RUN.

       HANDLE-EXECUTE.
      * Check if file exists
           OPEN INPUT SOURCE-FILE.
           IF WS-FILE-STATUS NOT = "00"
               DISPLAY "Error: File not found: " WS-FILENAME
                   UPON SYSERR
               MOVE 1 TO RETURN-CODE
               STOP RUN
           END-IF.
           CLOSE SOURCE-FILE.

      * Detect language from extension
           PERFORM DETECT-LANGUAGE.

           IF WS-LANGUAGE = "unknown"
               DISPLAY "Error: Unknown language for file: "
                   WS-FILENAME UPON SYSERR
               MOVE 1 TO RETURN-CODE
               STOP RUN
           END-IF.

      * Get API key from environment
           ACCEPT WS-API-KEY FROM ENVIRONMENT "UNSANDBOX_API_KEY".

           IF WS-API-KEY = SPACES
               DISPLAY "Error: UNSANDBOX_API_KEY not set" UPON SYSERR
               MOVE 1 TO RETURN-CODE
               STOP RUN
           END-IF.

      * Use curl to make request
           PERFORM MAKE-EXECUTE-REQUEST.

       HANDLE-SESSION.
      * Get API key
           ACCEPT WS-API-KEY FROM ENVIRONMENT "UNSANDBOX_API_KEY".
           IF WS-API-KEY = SPACES
               DISPLAY "Error: UNSANDBOX_API_KEY not set" UPON SYSERR
               MOVE 1 TO RETURN-CODE
               STOP RUN
           END-IF.

      * Parse session arguments (simplified)
      * For full implementation, would need to parse multiple args
           ACCEPT WS-ARG2 FROM ARGUMENT-VALUE.

           IF WS-ARG2 = "-l" OR WS-ARG2 = "--list"
               PERFORM SESSION-LIST
           ELSE
               IF WS-ARG2 = "--kill"
                   ACCEPT WS-ID FROM ARGUMENT-VALUE
                   PERFORM SESSION-KILL
               ELSE
                   DISPLAY "Error: Use --list or --kill ID"
                       UPON SYSERR
                   MOVE 1 TO RETURN-CODE
               END-IF
           END-IF.

       HANDLE-SERVICE.
      * Get API key
           ACCEPT WS-API-KEY FROM ENVIRONMENT "UNSANDBOX_API_KEY".
           IF WS-API-KEY = SPACES
               DISPLAY "Error: UNSANDBOX_API_KEY not set" UPON SYSERR
               MOVE 1 TO RETURN-CODE
               STOP RUN
           END-IF.

      * Initialize service parameters
           MOVE SPACES TO WS-NAME.
           MOVE SPACES TO WS-PORTS.
           MOVE SPACES TO WS-DOMAINS.
           MOVE SPACES TO WS-SERVICE-TYPE.
           MOVE SPACES TO WS-BOOTSTRAP.

      * Parse service arguments
           ACCEPT WS-ARG2 FROM ARGUMENT-VALUE.

           IF WS-ARG2 = "-l" OR WS-ARG2 = "--list"
               PERFORM SERVICE-LIST
           ELSE IF WS-ARG2 = "--info"
               ACCEPT WS-ID FROM ARGUMENT-VALUE
               PERFORM SERVICE-INFO
           ELSE IF WS-ARG2 = "--logs"
               ACCEPT WS-ID FROM ARGUMENT-VALUE
               PERFORM SERVICE-LOGS
           ELSE IF WS-ARG2 = "--freeze"
               ACCEPT WS-ID FROM ARGUMENT-VALUE
               PERFORM SERVICE-SLEEP
           ELSE IF WS-ARG2 = "--unfreeze"
               ACCEPT WS-ID FROM ARGUMENT-VALUE
               PERFORM SERVICE-WAKE
           ELSE IF WS-ARG2 = "--destroy"
               ACCEPT WS-ID FROM ARGUMENT-VALUE
               PERFORM SERVICE-DESTROY
           ELSE IF WS-ARG2 = "--dump-bootstrap"
               ACCEPT WS-ID FROM ARGUMENT-VALUE
               PERFORM SERVICE-DUMP-BOOTSTRAP
           ELSE IF WS-ARG2 = "--name"
               ACCEPT WS-NAME FROM ARGUMENT-VALUE
               PERFORM PARSE-SERVICE-CREATE-ARGS
               PERFORM SERVICE-CREATE
           ELSE
               DISPLAY "Error: Use --list, --info, --logs, "
                   "--freeze, --unfreeze, --destroy, --dump-bootstrap, or --name" UPON SYSERR
               MOVE 1 TO RETURN-CODE
           END-IF.

       DETECT-LANGUAGE.
      * Find last dot in filename
           MOVE FUNCTION LENGTH(FUNCTION TRIM(WS-FILENAME)) TO WS-LEN.
           MOVE 0 TO WS-DOT-POS.
           PERFORM VARYING WS-I FROM WS-LEN BY -1
               UNTIL WS-I < 1 OR WS-DOT-POS > 0
               IF WS-FILENAME(WS-I:1) = "."
                   MOVE WS-I TO WS-DOT-POS
               END-IF
           END-PERFORM.

           IF WS-DOT-POS = 0
               MOVE "unknown" TO WS-LANGUAGE
           ELSE
               COMPUTE WS-I = WS-LEN - WS-DOT-POS + 1
               MOVE WS-FILENAME(WS-DOT-POS:WS-I) TO WS-EXTENSION

               EVALUATE WS-EXTENSION
                   WHEN ".jl"     MOVE "julia"      TO WS-LANGUAGE
                   WHEN ".r"      MOVE "r"          TO WS-LANGUAGE
                   WHEN ".cr"     MOVE "crystal"    TO WS-LANGUAGE
                   WHEN ".f90"    MOVE "fortran"    TO WS-LANGUAGE
                   WHEN ".cob"    MOVE "cobol"      TO WS-LANGUAGE
                   WHEN ".pro"    MOVE "prolog"     TO WS-LANGUAGE
                   WHEN ".forth"  MOVE "forth"      TO WS-LANGUAGE
                   WHEN ".4th"    MOVE "forth"      TO WS-LANGUAGE
                   WHEN ".py"     MOVE "python"     TO WS-LANGUAGE
                   WHEN ".js"     MOVE "javascript" TO WS-LANGUAGE
                   WHEN ".rb"     MOVE "ruby"       TO WS-LANGUAGE
                   WHEN ".go"     MOVE "go"         TO WS-LANGUAGE
                   WHEN ".rs"     MOVE "rust"       TO WS-LANGUAGE
                   WHEN ".c"      MOVE "c"          TO WS-LANGUAGE
                   WHEN ".cpp"    MOVE "cpp"        TO WS-LANGUAGE
                   WHEN ".java"   MOVE "java"       TO WS-LANGUAGE
                   WHEN ".sh"     MOVE "bash"       TO WS-LANGUAGE
                   WHEN OTHER     MOVE "unknown"    TO WS-LANGUAGE
               END-EVALUATE
           END-IF.

       MAKE-EXECUTE-REQUEST.
      * Get public/secret keys with fallback
           ACCEPT WS-PUBLIC-KEY FROM ENVIRONMENT "UNSANDBOX_PUBLIC_KEY".
           IF WS-PUBLIC-KEY NOT = SPACES
               ACCEPT WS-SECRET-KEY FROM ENVIRONMENT "UNSANDBOX_SECRET_KEY"
               IF WS-SECRET-KEY = SPACES
                   DISPLAY "Error: UNSANDBOX_SECRET_KEY not set"
                       UPON SYSERR
                   MOVE 1 TO RETURN-CODE
                   STOP RUN
               END-IF
           ELSE
               ACCEPT WS-PUBLIC-KEY FROM ENVIRONMENT "UNSANDBOX_API_KEY"
               IF WS-PUBLIC-KEY = SPACES
                   DISPLAY "Error: UNSANDBOX_PUBLIC_KEY/SECRET_KEY or "
                       "UNSANDBOX_API_KEY not set" UPON SYSERR
                   MOVE 1 TO RETURN-CODE
                   STOP RUN
               END-IF
               MOVE WS-PUBLIC-KEY TO WS-SECRET-KEY
           END-IF.

      * Build curl command using shell with HMAC signature
           STRING "TS=$(date +%s); "
               "BODY=$(jq -Rs '{language: """
               FUNCTION TRIM(WS-LANGUAGE)
               """, code: .}' < '"
               FUNCTION TRIM(WS-FILENAME)
               "'); "
               "SIG=$(echo -n \"$TS:POST:/execute:$BODY\" | "
               "openssl dgst -sha256 -hmac '"
               FUNCTION TRIM(WS-SECRET-KEY)
               "' | cut -d' ' -f2); "
               "curl -s -X POST https://api.unsandbox.com/execute "
               "-H 'Content-Type: application/json' "
               "-H 'Authorization: Bearer "
               FUNCTION TRIM(WS-PUBLIC-KEY)
               "' "
               "-H 'X-Timestamp: '$TS "
               "-H 'X-Signature: '$SIG "
               "--data-binary \"$BODY\" -o /tmp/unsandbox_resp.json; "
               "jq -r '.stdout // empty' /tmp/unsandbox_resp.json | "
               "sed 's/^/\x1b[34m/' | sed 's/$/\x1b[0m/'; "
               "jq -r '.stderr // empty' /tmp/unsandbox_resp.json | "
               "sed 's/^/\x1b[31m/' | sed 's/$/\x1b[0m/' >&2; "
               "rm -f /tmp/unsandbox_resp.json"
               DELIMITED BY SIZE INTO WS-CURL-CMD
           END-STRING.

           CALL "SYSTEM" USING WS-CURL-CMD
               RETURNING WS-EXIT-CODE.

           MOVE WS-EXIT-CODE TO RETURN-CODE.

       SESSION-LIST.
           STRING "curl -s -X GET https://api.unsandbox.com/sessions "
               "-H 'Authorization: Bearer " FUNCTION TRIM(WS-API-KEY)
               "' | jq -r '.sessions[] | "
               '"\(.id) \(.shell) \(.status) \(.created_at)"'' "
               "2>/dev/null || echo 'No active sessions'"
               DELIMITED BY SIZE INTO WS-CURL-CMD
           END-STRING.

           CALL "SYSTEM" USING WS-CURL-CMD.

       SESSION-KILL.
           STRING "curl -s -X DELETE "
               "https://api.unsandbox.com/sessions/"
               FUNCTION TRIM(WS-ID) " "
               "-H 'Authorization: Bearer " FUNCTION TRIM(WS-API-KEY)
               "' >/dev/null && "
               "echo -e '\x1b[32mSession terminated: "
               FUNCTION TRIM(WS-ID) "\x1b[0m'"
               DELIMITED BY SIZE INTO WS-CURL-CMD
           END-STRING.

           CALL "SYSTEM" USING WS-CURL-CMD.

       SERVICE-LIST.
           STRING "curl -s -X GET https://api.unsandbox.com/services "
               "-H 'Authorization: Bearer " FUNCTION TRIM(WS-API-KEY)
               "' | jq -r '.services[] | "
               '"\(.id) \(.name) \(.status)"'' "
               "2>/dev/null || echo 'No services'"
               DELIMITED BY SIZE INTO WS-CURL-CMD
           END-STRING.

           CALL "SYSTEM" USING WS-CURL-CMD.

       SERVICE-INFO.
           STRING "curl -s -X GET "
               "https://api.unsandbox.com/services/"
               FUNCTION TRIM(WS-ID) " "
               "-H 'Authorization: Bearer " FUNCTION TRIM(WS-API-KEY)
               "' | jq ."
               DELIMITED BY SIZE INTO WS-CURL-CMD
           END-STRING.

           CALL "SYSTEM" USING WS-CURL-CMD.

       SERVICE-LOGS.
           STRING "curl -s -X GET "
               "https://api.unsandbox.com/services/"
               FUNCTION TRIM(WS-ID) "/logs "
               "-H 'Authorization: Bearer " FUNCTION TRIM(WS-API-KEY)
               "' | jq -r '.logs'"
               DELIMITED BY SIZE INTO WS-CURL-CMD
           END-STRING.

           CALL "SYSTEM" USING WS-CURL-CMD.

       SERVICE-SLEEP.
           STRING "curl -s -X POST "
               "https://api.unsandbox.com/services/"
               FUNCTION TRIM(WS-ID) "/sleep "
               "-H 'Authorization: Bearer " FUNCTION TRIM(WS-API-KEY)
               "' >/dev/null && "
               "echo -e '\x1b[32mService sleeping: "
               FUNCTION TRIM(WS-ID) "\x1b[0m'"
               DELIMITED BY SIZE INTO WS-CURL-CMD
           END-STRING.

           CALL "SYSTEM" USING WS-CURL-CMD.

       SERVICE-WAKE.
           STRING "curl -s -X POST "
               "https://api.unsandbox.com/services/"
               FUNCTION TRIM(WS-ID) "/wake "
               "-H 'Authorization: Bearer " FUNCTION TRIM(WS-API-KEY)
               "' >/dev/null && "
               "echo -e '\x1b[32mService waking: "
               FUNCTION TRIM(WS-ID) "\x1b[0m'"
               DELIMITED BY SIZE INTO WS-CURL-CMD
           END-STRING.

           CALL "SYSTEM" USING WS-CURL-CMD.

       SERVICE-DESTROY.
           STRING "curl -s -X DELETE "
               "https://api.unsandbox.com/services/"
               FUNCTION TRIM(WS-ID) " "
               "-H 'Authorization: Bearer " FUNCTION TRIM(WS-API-KEY)
               "' >/dev/null && "
               "echo -e '\x1b[32mService destroyed: "
               FUNCTION TRIM(WS-ID) "\x1b[0m'"
               DELIMITED BY SIZE INTO WS-CURL-CMD
           END-STRING.

           CALL "SYSTEM" USING WS-CURL-CMD.

       SERVICE-DUMP-BOOTSTRAP.
      * Check if WS-ARG3 contains --dump-file argument
           ACCEPT WS-ARG3 FROM ARGUMENT-VALUE.
           MOVE SPACES TO WS-BOOTSTRAP.
           IF WS-ARG3 = "--dump-file"
               ACCEPT WS-BOOTSTRAP FROM ARGUMENT-VALUE
           END-IF.

           STRING "echo 'Fetching bootstrap script from "
               FUNCTION TRIM(WS-ID) "...' >&2; "
               "RESP=$(curl -s -X POST "
               "https://api.unsandbox.com/services/"
               FUNCTION TRIM(WS-ID) "/execute "
               "-H 'Content-Type: application/json' "
               "-H 'Authorization: Bearer " FUNCTION TRIM(WS-API-KEY)
               "' -d '{\"command\":\"cat /tmp/bootstrap.sh\"}'); "
               "STDOUT=$(echo \"$RESP\" | jq -r '.stdout // empty'); "
               "if [ -n \"$STDOUT\" ]; then "
               DELIMITED BY SIZE INTO WS-CURL-CMD
           END-STRING.

           IF WS-BOOTSTRAP NOT = SPACES
               STRING FUNCTION TRIM(WS-CURL-CMD)
                   "echo \"$STDOUT\" > '"
                   FUNCTION TRIM(WS-BOOTSTRAP)
                   "' && chmod 755 '"
                   FUNCTION TRIM(WS-BOOTSTRAP)
                   "' && echo 'Bootstrap saved to "
                   FUNCTION TRIM(WS-BOOTSTRAP) "'; "
                   DELIMITED BY SIZE INTO WS-CURL-CMD
               END-STRING
           ELSE
               STRING FUNCTION TRIM(WS-CURL-CMD)
                   "echo \"$STDOUT\"; "
                   DELIMITED BY SIZE INTO WS-CURL-CMD
               END-STRING
           END-IF.

           STRING FUNCTION TRIM(WS-CURL-CMD)
               "else echo -e '\x1b[31mError: Failed to fetch "
               "bootstrap\x1b[0m' >&2; exit 1; fi"
               DELIMITED BY SIZE INTO WS-CURL-CMD
           END-STRING.

           CALL "SYSTEM" USING WS-CURL-CMD.

       PARSE-SERVICE-CREATE-ARGS.
      * Parse remaining arguments for service creation
      * This is a simplified parser that looks for specific flags
           ACCEPT WS-ARG3 FROM ARGUMENT-VALUE.
           PERFORM UNTIL WS-ARG3 = SPACES
               IF WS-ARG3 = "--ports"
                   ACCEPT WS-PORTS FROM ARGUMENT-VALUE
               ELSE IF WS-ARG3 = "--domains"
                   ACCEPT WS-DOMAINS FROM ARGUMENT-VALUE
               ELSE IF WS-ARG3 = "--type"
                   ACCEPT WS-SERVICE-TYPE FROM ARGUMENT-VALUE
               ELSE IF WS-ARG3 = "--bootstrap"
                   ACCEPT WS-BOOTSTRAP FROM ARGUMENT-VALUE
               END-IF
               ACCEPT WS-ARG3 FROM ARGUMENT-VALUE
           END-PERFORM.

       SERVICE-CREATE.
      * Build JSON payload for service creation
      * Start with base payload containing name
           STRING "curl -s -X POST "
               "https://api.unsandbox.com/services "
               "-H 'Content-Type: application/json' "
               "-H 'Authorization: Bearer " FUNCTION TRIM(WS-API-KEY)
               "' -d '{""name"":"""
               FUNCTION TRIM(WS-NAME)
               """"
               DELIMITED BY SIZE INTO WS-CURL-CMD
           END-STRING.

      * Add ports if provided
           IF WS-PORTS NOT = SPACES
               STRING FUNCTION TRIM(WS-CURL-CMD)
                   ",""ports"":[" FUNCTION TRIM(WS-PORTS) "]"
                   DELIMITED BY SIZE INTO WS-CURL-CMD
               END-STRING
           END-IF.

      * Add domains if provided
           IF WS-DOMAINS NOT = SPACES
               STRING FUNCTION TRIM(WS-CURL-CMD)
                   ",""domains"":["""
                   FUNCTION TRIM(WS-DOMAINS)
                   """]"
                   DELIMITED BY SIZE INTO WS-CURL-CMD
               END-STRING
           END-IF.

      * Add service_type if provided
           IF WS-SERVICE-TYPE NOT = SPACES
               STRING FUNCTION TRIM(WS-CURL-CMD)
                   ",""service_type"":"""
                   FUNCTION TRIM(WS-SERVICE-TYPE)
                   """"
                   DELIMITED BY SIZE INTO WS-CURL-CMD
               END-STRING
           END-IF.

      * Add bootstrap if provided
           IF WS-BOOTSTRAP NOT = SPACES
               STRING FUNCTION TRIM(WS-CURL-CMD)
                   ",""bootstrap"":"""
                   FUNCTION TRIM(WS-BOOTSTRAP)
                   """"
                   DELIMITED BY SIZE INTO WS-CURL-CMD
               END-STRING
           END-IF.

      * Close JSON and add output formatting
           STRING FUNCTION TRIM(WS-CURL-CMD)
               "}' | jq -r '.id + "" created""'"
               DELIMITED BY SIZE INTO WS-CURL-CMD
           END-STRING.

           CALL "SYSTEM" USING WS-CURL-CMD.

       HANDLE-KEY.
      * Get API key
           ACCEPT WS-API-KEY FROM ENVIRONMENT "UNSANDBOX_API_KEY".
           IF WS-API-KEY = SPACES
               DISPLAY "Error: UNSANDBOX_API_KEY not set" UPON SYSERR
               MOVE 1 TO RETURN-CODE
               STOP RUN
           END-IF.

      * Parse key arguments
           MOVE SPACES TO WS-EXTEND-FLAG.
           ACCEPT WS-ARG2 FROM ARGUMENT-VALUE.

           IF WS-ARG2 = "--extend"
               MOVE "true" TO WS-EXTEND-FLAG
           END-IF.

      * Validate key
           PERFORM VALIDATE-KEY.

       VALIDATE-KEY.
      * Build curl command to validate API key
           STRING "curl -s -X POST "
               FUNCTION TRIM(WS-PORTAL-BASE)
               "/keys/validate "
               "-H 'Content-Type: application/json' "
               "-H 'Authorization: Bearer " FUNCTION TRIM(WS-API-KEY)
               "' -o /tmp/unsandbox_key_resp.json; "
               "STATUS=$?; "
               "if [ $STATUS -ne 0 ]; then "
               "echo -e '\x1b[31mInvalid\x1b[0m'; "
               "exit 1; "
               "fi; "
               "EXPIRED=$(jq -r '.expired // false' "
               "/tmp/unsandbox_key_resp.json); "
               "PUBLIC_KEY=$(jq -r '.public_key // \"N/A\"' "
               "/tmp/unsandbox_key_resp.json); "
               DELIMITED BY SIZE INTO WS-CURL-CMD
           END-STRING.

           IF WS-EXTEND-FLAG = "true"
               STRING FUNCTION TRIM(WS-CURL-CMD)
                   "xdg-open '"
                   FUNCTION TRIM(WS-PORTAL-BASE)
                   "/keys/extend?pk='\"$PUBLIC_KEY\" 2>/dev/null; "
                   DELIMITED BY SIZE INTO WS-CURL-CMD
               END-STRING
           ELSE
               STRING FUNCTION TRIM(WS-CURL-CMD)
                   "if [ \"$EXPIRED\" = \"true\" ]; then "
                   "echo -e '\x1b[31mExpired\x1b[0m'; "
                   "echo 'Public Key: '$PUBLIC_KEY; "
                   "echo 'Tier: '$(jq -r '.tier // \"N/A\"' "
                   "/tmp/unsandbox_key_resp.json); "
                   "echo 'Expired: '$(jq -r '.expires_at // \"N/A\"' "
                   "/tmp/unsandbox_key_resp.json); "
                   "echo -e '\x1b[33mTo renew: Visit "
                   "https://unsandbox.com/keys/extend\x1b[0m'; "
                   "rm -f /tmp/unsandbox_key_resp.json; "
                   "exit 1; "
                   "else "
                   "echo -e '\x1b[32mValid\x1b[0m'; "
                   "echo 'Public Key: '$PUBLIC_KEY; "
                   "echo 'Tier: '$(jq -r '.tier // \"N/A\"' "
                   "/tmp/unsandbox_key_resp.json); "
                   "echo 'Status: '$(jq -r '.status // \"N/A\"' "
                   "/tmp/unsandbox_key_resp.json); "
                   "echo 'Expires: '$(jq -r '.expires_at // \"N/A\"' "
                   "/tmp/unsandbox_key_resp.json); "
                   "echo 'Time Remaining: '$(jq -r "
                   "'.time_remaining // \"N/A\"' "
                   "/tmp/unsandbox_key_resp.json); "
                   "echo 'Rate Limit: '$(jq -r '.rate_limit // \"N/A\"' "
                   "/tmp/unsandbox_key_resp.json); "
                   "echo 'Burst: '$(jq -r '.burst // \"N/A\"' "
                   "/tmp/unsandbox_key_resp.json); "
                   "echo 'Concurrency: '$(jq -r '.concurrency // \"N/A\"' "
                   "/tmp/unsandbox_key_resp.json); "
                   "fi; "
                   DELIMITED BY SIZE INTO WS-CURL-CMD
               END-STRING
           END-IF.

           STRING FUNCTION TRIM(WS-CURL-CMD)
               "rm -f /tmp/unsandbox_key_resp.json"
               DELIMITED BY SIZE INTO WS-CURL-CMD
           END-STRING.

           CALL "SYSTEM" USING WS-CURL-CMD.
