;; PUBLIC DOMAIN - NO LICENSE, NO WARRANTY
;;
;; This is free public domain software for the public good of a permacomputer hosted
;; at permacomputer.com - an always-on computer by the people, for the people. One
;; which is durable, easy to repair, and distributed like tap water for machine
;; learning intelligence.
;;
;; The permacomputer is community-owned infrastructure optimized around four values:
;;
;;   TRUTH    - First principles, math & science, open source code freely distributed
;;   FREEDOM  - Voluntary partnerships, freedom from tyranny & corporate control
;;   HARMONY  - Minimal waste, self-renewing systems with diverse thriving connections
;;   LOVE     - Be yourself without hurting others, cooperation through natural law
;;
;; This software contributes to that vision by enabling code execution across 42+
;; programming languages through a unified interface, accessible to all. Code is
;; seeds to sprout on any abandoned technology.
;;
;; Learn more: https://www.permacomputer.com
;;
;; Anyone is free to copy, modify, publish, use, compile, sell, or distribute this
;; software, either in source code form or as a compiled binary, for any purpose,
;; commercial or non-commercial, and by any means.
;;
;; NO WARRANTY. THE SOFTWARE IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY KIND.
;;
;; That said, our permacomputer's digital membrane stratum continuously runs unit,
;; integration, and functional tests on all of it's own software - with our
;; permacomputer monitoring itself, repairing itself, with minimal human in the
;; loop guidance. Our agents do their best.
;;
;; Copyright 2025 TimeHexOn & foxhop & russell@unturf
;; https://www.timehexon.com
;; https://www.foxhop.net
;; https://www.unturf.com/software


#!/usr/bin/env guile
!#

;;; Scheme UN CLI - Unsandbox CLI Client
;;;
;;; Full-featured CLI matching un.py capabilities
;;; Uses curl for HTTP (no external dependencies)

(use-modules (ice-9 popen)
             (ice-9 rdelim)
             (ice-9 format))

(define blue "\x1b[34m")
(define red "\x1b[31m")
(define green "\x1b[32m")
(define yellow "\x1b[33m")
(define reset "\x1b[0m")

(define portal-base "https://unsandbox.com")

(define ext-map
  '((".hs" . "haskell") (".ml" . "ocaml") (".clj" . "clojure")
    (".scm" . "scheme") (".lisp" . "commonlisp") (".erl" . "erlang")
    (".ex" . "elixir") (".exs" . "elixir") (".py" . "python")
    (".js" . "javascript") (".ts" . "typescript") (".rb" . "ruby")
    (".go" . "go") (".rs" . "rust") (".c" . "c") (".cpp" . "cpp")
    (".cc" . "cpp") (".java" . "java") (".kt" . "kotlin")
    (".cs" . "csharp") (".fs" . "fsharp") (".jl" . "julia")
    (".r" . "r") (".cr" . "crystal") (".d" . "d") (".nim" . "nim")
    (".zig" . "zig") (".v" . "v") (".dart" . "dart") (".sh" . "bash")
    (".pl" . "perl") (".lua" . "lua") (".php" . "php")))

(define (get-extension filename)
  (let ((dot-pos (string-rindex filename #\.)))
    (if dot-pos (substring filename dot-pos) "")))

(define (escape-json s)
  (string-append
    (string-concatenate
      (map (lambda (c)
             (cond
               ((char=? c #\\) "\\\\")
               ((char=? c #\") "\\\"")
               ((char=? c #\newline) "\\n")
               ((char=? c #\return) "\\r")
               ((char=? c #\tab) "\\t")
               (else (string c))))
           (string->list s)))))

(define (read-file filename)
  (call-with-input-file filename
    (lambda (port)
      (let loop ((lines '()))
        (let ((line (read-line port)))
          (if (eof-object? line)
              (string-join (reverse lines) "\n")
              (loop (cons line lines))))))))

(define (write-temp-file data)
  (let ((tmp-file (format #f "/tmp/un_scm_~a.json" (random 999999))))
    (call-with-output-file tmp-file
      (lambda (port) (display data port)))
    tmp-file))

(define (curl-post api-key endpoint json-data)
  (let* ((tmp-file (write-temp-file json-data))
         (keys (get-api-keys))
         (public-key (car keys))
         (secret-key (cadr keys))
         (auth-headers (build-auth-headers public-key secret-key "POST" endpoint json-data))
         (cmd (string-append "curl -s -X POST https://api.unsandbox.com" endpoint
                            " -H 'Content-Type: application/json' "
                            (string-join auth-headers " ")
                            " -d @" tmp-file))
         (port (open-input-pipe cmd))
         (output (let loop ((lines '()))
                   (let ((line (read-line port)))
                     (if (eof-object? line)
                         (string-join (reverse lines) "\n")
                         (loop (cons line lines)))))))
    (close-pipe port)
    (delete-file tmp-file)
    ;; Check for clock drift errors
    (when (and (string-contains output "timestamp")
               (or (string-contains output "401")
                   (string-contains output "expired")
                   (string-contains output "invalid")))
      (format (current-error-port) "~aError: Request timestamp expired (must be within 5 minutes of server time)~a\n" red reset)
      (format (current-error-port) "~aYour computer's clock may have drifted.~a\n" yellow reset)
      (format (current-error-port) "~aCheck your system time and sync with NTP if needed:~a\n" yellow reset)
      (format (current-error-port) "~a  Linux:   sudo ntpdate -s time.nist.gov~a\n" yellow reset)
      (format (current-error-port) "~a  macOS:   sudo sntp -sS time.apple.com~a\n" yellow reset)
      (format (current-error-port) "~a  Windows: w32tm /resync~a\n" yellow reset)
      (exit 1))
    output))

(define (curl-get api-key endpoint)
  (let* ((keys (get-api-keys))
         (public-key (car keys))
         (secret-key (cadr keys))
         (auth-headers (build-auth-headers public-key secret-key "GET" endpoint ""))
         (cmd (string-append "curl -s https://api.unsandbox.com" endpoint
                            " " (string-join auth-headers " ")))
         (port (open-input-pipe cmd))
         (output (let loop ((lines '()))
                   (let ((line (read-line port)))
                     (if (eof-object? line)
                         (string-join (reverse lines) "\n")
                         (loop (cons line lines)))))))
    (close-pipe port)
    ;; Check for clock drift errors
    (when (and (string-contains output "timestamp")
               (or (string-contains output "401")
                   (string-contains output "expired")
                   (string-contains output "invalid")))
      (format (current-error-port) "~aError: Request timestamp expired (must be within 5 minutes of server time)~a\n" red reset)
      (format (current-error-port) "~aYour computer's clock may have drifted.~a\n" yellow reset)
      (format (current-error-port) "~aCheck your system time and sync with NTP if needed:~a\n" yellow reset)
      (format (current-error-port) "~a  Linux:   sudo ntpdate -s time.nist.gov~a\n" yellow reset)
      (format (current-error-port) "~a  macOS:   sudo sntp -sS time.apple.com~a\n" yellow reset)
      (format (current-error-port) "~a  Windows: w32tm /resync~a\n" yellow reset)
      (exit 1))
    output))

(define (curl-delete api-key endpoint)
  (let* ((keys (get-api-keys))
         (public-key (car keys))
         (secret-key (cadr keys))
         (auth-headers (build-auth-headers public-key secret-key "DELETE" endpoint ""))
         (cmd (string-append "curl -s -X DELETE https://api.unsandbox.com" endpoint
                            " " (string-join auth-headers " ")))
         (port (open-input-pipe cmd))
         (output (let loop ((lines '()))
                   (let ((line (read-line port)))
                     (if (eof-object? line)
                         (string-join (reverse lines) "\n")
                         (loop (cons line lines)))))))
    (close-pipe port)
    ;; Check for clock drift errors
    (when (and (string-contains output "timestamp")
               (or (string-contains output "401")
                   (string-contains output "expired")
                   (string-contains output "invalid")))
      (format (current-error-port) "~aError: Request timestamp expired (must be within 5 minutes of server time)~a\n" red reset)
      (format (current-error-port) "~aYour computer's clock may have drifted.~a\n" yellow reset)
      (format (current-error-port) "~aCheck your system time and sync with NTP if needed:~a\n" yellow reset)
      (format (current-error-port) "~a  Linux:   sudo ntpdate -s time.nist.gov~a\n" yellow reset)
      (format (current-error-port) "~a  macOS:   sudo sntp -sS time.apple.com~a\n" yellow reset)
      (format (current-error-port) "~a  Windows: w32tm /resync~a\n" yellow reset)
      (exit 1))
    output))

(define (curl-post-portal api-key endpoint json-data)
  (let* ((tmp-file (write-temp-file json-data))
         (keys (get-api-keys))
         (public-key (car keys))
         (secret-key (cadr keys))
         (auth-headers (build-auth-headers public-key secret-key "POST" endpoint json-data))
         (cmd (string-append "curl -s -X POST " portal-base endpoint
                            " -H 'Content-Type: application/json' "
                            (string-join auth-headers " ")
                            " -d @" tmp-file))
         (port (open-input-pipe cmd))
         (output (let loop ((lines '()))
                   (let ((line (read-line port)))
                     (if (eof-object? line)
                         (string-join (reverse lines) "\n")
                         (loop (cons line lines)))))))
    (close-pipe port)
    (delete-file tmp-file)
    ;; Check for clock drift errors
    (when (and (string-contains output "timestamp")
               (or (string-contains output "401")
                   (string-contains output "expired")
                   (string-contains output "invalid")))
      (format (current-error-port) "~aError: Request timestamp expired (must be within 5 minutes of server time)~a\n" red reset)
      (format (current-error-port) "~aYour computer's clock may have drifted.~a\n" yellow reset)
      (format (current-error-port) "~aCheck your system time and sync with NTP if needed:~a\n" yellow reset)
      (format (current-error-port) "~a  Linux:   sudo ntpdate -s time.nist.gov~a\n" yellow reset)
      (format (current-error-port) "~a  macOS:   sudo sntp -sS time.apple.com~a\n" yellow reset)
      (format (current-error-port) "~a  Windows: w32tm /resync~a\n" yellow reset)
      (exit 1))
    output))

(define (get-api-keys)
  (let ((public-key (getenv "UNSANDBOX_PUBLIC_KEY"))
        (secret-key (getenv "UNSANDBOX_SECRET_KEY"))
        (api-key (getenv "UNSANDBOX_API_KEY")))
    (cond
      ((and public-key secret-key) (list public-key secret-key))
      (api-key (list api-key #f))
      (else (begin
              (display "Error: UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY not set (or UNSANDBOX_API_KEY for backwards compat)\n" (current-error-port))
              (exit 1))))))

(define (get-api-key)
  (car (get-api-keys)))

(define (hmac-sha256 secret message)
  "Compute HMAC-SHA256 using openssl command"
  (let* ((cmd (format #f "echo -n '~a' | openssl dgst -sha256 -hmac '~a' | awk '{print $2}'"
                      (string-append (list->string (map (lambda (c) (if (char=? c #\') #\space c)) (string->list message))))
                      (string-append (list->string (map (lambda (c) (if (char=? c #\') #\space c)) (string->list secret))))))
         (port (open-input-pipe cmd))
         (result (read-line port)))
    (close-pipe port)
    (string-trim-both result)))

(define (make-signature secret-key timestamp method path body)
  (let ((message (format #f "~a:~a:~a:~a" timestamp method path body)))
    (hmac-sha256 secret-key message)))

(define (build-auth-headers public-key secret-key method path body)
  (if secret-key
      (let* ((timestamp (number->string (quotient (current-time) 1)))
             (signature (make-signature secret-key timestamp method path body)))
        (list "-H" (format #f "Authorization: Bearer ~a" public-key)
              "-H" (format #f "X-Timestamp: ~a" timestamp)
              "-H" (format #f "X-Signature: ~a" signature)))
      (list "-H" (format #f "Authorization: Bearer ~a" public-key))))

(define (json-extract-string json key)
  "Extract string value for key from JSON (simple parser)"
  (let* ((pattern (format #f "\"~a\":\\s*\"([^\"]*)" key))
         (cmd (format #f "echo '~a' | grep -oP '~a' | sed 's/\"~a\":\\s*\"//'" json pattern key))
         (port (open-input-pipe cmd))
         (result (read-line port)))
    (close-pipe port)
    (if (eof-object? result) #f result)))

(define (json-has-field json field)
  "Check if JSON contains a field"
  (string-contains json (format #f "\"~a\"" field)))

(define (open-browser url)
  "Open URL in browser using xdg-open"
  (let ((cmd (format #f "xdg-open '~a' 2>/dev/null &" url)))
    (system cmd)))

(define (validate-key-cmd extend)
  (let* ((api-key (get-api-key))
         (response (curl-post-portal api-key "/keys/validate" "{}"))
         (status (json-extract-string response "status"))
         (public-key (json-extract-string response "public_key"))
         (tier (json-extract-string response "tier"))
         (valid-through (json-extract-string response "valid_through_datetime"))
         (valid-for (json-extract-string response "valid_for_human"))
         (rate-limit (json-extract-string response "rate_per_minute"))
         (burst (json-extract-string response "burst"))
         (concurrency (json-extract-string response "concurrency"))
         (expired-at (json-extract-string response "expired_at_datetime")))

    (cond
      ;; Valid key
      ((and status (string=? status "valid"))
       (format #t "~aValid~a\n\n" green reset)
       (when public-key (format #t "Public Key:          ~a\n" public-key))
       (when tier (format #t "Tier:                ~a\n" tier))
       (format #t "Status:              valid\n")
       (when valid-through (format #t "Expires:             ~a\n" valid-through))
       (when valid-for (format #t "Time Remaining:      ~a\n" valid-for))
       (when rate-limit (format #t "Rate Limit:          ~a/min\n" rate-limit))
       (when burst (format #t "Burst:               ~a\n" burst))
       (when concurrency (format #t "Concurrency:         ~a\n" concurrency))
       (when extend
         (if public-key
             (let ((url (format #f "~a/keys/extend?pk=~a" portal-base public-key)))
               (format #t "~aOpening browser to extend key...~a\n" blue reset)
               (open-browser url))
             (format #t "~aError: No public_key in response~a\n" red reset))))

      ;; Expired key
      ((and status (string=? status "expired"))
       (format #t "~aExpired~a\n\n" red reset)
       (when public-key (format #t "Public Key:          ~a\n" public-key))
       (when tier (format #t "Tier:                ~a\n" tier))
       (when expired-at (format #t "Expired:             ~a\n" expired-at))
       (format #t "\n~aTo renew:~a Visit ~a/keys/extend\n" yellow reset portal-base)
       (when extend
         (if public-key
             (let ((url (format #f "~a/keys/extend?pk=~a" portal-base public-key)))
               (format #t "~aOpening browser...~a\n" blue reset)
               (open-browser url))
             (format #t "~aError: No public_key in response~a\n" red reset))))

      ;; Invalid or error
      (else
       (format #t "~aInvalid~a\n" red reset)
       (display response)
       (newline)))))

(define (execute-cmd file)
  (let* ((api-key (get-api-key))
         (ext (get-extension file))
         (language (assoc-ref ext-map ext)))
    (when (not language)
      (format (current-error-port) "Error: Unknown extension: ~a\n" ext)
      (exit 1))
    (let* ((code (read-file file))
           (json (format #f "{\"language\":\"~a\",\"code\":\"~a\"}"
                         language (escape-json code)))
           (response (curl-post api-key "/execute" json)))
      (display response)
      (newline))))

(define (session-cmd action id shell)
  (let ((api-key (get-api-key)))
    (cond
      ((equal? action "list")
       (display (curl-get api-key "/sessions"))
       (newline))
      ((equal? action "kill")
       (curl-delete api-key (format #f "/sessions/~a" id))
       (format #t "~aSession terminated: ~a~a\n" green id reset))
      (else
        (let* ((sh (or shell "bash"))
               (json (format #f "{\"shell\":\"~a\"}" sh))
               (response (curl-post api-key "/sessions" json)))
          (format #t "~aSession created (WebSocket required)~a\n" yellow reset)
          (display response)
          (newline))))))

(define (service-cmd action id name ports bootstrap type)
  (let ((api-key (get-api-key)))
    (cond
      ((equal? action "list")
       (display (curl-get api-key "/services"))
       (newline))
      ((equal? action "info")
       (display (curl-get api-key (format #f "/services/~a" id)))
       (newline))
      ((equal? action "logs")
       (display (curl-get api-key (format #f "/services/~a/logs" id)))
       (newline))
      ((equal? action "sleep")
       (curl-post api-key (format #f "/services/~a/sleep" id) "{}")
       (format #t "~aService sleeping: ~a~a\n" green id reset))
      ((equal? action "wake")
       (curl-post api-key (format #f "/services/~a/wake" id) "{}")
       (format #t "~aService waking: ~a~a\n" green id reset))
      ((equal? action "destroy")
       (curl-delete api-key (format #f "/services/~a" id))
       (format #t "~aService destroyed: ~a~a\n" green id reset))
      ((equal? action "execute")
       (when (and id bootstrap)
         (let* ((json (format #f "{\"command\":\"~a\"}" (escape-json bootstrap)))
                (response (curl-post api-key (format #f "/services/~a/execute" id) json))
                (stdout-val (json-extract-string response "stdout")))
           (when stdout-val
             (display (format #f "~a~a~a" blue stdout-val reset))))))
      ((equal? action "dump-bootstrap")
       (when id
         (format (current-error-port) "Fetching bootstrap script from ~a...\n" id)
         (let* ((json "{\"command\":\"cat /tmp/bootstrap.sh\"}")
                (response (curl-post api-key (format #f "/services/~a/execute" id) json))
                (stdout-val (json-extract-string response "stdout")))
           (if stdout-val
               (if type
                   (begin
                     (call-with-output-file type
                       (lambda (port) (display stdout-val port)))
                     (system (format #f "chmod 755 ~a" type))
                     (format #t "Bootstrap saved to ~a\n" type))
                   (display stdout-val))
               (begin
                 (format (current-error-port) "~aError: Failed to fetch bootstrap (service not running or no bootstrap file)~a\n" red reset)
                 (exit 1))))))
      ((and (equal? action "create") name)
       (let* ((ports-json (if ports (format #f ",\"ports\":[~a]" ports) ""))
              (bootstrap-json (if bootstrap (format #f ",\"bootstrap\":\"~a\"" (escape-json bootstrap)) ""))
              (type-json (if type (format #f ",\"service_type\":\"~a\"" type) ""))
              (json (format #f "{\"name\":\"~a\"~a~a~a}" name ports-json bootstrap-json type-json))
              (response (curl-post api-key "/services" json)))
         (format #t "~aService created~a\n" green reset)
         (display response)
         (newline)))
      (else
        (display "Error: --name required to create service\n" (current-error-port))
        (exit 1)))))

(define (main args)
  (if (null? args)
      (begin
        (display "Usage: un.scm [options] <source_file>\n")
        (display "       un.scm session [options]\n")
        (display "       un.scm service [options]\n")
        (display "       un.scm key [--extend]\n")
        (exit 1))
      (cond
        ((equal? (car args) "key")
         (let ((extend (and (> (length args) 1) (equal? (cadr args) "--extend"))))
           (validate-key-cmd extend)))
        ((equal? (car args) "session")
         (if (and (> (length args) 1) (equal? (cadr args) "--list"))
             (session-cmd "list" #f #f)
             (if (and (> (length args) 2) (equal? (cadr args) "--kill"))
                 (session-cmd "kill" (caddr args) #f)
                 (session-cmd "create" #f #f))))
        ((equal? (car args) "service")
         (cond
           ((and (> (length args) 1) (equal? (cadr args) "--list"))
            (service-cmd "list" #f #f #f #f #f))
           ((and (> (length args) 2) (equal? (cadr args) "--info"))
            (service-cmd "info" (caddr args) #f #f #f #f))
           ((and (> (length args) 2) (equal? (cadr args) "--logs"))
            (service-cmd "logs" (caddr args) #f #f #f #f))
           ((and (> (length args) 2) (equal? (cadr args) "--freeze"))
            (service-cmd "sleep" (caddr args) #f #f #f #f))
           ((and (> (length args) 2) (equal? (cadr args) "--unfreeze"))
            (service-cmd "wake" (caddr args) #f #f #f #f))
           ((and (> (length args) 2) (equal? (cadr args) "--destroy"))
            (service-cmd "destroy" (caddr args) #f #f #f #f))
           ((and (> (length args) 3) (equal? (cadr args) "--execute"))
            (service-cmd "execute" (caddr args) #f #f (list-ref args 3) #f))
           ((and (> (length args) 3) (equal? (cadr args) "--dump-bootstrap"))
            (service-cmd "dump-bootstrap" (caddr args) #f #f #f (list-ref args 3)))
           ((and (> (length args) 2) (equal? (cadr args) "--dump-bootstrap"))
            (service-cmd "dump-bootstrap" (caddr args) #f #f #f #f))
           ((and (> (length args) 2) (equal? (cadr args) "--name"))
            (let ((name (caddr args))
                  (ports (if (and (> (length args) 4) (equal? (list-ref args 3) "--ports"))
                             (list-ref args 4) #f))
                  (bootstrap (if (and (> (length args) 6) (equal? (list-ref args 5) "--bootstrap"))
                                 (list-ref args 6) #f))
                  (type (if (and (> (length args) 8) (equal? (list-ref args 7) "--type"))
                            (list-ref args 8) #f)))
              (service-cmd "create" #f name ports bootstrap type)))
           (else
             (display "Error: Invalid service command\n" (current-error-port))
             (exit 1))))
        (else
          (execute-cmd (car args))))))

(main (cdr (command-line)))
