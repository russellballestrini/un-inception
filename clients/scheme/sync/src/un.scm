#!/usr/bin/env guile
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

(define languages-cache-ttl 3600) ;; 1 hour in seconds

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

(define (base64-encode-file filename)
  "Base64 encode a file using shell command"
  (let* ((cmd (format #f "base64 -w0 ~a" filename))
         (port (open-input-pipe cmd))
         (result (let loop ((chars '()))
                   (let ((char (read-char port)))
                     (if (eof-object? char)
                         (list->string (reverse chars))
                         (loop (cons char chars)))))))
    (close-pipe port)
    (string-trim-both result)))

(define (build-input-files-json files)
  "Build input_files JSON array from list of filenames"
  (if (null? files)
      ""
      (let ((entries (map (lambda (f)
                            (let* ((basename (basename f))
                                   (content (base64-encode-file f)))
                              (format #f "{\"filename\":\"~a\",\"content\":\"~a\"}"
                                      basename content)))
                          files)))
        (format #f ",\"input_files\":[~a]" (string-join entries ",")))))

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

(define (parse-http-response-with-code response)
  "Parse response to extract body and HTTP status code from curl -w output"
  (let* ((lines (string-split response #\newline))
         (last-line (if (null? lines) "" (car (last-pair lines))))
         (code (string->number (string-trim-both last-line))))
    (if code
        (cons (string-join (reverse (cdr (reverse lines))) "\n") code)
        (cons response 0))))

(define (handle-sudo-challenge response-data public-key secret-key method endpoint body)
  "Handle 428 sudo OTP challenge - prompts user for OTP and retries"
  (let ((challenge-id (json-extract-string response-data "challenge_id")))
    (format (current-error-port) "~aConfirmation required. Check your email for a one-time code.~a\n" yellow reset)
    (display "Enter OTP: " (current-error-port))
    (force-output (current-error-port))
    (let ((otp (string-trim-both (read-line))))
      (when (string=? otp "")
        (display "Error: Operation cancelled\n" (current-error-port))
        (exit 1))
      ;; Retry the request with sudo headers
      (let* ((auth-headers (build-auth-headers public-key secret-key method endpoint (or body "")))
             (sudo-otp-header (format #f "-H 'X-Sudo-OTP: ~a'" otp))
             (sudo-challenge-header (format #f "-H 'X-Sudo-Challenge: ~a'" (or challenge-id "")))
             (method-flag (cond
                           ((equal? method "DELETE") "-X DELETE")
                           ((equal? method "POST") "-X POST")
                           (else (format #f "-X ~a" method))))
             (content-header (if body "-H 'Content-Type: application/json'" ""))
             (body-part (if body (format #f "-d '~a'" body) ""))
             (cmd (string-append "curl -s -w '\\n%{http_code}' " method-flag
                                " https://api.unsandbox.com" endpoint
                                " " (string-join auth-headers " ")
                                " " sudo-otp-header
                                " " sudo-challenge-header
                                " " content-header
                                " " body-part))
             (port (open-input-pipe cmd))
             (output (let loop ((lines '()))
                       (let ((line (read-line port)))
                         (if (eof-object? line)
                             (string-join (reverse lines) "\n")
                             (loop (cons line lines)))))))
        (close-pipe port)
        (let* ((parsed (parse-http-response-with-code output))
               (resp-body (car parsed))
               (http-code (cdr parsed)))
          (if (and (>= http-code 200) (< http-code 300))
              (cons #t resp-body)
              (begin
                (format (current-error-port) "~aError: HTTP ~a~a\n" red http-code reset)
                (format (current-error-port) "~a\n" resp-body)
                (cons #f resp-body))))))))

(define (curl-delete-with-sudo api-key endpoint)
  "DELETE request that handles 428 sudo OTP challenge"
  (let* ((keys (get-api-keys))
         (public-key (car keys))
         (secret-key (cadr keys))
         (auth-headers (build-auth-headers public-key secret-key "DELETE" endpoint ""))
         (cmd (string-append "curl -s -w '\\n%{http_code}' -X DELETE https://api.unsandbox.com" endpoint
                            " " (string-join auth-headers " ")))
         (port (open-input-pipe cmd))
         (output (let loop ((lines '()))
                   (let ((line (read-line port)))
                     (if (eof-object? line)
                         (string-join (reverse lines) "\n")
                         (loop (cons line lines)))))))
    (close-pipe port)
    (let* ((parsed (parse-http-response-with-code output))
           (body (car parsed))
           (http-code (cdr parsed)))
      ;; Check for clock drift
      (when (and (string-contains body "timestamp")
                 (or (string-contains body "401")
                     (string-contains body "expired")
                     (string-contains body "invalid")))
        (format (current-error-port) "~aError: Request timestamp expired~a\n" red reset)
        (exit 1))
      (if (= http-code 428)
          (handle-sudo-challenge body public-key secret-key "DELETE" endpoint #f)
          (cons (and (>= http-code 200) (< http-code 300)) body)))))

(define (curl-post-with-sudo api-key endpoint json-data)
  "POST request that handles 428 sudo OTP challenge"
  (let* ((tmp-file (write-temp-file json-data))
         (keys (get-api-keys))
         (public-key (car keys))
         (secret-key (cadr keys))
         (auth-headers (build-auth-headers public-key secret-key "POST" endpoint json-data))
         (cmd (string-append "curl -s -w '\\n%{http_code}' -X POST https://api.unsandbox.com" endpoint
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
    (let* ((parsed (parse-http-response-with-code output))
           (body (car parsed))
           (http-code (cdr parsed)))
      ;; Check for clock drift
      (when (and (string-contains body "timestamp")
                 (or (string-contains body "401")
                     (string-contains body "expired")
                     (string-contains body "invalid")))
        (format (current-error-port) "~aError: Request timestamp expired~a\n" red reset)
        (exit 1))
      (if (= http-code 428)
          (handle-sudo-challenge body public-key secret-key "POST" endpoint json-data)
          (cons (and (>= http-code 200) (< http-code 300)) body)))))

(define (curl-patch api-key endpoint json-data)
  (let* ((tmp-file (write-temp-file json-data))
         (keys (get-api-keys))
         (public-key (car keys))
         (secret-key (cadr keys))
         (auth-headers (build-auth-headers public-key secret-key "PATCH" endpoint json-data))
         (cmd (string-append "curl -s -X PATCH https://api.unsandbox.com" endpoint
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

(define (curl-put-text api-key endpoint content)
  "PUT request with text/plain content type (for vault)"
  (let* ((tmp-file (write-temp-file content))
         (keys (get-api-keys))
         (public-key (car keys))
         (secret-key (cadr keys))
         (auth-headers (build-auth-headers public-key secret-key "PUT" endpoint content))
         (cmd (string-append "curl -s -X PUT https://api.unsandbox.com" endpoint
                            " -H 'Content-Type: text/plain' "
                            (string-join auth-headers " ")
                            " --data-binary @" tmp-file))
         (port (open-input-pipe cmd))
         (output (let loop ((lines '()))
                   (let ((line (read-line port)))
                     (if (eof-object? line)
                         (string-join (reverse lines) "\n")
                         (loop (cons line lines)))))))
    (close-pipe port)
    (delete-file tmp-file)
    output))

(define (build-env-content env-vars env-file)
  "Build env content from -e args and --env-file"
  (let* ((var-lines env-vars)
         (file-lines (if (and env-file (file-exists? env-file))
                         (let ((content (read-file env-file)))
                           (filter (lambda (line)
                                     (let ((trimmed (string-trim-both line)))
                                       (and (> (string-length trimmed) 0)
                                            (not (char=? (string-ref trimmed 0) #\#)))))
                                   (string-split content #\newline)))
                         '())))
    (string-join (append var-lines file-lines) "\n")))

;; Service vault functions
(define (service-env-status api-key service-id)
  (display (curl-get api-key (format #f "/services/~a/env" service-id)))
  (newline))

(define (service-env-set api-key service-id content)
  (display (curl-put-text api-key (format #f "/services/~a/env" service-id) content))
  (newline))

(define (service-env-export api-key service-id)
  (let* ((response (curl-post api-key (format #f "/services/~a/env/export" service-id) "{}"))
         (content (json-extract-string response "content")))
    (when content (display content))))

(define (service-env-delete api-key service-id)
  (curl-delete api-key (format #f "/services/~a/env" service-id))
  (format #t "~aVault deleted for: ~a~a\n" green service-id reset))

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

(define (json-extract-array json key)
  "Extract array values for key from JSON (simple parser)"
  ;; This extracts items from a JSON array like "languages":["python","javascript",...]
  (let* ((pattern (format #f "\"~a\":\\s*\\[([^\\]]*)\\]" key))
         (cmd (format #f "echo '~a' | grep -oP '\"~a\":\\s*\\[[^]]*\\]' | sed 's/\"~a\":\\s*\\[//' | sed 's/\\]//' | tr ',' '\\n' | sed 's/\"//g' | sed 's/^ *//' | sed 's/ *$//'" json key key))
         (port (open-input-pipe cmd))
         (result (let loop ((lines '()))
                   (let ((line (read-line port)))
                     (if (eof-object? line)
                         (reverse lines)
                         (loop (cons (string-trim-both line) lines)))))))
    (close-pipe port)
    (filter (lambda (s) (> (string-length s) 0)) result)))

(define (get-languages-cache-path)
  "Get the path to the languages cache file"
  (let ((home (getenv "HOME")))
    (string-append home "/.unsandbox/languages.json")))

(define (load-languages-cache)
  "Load languages from cache if valid"
  (let ((cache-path (get-languages-cache-path)))
    (if (file-exists? cache-path)
        (catch #t
          (lambda ()
            (let* ((content (read-file cache-path))
                   (timestamp-str (json-extract-string content "timestamp")))
              (if timestamp-str
                  (let* ((cache-time (string->number timestamp-str))
                         (now (current-time)))
                    (if (< (- now cache-time) languages-cache-ttl)
                        content
                        #f))
                  #f)))
          (lambda args #f))
        #f)))

(define (save-languages-cache languages-json)
  "Save languages to cache"
  (let* ((cache-path (get-languages-cache-path))
         (cache-dir (dirname cache-path)))
    ;; Create directory if it doesn't exist
    (catch #t
      (lambda () (mkdir cache-dir))
      (lambda args #f))
    (let* ((timestamp (current-time))
           (cache-content (format #f "{\"languages\":~a,\"timestamp\":~a}" languages-json timestamp)))
      (call-with-output-file cache-path
        (lambda (port) (display cache-content port))))))

(define (languages-cmd json-output)
  (let* ((api-key (get-api-key))
         ;; Try cache first
         (cached (load-languages-cache))
         (response (if cached
                       cached
                       (let ((resp (curl-get api-key "/languages")))
                         ;; Extract and cache the languages array
                         (let ((langs (json-extract-array resp "languages")))
                           (when (not (null? langs))
                             (let ((languages-json (string-append "[" (string-join (map (lambda (l) (format #f "\"~a\"" l)) langs) ",") "]")))
                               (save-languages-cache languages-json))))
                         resp)))
         (langs (json-extract-array response "languages")))
    (if json-output
        ;; JSON array output
        (format #t "[~a]\n" (string-join (map (lambda (l) (format #f "\"~a\"" l)) langs) ","))
        ;; One language per line (default)
        (for-each (lambda (lang)
                    (display lang)
                    (newline))
                  langs))))

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

(define (session-cmd action id shell input-files)
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
               (input-files-json (build-input-files-json input-files))
               (json (format #f "{\"shell\":\"~a\"~a}" sh input-files-json))
               (response (curl-post api-key "/sessions" json)))
          (format #t "~aSession created (WebSocket required)~a\n" yellow reset)
          (display response)
          (newline))))))

(define (service-cmd action id name ports bootstrap bootstrap-file type input-files env-vars env-file vcpu . rest)
  (define unfreeze-on-demand (and (pair? rest) (car rest)))
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
       (curl-post api-key (format #f "/services/~a/freeze" id) "{}")
       (format #t "~aService frozen: ~a~a\n" green id reset))
      ((equal? action "wake")
       (curl-post api-key (format #f "/services/~a/unfreeze" id) "{}")
       (format #t "~aService unfreezing: ~a~a\n" green id reset))
      ((equal? action "destroy")
       (let ((result (curl-delete-with-sudo api-key (format #f "/services/~a" id))))
         (if (car result)
             (format #t "~aService destroyed: ~a~a\n" green id reset)
             (begin
               (format (current-error-port) "~aError destroying service~a\n" red reset)
               (exit 1)))))
      ((equal? action "resize")
       (if (and vcpu (>= vcpu 1) (<= vcpu 8))
           (let* ((json (format #f "{\"vcpu\":~a}" vcpu))
                  (ram (* vcpu 2)))
             (curl-patch api-key (format #f "/services/~a" id) json)
             (format #t "~aService resized to ~a vCPU, ~a GB RAM~a\n" green vcpu ram reset))
           (begin
             (format (current-error-port) "~aError: --resize requires --vcpu N (1-8)~a\n" red reset)
             (exit 1))))
      ((equal? action "set-unfreeze-on-demand")
       (if bootstrap  ;; reusing bootstrap as the enabled value
           (let* ((enabled (or (equal? bootstrap "true") (equal? bootstrap "1")))
                  (enabled-str (if enabled "true" "false"))
                  (msg (if enabled "enabled" "disabled"))
                  (json (format #f "{\"unfreeze_on_demand\":~a}" enabled-str)))
             (curl-patch api-key (format #f "/services/~a" id) json)
             (format #t "~aUnfreeze-on-demand ~a for service: ~a~a\n" green msg id reset))
           (begin
             (format (current-error-port) "~aError: --set-unfreeze-on-demand requires true/false or 1/0~a\n" red reset)
             (exit 1))))
      ((equal? action "env-status")
       (service-env-status api-key id))
      ((equal? action "env-set")
       (let ((content (build-env-content env-vars env-file)))
         (if (> (string-length content) 0)
             (service-env-set api-key id content)
             (begin
               (format (current-error-port) "~aError: No environment variables to set~a\n" red reset)
               (exit 1)))))
      ((equal? action "env-export")
       (service-env-export api-key id))
      ((equal? action "env-delete")
       (service-env-delete api-key id))
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
              (bootstrap-content-json (if bootstrap-file
                                          (format #f ",\"bootstrap_content\":\"~a\"" (escape-json (read-file bootstrap-file)))
                                          ""))
              (type-json (if type (format #f ",\"service_type\":\"~a\"" type) ""))
              (unfreeze-on-demand-json (if unfreeze-on-demand ",\"unfreeze_on_demand\":true" ""))
              (input-files-json (build-input-files-json input-files))
              (json (format #f "{\"name\":\"~a\"~a~a~a~a~a~a}" name ports-json bootstrap-json bootstrap-content-json type-json unfreeze-on-demand-json input-files-json))
              (response (curl-post api-key "/services" json))
              (service-id (json-extract-string response "id")))
         (format #t "~aService created~a\n" green reset)
         (display response)
         (newline)
         ;; Auto-set vault if env vars were provided
         (let ((env-content (build-env-content env-vars env-file)))
           (when (and service-id (> (string-length env-content) 0))
             (format #t "~aSetting vault for service...~a\n" yellow reset)
             (service-env-set api-key service-id env-content)))))
      (else
        (display "Error: --name required to create service, or use env subcommand\n" (current-error-port))
        (exit 1)))))

(define (image-cmd action id source-type visibility-mode name ports)
  (let ((api-key (get-api-key)))
    (cond
      ((equal? action "list")
       (let* ((response (curl-get api-key "/images"))
              (images (json-extract-array response "images")))
         (if (null? images)
             (display "No images found\n")
             (begin
               (format #t "~a~a ~a ~a ~a~a\n" blue "ID" "Name" "Visibility" "Created" reset)
               (display response)
               (newline)))))
      ((equal? action "info")
       (display (curl-get api-key (format #f "/images/~a" id)))
       (newline))
      ((equal? action "delete")
       (let ((result (curl-delete-with-sudo api-key (format #f "/images/~a" id))))
         (if (car result)
             (format #t "~aImage deleted successfully~a\n" green reset)
             (begin
               (format (current-error-port) "~aError deleting image~a\n" red reset)
               (exit 1)))))
      ((equal? action "lock")
       (curl-post api-key (format #f "/images/~a/lock" id) "{}")
       (format #t "~aImage locked successfully~a\n" green reset))
      ((equal? action "unlock")
       (let ((result (curl-post-with-sudo api-key (format #f "/images/~a/unlock" id) "{}")))
         (if (car result)
             (format #t "~aImage unlocked successfully~a\n" green reset)
             (begin
               (format (current-error-port) "~aError unlocking image~a\n" red reset)
               (exit 1)))))
      ((equal? action "publish")
       (if (not source-type)
           (begin
             (format (current-error-port) "~aError: --source-type required for --publish (service or snapshot)~a\n" red reset)
             (exit 1))
           (let* ((name-json (if name (format #f ",\"name\":\"~a\"" (escape-json name)) ""))
                  (json (format #f "{\"source_type\":\"~a\",\"source_id\":\"~a\"~a}" source-type id name-json))
                  (response (curl-post api-key "/images/publish" json))
                  (image-id (json-extract-string response "id")))
             (format #t "~aImage published successfully~a\n" green reset)
             (format #t "Image ID: ~a\n" (or image-id "N/A")))))
      ((equal? action "visibility")
       (if (not visibility-mode)
           (begin
             (format (current-error-port) "~aError: visibility mode required (private, unlisted, or public)~a\n" red reset)
             (exit 1))
           (let ((json (format #f "{\"visibility\":\"~a\"}" visibility-mode)))
             (curl-post api-key (format #f "/images/~a/visibility" id) json)
             (format #t "~aImage visibility set to ~a~a\n" green visibility-mode reset))))
      ((equal? action "spawn")
       (let* ((name-json (if name (format #f "\"name\":\"~a\"" (escape-json name)) ""))
              (ports-json (if ports (format #f "~a\"ports\":[~a]" (if name "," "") ports) ""))
              (json (format #f "{~a~a}" name-json ports-json))
              (response (curl-post api-key (format #f "/images/~a/spawn" id) json))
              (service-id (json-extract-string response "id")))
         (format #t "~aService spawned from image~a\n" green reset)
         (format #t "Service ID: ~a\n" (or service-id "N/A"))))
      ((equal? action "clone")
       (let* ((name-json (if name (format #f "\"name\":\"~a\"" (escape-json name)) ""))
              (json (format #f "{~a}" name-json))
              (response (curl-post api-key (format #f "/images/~a/clone" id) json))
              (image-id (json-extract-string response "id")))
         (format #t "~aImage cloned successfully~a\n" green reset)
         (format #t "Image ID: ~a\n" (or image-id "N/A"))))
      (else
        (display "Error: Specify --list, --info ID, --delete ID, --lock ID, --unlock ID, --publish ID, --visibility ID MODE, --spawn ID, or --clone ID\n" (current-error-port))
        (exit 1)))))

(define (parse-input-files args)
  "Parse -f flags from args and return list of filenames"
  (let loop ((args args) (files '()))
    (if (null? args)
        (reverse files)
        (if (and (equal? (car args) "-f") (pair? (cdr args)))
            (let ((file (cadr args)))
              (if (file-exists? file)
                  (loop (cddr args) (cons file files))
                  (begin
                    (format (current-error-port) "Error: File not found: ~a\n" file)
                    (exit 1))))
            (loop (cdr args) files)))))

(define (main args)
  (if (null? args)
      (begin
        (display "Usage: un.scm [options] <source_file>\n")
        (display "       un.scm session [options]\n")
        (display "       un.scm service [options]\n")
        (display "       un.scm image [options]\n")
        (display "       un.scm key [--extend]\n")
        (display "       un.scm languages [--json]\n")
        (display "\nLanguages options:\n")
        (display "  --json              Output as JSON array\n")
        (display "\nImage options:\n")
        (display "  --list              List all images\n")
        (display "  --info ID           Get image details\n")
        (display "  --delete ID         Delete an image\n")
        (display "  --lock ID           Lock image to prevent deletion\n")
        (display "  --unlock ID         Unlock image\n")
        (display "  --publish ID        Publish image from service/snapshot\n")
        (display "  --source-type TYPE  Source type: service or snapshot\n")
        (display "  --visibility ID MODE  Set visibility: private, unlisted, public\n")
        (display "  --spawn ID          Spawn new service from image\n")
        (display "  --clone ID          Clone an image\n")
        (display "  --name NAME         Name for spawned service or cloned image\n")
        (display "  --ports PORTS       Ports for spawned service\n")
        (exit 1))
      (cond
        ((equal? (car args) "languages")
         (let ((json-output (and (> (length args) 1) (equal? (cadr args) "--json"))))
           (languages-cmd json-output)))
        ((equal? (car args) "key")
         (let ((extend (and (> (length args) 1) (equal? (cadr args) "--extend"))))
           (validate-key-cmd extend)))
        ((equal? (car args) "image")
         (cond
           ((and (> (length args) 1) (equal? (cadr args) "--list"))
            (image-cmd "list" #f #f #f #f #f))
           ((and (> (length args) 2) (equal? (cadr args) "--info"))
            (image-cmd "info" (caddr args) #f #f #f #f))
           ((and (> (length args) 2) (equal? (cadr args) "--delete"))
            (image-cmd "delete" (caddr args) #f #f #f #f))
           ((and (> (length args) 2) (equal? (cadr args) "--lock"))
            (image-cmd "lock" (caddr args) #f #f #f #f))
           ((and (> (length args) 2) (equal? (cadr args) "--unlock"))
            (image-cmd "unlock" (caddr args) #f #f #f #f))
           ((and (> (length args) 2) (equal? (cadr args) "--publish"))
            (let* ((publish-id (caddr args))
                   (rest-args (cdddr args))
                   (source-type #f)
                   (name #f))
              (let loop ((args rest-args))
                (when (pair? args)
                  (cond
                    ((and (equal? (car args) "--source-type") (pair? (cdr args)))
                     (set! source-type (cadr args))
                     (loop (cddr args)))
                    ((and (equal? (car args) "--name") (pair? (cdr args)))
                     (set! name (cadr args))
                     (loop (cddr args)))
                    (else (loop (cdr args))))))
              (image-cmd "publish" publish-id source-type #f name #f)))
           ((and (> (length args) 3) (equal? (cadr args) "--visibility"))
            (image-cmd "visibility" (caddr args) #f (list-ref args 3) #f #f))
           ((and (> (length args) 2) (equal? (cadr args) "--spawn"))
            (let* ((spawn-id (caddr args))
                   (rest-args (cdddr args))
                   (name #f)
                   (ports #f))
              (let loop ((args rest-args))
                (when (pair? args)
                  (cond
                    ((and (equal? (car args) "--name") (pair? (cdr args)))
                     (set! name (cadr args))
                     (loop (cddr args)))
                    ((and (equal? (car args) "--ports") (pair? (cdr args)))
                     (set! ports (cadr args))
                     (loop (cddr args)))
                    (else (loop (cdr args))))))
              (image-cmd "spawn" spawn-id #f #f name ports)))
           ((and (> (length args) 2) (equal? (cadr args) "--clone"))
            (let* ((clone-id (caddr args))
                   (rest-args (cdddr args))
                   (name #f))
              (let loop ((args rest-args))
                (when (pair? args)
                  (cond
                    ((and (equal? (car args) "--name") (pair? (cdr args)))
                     (set! name (cadr args))
                     (loop (cddr args)))
                    (else (loop (cdr args))))))
              (image-cmd "clone" clone-id #f #f name #f)))
           (else
             (display "Error: Invalid image command\n" (current-error-port))
             (exit 1))))
        ((equal? (car args) "session")
         (if (and (> (length args) 1) (equal? (cadr args) "--list"))
             (session-cmd "list" #f #f '())
             (if (and (> (length args) 2) (equal? (cadr args) "--kill"))
                 (session-cmd "kill" (caddr args) #f '())
                 ;; Parse session create options including -f
                 (let* ((rest-args (cdr args))
                        (input-files (parse-input-files rest-args))
                        (shell #f))
                   ;; Parse --shell option
                   (let loop ((args rest-args))
                     (when (pair? args)
                       (cond
                         ((and (or (equal? (car args) "--shell") (equal? (car args) "-s")) (pair? (cdr args)))
                          (set! shell (cadr args))
                          (loop (cddr args)))
                         ((equal? (car args) "-f")
                          (loop (cdr args)))  ; skip -f, already parsed
                         ((and (string? (car args)) (> (string-length (car args)) 0) (char=? (string-ref (car args) 0) #\-))
                          (format (current-error-port) "~aUnknown option: ~a~a\n" red (car args) reset)
                          (format (current-error-port) "Usage: un.scm session [options]\n")
                          (exit 1))
                         (else (loop (cdr args))))))
                   (session-cmd "create" #f shell input-files)))))
        ((equal? (car args) "service")
         (cond
           ((and (> (length args) 1) (equal? (cadr args) "--list"))
            (service-cmd "list" #f #f #f #f #f #f '() '() #f #f))
           ((and (> (length args) 2) (equal? (cadr args) "--info"))
            (service-cmd "info" (caddr args) #f #f #f #f #f '() '() #f #f))
           ((and (> (length args) 2) (equal? (cadr args) "--logs"))
            (service-cmd "logs" (caddr args) #f #f #f #f #f '() '() #f #f))
           ((and (> (length args) 2) (equal? (cadr args) "--freeze"))
            (service-cmd "sleep" (caddr args) #f #f #f #f #f '() '() #f #f))
           ((and (> (length args) 2) (equal? (cadr args) "--unfreeze"))
            (service-cmd "wake" (caddr args) #f #f #f #f #f '() '() #f #f))
           ((and (> (length args) 2) (equal? (cadr args) "--destroy"))
            (service-cmd "destroy" (caddr args) #f #f #f #f #f '() '() #f #f))
           ((and (> (length args) 2) (equal? (cadr args) "--resize"))
            ;; Parse --resize ID -v N
            (let* ((resize-id (caddr args))
                   (rest-args (cdddr args))
                   (vcpu-val #f))
              ;; Look for -v or --vcpu
              (let loop ((args rest-args))
                (when (pair? args)
                  (cond
                    ((and (or (equal? (car args) "-v") (equal? (car args) "--vcpu")) (pair? (cdr args)))
                     (set! vcpu-val (string->number (cadr args)))
                     (loop (cddr args)))
                    (else (loop (cdr args))))))
              (service-cmd "resize" resize-id #f #f #f #f #f '() '() #f vcpu-val)))
           ((and (> (length args) 3) (equal? (cadr args) "--set-unfreeze-on-demand"))
            ;; Parse --set-unfreeze-on-demand ID true/false
            (let* ((service-id (caddr args))
                   (enabled-val (list-ref args 3)))
              (service-cmd "set-unfreeze-on-demand" service-id #f #f enabled-val #f #f '() '() #f #f)))
           ((and (> (length args) 3) (equal? (cadr args) "--execute"))
            (service-cmd "execute" (caddr args) #f #f (list-ref args 3) #f #f '() '() #f #f))
           ((and (> (length args) 3) (equal? (cadr args) "--dump-bootstrap"))
            (service-cmd "dump-bootstrap" (caddr args) #f #f #f #f (list-ref args 3) '() '() #f #f))
           ((and (> (length args) 2) (equal? (cadr args) "--dump-bootstrap"))
            (service-cmd "dump-bootstrap" (caddr args) #f #f #f #f #f '() '() #f #f))
           ;; Service env subcommand: service env <action> <id> [options]
           ((and (> (length args) 1) (equal? (cadr args) "env"))
            (if (< (length args) 4)
                (begin
                  (display "Usage: un.scm service env <status|set|export|delete> <service_id> [options]\n" (current-error-port))
                  (exit 1))
                (let* ((env-action (caddr args))
                       (service-id (list-ref args 3))
                       (rest-args (if (> (length args) 4) (list-tail args 4) '())))
                  (cond
                    ((equal? env-action "status")
                     (service-cmd "env-status" service-id #f #f #f #f #f '() '() #f #f))
                    ((equal? env-action "set")
                     ;; Parse -e and --env-file from rest-args
                     (let loop ((args rest-args) (env-vars '()) (env-file #f))
                       (if (null? args)
                           (service-cmd "env-set" service-id #f #f #f #f #f '() env-vars env-file #f)
                           (cond
                             ((and (equal? (car args) "-e") (pair? (cdr args)))
                              (loop (cddr args) (cons (cadr args) env-vars) env-file))
                             ((and (equal? (car args) "--env-file") (pair? (cdr args)))
                              (loop (cddr args) env-vars (cadr args)))
                             (else (loop (cdr args) env-vars env-file))))))
                    ((equal? env-action "export")
                     (service-cmd "env-export" service-id #f #f #f #f #f '() '() #f #f))
                    ((equal? env-action "delete")
                     (service-cmd "env-delete" service-id #f #f #f #f #f '() '() #f #f))
                    (else
                      (format (current-error-port) "~aUnknown env action: ~a~a\n" red env-action reset)
                      (exit 1))))))
           ((and (> (length args) 2) (equal? (cadr args) "--name"))
            (let* ((name (caddr args))
                   (rest-args (cdddr args))
                   (ports #f)
                   (bootstrap #f)
                   (bootstrap-file #f)
                   (type #f)
                   (env-vars '())
                   (env-file #f)
                   (unfreeze-on-demand-flag #f)
                   (input-files (parse-input-files rest-args)))
              ;; Parse remaining args
              (let loop ((args rest-args))
                (when (pair? args)
                  (cond
                    ((and (pair? (cdr args)) (equal? (car args) "--ports"))
                     (set! ports (cadr args))
                     (loop (cddr args)))
                    ((and (pair? (cdr args)) (equal? (car args) "--bootstrap"))
                     (set! bootstrap (cadr args))
                     (loop (cddr args)))
                    ((and (pair? (cdr args)) (equal? (car args) "--bootstrap-file"))
                     (set! bootstrap-file (cadr args))
                     (loop (cddr args)))
                    ((and (pair? (cdr args)) (equal? (car args) "--type"))
                     (set! type (cadr args))
                     (loop (cddr args)))
                    ((and (pair? (cdr args)) (equal? (car args) "-e"))
                     (set! env-vars (cons (cadr args) env-vars))
                     (loop (cddr args)))
                    ((and (pair? (cdr args)) (equal? (car args) "--env-file"))
                     (set! env-file (cadr args))
                     (loop (cddr args)))
                    ((equal? (car args) "--unfreeze-on-demand")
                     (set! unfreeze-on-demand-flag #t)
                     (loop (cdr args)))
                    ((equal? (car args) "-f")
                     (loop (cddr args))) ; skip -f, already parsed
                    (else (loop (cdr args))))))
              (service-cmd "create" #f name ports bootstrap bootstrap-file type input-files env-vars env-file #f unfreeze-on-demand-flag)))
           (else
             (display "Error: Invalid service command\n" (current-error-port))
             (exit 1))))
        (else
          (execute-cmd (car args))))))

(main (cdr (command-line)))
