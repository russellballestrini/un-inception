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
             (ice-9 regex)
             (ice-9 format)
             (rnrs bytevectors))

(define blue "\x1b[34m")
(define red "\x1b[31m")
(define green "\x1b[32m")
(define yellow "\x1b[33m")
(define reset "\x1b[0m")

(define api-base
  ;; UNSANDBOX_API_URL points every request at staging or a local endpoint
  (let ((url (or (getenv "UNSANDBOX_API_URL") "https://api.unsandbox.com")))
    (string-trim-right url #\/)))

(define portal-base
  ;; UNSANDBOX_PORTAL_URL: portal for /logs/{source} and /keys/extend
  (let ((url (getenv "UNSANDBOX_PORTAL_URL")))
    (string-trim-right (if (and url (> (string-length url) 0)) url "https://unsandbox.com") #\/)))

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
         (cmd (string-append "curl -s -X POST " api-base endpoint
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
         (cmd (string-append "curl -s " api-base endpoint
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
         (cmd (string-append "curl -s -X DELETE " api-base endpoint
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
                                " " api-base endpoint
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
         (cmd (string-append "curl -s -w '\\n%{http_code}' -X DELETE " api-base endpoint
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
         (cmd (string-append "curl -s -w '\\n%{http_code}' -X POST " api-base endpoint
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
         (cmd (string-append "curl -s -X PATCH " api-base endpoint
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
         (cmd (string-append "curl -s -X PUT " api-base endpoint
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

(define *account-index* #f)

(define (load-accounts-csv path index)
  "Load row INDEX from a CSV file of public_key,secret_key pairs.
   Skips blank lines and lines starting with #. Returns (list pk sk) or #f."
  (if (not (access? path R_OK))
      #f
      (catch #t
        (lambda ()
          (let ((port (open-input-file path))
                (rows '()))
            (let loop ()
              (let ((line (read-line port)))
                (when (not (eof-object? line))
                  (let ((trimmed (string-trim-both line)))
                    (when (and (> (string-length trimmed) 0)
                               (not (char=? (string-ref trimmed 0) #\#)))
                      ;; fields: public_key,secret_key[,label...]
                      (let ((fields (string-split trimmed #\,)))
                        (when (>= (length fields) 2)
                          (set! rows (append rows
                                             (list (list
                                                    (string-trim-both (car fields))
                                                    (string-trim-both (cadr fields))))))))))
                  (loop))))
            (close-input-port port)
            (if (< index (length rows))
                (list-ref rows index)
                #f)))
        (lambda (key . args) #f))))

(define (get-api-keys)
  (let ((public-key (getenv "UNSANDBOX_PUBLIC_KEY"))
        (secret-key (getenv "UNSANDBOX_SECRET_KEY"))
        (api-key (getenv "UNSANDBOX_API_KEY"))
        (home (getenv "HOME")))
    (cond
      ;; --account N: load row N from accounts.csv, bypasses env vars
      ((not (eq? *account-index* #f))
       (let ((result (or (load-accounts-csv
                           (string-append home "/.unsandbox/accounts.csv")
                           *account-index*)
                         (load-accounts-csv "./accounts.csv" *account-index*))))
         (or result
             (begin
               (format (current-error-port) "Error: account ~a not found in accounts.csv\n" *account-index*)
               (exit 1)))))
      ;; env vars
      ((and public-key secret-key) (list public-key secret-key))
      (api-key (list api-key #f))
      ;; accounts.csv fallback (row 0 or UNSANDBOX_ACCOUNT env var)
      (else
       (let* ((acc-env (getenv "UNSANDBOX_ACCOUNT"))
              (row-idx (if acc-env
                           (catch #t
                             (lambda () (string->number acc-env))
                             (lambda (key . args) 0))
                           0))
              (row-idx (if (number? row-idx) (inexact->exact row-idx) 0))
              (result (or (load-accounts-csv
                            (string-append home "/.unsandbox/accounts.csv")
                            row-idx)
                          (load-accounts-csv "./accounts.csv" row-idx))))
         (or result
             (begin
               (display "Error: UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY not set (or UNSANDBOX_API_KEY for backwards compat)\n" (current-error-port))
               (exit 1))))))))

(define (get-api-key)
  (car (get-api-keys)))

;; SHA-256 in pure Scheme (FIPS 180-4). Signing stays in-process, so no
;; subprocess ever sees the secret on its command line.
(define sha256-k
  #(#x428a2f98 #x71374491 #xb5c0fbcf #xe9b5dba5 #x3956c25b #x59f111f1 #x923f82a4 #xab1c5ed5
    #xd807aa98 #x12835b01 #x243185be #x550c7dc3 #x72be5d74 #x80deb1fe #x9bdc06a7 #xc19bf174
    #xe49b69c1 #xefbe4786 #x0fc19dc6 #x240ca1cc #x2de92c6f #x4a7484aa #x5cb0a9dc #x76f988da
    #x983e5152 #xa831c66d #xb00327c8 #xbf597fc7 #xc6e00bf3 #xd5a79147 #x06ca6351 #x14292967
    #x27b70a85 #x2e1b2138 #x4d2c6dfc #x53380d13 #x650a7354 #x766a0abb #x81c2c92e #x92722c85
    #xa2bfe8a1 #xa81a664b #xc24b8b70 #xc76c51a3 #xd192e819 #xd6990624 #xf40e3585 #x106aa070
    #x19a4c116 #x1e376c08 #x2748774c #x34b0bcb5 #x391c0cb3 #x4ed8aa4a #x5b9cca4f #x682e6ff3
    #x748f82ee #x78a5636f #x84c87814 #x8cc70208 #x90befffa #xa4506ceb #xbef9a3f7 #xc67178f2))

(define (sha256-bytes msg)
  "SHA-256 of bytevector MSG; returns the 32-byte raw digest as a bytevector."
  (define (u32 x) (logand x #xFFFFFFFF))
  (define (rotr x n) (u32 (logior (ash x (- n)) (ash x (- 32 n)))))
  (let* ((len (bytevector-length msg))
         (r (modulo (+ len 9) 64))
         (total (+ len 9 (if (= r 0) 0 (- 64 r))))
         (buf (make-bytevector total 0))
         (h (vector #x6a09e667 #xbb67ae85 #x3c6ef372 #xa54ff53a
                    #x510e527f #x9b05688c #x1f83d9ab #x5be0cd19))
         (w (make-vector 64 0)))
    (bytevector-copy! msg 0 buf 0 len)
    (bytevector-u8-set! buf len #x80)
    (bytevector-u64-set! buf (- total 8) (* len 8) (endianness big))
    (do ((off 0 (+ off 64))) ((>= off total))
      (do ((t 0 (+ t 1))) ((= t 16))
        (vector-set! w t (bytevector-u32-ref buf (+ off (* 4 t)) (endianness big))))
      (do ((t 16 (+ t 1))) ((= t 64))
        (let* ((x (vector-ref w (- t 15)))
               (y (vector-ref w (- t 2)))
               (s0 (logxor (rotr x 7) (rotr x 18) (ash x -3)))
               (s1 (logxor (rotr y 17) (rotr y 19) (ash y -10))))
          (vector-set! w t (u32 (+ (vector-ref w (- t 16)) s0 (vector-ref w (- t 7)) s1)))))
      (let loop ((t 0)
                 (a (vector-ref h 0)) (b (vector-ref h 1)) (c (vector-ref h 2)) (d (vector-ref h 3))
                 (e (vector-ref h 4)) (f (vector-ref h 5)) (g (vector-ref h 6)) (hh (vector-ref h 7)))
        (if (< t 64)
            (let* ((s1 (logxor (rotr e 6) (rotr e 11) (rotr e 25)))
                   (ch (logxor (logand e f) (logand (u32 (lognot e)) g)))
                   (t1 (u32 (+ hh s1 ch (vector-ref sha256-k t) (vector-ref w t))))
                   (s0 (logxor (rotr a 2) (rotr a 13) (rotr a 22)))
                   (maj (logxor (logand a b) (logand a c) (logand b c)))
                   (t2 (u32 (+ s0 maj))))
              (loop (+ t 1) (u32 (+ t1 t2)) a b c (u32 (+ d t1)) e f g))
            (for-each (lambda (i v) (vector-set! h i (u32 (+ (vector-ref h i) v))))
                      '(0 1 2 3 4 5 6 7) (list a b c d e f g hh)))))
    (let ((out (make-bytevector 32 0)))
      (do ((i 0 (+ i 1))) ((= i 8) out)
        (bytevector-u32-set! out (* 4 i) (vector-ref h i) (endianness big))))))

(define (sha256-bv-append a b)
  (let ((out (make-bytevector (+ (bytevector-length a) (bytevector-length b)))))
    (bytevector-copy! a 0 out 0 (bytevector-length a))
    (bytevector-copy! b 0 out (bytevector-length a) (bytevector-length b))
    out))

(define (hmac-sha256 secret message)
  "Compute HMAC-SHA256 (RFC 2104) in-process; returns lowercase hex."
  (let* ((k0 (string->utf8 secret))
         (k1 (if (> (bytevector-length k0) 64) (sha256-bytes k0) k0))
         (key (make-bytevector 64 0))
         (pad-key (lambda (pad)
                    (let ((out (make-bytevector 64 0)))
                      (do ((i 0 (+ i 1))) ((= i 64) out)
                        (bytevector-u8-set! out i (logxor pad (bytevector-u8-ref key i))))))))
    (bytevector-copy! k1 0 key 0 (bytevector-length k1))
    (let* ((inner (sha256-bytes (sha256-bv-append (pad-key #x36) (string->utf8 message))))
           (mac (sha256-bytes (sha256-bv-append (pad-key #x5c) inner))))
      (apply string-append
             (map (lambda (b) (string-pad (number->string b 16) 2 #\0))
                  (bytevector->u8-list mac))))))

(define (make-signature secret-key timestamp method path body)
  ;; The server verifies against the path without its query string
  ;; (conn.request_path), so sign only the part before '?'.
  (let ((message (format #f "~a:~a:~a:~a" timestamp method
                         (car (string-split path #\?)) body)))
    (hmac-sha256 secret-key message)))

(define (build-auth-headers public-key secret-key method path body)
  (if secret-key
      (let* ((timestamp (number->string (quotient (current-time) 1)))
             (signature (make-signature secret-key timestamp method path body)))
        ;; quoted: callers join these into a shell command line
        (list "-H" (format #f "'Authorization: Bearer ~a'" public-key)
              "-H" (format #f "'X-Timestamp: ~a'" timestamp)
              "-H" (format #f "'X-Signature: ~a'" signature)))
      (list "-H" (format #f "'Authorization: Bearer ~a'" public-key))))

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

(define (key-json-string json key)
  (let ((m (string-match (string-append "\"" key "\"[ \t\r\n]*:[ \t\r\n]*\"([^\"]*)\"") json)))
    (and m (match:substring m 1))))

(define (key-json-int json key)
  (let ((m (string-match (string-append "\"" key "\"[ \t\r\n]*:[ \t\r\n]*(-?[0-9]+)") json)))
    (and m (string->number (match:substring m 1)))))

(define (key-validate-post public-key secret-key)
  "POST /keys/validate, empty body, signed. Headers via a 0600 file. Returns (body . http-code)."
  (let* ((path "/keys/validate")
         (timestamp (number->string (current-time)))
         (hdr-file (format #f "/tmp/un_scm_key_~a_~a" (getpid) (random 999999)))
         (headers (string-append
                   "Authorization: Bearer " public-key "\n"
                   "Content-Type: application/json\n"
                   (if secret-key
                       (string-append "X-Timestamp: " timestamp "\n"
                                      "X-Signature: " (make-signature secret-key timestamp "POST" path "") "\n")
                       ""))))
    (write-private-file hdr-file headers)
    (let* ((port (open-pipe* OPEN_READ "curl" "-s" "-w" "\n%{http_code}" "-X" "POST"
                             (string-append api-base path)
                             "-H" (string-append "@" hdr-file)))
           (output (read-file-port port)))
      (close-pipe port)
      (delete-file hdr-file)
      (parse-http-response-with-code output))))

(define (validate-key-cmd extend)
  (let* ((keys (get-api-keys))
         (public-key (car keys))
         (secret-key (cadr keys)))
    (if extend
        (begin
          (format #t "~aOpening browser to extend key...~a\n" blue reset)
          (open-browser (format #f "~a/keys/extend?pk=~a" portal-base public-key)))
        (let* ((result (key-validate-post public-key secret-key))
               (response (car result))
               (code (cdr result))
               (state #f)
               (reason #f))
          (cond
           ((= code 200)
            (set! state (or (key-json-string response "status")
                            (if (string-match "\"valid\"[ \t]*:[ \t]*true" response) "active" "invalid")))
            (set! reason (string-append "status " state)))
           ((= code 401)
            (let ((err (or (key-json-string response "error") "unauthorized")))
              (set! reason err)
              (set! state (cond ((member err '(":expired" "expired")) "expired")
                                ((member err '(":suspended" "suspended")) "suspended")
                                (else "invalid")))))
           ((= code 0)
            (format (current-error-port) "~aError: request to ~a/keys/validate failed~a\n" red api-base reset)
            (exit 1))
           (else
            (format (current-error-port) "~aError: HTTP ~a ~a~a\n" red code
                    (or (key-json-string response "error") (key-json-string response "message") "") reset)
            (exit 4)))
          (cond
           ((string=? state "active")
            (let* ((tier (key-json-int response "tier"))
                   (status (key-json-string response "status"))
                   (vt (key-json-int response "valid_through"))
                   (secs (and vt (quotient vt 1000)))
                   (left (and secs (- secs (current-time))))
                   (expires (if secs (strftime "%Y-%m-%dT%H:%M:%SZ" (gmtime secs)) "N/A"))
                   (remaining (cond ((not left) "N/A")
                                    ((<= left 0) "expired")
                                    (else (format #f "~ad ~ah ~am" (quotient left 86400)
                                                  (quotient (remainder left 86400) 3600)
                                                  (quotient (remainder left 3600) 60))))))
              (format #t "~aValid~a\n" green reset)
              (format #t "Public Key: ~a\n" public-key)
              (format #t "Tier: ~a\n" (or tier "N/A"))
              (format #t "Status: ~a\n" (or status "N/A"))
              (format #t "Expires: ~a\n" expires)
              (format #t "Time Remaining: ~a\n" remaining)))
           ((string=? state "expired")
            (format #t "~aExpired~a\n" red reset)
            (format #t "Public Key: ~a\n" public-key)
            (format #t "~aTo renew: Visit ~a/keys/extend~a\n" yellow portal-base reset)
            (exit 3))
           ((string=? state "suspended")
            (format #t "~aSuspended~a\n" red reset)
            (format #t "Public Key: ~a\n" public-key)
            (exit 3))
           (else
            (format #t "~aInvalid~a\n" red reset)
            (format #t "Public Key: ~a\n" public-key)
            (format #t "Reason: ~a\n" reason)
            (exit 3)))))))

(define (execute-cmd file)
  (let* ((api-key (get-api-key))
         (ext (get-extension file))
         (language (assoc-ref ext-map ext)))
    (when (not language)
      (format (current-error-port) "Error: Unknown extension: ~a\n" ext)
      (exit 1))
    (execute-code api-key language (read-file file))))

;; Shared by file and inline (un.scm -s LANG 'CODE', CLI_SPEC.md -s/--shell)
(define (execute-code api-key language code)
  (let* ((json (format #f "{\"language\":\"~a\",\"code\":\"~a\"~a}"
                       language (escape-json code) (os-json)))
         (response (curl-post api-key "/execute" json)))
    (display response)
    (newline)))

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
               (json (format #f "{\"shell\":\"~a\"~a~a}" sh input-files-json (os-json)))
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
              (json (format #f "{\"name\":\"~a\"~a~a~a~a~a~a~a}" name ports-json bootstrap-json bootstrap-content-json type-json unfreeze-on-demand-json input-files-json (os-json)))
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

;; Image access management functions
(define (image-grant-access id trusted-key)
  (let* ((api-key (get-api-key))
         (json (format #f "{\"trusted_api_key\":\"~a\"}" trusted-key)))
    (curl-post api-key (format #f "/images/~a/grant-access" id) json)
    (format #t "~aAccess granted to: ~a~a\n" green trusted-key reset)))

(define (image-revoke-access id trusted-key)
  (let* ((api-key (get-api-key))
         (json (format #f "{\"trusted_api_key\":\"~a\"}" trusted-key)))
    (curl-post api-key (format #f "/images/~a/revoke-access" id) json)
    (format #t "~aAccess revoked from: ~a~a\n" green trusted-key reset)))

(define (image-list-trusted id)
  (let ((api-key (get-api-key)))
    (display (curl-get api-key (format #f "/images/~a/trusted" id)))
    (newline)))

(define (image-transfer id to-key)
  (let* ((api-key (get-api-key))
         (json (format #f "{\"to_api_key\":\"~a\"}" to-key)))
    (curl-post api-key (format #f "/images/~a/transfer" id) json)
    (format #t "~aImage transferred to: ~a~a\n" green to-key reset)))

;; Snapshot functions
(define (snapshot-list)
  (let ((api-key (get-api-key)))
    (display (curl-get api-key "/snapshots"))
    (newline)))

(define (snapshot-info id)
  (let ((api-key (get-api-key)))
    (display (curl-get api-key (format #f "/snapshots/~a" id)))
    (newline)))

(define (snapshot-session session-id name hot)
  (let* ((api-key (get-api-key))
         (name-json (if name (format #f ",\"name\":\"~a\"" (escape-json name)) ""))
         (hot-json (if hot ",\"hot\":true" ""))
         (json (format #f "{\"session_id\":\"~a\"~a~a}" session-id name-json hot-json)))
    (format #t "~aSnapshot created~a\n" green reset)
    (display (curl-post api-key "/snapshots" json))
    (newline)))

(define (snapshot-service-create service-id name hot)
  (let* ((api-key (get-api-key))
         (name-json (if name (format #f ",\"name\":\"~a\"" (escape-json name)) ""))
         (hot-json (if hot ",\"hot\":true" ""))
         (json (format #f "{\"service_id\":\"~a\"~a~a}" service-id name-json hot-json)))
    (format #t "~aSnapshot created~a\n" green reset)
    (display (curl-post api-key "/snapshots" json))
    (newline)))

(define (snapshot-restore id)
  (let ((api-key (get-api-key)))
    (curl-post api-key (format #f "/snapshots/~a/restore" id) "{}")
    (format #t "~aSnapshot restored: ~a~a\n" green id reset)))

(define (snapshot-delete id)
  (let* ((api-key (get-api-key))
         (result (curl-delete-with-sudo api-key (format #f "/snapshots/~a" id))))
    (if (car result)
        (format #t "~aSnapshot deleted: ~a~a\n" green id reset)
        (begin
          (format (current-error-port) "~aError deleting snapshot~a\n" red reset)
          (exit 1)))))

(define (snapshot-lock id)
  (let ((api-key (get-api-key)))
    (curl-post api-key (format #f "/snapshots/~a/lock" id) "{}")
    (format #t "~aSnapshot locked: ~a~a\n" green id reset)))

(define (snapshot-unlock id)
  (let* ((api-key (get-api-key))
         (result (curl-post-with-sudo api-key (format #f "/snapshots/~a/unlock" id) "{}")))
    (if (car result)
        (format #t "~aSnapshot unlocked: ~a~a\n" green id reset)
        (begin
          (format (current-error-port) "~aError unlocking snapshot~a\n" red reset)
          (exit 1)))))

(define (snapshot-clone id clone-type name ports shell)
  (let* ((api-key (get-api-key))
         (type-json (format #f "\"clone_type\":\"~a\"" clone-type))
         (name-json (if name (format #f ",\"name\":\"~a\"" (escape-json name)) ""))
         (ports-json (if ports (format #f ",\"ports\":[~a]" ports) ""))
         (shell-json (if shell (format #f ",\"shell\":\"~a\"" shell) ""))
         (json (format #f "{~a~a~a~a}" type-json name-json ports-json shell-json)))
    (format #t "~aSnapshot cloned~a\n" green reset)
    (display (curl-post api-key (format #f "/snapshots/~a/clone" id) json))
    (newline)))

(define (snapshot-cmd action id name ports shell hot)
  (cond
    ((equal? action "list") (snapshot-list))
    ((equal? action "info") (snapshot-info id))
    ((equal? action "session") (snapshot-session id name hot))
    ((equal? action "service") (snapshot-service-create id name hot))
    ((equal? action "restore") (snapshot-restore id))
    ((equal? action "delete") (snapshot-delete id))
    ((equal? action "lock") (snapshot-lock id))
    ((equal? action "unlock") (snapshot-unlock id))
    ((equal? action "clone") (snapshot-clone id "session" name ports shell))
    (else
      (display "Error: Unknown snapshot action\n" (current-error-port))
      (exit 1))))

;; Session additional functions
(define (session-info id)
  (let ((api-key (get-api-key)))
    (display (curl-get api-key (format #f "/sessions/~a" id)))
    (newline)))

(define (session-boost id vcpu)
  (let* ((api-key (get-api-key))
         (json (format #f "{\"vcpu\":~a}" vcpu)))
    (curl-patch api-key (format #f "/sessions/~a" id) json)
    (format #t "~aSession boosted to ~a vCPU~a\n" green vcpu reset)))

(define (session-unboost id)
  (let* ((api-key (get-api-key))
         (json "{\"vcpu\":1}"))
    (curl-patch api-key (format #f "/sessions/~a" id) json)
    (format #t "~aSession unboosted to 1 vCPU~a\n" green reset)))

(define (session-execute id command)
  (let* ((api-key (get-api-key))
         (json (format #f "{\"command\":\"~a\"}" (escape-json command)))
         (response (curl-post api-key (format #f "/sessions/~a/execute" id) json))
         (stdout-val (json-extract-string response "stdout")))
    (when stdout-val
      (display (format #f "~a~a~a" blue stdout-val reset)))))

;; Service additional functions
(define (service-lock id)
  (let ((api-key (get-api-key)))
    (curl-post api-key (format #f "/services/~a/lock" id) "{}")
    (format #t "~aService locked: ~a~a\n" green id reset)))

(define (service-unlock id)
  (let* ((api-key (get-api-key))
         (result (curl-post-with-sudo api-key (format #f "/services/~a/unlock" id) "{}")))
    (if (car result)
        (format #t "~aService unlocked: ~a~a\n" green id reset)
        (begin
          (format (current-error-port) "~aError unlocking service~a\n" red reset)
          (exit 1)))))

(define (service-redeploy id bootstrap)
  (let* ((api-key (get-api-key))
         (json (if bootstrap
                   (format #f "{\"bootstrap\":\"~a\"}" (escape-json bootstrap))
                   "{}")))
    (curl-post api-key (format #f "/services/~a/redeploy" id) json)
    (format #t "~aService redeploying: ~a~a\n" green id reset)))

;; PaaS logs: GET {portal}/logs/{source}[/stream], HMAC over the path only.
;; Partner keys with log access; anything else answers 401/403.
(define (url-encode str)
  (apply string-append
         (map (lambda (b)
                (let ((c (integer->char b)))
                  (if (and (< b 128) (or (char-alphabetic? c) (char-numeric? c) (memv c '(#\- #\_ #\. #\~))))
                      (string c)
                      (string-upcase (format #f "%~2,'0x" b)))))
              (bytevector->u8-list (string->utf8 str)))))

(define (logs-query base params)
  ;; params: list of (key . value); empty/#f values are skipped
  (let loop ((ps params) (url base) (sep "?"))
    (cond ((null? ps) url)
          ((and (cdar ps) (> (string-length (cdar ps)) 0))
           (loop (cdr ps) (string-append url sep (caar ps) "=" (url-encode (cdar ps))) "&"))
          (else (loop (cdr ps) url sep)))))

;; Minimal JSON reader: objects -> (obj (key . val) ...), arrays -> vector,
;; strings, numbers, #t/#f/'null.
(define (json-parse str)
  (define pos 0)
  (define len (string-length str))
  (define (peek) (if (< pos len) (string-ref str pos) #\nul))
  (define (skip-ws) (let loop () (when (and (< pos len) (char-whitespace? (peek))) (set! pos (+ pos 1)) (loop))))
  (define (parse-string)
    (set! pos (+ pos 1))
    (let loop ((acc '()))
      (let ((c (peek)))
        (set! pos (+ pos 1))
        (cond ((>= (- pos 1) len) (list->string (reverse acc)))
              ((char=? c #\") (list->string (reverse acc)))
              ((char=? c #\\)
               (let ((e (peek)))
                 (set! pos (+ pos 1))
                 (case e
                   ((#\n) (loop (cons #\newline acc)))
                   ((#\t) (loop (cons #\tab acc)))
                   ((#\r) (loop (cons #\return acc)))
                   ((#\b) (loop (cons #\backspace acc)))
                   ((#\f) (loop (cons #\page acc)))
                   ((#\u) (let ((n (string->number (substring str pos (min len (+ pos 4))) 16)))
                            (set! pos (+ pos 4))
                            (loop (cons (integer->char (or n 63)) acc))))
                   (else (loop (cons e acc))))))
              (else (loop (cons c acc)))))))
  (define (parse-value)
    (skip-ws)
    (let ((c (peek)))
      (cond
        ((char=? c #\{)
         (set! pos (+ pos 1))
         (let loop ((acc '()))
           (skip-ws)
           (cond ((char=? (peek) #\}) (set! pos (+ pos 1)) (cons 'obj (reverse acc)))
                 ((char=? (peek) #\,) (set! pos (+ pos 1)) (loop acc))
                 ((char=? (peek) #\")
                  (let ((k (parse-string)))
                    (skip-ws) (set! pos (+ pos 1)) ; colon
                    (loop (cons (cons k (parse-value)) acc))))
                 (else (set! pos len) (cons 'obj (reverse acc))))))
        ((char=? c #\[)
         (set! pos (+ pos 1))
         (let loop ((acc '()))
           (skip-ws)
           (cond ((char=? (peek) #\]) (set! pos (+ pos 1)) (list->vector (reverse acc)))
                 ((char=? (peek) #\,) (set! pos (+ pos 1)) (loop acc))
                 ((>= pos len) (list->vector (reverse acc)))
                 (else (loop (cons (parse-value) acc))))))
        ((char=? c #\") (parse-string))
        (else
         (let loop ((end pos))
           (if (and (< end len) (not (memv (string-ref str end) '(#\, #\} #\] #\space #\newline))))
               (loop (+ end 1))
               (let ((tok (substring str pos end)))
                 (set! pos end)
                 (cond ((string=? tok "true") #t) ((string=? tok "false") #f)
                       ((string=? tok "null") 'null) (else (or (string->number tok) tok))))))))))
  (catch #t (lambda () (parse-value)) (lambda args #f)))

(define (json-get obj key)
  (and (pair? obj) (eq? (car obj) 'obj)
       (let ((p (assoc key (cdr obj)))) (and p (cdr p)))))

(define (log-cursor-path)
  (string-append (or (getenv "HOME") ".") "/.unsandbox/paas_log_cursor"))

(define (read-log-cursor)
  ;; seconds since our last successful fetch as "Ns" (min 5), or #f
  (catch #t
    (lambda ()
      (let* ((line (call-with-input-file (log-cursor-path) read-line))
             (ts (and (string? line) (string->number (string-trim-both line)))))
        (and ts (> ts 0) (format #f "~as" (max 5 (- (current-time) ts))))))
    (lambda args #f)))

(define (write-log-cursor)
  (catch #t
    (lambda ()
      (let ((dir (string-append (or (getenv "HOME") ".") "/.unsandbox")))
        (unless (file-exists? dir) (mkdir dir #o700))
        (call-with-output-file (log-cursor-path)
          (lambda (port) (format port "~a\n" (current-time))))))
    (lambda args #f)))

(define (logs-curl-args public-key secret-key path url stream?)
  ;; argv for curl (no shell); the secret only signs, it never reaches argv
  (append (list "curl" "-s" "-A" "un-cli/2.0")
          (if stream? (list "-N" "--http1.1" "-H" "Accept: text/event-stream") '())
          (list "-w" "\n__UN_HTTP__%{http_code}\n")
          (map (lambda (h) (string-trim-both h #\')) (build-auth-headers public-key secret-key "GET" path ""))
          (list url)))

(define (logs-print-line source line)
  (if (and (string? source) (> (string-length source) 0))
      (format #t "~12a ~a\n" source line)
      (format #t "~a\n" line))
  (force-output (current-output-port)))

(define (logs-error-text body)
  (let* ((j (json-parse body)) (e (json-get j "error")))
    (if (string? e) (string-append " — " e) "")))

(define (paas-logs-fetch public-key secret-key source lines since grep-pattern level json-output)
  "Batch fetch; prints lines (or raw JSON). Returns 0 or 1."
  (let* ((path (string-append "/logs/" source))
         (url (logs-query (string-append portal-base path)
                          (list (cons "lines" (number->string lines)) (cons "since" since)
                                (cons "grep" grep-pattern) (cons "level" level))))
         (port (apply open-pipe* OPEN_READ (logs-curl-args public-key secret-key path url #f)))
         (out (read-file-port port))
         (st (close-pipe port))
         (mark (string-contains out "\n__UN_HTTP__"))
         (body (if mark (substring out 0 mark) out))
         (code (if mark (or (string->number (string-trim-both (substring out (+ mark 12)))) 0) 0)))
    (cond
      ((= code 0)
       (format (current-error-port) "Error: Network error fetching logs (curl exit ~a)\n" (status:exit-val st)) 1)
      ((not (= code 200))
       (format (current-error-port) "Error: HTTP ~a~a\n" code (logs-error-text body)) 1)
      (else
       (write-log-cursor)
       (if json-output
           (begin (display body) (newline) 0)
           (let* ((j (json-parse body)) (ls (json-get j "lines")))
             (if (not (vector? ls))
                 (begin (display "Error: Invalid response format\n" (current-error-port)) 1)
                 (let ((all? (json-get j "sources")))
                   (for-each (lambda (e)
                               (cond ((string? e) (logs-print-line #f e))
                                     ((json-get e "line")
                                      => (lambda (l) (logs-print-line (and all? (json-get e "source")) l)))))
                             (vector->list ls))
                   0))))))))

(define (paas-logs-stream public-key secret-key source grep-pattern level)
  "Follow via SSE; reconnects with backoff, re-signing each time. Returns exit code."
  (let ((path (string-append "/logs/" source "/stream")))
    (format (current-error-port) "Streaming ~a logs... (Ctrl+C to stop)\n" source)
    (let loop ((delay 2) (last-recv 0) (reconnect? #f))
      (let* ((since (and reconnect? (> last-recv 0)
                         (format #f "~as" (max 10 (+ (- (current-time) last-recv) 5)))))
             (url (logs-query (string-append portal-base path)
                              (list (cons "grep" grep-pattern) (cons "level" level) (cons "since" since))))
             (started (current-time))
             (port (apply open-pipe* OPEN_READ (logs-curl-args public-key secret-key path url #t)))
             (code 0))
        (let read-loop ()
          (let ((line (read-line port)))
            (unless (eof-object? line)
              (cond
                ((string-prefix? "__UN_HTTP__" line)
                 (set! code (or (string->number (string-trim-both (substring line 11))) 0)))
                ((string-prefix? "data: " line)
                 (let* ((payload (substring line 6)) (j (json-parse payload)) (l (json-get j "line")))
                   (cond ((string=? payload ":keepalive") #f)
                         ((string? l) (logs-print-line (json-get j "source") l) (set! last-recv (current-time)))
                         ((json-get j "event") #f)
                         (else (logs-print-line #f payload) (set! last-recv (current-time)))))))
              (read-loop))))
        (close-pipe port)
        (cond
          ((memv code '(401 403))
           (format (current-error-port) "Error: HTTP ~a (auth failed)\n" code) 1)
          ((or (= code 0) (= code 200) (>= code 500))
           (let ((delay (if (> (- (current-time) started) 30) 2 delay)))
             (sleep delay)
             (loop (min 30 (* delay 2)) last-recv #t)))
          (else (format (current-error-port) "Error: HTTP ~a\n" code) 1))))))

(define (paas-logs-help)
  (for-each (lambda (l) (display l (current-error-port)) (newline (current-error-port)))
            '("Usage: un.scm paas logs [options]" ""
              "Fetch or stream PaaS production logs." ""
              "Sources:"
              "  --all              All sources (default)"
              "  --api              API server logs"
              "  --portal           Portal server logs"
              "  --pool NODE        Pool node logs (e.g., cammy, ai)"
              "  --source SOURCE    Source string (api, portal, pool/cammy, all)" ""
              "Options:"
              "  --lines N, -n N    Number of lines (default: 100, max: 10000)"
              "  --since TIME       Time window: 1m, 5m, 15m, 1h, 6h, 1d (default: 5m)"
              "  --grep PATTERN     Filter log lines by pattern"
              "  --level LVL, -l    Min log level: debug, info, notice, warning, err, crit"
              "  --follow, -f       Follow logs in real-time (Ctrl+C to stop)"
              "  --json             Output raw JSON response"
              "  -p KEY             Public key"
              "  -k KEY             Secret key"
              "  -h                 Show this help" ""
              "Requires a partner API key with log access enabled.")))

(define (paas-cmd args)
  (set-port-encoding! (current-output-port) "UTF-8")
  (set-port-encoding! (current-error-port) "UTF-8")
  (cond
    ((null? args)
     (display "Usage: un.scm paas <command> [options]\n\nCommands:\n  logs    Fetch or stream production logs\n" (current-error-port))
     (exit 1))
    ((not (equal? (car args) "logs"))
     (format (current-error-port) "Error: Unknown paas command '~a'\n" (car args))
     (exit 1))
    (else
     (let ((source "all") (lines 100) (since #f) (grep-pattern #f) (level #f)
           (follow #f) (json-output #f) (pk #f) (sk #f))
       (let loop ((a (cdr args)))
         (unless (null? a)
           (let ((opt (car a)) (val (and (pair? (cdr a)) (cadr a))))
             (cond
               ((member opt '("-h" "--help")) (paas-logs-help) (exit 0))
               ((equal? opt "--all") (set! source "all") (loop (cdr a)))
               ((equal? opt "--api") (set! source "api") (loop (cdr a)))
               ((equal? opt "--portal") (set! source "portal") (loop (cdr a)))
               ((member opt '("--follow" "-f")) (set! follow #t) (loop (cdr a)))
               ((equal? opt "--json") (set! json-output #t) (loop (cdr a)))
               ((not val) (loop (cdr a)))
               ((equal? opt "--pool") (set! source (string-append "pool/" val)) (loop (cddr a)))
               ((equal? opt "--source") (set! source val) (loop (cddr a)))
               ((member opt '("--lines" "-n"))
                (set! lines (max 1 (min 10000 (let ((n (string->number val))) (if (integer? n) n 1)))))
                (loop (cddr a)))
               ((equal? opt "--since") (set! since val) (loop (cddr a)))
               ((equal? opt "--grep") (set! grep-pattern val) (loop (cddr a)))
               ((member opt '("--level" "-l")) (set! level val) (loop (cddr a)))
               ((equal? opt "-p") (set! pk val) (loop (cddr a)))
               ((equal? opt "-k") (set! sk val) (loop (cddr a)))
               (else (loop (cdr a)))))))
       (let* ((keys (if (and pk sk) (list pk sk) (get-api-keys)))
              (public-key (car keys)) (secret-key (cadr keys)))
         (exit (if follow
                   (paas-logs-stream public-key secret-key source grep-pattern level)
                   (paas-logs-fetch public-key secret-key source lines
                                    (or since (read-log-cursor) "5m")
                                    grep-pattern level json-output))))))))

;; Library form kept for callers: prints the raw JSON response.
(define* (logs-fetch source lines since grep-pattern #:optional level)
  (let ((keys (get-api-keys)))
    (paas-logs-fetch (car keys) (cadr keys) (or source "all") (or lines 100)
                     (or since "5m") grep-pattern level #t)))

;; Utility functions
(define (health-check)
  (let* ((cmd (string-append "curl -s " api-base "/health"))
         (port (open-input-pipe cmd))
         (result (let loop ((chars '()))
                   (let ((char (read-char port)))
                     (if (eof-object? char)
                         (list->string (reverse chars))
                         (loop (cons char chars)))))))
    (close-pipe port)
    (display result)
    (newline)
    (string-contains result "ok")))

(define (sdk-version)
  "4.2.0")

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

;; ============================================================================
;; un key --rotate [--otp CODE]: rotate this key's secret (sudo OTP).
;; See CLI_SPEC.md "un key --rotate". curl runs without a shell; headers travel
;; in a 0600 temp file so the OTP never appears on a command line.
;; ============================================================================

(define (json-string-field json key)
  "Extract a string field from JSON without a subprocess (keeps secrets off argv)."
  (let ((m (string-match (format #f "\"~a\"[ \t\r\n]*:[ \t\r\n]*\"([^\"]*)\"" key) json)))
    (and m (match:substring m 1))))

(define (sha256-hex-prefix text n)
  "First N hex chars of sha256(TEXT). Only ever called with a public key."
  (let* ((port (open-pipe* OPEN_READ "sh" "-c" "printf '%s' \"$1\" | sha256sum" "sh" text))
         (line (read-line port)))
    (close-pipe port)
    (substring line 0 n)))

(define (credentials-csv-path)
  "accounts.csv our credentials came from, or #f (env vars). Mirrors get-api-keys tiers."
  (let ((home (or (getenv "HOME") ".")))
    (define (first-csv idx)
      (let ((home-csv (string-append home "/.unsandbox/accounts.csv")))
        (cond
          ((load-accounts-csv home-csv idx) home-csv)
          ((load-accounts-csv "./accounts.csv" idx) "./accounts.csv")
          (else #f))))
    (cond
      ((not (eq? *account-index* #f)) (first-csv *account-index*))
      ((and (getenv "UNSANDBOX_PUBLIC_KEY") (getenv "UNSANDBOX_SECRET_KEY")) #f)
      ((getenv "UNSANDBOX_API_KEY") #f)
      (else
       (let* ((acc-env (getenv "UNSANDBOX_ACCOUNT"))
              (n (and acc-env (string->number acc-env))))
         (first-csv (if (number? n) (inexact->exact n) 0)))))))

(define (write-private-file path content)
  "Create/truncate PATH with mode 0600 and write CONTENT."
  (let ((port (open path (logior O_WRONLY O_CREAT O_TRUNC) #o600)))
    (display content port)
    (close-port port)
    (chmod path #o600)))

(define (rewrite-accounts-csv path public-key new-secret)
  "Replace the secret on the line whose first field is PUBLIC-KEY; keep every other
   line, field, comment and the order. Temp file (0600) in the same dir + rename."
  (let* ((lines (string-split (read-file-raw path) #\newline))
         (changed #f)
         (new-lines
          (map (lambda (line)
                 (let ((trimmed (string-trim-both line)))
                   (if (or changed
                           (= (string-length trimmed) 0)
                           (char=? (string-ref trimmed 0) #\#))
                       line
                       (let ((fields (string-split line #\,)))
                         (if (and (>= (length fields) 2)
                                  (string=? (string-trim-both (car fields)) public-key))
                             (begin
                               (set! changed #t)
                               (string-join (cons (car fields) (cons new-secret (cddr fields))) ","))
                             line)))))
               lines)))
    (and changed
         (let ((tmp (string-append (dirname path) "/." (basename path) "."
                                   (number->string (getpid)) ".tmp")))
           (write-private-file tmp (string-join new-lines "\n"))
           (rename-file tmp path)
           #t))))

(define (read-file-raw path)
  "Whole file as one string, byte-for-byte line structure preserved."
  (call-with-input-file path
    (lambda (port)
      (let loop ((chars '()))
        (let ((c (read-char port)))
          (if (eof-object? c)
              (list->string (reverse chars))
              (loop (cons c chars))))))))

(define (rotate-post public-key secret-key otp challenge)
  "POST /keys/self/rotate-secret with body {}. Returns (body . http-code)."
  (let* ((path "/keys/self/rotate-secret")
         (body "{}")
         (timestamp (number->string (current-time)))
         (signature (make-signature secret-key timestamp "POST" path body))
         (hdr-file (format #f "/tmp/un_scm_hdr_~a_~a" (getpid) (random 999999)))
         (headers (string-append
                   "Authorization: Bearer " public-key "\n"
                   "X-Timestamp: " timestamp "\n"
                   "X-Signature: " signature "\n"
                   "Content-Type: application/json\n"
                   (if otp
                       (string-append "X-Sudo-OTP: " otp "\n"
                                      "X-Sudo-Challenge: " (or challenge "") "\n")
                       ""))))
    (write-private-file hdr-file headers)
    (let* ((port (open-pipe* OPEN_READ "curl" "-s" "-w" "\n%{http_code}" "-X" "POST"
                             (string-append api-base path)
                             "-H" (string-append "@" hdr-file)
                             "--data-binary" body))
           (output (read-file-port port)))
      (close-pipe port)
      (delete-file hdr-file)
      (parse-http-response-with-code output))))

(define (read-file-port port)
  (let loop ((chars '()))
    (let ((c (read-char port)))
      (if (eof-object? c)
          (list->string (reverse chars))
          (loop (cons c chars))))))

(define (key-rotate-cmd otp-arg)
  (let* ((keys (get-api-keys))
         (public-key (car keys))
         (secret-key (cadr keys))
         (unsandbox-dir (string-append (or (getenv "HOME") ".") "/.unsandbox"))
         (challenge-file (string-append unsandbox-dir "/rotate-"
                                        (sha256-hex-prefix public-key 16) ".challenge")))
    (define (fail body code)
      (let ((err (json-string-field body "error"))
            (msg (json-string-field body "message")))
        (cond
          ((and (= code 403) (equal? err "email_required"))
           (display "Error: this key has no email; add one or use POST /keys/recover\n" (current-error-port))
           (exit 3))
          ((= code 401)
           (display "Error: authentication failed\n" (current-error-port))
           (exit 3))
          (else
           (format (current-error-port) "Error: ~a\n" (or msg err (format #f "HTTP ~a" code)))
           (exit 4)))))
    (unless secret-key
      (display "Error: key rotation requires UNSANDBOX_SECRET_KEY (HMAC auth)\n" (current-error-port))
      (exit 3))
    (let ((resp
           (if otp-arg
               (let ((challenge (and (file-exists? challenge-file)
                                     (string-trim-both (read-file-raw challenge-file)))))
                 (unless challenge
                   (display "Error: no rotation in progress; run: un key --rotate\n" (current-error-port))
                   (exit 2))
                 (let ((r (rotate-post public-key secret-key (string-trim-both otp-arg) challenge)))
                   (when (file-exists? challenge-file) (delete-file challenge-file))
                   r))
               (let ((r (rotate-post public-key secret-key #f #f)))
                 (if (= (cdr r) 428)
                     (let ((challenge (or (json-string-field (car r) "challenge_id") "")))
                       (if (isatty? (current-input-port))
                           (begin
                             (display "One-time code emailed to the key owner.\n" (current-error-port))
                             (display "Code: " (current-error-port))
                             (force-output (current-error-port))
                             (let* ((line (read-line))
                                    (otp (if (eof-object? line) "" (string-trim-both line))))
                               (when (string=? otp "")
                                 (display "Error: cancelled\n" (current-error-port))
                                 (exit 1))
                               (rotate-post public-key secret-key otp challenge)))
                           (begin
                             (unless (file-exists? unsandbox-dir)
                               (mkdir unsandbox-dir #o700))
                             (chmod unsandbox-dir #o700)
                             (write-private-file challenge-file challenge)
                             (display "One-time code emailed to the key owner. Finish with: un key --rotate --otp CODE\n")
                             (exit 0))))
                     r)))))
      (unless (= (cdr resp) 200)
        (fail (car resp) (cdr resp)))
      (let ((new-secret (json-string-field (car resp) "secret"))
            (csv-path (credentials-csv-path)))
        (unless new-secret
          (display "Error: API returned no secret\n" (current-error-port))
          (exit 4))
        (if (and csv-path (rewrite-accounts-csv csv-path public-key new-secret))
            (format #t "Secret rotated. Updated ~a (public key ~a).\n" csv-path public-key)
            (begin
              (format #t "New secret (shown once, store it now): ~a\n" new-secret)
              (display "Warning: UNSANDBOX_SECRET_KEY / -k still hold the old, now invalid, secret.\n")))))))

(define (main args)
  (if (null? args)
      (begin
        (display "Usage: un.scm [options] <source_file>\n")
        (display "       un.scm session [options]\n")
        (display "       un.scm service [options]\n")
        (display "       un.scm image [options]\n")
        (display "       un.scm key [--extend]\n")
        (display "       un.scm key --rotate [--otp CODE]\n")
        (display "       un.scm languages [--json]\n")
        (display "       un.scm paas logs [options]\n")
        (display "       un.scm [options] -s LANG 'CODE'\n")
        (display "\nGlobal options:\n")
        (display "  --os OS             Target OS: linux (default) or freebsd (execute, session, service)\n")
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
        ((equal? (car args) "paas")
         (paas-cmd (cdr args)))
        ((equal? (car args) "languages")
         (let ((json-output (and (> (length args) 1) (equal? (cadr args) "--json"))))
           (languages-cmd json-output)))
        ((equal? (car args) "key")
         (let ((extend (member "--extend" (cdr args)))
               (rotate (member "--rotate" (cdr args)))
               (otp (let ((m (member "--otp" (cdr args))))
                      (and m (pair? (cdr m)) (cadr m)))))
           (if (or rotate otp)
               (key-rotate-cmd otp)
               (validate-key-cmd (and extend #t)))))
        ((equal? (car args) "snapshot")
         (cond
           ((and (> (length args) 1) (equal? (cadr args) "--list"))
            (snapshot-cmd "list" #f #f #f #f #f))
           ((and (> (length args) 2) (equal? (cadr args) "--info"))
            (snapshot-cmd "info" (caddr args) #f #f #f #f))
           ((and (> (length args) 2) (equal? (cadr args) "--session"))
            (snapshot-cmd "session" (caddr args) #f #f #f #f))
           ((and (> (length args) 2) (equal? (cadr args) "--service"))
            (snapshot-cmd "service" (caddr args) #f #f #f #f))
           ((and (> (length args) 2) (equal? (cadr args) "--restore"))
            (snapshot-cmd "restore" (caddr args) #f #f #f #f))
           ((and (> (length args) 2) (equal? (cadr args) "--delete"))
            (snapshot-cmd "delete" (caddr args) #f #f #f #f))
           ((and (> (length args) 2) (equal? (cadr args) "--lock"))
            (snapshot-cmd "lock" (caddr args) #f #f #f #f))
           ((and (> (length args) 2) (equal? (cadr args) "--unlock"))
            (snapshot-cmd "unlock" (caddr args) #f #f #f #f))
           ((and (> (length args) 2) (equal? (cadr args) "--clone"))
            (snapshot-cmd "clone" (caddr args) #f #f #f #f))
           (else
             (snapshot-cmd "list" #f #f #f #f #f))))
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
        ((and (or (equal? (car args) "-s") (equal? (car args) "--shell")) (> (length args) 2))
         (execute-code (get-api-key) (cadr args) (caddr args)))
        (else
          (execute-cmd (car args))))))

(define (strip-account-flag args)
  "Remove --account N from args list, setting *account-index* as side-effect."
  (let loop ((in args) (out '()))
    (cond
      ((null? in) (reverse out))
      ((and (equal? (car in) "--account") (pair? (cdr in)))
       (let ((n (string->number (cadr in))))
         (set! *account-index* (if (number? n) (inexact->exact n) #f))
         (loop (cddr in) out)))
      (else (loop (cdr in) (cons (car in) out))))))

;; Global --os OS / --os=OS: linux | freebsd, case-insensitive, sent as "os"
;; in POST /execute, /sessions, /services only when given. Invalid or missing
;; values exit 1 before any credential lookup or request.
(define *target-os* #f)

(define (os-json)
  (if *target-os* (format #f ",\"os\":\"~a\"" *target-os*) ""))

(define (normalize-os value)
  (let ((lower (string-downcase value)))
    (if (member lower '("linux" "freebsd"))
        lower
        (begin
          (format (current-error-port) "Error: invalid --os '~a' (expected linux or freebsd)\n" value)
          (exit 1)))))

(define (strip-os-flag args)
  "Remove --os OS / --os=OS from args list, setting *target-os* as side-effect."
  (let loop ((in args) (out '()))
    (cond
      ((null? in) (reverse out))
      ((equal? (car in) "--os")
       (when (null? (cdr in))
         (display "Error: --os requires a value: linux or freebsd\n" (current-error-port))
         (exit 1))
       (set! *target-os* (normalize-os (cadr in)))
       (loop (cddr in) out))
      ((string-prefix? "--os=" (car in))
       (set! *target-os* (normalize-os (substring (car in) 5)))
       (loop (cdr in) out))
      (else (loop (cdr in) (cons (car in) out))))))

(main (strip-os-flag (strip-account-flag (cdr (command-line)))))
