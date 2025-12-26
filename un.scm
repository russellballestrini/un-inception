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
         (cmd (format #f "curl -s -X POST https://api.unsandbox.com~a -H 'Content-Type: application/json' -H 'Authorization: Bearer ~a' -d @~a"
                      endpoint api-key tmp-file))
         (port (open-input-pipe cmd))
         (output (let loop ((lines '()))
                   (let ((line (read-line port)))
                     (if (eof-object? line)
                         (string-join (reverse lines) "\n")
                         (loop (cons line lines)))))))
    (close-pipe port)
    (delete-file tmp-file)
    output))

(define (curl-get api-key endpoint)
  (let* ((cmd (format #f "curl -s https://api.unsandbox.com~a -H 'Authorization: Bearer ~a'"
                      endpoint api-key))
         (port (open-input-pipe cmd))
         (output (let loop ((lines '()))
                   (let ((line (read-line port)))
                     (if (eof-object? line)
                         (string-join (reverse lines) "\n")
                         (loop (cons line lines)))))))
    (close-pipe port)
    output))

(define (curl-delete api-key endpoint)
  (let* ((cmd (format #f "curl -s -X DELETE https://api.unsandbox.com~a -H 'Authorization: Bearer ~a'"
                      endpoint api-key))
         (port (open-input-pipe cmd))
         (output (let loop ((lines '()))
                   (let ((line (read-line port)))
                     (if (eof-object? line)
                         (string-join (reverse lines) "\n")
                         (loop (cons line lines)))))))
    (close-pipe port)
    output))

(define (get-api-key)
  (or (getenv "UNSANDBOX_API_KEY")
      (begin
        (display "Error: UNSANDBOX_API_KEY not set\n" (current-error-port))
        (exit 1))))

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
        (exit 1))
      (cond
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
           ((and (> (length args) 2) (equal? (cadr args) "--sleep"))
            (service-cmd "sleep" (caddr args) #f #f #f #f))
           ((and (> (length args) 2) (equal? (cadr args) "--wake"))
            (service-cmd "wake" (caddr args) #f #f #f #f))
           ((and (> (length args) 2) (equal? (cadr args) "--destroy"))
            (service-cmd "destroy" (caddr args) #f #f #f #f))
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
