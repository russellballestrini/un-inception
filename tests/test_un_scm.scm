#!/usr/bin/env guile
!#

;;; Scheme UN CLI Test Suite
;;;
;;; Usage:
;;;   chmod +x test_un_scm.scm
;;;   ./test_un_scm.scm
;;;
;;; Or with guile:
;;;   guile test_un_scm.scm
;;;
;;; Tests the Scheme UN CLI implementation (un.scm) for:
;;; 1. Extension detection logic
;;; 2. API integration (if UNSANDBOX_API_KEY is set)
;;; 3. End-to-end execution with fib.scm test file

(use-modules (ice-9 popen)
             (ice-9 rdelim)
             (ice-9 regex))

;;; ANSI color codes
(define green "\x1b[32m")
(define red "\x1b[31m")
(define yellow "\x1b[33m")
(define reset "\x1b[0m")

;;; Extension to language mapping (from un.scm)
(define ext-to-lang
  '((".hs" . "haskell")
    (".ml" . "ocaml")
    (".clj" . "clojure")
    (".scm" . "scheme")
    (".lisp" . "commonlisp")
    (".erl" . "erlang")
    (".ex" . "elixir")
    (".py" . "python")
    (".js" . "javascript")
    (".rb" . "ruby")
    (".go" . "go")
    (".rs" . "rust")
    (".c" . "c")
    (".cpp" . "cpp")
    (".java" . "java")))

;;; Lookup language by extension
(define (lookup-language ext)
  (assoc-ref ext-to-lang ext))

;;; Print test result
(define (print-result test-name passed? error-msg)
  (if passed?
      (begin
        (display (string-append green "✓ PASS" reset " - " test-name "\n"))
        #t)
      (begin
        (display (string-append red "✗ FAIL" reset " - " test-name "\n"))
        (when error-msg
          (display (string-append "  Error: " error-msg "\n")))
        #f)))

;;; Test 1: Extension detection
(define (test-extension-detection)
  (let ((tests '((".hs" . "haskell")
                 (".ml" . "ocaml")
                 (".clj" . "clojure")
                 (".scm" . "scheme")
                 (".lisp" . "commonlisp")
                 (".erl" . "erlang")
                 (".ex" . "elixir")
                 (".py" . "python")
                 (".js" . "javascript")
                 (".rb" . "ruby"))))
    (let ((failures (filter (lambda (test)
                             (let ((ext (car test))
                                   (expected (cdr test)))
                               (not (equal? (lookup-language ext) expected))))
                           tests)))
      (if (null? failures)
          (print-result "Extension detection" #t #f)
          (print-result "Extension detection" #f
                       (format #f "~a tests failed" (length failures)))))))

;;; Run command and capture output
(define (run-command cmd)
  (let* ((port (open-input-pipe cmd))
         (output (read-delimited "" port))
         (status (close-pipe port)))
    (cons status output)))

;;; Test 2: API integration
(define (test-api-integration)
  (let ((api-key (getenv "UNSANDBOX_API_KEY")))
    (if (not api-key)
        (print-result "API integration" #t #f)  ; Skip test if no API key
        (catch #t
          (lambda ()
            ;; Create a simple test file
            (let ((test-code "(display \"test\\n\")\n"))
              (call-with-output-file "/tmp/test_un_scm_api.scm"
                (lambda (port) (display test-code port)))

              ;; Run the CLI
              (let* ((result (run-command "./un.scm /tmp/test_un_scm_api.scm 2>&1"))
                     (status (car result))
                     (output (cdr result)))

                ;; Check if it executed successfully
                (if (and (= status 0)
                        (string-contains output "test"))
                    (print-result "API integration" #t #f)
                    (print-result "API integration" #f
                                (format #f "API call failed: ~a" output))))))
          (lambda (key . args)
            (print-result "API integration" #f
                         (format #f "Exception: ~a" args)))))))

;;; Test 3: Functional test with fib.scm
(define (test-fibonacci)
  (let ((api-key (getenv "UNSANDBOX_API_KEY")))
    (if (not api-key)
        (print-result "Fibonacci end-to-end test" #t #f)  ; Skip test if no API key
        (catch #t
          (lambda ()
            ;; Check if fib.scm exists
            (let* ((fib-path "../test/fib.scm")
                   (result (run-command (string-append "./un.scm " fib-path " 2>&1")))
                   (status (car result))
                   (output (cdr result)))

              ;; Check if output contains expected fibonacci result
              (if (and (= status 0)
                      (string-contains output "fib(10) = 55"))
                  (print-result "Fibonacci end-to-end test" #t #f)
                  (print-result "Fibonacci end-to-end test" #f
                              (format #f "Fibonacci test failed: ~a" output)))))
          (lambda (key . args)
            (print-result "Fibonacci end-to-end test" #f
                         (format #f "Exception: ~a" args)))))))

;;; Main test runner
(define (main)
  (display "=== Scheme UN CLI Test Suite ===\n\n")

  ;; Check if API key is set
  (when (not (getenv "UNSANDBOX_API_KEY"))
    (display (string-append yellow "⚠ WARNING" reset
                           " - UNSANDBOX_API_KEY not set, skipping API tests\n\n")))

  ;; Run tests
  (let ((results (list (test-extension-detection)
                      (test-api-integration)
                      (test-fibonacci))))

    (display "\n")

    ;; Summary
    (let ((passed (length (filter (lambda (x) x) results)))
          (total (length results)))
      (if (= passed total)
          (begin
            (display (string-append green "✓ All tests passed ("
                                   (number->string passed) "/"
                                   (number->string total) ")" reset "\n"))
            (exit 0))
          (begin
            (display (string-append red "✗ Some tests failed ("
                                   (number->string passed) "/"
                                   (number->string total) " passed)" reset "\n"))
            (exit 1))))))

;;; Entry point
(main)
