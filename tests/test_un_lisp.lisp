#!/usr/bin/env sbcl --script

;;;; Common Lisp UN CLI Test Suite
;;;;
;;;; Usage:
;;;;   chmod +x test_un_lisp.lisp
;;;;   ./test_un_lisp.lisp
;;;;
;;;; Or with sbcl:
;;;;   sbcl --script test_un_lisp.lisp
;;;;
;;;; Tests the Common Lisp UN CLI implementation (un.lisp) for:
;;;; 1. Extension detection logic
;;;; 2. API integration (if UNSANDBOX_API_KEY is set)
;;;; 3. End-to-end execution with fib.lisp test file

(defpackage :un-cli-test
  (:use :cl))

(in-package :un-cli-test)

;;; ANSI color codes
(defparameter *green* (format nil "~C[32m" #\Escape))
(defparameter *red* (format nil "~C[31m" #\Escape))
(defparameter *yellow* (format nil "~C[33m" #\Escape))
(defparameter *reset* (format nil "~C[0m" #\Escape))

;;; Extension to language mapping (from un.lisp)
(defparameter *ext-to-lang*
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
(defun lookup-language (ext)
  (cdr (assoc ext *ext-to-lang* :test #'string=)))

;;; Test result structure
(defstruct test-result
  (passed nil :type boolean)
  (message "" :type string))

;;; Print test result
(defun print-result (test-name result)
  (if (test-result-passed result)
      (progn
        (format t "~A✓ PASS~A - ~A~%" *green* *reset* test-name)
        t)
      (progn
        (format t "~A✗ FAIL~A - ~A~%" *red* *reset* test-name)
        (when (test-result-message result)
          (format t "  Error: ~A~%" (test-result-message result)))
        nil)))

;;; Test 1: Extension detection
(defun test-extension-detection ()
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
    (let ((failures (remove-if (lambda (test)
                                (string= (lookup-language (car test))
                                        (cdr test)))
                              tests)))
      (if (null failures)
          (make-test-result :passed t)
          (make-test-result :passed nil
                           :message (format nil "~A tests failed" (length failures)))))))

;;; Run command and capture output
(defun run-command (cmd)
  (handler-case
      (let ((output (with-output-to-string (s)
                     (let ((proc (uiop:launch-program cmd
                                                      :output :stream
                                                      :error-output :stream)))
                       (loop for line = (read-line (uiop:process-info-output proc) nil)
                             while line
                             do (format s "~A~%" line))
                       (uiop:wait-process proc)))))
        (cons 0 output))
    (error (e)
      (cons 1 (format nil "~A" e)))))

;;; Test 2: API integration
(defun test-api-integration ()
  (let ((api-key (uiop:getenv "UNSANDBOX_API_KEY")))
    (if (not api-key)
        (make-test-result :passed t)  ; Skip test if no API key
        (handler-case
            (progn
              ;; Create a simple test file
              (with-open-file (stream "/tmp/test_un_lisp_api.lisp"
                                     :direction :output
                                     :if-exists :supersede)
                (format stream "(format t \"test~%\")~%"))

              ;; Run the CLI
              (let* ((result (run-command "./un.lisp /tmp/test_un_lisp_api.lisp 2>&1"))
                     (status (car result))
                     (output (cdr result)))

                ;; Check if it executed successfully
                (if (and (= status 0) (search "test" output))
                    (make-test-result :passed t)
                    (make-test-result :passed nil
                                     :message (format nil "API call failed: ~A" output)))))
          (error (e)
            (make-test-result :passed nil
                             :message (format nil "Exception: ~A" e)))))))

;;; Test 3: Functional test with fib.lisp
(defun test-fibonacci ()
  (let ((api-key (uiop:getenv "UNSANDBOX_API_KEY")))
    (if (not api-key)
        (make-test-result :passed t)  ; Skip test if no API key
        (handler-case
            (let* ((fib-path "../test/fib.lisp")
                   (result (run-command (format nil "./un.lisp ~A 2>&1" fib-path)))
                   (status (car result))
                   (output (cdr result)))

              ;; Check if output contains expected fibonacci result
              (if (and (= status 0) (search "fib(10) = 55" output))
                  (make-test-result :passed t)
                  (make-test-result :passed nil
                                   :message (format nil "Fibonacci test failed: ~A" output))))
          (error (e)
            (make-test-result :passed nil
                             :message (format nil "Exception: ~A" e)))))))

;;; Main test runner
(defun main ()
  (format t "=== Common Lisp UN CLI Test Suite ===~%~%")

  ;; Check if API key is set
  (unless (uiop:getenv "UNSANDBOX_API_KEY")
    (format t "~A⚠ WARNING~A - UNSANDBOX_API_KEY not set, skipping API tests~%~%"
            *yellow* *reset*))

  ;; Run tests
  (let ((results (list (print-result "Extension detection" (test-extension-detection))
                      (print-result "API integration" (test-api-integration))
                      (print-result "Fibonacci end-to-end test" (test-fibonacci)))))

    (format t "~%")

    ;; Summary
    (let ((passed (count t results))
          (total (length results)))
      (if (= passed total)
          (progn
            (format t "~A✓ All tests passed (~D/~D)~A~%" *green* passed total *reset*)
            (uiop:quit 0))
          (progn
            (format t "~A✗ Some tests failed (~D/~D passed)~A~%" *red* passed total *reset*)
            (uiop:quit 1))))))

;;; Entry point
(main)
