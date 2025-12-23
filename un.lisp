;; PUBLIC DOMAIN - NO LICENSE, NO WARRANTY
;;
;; This is free public domain software for the public good of a permacomputer hosted
;; at permacomputer.com - an always-on computer by the people, for the people. One
;; which is durable, easy to repair, and distributed like tap water for machine
;; learning intelligence.
;;
;; The permacomputer is community-owned infrastructure optimized around four values:
;;
;;   TRUTH    - Source code must be open source & freely distributed
;;   FREEDOM  - Voluntary participation without corporate control
;;   HARMONY  - Systems operating with minimal waste that self-renew
;;   LOVE     - Individual rights protected while fostering cooperation
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


#!/usr/bin/env sbcl --script

;;;; Common Lisp UN CLI - Unsandbox CLI Client
;;;;
;;;; Full-featured CLI matching un.py capabilities
;;;; Uses curl for HTTP (no external dependencies)

(defpackage :un-cli
  (:use :cl))

(in-package :un-cli)

(defparameter *blue* (format nil "~C[34m" #\Escape))
(defparameter *red* (format nil "~C[31m" #\Escape))
(defparameter *green* (format nil "~C[32m" #\Escape))
(defparameter *yellow* (format nil "~C[33m" #\Escape))
(defparameter *reset* (format nil "~C[0m" #\Escape))

(defparameter *ext-map*
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

(defun get-extension (filename)
  (let ((dot-pos (position #\. filename :from-end t)))
    (if dot-pos (subseq filename dot-pos) "")))

(defun escape-json (s)
  (with-output-to-string (out)
    (loop for c across s do
      (cond
        ((char= c #\\) (write-string "\\\\" out))
        ((char= c #\") (write-string "\\\"" out))
        ((char= c #\Newline) (write-string "\\n" out))
        ((char= c #\Return) (write-string "\\r" out))
        ((char= c #\Tab) (write-string "\\t" out))
        (t (write-char c out))))))

(defun read-file (filename)
  (with-open-file (stream filename)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))

(defun write-temp-file (data)
  (let ((tmp-file (format nil "/tmp/un_lisp_~a.json" (random 999999))))
    (with-open-file (stream tmp-file :direction :output :if-exists :supersede)
      (write-string data stream))
    tmp-file))

(defun run-curl (args)
  (with-output-to-string (out)
    (let ((process (uiop:launch-program args :output :stream :error-output nil)))
      (loop for line = (read-line (uiop:process-info-output process) nil)
            while line do (format out "~a~%" line))
      (uiop:wait-process process))))

(defun curl-post (api-key endpoint json-data)
  (let ((tmp-file (write-temp-file json-data)))
    (unwind-protect
         (run-curl (list "curl" "-s" "-X" "POST"
                        (format nil "https://api.unsandbox.com~a" endpoint)
                        "-H" "Content-Type: application/json"
                        "-H" (format nil "Authorization: Bearer ~a" api-key)
                        "-d" (format nil "@~a" tmp-file)))
      (delete-file tmp-file))))

(defun curl-get (api-key endpoint)
  (run-curl (list "curl" "-s"
                 (format nil "https://api.unsandbox.com~a" endpoint)
                 "-H" (format nil "Authorization: Bearer ~a" api-key))))

(defun curl-delete (api-key endpoint)
  (run-curl (list "curl" "-s" "-X" "DELETE"
                 (format nil "https://api.unsandbox.com~a" endpoint)
                 "-H" (format nil "Authorization: Bearer ~a" api-key))))

(defun get-api-key ()
  (or (uiop:getenv "UNSANDBOX_API_KEY")
      (progn
        (format t "Error: UNSANDBOX_API_KEY not set~%")
        (uiop:quit 1))))

(defun execute-cmd (file)
  (let* ((api-key (get-api-key))
         (ext (get-extension file))
         (language (cdr (assoc ext *ext-map* :test #'string=))))
    (unless language
      (format t "Error: Unknown extension: ~a~%" ext)
      (uiop:quit 1))
    (let* ((code (read-file file))
           (json (format nil "{\"language\":\"~a\",\"code\":\"~a\"}"
                         language (escape-json code)))
           (response (curl-post api-key "/execute" json)))
      (format t "~a~%" response))))

(defun session-cmd (action id shell)
  (let ((api-key (get-api-key)))
    (cond
      ((string= action "list")
       (format t "~a~%" (curl-get api-key "/sessions")))
      ((string= action "kill")
       (curl-delete api-key (format nil "/sessions/~a" id))
       (format t "~aSession terminated: ~a~a~%" *green* id *reset*))
      (t
        (let* ((sh (or shell "bash"))
               (json (format nil "{\"shell\":\"~a\"}" sh))
               (response (curl-post api-key "/sessions" json)))
          (format t "~aSession created (WebSocket required)~a~%" *yellow* *reset*)
          (format t "~a~%" response))))))

(defun service-cmd (action id name ports bootstrap)
  (let ((api-key (get-api-key)))
    (cond
      ((string= action "list")
       (format t "~a~%" (curl-get api-key "/services")))
      ((string= action "info")
       (format t "~a~%" (curl-get api-key (format nil "/services/~a" id))))
      ((string= action "logs")
       (format t "~a~%" (curl-get api-key (format nil "/services/~a/logs" id))))
      ((string= action "sleep")
       (curl-post api-key (format nil "/services/~a/sleep" id) "{}")
       (format t "~aService sleeping: ~a~a~%" *green* id *reset*))
      ((string= action "wake")
       (curl-post api-key (format nil "/services/~a/wake" id) "{}")
       (format t "~aService waking: ~a~a~%" *green* id *reset*))
      ((string= action "destroy")
       (curl-delete api-key (format nil "/services/~a" id))
       (format t "~aService destroyed: ~a~a~%" *green* id *reset*))
      ((and (string= action "create") name)
       (let* ((ports-json (if ports (format nil ",\"ports\":[~a]" ports) ""))
              (bootstrap-json (if bootstrap (format nil ",\"bootstrap\":\"~a\"" (escape-json bootstrap)) ""))
              (json (format nil "{\"name\":\"~a\"~a~a}" name ports-json bootstrap-json))
              (response (curl-post api-key "/services" json)))
         (format t "~aService created~a~%" *green* *reset*)
         (format t "~a~%" response)))
      (t
        (format t "Error: --name required to create service~%")
        (uiop:quit 1)))))

(defun main ()
  (let ((args (uiop:command-line-arguments)))
    (if (null args)
        (progn
          (format t "Usage: un.lisp [options] <source_file>~%")
          (format t "       un.lisp session [options]~%")
          (format t "       un.lisp service [options]~%")
          (uiop:quit 1))
        (cond
          ((string= (first args) "session")
           (cond
             ((and (> (length args) 1) (string= (second args) "--list"))
              (session-cmd "list" nil nil))
             ((and (> (length args) 2) (string= (second args) "--kill"))
              (session-cmd "kill" (third args) nil))
             (t
               (session-cmd "create" nil nil))))
          ((string= (first args) "service")
           (cond
             ((and (> (length args) 1) (string= (second args) "--list"))
              (service-cmd "list" nil nil nil nil))
             ((and (> (length args) 2) (string= (second args) "--info"))
              (service-cmd "info" (third args) nil nil nil))
             ((and (> (length args) 2) (string= (second args) "--logs"))
              (service-cmd "logs" (third args) nil nil nil))
             ((and (> (length args) 2) (string= (second args) "--sleep"))
              (service-cmd "sleep" (third args) nil nil nil))
             ((and (> (length args) 2) (string= (second args) "--wake"))
              (service-cmd "wake" (third args) nil nil nil))
             ((and (> (length args) 2) (string= (second args) "--destroy"))
              (service-cmd "destroy" (third args) nil nil nil))
             ((and (> (length args) 2) (string= (second args) "--name"))
              (let ((name (third args))
                    (ports (when (and (> (length args) 4) (string= (fourth args) "--ports"))
                             (fifth args)))
                    (bootstrap (when (and (> (length args) 6) (string= (sixth args) "--bootstrap"))
                                 (seventh args))))
                (service-cmd "create" nil name ports bootstrap)))
             (t
               (format t "Error: Invalid service command~%")
               (uiop:quit 1))))
          (t
            (execute-cmd (first args)))))))

(main)
