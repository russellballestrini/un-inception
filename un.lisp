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

(defparameter *portal-base* "https://unsandbox.com")

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

(defun curl-post-portal (api-key endpoint json-data)
  (let ((tmp-file (write-temp-file json-data)))
    (unwind-protect
         (run-curl (list "curl" "-s" "-X" "POST"
                        (format nil "~a~a" *portal-base* endpoint)
                        "-H" "Content-Type: application/json"
                        "-H" (format nil "Authorization: Bearer ~a" api-key)
                        "-d" (format nil "@~a" tmp-file)))
      (delete-file tmp-file))))

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

(defun service-cmd (action id name ports bootstrap service-type)
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
      ((string= action "execute")
       (when (and id bootstrap)
         (let* ((json (format nil "{\"command\":\"~a\"}" (escape-json bootstrap)))
                (response (curl-post api-key (format nil "/services/~a/execute" id) json))
                (stdout-val (parse-json-field response "stdout")))
           (when stdout-val
             (format t "~a~a~a" *blue* stdout-val *reset*)))))
      ((string= action "dump-bootstrap")
       (when id
         (format *error-output* "Fetching bootstrap script from ~a...~%" id)
         (let* ((json "{\"command\":\"cat /tmp/bootstrap.sh\"}")
                (response (curl-post api-key (format nil "/services/~a/execute" id) json))
                (stdout-val (parse-json-field response "stdout")))
           (if stdout-val
               (if service-type
                   (progn
                     (with-open-file (stream service-type :direction :output :if-exists :supersede)
                       (write-string stdout-val stream))
                     (uiop:run-program (list "chmod" "755" service-type))
                     (format t "Bootstrap saved to ~a~%" service-type))
                   (format t "~a" stdout-val))
               (progn
                 (format *error-output* "~aError: Failed to fetch bootstrap (service not running or no bootstrap file)~a~%" *red* *reset*)
                 (uiop:quit 1))))))
      ((and (string= action "create") name)
       (let* ((ports-json (if ports (format nil ",\"ports\":[~a]" ports) ""))
              (bootstrap-json (if bootstrap (format nil ",\"bootstrap\":\"~a\"" (escape-json bootstrap)) ""))
              (type-json (if service-type (format nil ",\"service_type\":\"~a\"" service-type) ""))
              (json (format nil "{\"name\":\"~a\"~a~a~a}" name ports-json bootstrap-json type-json))
              (response (curl-post api-key "/services" json)))
         (format t "~aService created~a~%" *green* *reset*)
         (format t "~a~%" response)))
      (t
        (format t "Error: --name required to create service~%")
        (uiop:quit 1)))))

(defun parse-json-field (json field)
  "Simple JSON field parser - extracts value for given field"
  (let* ((field-pattern (format nil "\"~a\":" field))
         (start (search field-pattern json)))
    (when start
      (let* ((value-start (+ start (length field-pattern)))
             (first-char (char json value-start)))
        (cond
          ((char= first-char #\")
           ;; String value
           (let ((str-start (1+ value-start)))
             (loop for i from str-start below (length json)
                   when (and (char= (char json i) #\")
                            (not (char= (char json (1- i)) #\\)))
                   return (subseq json str-start i))))
          ((char= first-char #\{)
           ;; Object value - skip for now
           nil)
          ((char= first-char #\[)
           ;; Array value - skip for now
           nil)
          (t
           ;; Number, boolean, or null
           (let ((end (or (position #\, json :start value-start)
                         (position #\} json :start value-start)
                         (length json))))
             (string-trim '(#\Space #\Tab #\Newline #\Return)
                         (subseq json value-start end)))))))))

(defun open-browser (url)
  "Open URL in default browser"
  (uiop:run-program (list "xdg-open" url) :ignore-error-status t))

(defun validate-key (extend-flag)
  (let* ((api-key (get-api-key))
         (response (curl-post-portal api-key "/keys/validate" "{}"))
         (status (parse-json-field response "status"))
         (public-key (parse-json-field response "public_key"))
         (tier (parse-json-field response "tier"))
         (expires-at (parse-json-field response "expires_at")))

    (cond
      ((string= status "valid")
       (format t "~aValid~a~%" *green* *reset*)
       (when public-key (format t "Public Key: ~a~%" public-key))
       (when tier (format t "Tier: ~a~%" tier))
       (when expires-at (format t "Expires: ~a~%" expires-at))
       (let ((time-remaining (parse-json-field response "time_remaining"))
             (rate-limit (parse-json-field response "rate_limit"))
             (burst (parse-json-field response "burst"))
             (concurrency (parse-json-field response "concurrency")))
         (when time-remaining (format t "Time Remaining: ~a~%" time-remaining))
         (when rate-limit (format t "Rate Limit: ~a~%" rate-limit))
         (when burst (format t "Burst: ~a~%" burst))
         (when concurrency (format t "Concurrency: ~a~%" concurrency)))
       (when extend-flag
         (if public-key
             (let ((extend-url (format nil "~a/keys/extend?pk=~a" *portal-base* public-key)))
               (format t "~aOpening browser to extend key...~a~%" *blue* *reset*)
               (open-browser extend-url))
             (format t "~aError: No public_key in response~a~%" *red* *reset*))))

      ((string= status "expired")
       (format t "~aExpired~a~%" *red* *reset*)
       (when public-key (format t "Public Key: ~a~%" public-key))
       (when tier (format t "Tier: ~a~%" tier))
       (when expires-at (format t "Expired: ~a~%" expires-at))
       (format t "~aTo renew: Visit ~a/keys/extend~a~%" *yellow* *portal-base* *reset*)
       (when extend-flag
         (if public-key
             (let ((extend-url (format nil "~a/keys/extend?pk=~a" *portal-base* public-key)))
               (format t "~aOpening browser to extend key...~a~%" *blue* *reset*)
               (open-browser extend-url))
             (format t "~aError: No public_key in response~a~%" *red* *reset*))))

      ((string= status "invalid")
       (format t "~aInvalid~a~%" *red* *reset*)
       (format t "Response: ~a~%" response))

      (t
       (format t "~aUnknown status~a~%" *red* *reset*)
       (format t "Response: ~a~%" response)))))

(defun key-cmd (extend-flag)
  (validate-key extend-flag))

(defun main ()
  (let ((args (uiop:command-line-arguments)))
    (if (null args)
        (progn
          (format t "Usage: un.lisp [options] <source_file>~%")
          (format t "       un.lisp session [options]~%")
          (format t "       un.lisp service [options]~%")
          (format t "       un.lisp key [--extend]~%")
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
              (service-cmd "list" nil nil nil nil nil))
             ((and (> (length args) 2) (string= (second args) "--info"))
              (service-cmd "info" (third args) nil nil nil nil))
             ((and (> (length args) 2) (string= (second args) "--logs"))
              (service-cmd "logs" (third args) nil nil nil nil))
             ((and (> (length args) 2) (string= (second args) "--freeze"))
              (service-cmd "sleep" (third args) nil nil nil nil))
             ((and (> (length args) 2) (string= (second args) "--unfreeze"))
              (service-cmd "wake" (third args) nil nil nil nil))
             ((and (> (length args) 2) (string= (second args) "--destroy"))
              (service-cmd "destroy" (third args) nil nil nil nil))
             ((and (> (length args) 3) (string= (second args) "--execute"))
              (service-cmd "execute" (third args) nil nil (fourth args) nil))
             ((and (> (length args) 3) (string= (second args) "--dump-bootstrap"))
              (service-cmd "dump-bootstrap" (third args) nil nil nil (fourth args)))
             ((and (> (length args) 2) (string= (second args) "--dump-bootstrap"))
              (service-cmd "dump-bootstrap" (third args) nil nil nil nil))
             ((and (> (length args) 2) (string= (second args) "--name"))
              (let* ((name (third args))
                     (rest-args (nthcdr 3 args))
                     (ports nil)
                     (bootstrap nil)
                     (service-type nil))
                (loop for i from 0 below (length rest-args) by 2
                      do (let ((opt (nth i rest-args))
                               (val (nth (1+ i) rest-args)))
                           (cond
                             ((string= opt "--ports") (setf ports val))
                             ((string= opt "--bootstrap") (setf bootstrap val))
                             ((string= opt "--type") (setf service-type val)))))
                (service-cmd "create" nil name ports bootstrap service-type)))
             (t
               (format t "Error: Invalid service command~%")
               (uiop:quit 1))))
          ((string= (first args) "key")
           (let ((extend-flag (and (> (length args) 1) (string= (second args) "--extend"))))
             (key-cmd extend-flag)))
          (t
            (execute-cmd (first args)))))))

(main)
