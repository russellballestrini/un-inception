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

(defparameter *languages-cache-ttl* 3600) ;; 1 hour in seconds

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

(defun read-file-binary (filename)
  "Read file as binary and return as vector of bytes"
  (with-open-file (stream filename :element-type '(unsigned-byte 8))
    (let* ((len (file-length stream))
           (data (make-array len :element-type '(unsigned-byte 8))))
      (read-sequence data stream)
      data)))

(defun base64-encode-file (filename)
  "Base64 encode a file using shell command"
  (let* ((cmd (format nil "base64 -w0 ~a" (uiop:escape-sh-token filename)))
         (result (string-trim '(#\Space #\Tab #\Newline #\Return)
                             (uiop:run-program cmd :output :string))))
    result))

(defun build-input-files-json (files)
  "Build input_files JSON array from list of filenames"
  (if (null files)
      ""
      (format nil ",\"input_files\":[~{~a~^,~}]"
              (mapcar (lambda (f)
                        (let* ((basename (file-namestring f))
                               (content (base64-encode-file f)))
                          (format nil "{\"filename\":\"~a\",\"content\":\"~a\"}"
                                  basename content)))
                      files))))

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

(defun check-clock-drift (response)
  "Check if response indicates clock drift error"
  (when (and (search "timestamp" response)
             (or (search "401" response)
                 (search "expired" response)
                 (search "invalid" response)))
    (format t "~aError: Request timestamp expired (must be within 5 minutes of server time)~a~%" *red* *reset*)
    (format t "~aYour computer's clock may have drifted.~a~%" *yellow* *reset*)
    (format t "Check your system time and sync with NTP if needed:~%")
    (format t "  Linux:   sudo ntpdate -s time.nist.gov~%")
    (format t "  macOS:   sudo sntp -sS time.apple.com~%")
    (format t "  Windows: w32tm /resync~a~%" *reset*)
    (uiop:quit 1)))

(defun curl-post (api-key endpoint json-data)
  (let ((tmp-file (write-temp-file json-data)))
    (unwind-protect
         (destructuring-bind (public-key secret-key) (get-api-keys)
           (let* ((auth-headers (build-auth-headers public-key secret-key "POST" endpoint json-data))
                  (base-args (list "curl" "-s" "-X" "POST"
                                  (format nil "https://api.unsandbox.com~a" endpoint)
                                  "-H" "Content-Type: application/json"))
                  (response (run-curl (append base-args auth-headers (list "-d" (format nil "@~a" tmp-file))))))
             (check-clock-drift response)
             response))
      (delete-file tmp-file))))

(defun curl-get (api-key endpoint)
  (destructuring-bind (public-key secret-key) (get-api-keys)
    (let* ((auth-headers (build-auth-headers public-key secret-key "GET" endpoint ""))
           (base-args (list "curl" "-s"
                           (format nil "https://api.unsandbox.com~a" endpoint)))
           (response (run-curl (append base-args auth-headers))))
      (check-clock-drift response)
      response)))

(defun curl-delete (api-key endpoint)
  (destructuring-bind (public-key secret-key) (get-api-keys)
    (let* ((auth-headers (build-auth-headers public-key secret-key "DELETE" endpoint ""))
           (base-args (list "curl" "-s" "-X" "DELETE"
                           (format nil "https://api.unsandbox.com~a" endpoint)))
           (response (run-curl (append base-args auth-headers))))
      (check-clock-drift response)
      response)))

(defun curl-post-portal (api-key endpoint json-data)
  (let ((tmp-file (write-temp-file json-data)))
    (unwind-protect
         (destructuring-bind (public-key secret-key) (get-api-keys)
           (let* ((auth-headers (build-auth-headers public-key secret-key "POST" endpoint json-data))
                  (base-args (list "curl" "-s" "-X" "POST"
                                  (format nil "~a~a" *portal-base* endpoint)
                                  "-H" "Content-Type: application/json"))
                  (response (run-curl (append base-args auth-headers (list "-d" (format nil "@~a" tmp-file))))))
             (check-clock-drift response)
             response))
      (delete-file tmp-file))))

(defun curl-patch (api-key endpoint json-data)
  (let ((tmp-file (write-temp-file json-data)))
    (unwind-protect
         (destructuring-bind (public-key secret-key) (get-api-keys)
           (let* ((auth-headers (build-auth-headers public-key secret-key "PATCH" endpoint json-data))
                  (base-args (list "curl" "-s" "-X" "PATCH"
                                  (format nil "https://api.unsandbox.com~a" endpoint)
                                  "-H" "Content-Type: application/json"))
                  (response (run-curl (append base-args auth-headers (list "-d" (format nil "@~a" tmp-file))))))
             (check-clock-drift response)
             response))
      (delete-file tmp-file))))

(defun curl-put-text (api-key endpoint content)
  "PUT request with text/plain content type (for vault)"
  (let ((tmp-file (write-temp-file content)))
    (unwind-protect
         (destructuring-bind (public-key secret-key) (get-api-keys)
           (let* ((auth-headers (build-auth-headers public-key secret-key "PUT" endpoint content))
                  (base-args (list "curl" "-s" "-X" "PUT"
                                  (format nil "https://api.unsandbox.com~a" endpoint)
                                  "-H" "Content-Type: text/plain"))
                  (response (run-curl (append base-args auth-headers (list "--data-binary" (format nil "@~a" tmp-file))))))
             (check-clock-drift response)
             response))
      (delete-file tmp-file))))

(defun build-env-content (env-vars env-file)
  "Build env content from list of env vars and env file"
  (let ((lines '()))
    ;; Add env vars
    (dolist (var env-vars)
      (push var lines))
    ;; Add env file contents
    (when (and env-file (probe-file env-file))
      (with-open-file (stream env-file)
        (loop for line = (read-line stream nil)
              while line
              do (let ((trimmed (string-trim '(#\Space #\Tab) line)))
                   (when (and (> (length trimmed) 0)
                             (not (char= (char trimmed 0) #\#)))
                     (push line lines))))))
    (format nil "~{~a~^~%~}" (nreverse lines))))

(defun service-env-status (api-key service-id)
  (format t "~a~%" (curl-get api-key (format nil "/services/~a/env" service-id))))

(defun service-env-set (api-key service-id content)
  (format t "~a~%" (curl-put-text api-key (format nil "/services/~a/env" service-id) content)))

(defun service-env-export (api-key service-id)
  (let* ((response (curl-post api-key (format nil "/services/~a/env/export" service-id) "{}"))
         (content (parse-json-field response "content")))
    (when content (format t "~a" content))))

(defun service-env-delete (api-key service-id)
  (curl-delete api-key (format nil "/services/~a/env" service-id))
  (format t "~aVault deleted: ~a~a~%" *green* service-id *reset*))

(defun get-api-keys ()
  (let ((public-key (uiop:getenv "UNSANDBOX_PUBLIC_KEY"))
        (secret-key (uiop:getenv "UNSANDBOX_SECRET_KEY"))
        (api-key (uiop:getenv "UNSANDBOX_API_KEY")))
    (cond
      ((and public-key secret-key) (list public-key secret-key))
      (api-key (list api-key nil))
      (t (progn
           (format t "Error: UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY not set (or UNSANDBOX_API_KEY for backwards compat)~%")
           (uiop:quit 1))))))

(defun get-api-key ()
  (first (get-api-keys)))

(defun hmac-sha256 (secret message)
  "Compute HMAC-SHA256 using openssl command"
  (let* ((secret-escaped (uiop:escape-sh-token secret))
         (message-escaped (uiop:escape-sh-token message))
         (cmd (format nil "echo -n ~a | openssl dgst -sha256 -hmac ~a | awk '{print $2}'"
                      message-escaped secret-escaped))
         (result (string-trim '(#\Space #\Tab #\Newline #\Return)
                             (uiop:run-program cmd :output :string))))
    result))

(defun make-signature (secret-key timestamp method path body)
  (let ((message (format nil "~a:~a:~a:~a" timestamp method path body)))
    (hmac-sha256 secret-key message)))

(defun build-auth-headers (public-key secret-key method path body)
  (if secret-key
      (let* ((timestamp (write-to-string (floor (get-universal-time))))
             (signature (make-signature secret-key timestamp method path body)))
        (list "-H" (format nil "Authorization: Bearer ~a" public-key)
              "-H" (format nil "X-Timestamp: ~a" timestamp)
              "-H" (format nil "X-Signature: ~a" signature)))
      (list "-H" (format nil "Authorization: Bearer ~a" public-key))))

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

(defun session-cmd (action id shell input-files)
  (let ((api-key (get-api-key)))
    (cond
      ((string= action "list")
       (format t "~a~%" (curl-get api-key "/sessions")))
      ((string= action "kill")
       (curl-delete api-key (format nil "/sessions/~a" id))
       (format t "~aSession terminated: ~a~a~%" *green* id *reset*))
      (t
        (let* ((sh (or shell "bash"))
               (input-files-json (build-input-files-json input-files))
               (json (format nil "{\"shell\":\"~a\"~a}" sh input-files-json))
               (response (curl-post api-key "/sessions" json)))
          (format t "~aSession created (WebSocket required)~a~%" *yellow* *reset*)
          (format t "~a~%" response))))))

(defun service-cmd (action id name ports bootstrap bootstrap-file service-type input-files env-vars env-file)
  (let ((api-key (get-api-key)))
    (cond
      ((string= action "list")
       (format t "~a~%" (curl-get api-key "/services")))
      ((string= action "info")
       (format t "~a~%" (curl-get api-key (format nil "/services/~a" id))))
      ((string= action "logs")
       (format t "~a~%" (curl-get api-key (format nil "/services/~a/logs" id))))
      ((string= action "sleep")
       (curl-post api-key (format nil "/services/~a/freeze" id) "{}")
       (format t "~aService frozen: ~a~a~%" *green* id *reset*))
      ((string= action "wake")
       (curl-post api-key (format nil "/services/~a/unfreeze" id) "{}")
       (format t "~aService unfreezing: ~a~a~%" *green* id *reset*))
      ((string= action "destroy")
       (curl-delete api-key (format nil "/services/~a" id))
       (format t "~aService destroyed: ~a~a~%" *green* id *reset*))
      ((string= action "resize")
       (if (or (null service-type) (string= service-type ""))
           (progn
             (format *error-output* "~aError: --resize requires --vcpu N (1-8)~a~%" *red* *reset*)
             (uiop:quit 1))
           (let* ((vcpu (parse-integer service-type))
                  (ram (* vcpu 2))
                  (json (format nil "{\"vcpu\":~a}" vcpu)))
             (curl-patch api-key (format nil "/services/~a" id) json)
             (format t "~aService resized to ~a vCPU, ~a GB RAM~a~%" *green* vcpu ram *reset*))))
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
      ;; Vault commands
      ((string= action "env-status")
       (service-env-status api-key id))
      ((string= action "env-set")
       (let ((content (build-env-content env-vars env-file)))
         (if (> (length content) 0)
             (service-env-set api-key id content)
             (progn
               (format *error-output* "~aError: No environment variables to set~a~%" *red* *reset*)
               (uiop:quit 1)))))
      ((string= action "env-export")
       (service-env-export api-key id))
      ((string= action "env-delete")
       (service-env-delete api-key id))
      ;; Create service
      ((and (string= action "create") name)
       (let* ((ports-json (if ports (format nil ",\"ports\":[~a]" ports) ""))
              (bootstrap-json (if bootstrap (format nil ",\"bootstrap\":\"~a\"" (escape-json bootstrap)) ""))
              (bootstrap-content-json (if bootstrap-file
                                          (format nil ",\"bootstrap_content\":\"~a\"" (escape-json (read-file bootstrap-file)))
                                          ""))
              (type-json (if service-type (format nil ",\"service_type\":\"~a\"" service-type) ""))
              (input-files-json (build-input-files-json input-files))
              (json (format nil "{\"name\":\"~a\"~a~a~a~a~a}" name ports-json bootstrap-json bootstrap-content-json type-json input-files-json))
              (response (curl-post api-key "/services" json))
              (service-id (parse-json-field response "id")))
         (format t "~aService created~a~%" *green* *reset*)
         (format t "~a~%" response)
         ;; Auto-set vault if env vars were provided
         (let ((env-content (build-env-content env-vars env-file)))
           (when (and service-id (> (length env-content) 0))
             (format t "~aSetting vault for service...~a~%" *yellow* *reset*)
             (service-env-set api-key service-id env-content)))))
      (t
        (format t "Error: --name required to create service, or use env subcommand~%")
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

(defun image-cmd (action id name ports source-type visibility-mode)
  (let ((api-key (get-api-key)))
    (cond
      ((string= action "list")
       (format t "~a~%" (curl-get api-key "/images")))
      ((string= action "info")
       (format t "~a~%" (curl-get api-key (format nil "/images/~a" id))))
      ((string= action "delete")
       (curl-delete api-key (format nil "/images/~a" id))
       (format t "~aImage deleted: ~a~a~%" *green* id *reset*))
      ((string= action "lock")
       (curl-post api-key (format nil "/images/~a/lock" id) "{}")
       (format t "~aImage locked: ~a~a~%" *green* id *reset*))
      ((string= action "unlock")
       (curl-post api-key (format nil "/images/~a/unlock" id) "{}")
       (format t "~aImage unlocked: ~a~a~%" *green* id *reset*))
      ((string= action "publish")
       (if (or (null source-type) (string= source-type ""))
           (progn
             (format *error-output* "~aError: --source-type required (service or snapshot)~a~%" *red* *reset*)
             (uiop:quit 1))
           (let* ((name-json (if name (format nil ",\"name\":\"~a\"" (escape-json name)) ""))
                  (json (format nil "{\"source_type\":\"~a\",\"source_id\":\"~a\"~a}" source-type id name-json))
                  (response (curl-post api-key "/images/publish" json)))
             (format t "~aImage published~a~%" *green* *reset*)
             (format t "~a~%" response))))
      ((string= action "visibility")
       (if (or (null visibility-mode) (string= visibility-mode ""))
           (progn
             (format *error-output* "~aError: --visibility requires MODE (private, unlisted, or public)~a~%" *red* *reset*)
             (uiop:quit 1))
           (let ((json (format nil "{\"visibility\":\"~a\"}" visibility-mode)))
             (curl-post api-key (format nil "/images/~a/visibility" id) json)
             (format t "~aImage visibility set to ~a: ~a~a~%" *green* visibility-mode id *reset*))))
      ((string= action "spawn")
       (let* ((name-json (if name (format nil "\"name\":\"~a\"" (escape-json name)) ""))
              (ports-json (if ports (format nil "\"ports\":[~a]" ports) ""))
              (content (cond
                        ((and name ports) (format nil "~a,~a" name-json ports-json))
                        (name name-json)
                        (ports ports-json)
                        (t "")))
              (json (format nil "{~a}" content))
              (response (curl-post api-key (format nil "/images/~a/spawn" id) json)))
         (format t "~aService spawned from image~a~%" *green* *reset*)
         (format t "~a~%" response)))
      ((string= action "clone")
       (let* ((name-json (if name (format nil "\"name\":\"~a\"" (escape-json name)) ""))
              (json (format nil "{~a}" name-json))
              (response (curl-post api-key (format nil "/images/~a/clone" id) json)))
         (format t "~aImage cloned~a~%" *green* *reset*)
         (format t "~a~%" response)))
      (t
        (format t "~aError: Use --list, --info ID, --delete ID, --lock ID, --unlock ID, --publish ID, --visibility ID MODE, --spawn ID, or --clone ID~a~%" *red* *reset*)
        (uiop:quit 1)))))

(defun parse-input-files (args)
  "Parse -f flags from args and return list of filenames"
  (let ((files nil))
    (loop for i from 0 below (1- (length args))
          do (when (string= (nth i args) "-f")
               (let ((file (nth (1+ i) args)))
                 (if (probe-file file)
                     (push file files)
                     (progn
                       (format *error-output* "Error: File not found: ~a~%" file)
                       (uiop:quit 1))))))
    (nreverse files)))

(defun get-languages-cache-path ()
  "Get the path to the languages cache file"
  (let ((home (uiop:getenv "HOME")))
    (format nil "~a/.unsandbox/languages.json" home)))

(defun load-languages-cache ()
  "Load languages from cache if valid"
  (let ((cache-path (get-languages-cache-path)))
    (when (probe-file cache-path)
      (handler-case
          (let* ((content (read-file cache-path))
                 (timestamp-str (parse-json-field content "timestamp")))
            (when timestamp-str
              (let* ((cache-time (parse-integer timestamp-str))
                     (now (get-universal-time)))
                (when (< (- now cache-time) *languages-cache-ttl*)
                  content))))
        (error () nil)))))

(defun save-languages-cache (languages-json)
  "Save languages to cache"
  (let* ((cache-path (get-languages-cache-path))
         (cache-dir (directory-namestring cache-path)))
    (ensure-directories-exist cache-path)
    (let* ((timestamp (get-universal-time))
           (cache-content (format nil "{\"languages\":~a,\"timestamp\":~a}" languages-json timestamp)))
      (with-open-file (stream cache-path :direction :output :if-exists :supersede)
        (write-string cache-content stream)))))

(defun languages-cmd (json-output)
  "List available programming languages"
  (let* ((api-key (get-api-key))
         ;; Try cache first
         (cached (load-languages-cache))
         (response (if cached
                       cached
                       (let ((resp (curl-get api-key "/languages")))
                         ;; Extract and cache the languages array
                         (let ((start (search "\"languages\":[" resp)))
                           (when start
                             (let* ((array-start (+ start (length "\"languages\":")))
                                    (array-end (position #\] resp :start array-start)))
                               (when array-end
                                 (let ((languages-json (subseq resp array-start (1+ array-end))))
                                   (save-languages-cache languages-json))))))
                         resp)))
         (languages-start (search "\"languages\":[" response)))
    (if json-output
        ;; JSON output - extract and print the languages array
        (if languages-start
            (let* ((array-start (+ languages-start (length "\"languages\":")))
                   (array-end (position #\] response :start array-start))
                   (array-content (subseq response array-start (1+ array-end))))
              (format t "~a~%" array-content))
            (format t "[]~%"))
        ;; Plain output - one language per line
        (if languages-start
            (let* ((array-start (+ languages-start (length "\"languages\":[")))
                   (array-end (position #\] response :start array-start))
                   (array-content (subseq response array-start array-end)))
              ;; Parse each quoted string
              (loop for i = 0 then (+ j 1)
                    for start = (position #\" array-content :start i)
                    while start
                    for end = (position #\" array-content :start (1+ start))
                    while end
                    for j = end
                    do (format t "~a~%" (subseq array-content (1+ start) end))))
            (format t "")))))

(defun main ()
  (let ((args (uiop:command-line-arguments)))
    (if (null args)
        (progn
          (format t "Usage: un.lisp [options] <source_file>~%")
          (format t "       un.lisp session [options]~%")
          (format t "       un.lisp service [options]~%")
          (format t "       un.lisp image [options]~%")
          (format t "       un.lisp languages [--json]~%")
          (format t "       un.lisp key [--extend]~%")
          (format t "~%Image options:~%")
          (format t "  --list              List all images~%")
          (format t "  --info ID           Get image details~%")
          (format t "  --delete ID         Delete an image~%")
          (format t "  --lock ID           Lock image to prevent deletion~%")
          (format t "  --unlock ID         Unlock image~%")
          (format t "  --publish ID        Publish image (requires --source-type)~%")
          (format t "  --source-type TYPE  Source type: service or snapshot~%")
          (format t "  --visibility ID MODE  Set visibility: private, unlisted, public~%")
          (format t "  --spawn ID          Spawn service from image~%")
          (format t "  --clone ID          Clone an image~%")
          (format t "  --name NAME         Name for spawned service or cloned image~%")
          (format t "  --ports PORTS       Ports for spawned service~%")
          (uiop:quit 1))
        (cond
          ((string= (first args) "languages")
           (let ((json-flag (and (> (length args) 1) (string= (second args) "--json"))))
             (languages-cmd json-flag)))
          ((string= (first args) "session")
           (cond
             ((and (> (length args) 1) (string= (second args) "--list"))
              (session-cmd "list" nil nil nil))
             ((and (> (length args) 2) (string= (second args) "--kill"))
              (session-cmd "kill" (third args) nil nil))
             (t
               ;; Parse session create options including -f
               (let* ((rest-args (cdr args))
                      (shell nil)
                      (input-files (parse-input-files rest-args)))
                 (loop for i from 0 below (1- (length rest-args))
                       do (let ((opt (nth i rest-args))
                                (val (nth (1+ i) rest-args)))
                            (cond
                              ((or (string= opt "--shell") (string= opt "-s")) (setf shell val))
                              ((string= opt "-f") nil) ; already parsed
                              ((and (> (length opt) 0) (char= (char opt 0) #\-))
                               (format *error-output* "Unknown option: ~a~%" opt)
                               (format *error-output* "Usage: un.lisp session [options]~%")
                               (uiop:quit 1)))))
                 (session-cmd "create" nil shell input-files)))))
          ((string= (first args) "service")
           (cond
             ((and (> (length args) 1) (string= (second args) "--list"))
              (service-cmd "list" nil nil nil nil nil nil nil nil nil))
             ((and (> (length args) 2) (string= (second args) "--info"))
              (service-cmd "info" (third args) nil nil nil nil nil nil nil nil))
             ((and (> (length args) 2) (string= (second args) "--logs"))
              (service-cmd "logs" (third args) nil nil nil nil nil nil nil nil))
             ((and (> (length args) 2) (string= (second args) "--freeze"))
              (service-cmd "sleep" (third args) nil nil nil nil nil nil nil nil))
             ((and (> (length args) 2) (string= (second args) "--unfreeze"))
              (service-cmd "wake" (third args) nil nil nil nil nil nil nil nil))
             ((and (> (length args) 2) (string= (second args) "--destroy"))
              (service-cmd "destroy" (third args) nil nil nil nil nil nil nil nil))
             ((and (> (length args) 3) (string= (second args) "--resize"))
              ;; --resize ID --vcpu N: id is third, vcpu is fourth (after -v flag)
              (let ((id (third args))
                    (vcpu (if (and (> (length args) 4)
                                   (or (string= (fourth args) "-v")
                                       (string= (fourth args) "--vcpu")))
                              (fifth args)
                              nil)))
                (service-cmd "resize" id nil nil nil nil vcpu nil nil nil)))
             ((and (> (length args) 3) (string= (second args) "--execute"))
              (service-cmd "execute" (third args) nil nil (fourth args) nil nil nil nil nil))
             ((and (> (length args) 3) (string= (second args) "--dump-bootstrap"))
              (service-cmd "dump-bootstrap" (third args) nil nil nil nil (fourth args) nil nil nil))
             ((and (> (length args) 2) (string= (second args) "--dump-bootstrap"))
              (service-cmd "dump-bootstrap" (third args) nil nil nil nil nil nil nil nil))
             ;; Service env subcommand: service env <action> <id> [options]
             ((and (> (length args) 1) (string= (second args) "env"))
              (if (< (length args) 4)
                  (progn
                    (format *error-output* "Usage: un.lisp service env <status|set|export|delete> <service_id> [options]~%")
                    (uiop:quit 1))
                  (let* ((env-action (third args))
                         (service-id (fourth args))
                         (rest-args (if (> (length args) 4) (nthcdr 4 args) nil)))
                    (cond
                      ((string= env-action "status")
                       (service-cmd "env-status" service-id nil nil nil nil nil nil nil nil))
                      ((string= env-action "set")
                       ;; Parse -e and --env-file from rest-args
                       (let ((env-vars nil)
                             (env-file nil))
                         (loop for i from 0 below (1- (length rest-args))
                               do (let ((opt (nth i rest-args))
                                        (val (nth (1+ i) rest-args)))
                                    (cond
                                      ((string= opt "-e") (push val env-vars))
                                      ((string= opt "--env-file") (setf env-file val)))))
                         (service-cmd "env-set" service-id nil nil nil nil nil nil (nreverse env-vars) env-file)))
                      ((string= env-action "export")
                       (service-cmd "env-export" service-id nil nil nil nil nil nil nil nil))
                      ((string= env-action "delete")
                       (service-cmd "env-delete" service-id nil nil nil nil nil nil nil nil))
                      (t
                        (format *error-output* "~aUnknown env action: ~a~a~%" *red* env-action *reset*)
                        (uiop:quit 1))))))
             ((and (> (length args) 2) (string= (second args) "--name"))
              (let* ((name (third args))
                     (rest-args (nthcdr 3 args))
                     (ports nil)
                     (bootstrap nil)
                     (bootstrap-file nil)
                     (service-type nil)
                     (env-vars nil)
                     (env-file nil)
                     (input-files (parse-input-files rest-args)))
                (loop for i from 0 below (1- (length rest-args))
                      do (let ((opt (nth i rest-args))
                               (val (nth (1+ i) rest-args)))
                           (cond
                             ((string= opt "--ports") (setf ports val))
                             ((string= opt "--bootstrap") (setf bootstrap val))
                             ((string= opt "--bootstrap-file") (setf bootstrap-file val))
                             ((string= opt "--type") (setf service-type val))
                             ((string= opt "-e") (push val env-vars))
                             ((string= opt "--env-file") (setf env-file val)))))
                (service-cmd "create" nil name ports bootstrap bootstrap-file service-type input-files (nreverse env-vars) env-file)))
             (t
               (format t "Error: Invalid service command~%")
               (uiop:quit 1))))
          ((string= (first args) "key")
           (let ((extend-flag (and (> (length args) 1) (string= (second args) "--extend"))))
             (key-cmd extend-flag)))
          ((string= (first args) "image")
           (cond
             ((and (> (length args) 1) (string= (second args) "--list"))
              (image-cmd "list" nil nil nil nil nil))
             ((and (> (length args) 2) (string= (second args) "--info"))
              (image-cmd "info" (third args) nil nil nil nil))
             ((and (> (length args) 2) (string= (second args) "--delete"))
              (image-cmd "delete" (third args) nil nil nil nil))
             ((and (> (length args) 2) (string= (second args) "--lock"))
              (image-cmd "lock" (third args) nil nil nil nil))
             ((and (> (length args) 2) (string= (second args) "--unlock"))
              (image-cmd "unlock" (third args) nil nil nil nil))
             ((and (> (length args) 2) (string= (second args) "--publish"))
              ;; --publish ID --source-type TYPE [--name NAME]
              (let* ((source-id (third args))
                     (rest-args (nthcdr 3 args))
                     (source-type nil)
                     (name nil))
                (loop for i from 0 below (1- (length rest-args))
                      do (let ((opt (nth i rest-args))
                               (val (nth (1+ i) rest-args)))
                           (cond
                             ((string= opt "--source-type") (setf source-type val))
                             ((string= opt "--name") (setf name val)))))
                (image-cmd "publish" source-id name nil source-type nil)))
             ((and (> (length args) 3) (string= (second args) "--visibility"))
              ;; --visibility ID MODE
              (image-cmd "visibility" (third args) nil nil nil (fourth args)))
             ((and (> (length args) 2) (string= (second args) "--spawn"))
              ;; --spawn ID [--name NAME] [--ports PORTS]
              (let* ((image-id (third args))
                     (rest-args (nthcdr 3 args))
                     (name nil)
                     (ports nil))
                (loop for i from 0 below (1- (length rest-args))
                      do (let ((opt (nth i rest-args))
                               (val (nth (1+ i) rest-args)))
                           (cond
                             ((string= opt "--name") (setf name val))
                             ((string= opt "--ports") (setf ports val)))))
                (image-cmd "spawn" image-id name ports nil nil)))
             ((and (> (length args) 2) (string= (second args) "--clone"))
              ;; --clone ID [--name NAME]
              (let* ((image-id (third args))
                     (rest-args (nthcdr 3 args))
                     (name nil))
                (loop for i from 0 below (1- (length rest-args))
                      do (let ((opt (nth i rest-args))
                               (val (nth (1+ i) rest-args)))
                           (when (string= opt "--name") (setf name val))))
                (image-cmd "clone" image-id name nil nil nil)))
             (t
               (format t "~aError: Invalid image command~a~%" *red* *reset*)
               (uiop:quit 1))))
          (t
            (execute-cmd (first args)))))))

(main)
