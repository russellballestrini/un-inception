#!/usr/bin/env -S sbcl --script
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


;;;; Common Lisp UN CLI - Unsandbox CLI Client
;;;;
;;;; Full-featured CLI matching un.py capabilities
;;;; Uses curl for HTTP (no external dependencies)

(require :asdf)
(require :sb-posix)

(defpackage :un-cli
  (:use :cl))

(in-package :un-cli)

(defparameter *blue* (format nil "~C[34m" #\Escape))
(defparameter *red* (format nil "~C[31m" #\Escape))
(defparameter *green* (format nil "~C[32m" #\Escape))
(defparameter *yellow* (format nil "~C[33m" #\Escape))
(defparameter *reset* (format nil "~C[0m" #\Escape))

(defparameter *api-base*
  (string-right-trim "/" (or (sb-ext:posix-getenv "UNSANDBOX_API_URL") "https://api.unsandbox.com")))

(defparameter *portal-base*
  ;; UNSANDBOX_PORTAL_URL: portal for /logs/{source} and /keys/extend
  (let ((u (sb-ext:posix-getenv "UNSANDBOX_PORTAL_URL")))
    (string-right-trim "/" (if (and u (plusp (length u))) u "https://unsandbox.com"))))

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
                                  (format nil "~a~a" *api-base* endpoint)
                                  "-H" "Content-Type: application/json"))
                  (response (run-curl (append base-args auth-headers (list "-d" (format nil "@~a" tmp-file))))))
             (check-clock-drift response)
             response))
      (delete-file tmp-file))))

(defun curl-get (api-key endpoint)
  (destructuring-bind (public-key secret-key) (get-api-keys)
    (let* ((auth-headers (build-auth-headers public-key secret-key "GET" endpoint ""))
           (base-args (list "curl" "-s"
                           (format nil "~a~a" *api-base* endpoint)))
           (response (run-curl (append base-args auth-headers))))
      (check-clock-drift response)
      response)))

(defun curl-delete (api-key endpoint)
  (destructuring-bind (public-key secret-key) (get-api-keys)
    (let* ((auth-headers (build-auth-headers public-key secret-key "DELETE" endpoint ""))
           (base-args (list "curl" "-s" "-X" "DELETE"
                           (format nil "~a~a" *api-base* endpoint)))
           (response (run-curl (append base-args auth-headers))))
      (check-clock-drift response)
      response)))

(defun run-curl-with-status (args)
  "Run curl and return (response-body . http-code)"
  (with-output-to-string (out)
    (let ((process (uiop:launch-program
                    (append args (list "-w" "\\n%{http_code}"))
                    :output :stream :error-output nil)))
      (loop for line = (read-line (uiop:process-info-output process) nil)
            while line do (format out "~a~%" line))
      (uiop:wait-process process))))

(defun parse-response-with-status (response)
  "Parse response to extract body and HTTP status code"
  (let* ((lines (remove-if (lambda (s) (zerop (length s)))
                           (uiop:split-string response :separator '(#\Newline))))
         (last-line (car (last lines)))
         (code (ignore-errors (parse-integer last-line))))
    (if code
        (cons (format nil "~{~a~^~%~}" (butlast lines)) code)
        (cons response 0))))

(defun handle-sudo-challenge (response-data public-key secret-key method endpoint body)
  "Handle 428 sudo OTP challenge - prompts user for OTP and retries"
  (let ((challenge-id (parse-json-field response-data "challenge_id")))
    (format *error-output* "~aConfirmation required. Check your email for a one-time code.~a~%" *yellow* *reset*)
    (format *error-output* "Enter OTP: ")
    (force-output *error-output*)
    (let ((otp (string-trim '(#\Space #\Tab #\Newline #\Return) (read-line))))
      (when (zerop (length otp))
        (format *error-output* "Error: Operation cancelled~%")
        (uiop:quit 1))
      ;; Retry the request with sudo headers
      (let* ((auth-headers (build-auth-headers public-key secret-key method endpoint (or body "")))
             (sudo-headers (list "-H" (format nil "X-Sudo-OTP: ~a" otp)
                                 "-H" (format nil "X-Sudo-Challenge: ~a" (or challenge-id ""))))
             (method-args (cond
                           ((string= method "DELETE") (list "-X" "DELETE"))
                           ((string= method "POST") (list "-X" "POST"))
                           (t (list "-X" method))))
             (content-headers (if body (list "-H" "Content-Type: application/json") nil))
             (body-args (if body (list "-d" body) nil))
             (all-args (append (list "curl" "-s" "-w" "\\n%{http_code}")
                              method-args
                              (list (format nil "~a~a" *api-base* endpoint))
                              auth-headers
                              sudo-headers
                              content-headers
                              body-args))
             (response (run-curl all-args))
             (parsed (parse-response-with-status response))
             (resp-body (car parsed))
             (http-code (cdr parsed)))
        (if (and (>= http-code 200) (< http-code 300))
            (list t resp-body)
            (progn
              (format *error-output* "~aError: HTTP ~a~a~%" *red* http-code *reset*)
              (format *error-output* "~a~%" resp-body)
              (list nil resp-body)))))))

(defun curl-delete-with-sudo (api-key endpoint)
  "DELETE request that handles 428 sudo OTP challenge"
  (destructuring-bind (public-key secret-key) (get-api-keys)
    (let* ((auth-headers (build-auth-headers public-key secret-key "DELETE" endpoint ""))
           (base-args (append (list "curl" "-s" "-w" "\\n%{http_code}" "-X" "DELETE"
                                   (format nil "~a~a" *api-base* endpoint))
                             auth-headers))
           (response (run-curl base-args))
           (parsed (parse-response-with-status response))
           (body (car parsed))
           (http-code (cdr parsed)))
      (check-clock-drift body)
      (if (= http-code 428)
          (handle-sudo-challenge body public-key secret-key "DELETE" endpoint nil)
          (list (and (>= http-code 200) (< http-code 300)) body)))))

(defun curl-post-with-sudo (api-key endpoint json-data)
  "POST request that handles 428 sudo OTP challenge"
  (let ((tmp-file (write-temp-file json-data)))
    (unwind-protect
         (destructuring-bind (public-key secret-key) (get-api-keys)
           (let* ((auth-headers (build-auth-headers public-key secret-key "POST" endpoint json-data))
                  (base-args (append (list "curl" "-s" "-w" "\\n%{http_code}" "-X" "POST"
                                          (format nil "~a~a" *api-base* endpoint)
                                          "-H" "Content-Type: application/json")
                                    auth-headers
                                    (list "-d" (format nil "@~a" tmp-file))))
                  (response (run-curl base-args))
                  (parsed (parse-response-with-status response))
                  (body (car parsed))
                  (http-code (cdr parsed)))
             (check-clock-drift body)
             (if (= http-code 428)
                 (handle-sudo-challenge body public-key secret-key "POST" endpoint json-data)
                 (list (and (>= http-code 200) (< http-code 300)) body))))
      (delete-file tmp-file))))

(defun curl-patch (api-key endpoint json-data)
  (let ((tmp-file (write-temp-file json-data)))
    (unwind-protect
         (destructuring-bind (public-key secret-key) (get-api-keys)
           (let* ((auth-headers (build-auth-headers public-key secret-key "PATCH" endpoint json-data))
                  (base-args (list "curl" "-s" "-X" "PATCH"
                                  (format nil "~a~a" *api-base* endpoint)
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
                                  (format nil "~a~a" *api-base* endpoint)
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

(defparameter *account-index* nil)

(defun load-accounts-csv (path index)
  "Load row INDEX from a CSV file of public_key,secret_key pairs.
   Skips blank lines and lines starting with #. Returns (list pk sk) or nil."
  (when (probe-file path)
    (handler-case
      (with-open-file (stream path)
        (let ((rows nil))
          (loop for line = (read-line stream nil nil)
                while line do
                (let ((trimmed (string-trim '(#\Space #\Tab #\Return) line)))
                  (when (and (> (length trimmed) 0)
                             (not (char= (char trimmed 0) #\#)))
                    (let* ((comma-pos (position #\, trimmed))
                           (end-pos (and comma-pos (position #\, trimmed :start (1+ comma-pos)))))
                      ;; public_key,secret_key[,label...] -- only field 2 is the secret
                      (when comma-pos
                        (push (list (string-trim '(#\Space #\Tab) (subseq trimmed 0 comma-pos))
                                    (string-trim '(#\Space #\Tab) (subseq trimmed (1+ comma-pos) end-pos)))
                              rows))))))
          (let ((rows (nreverse rows)))
            (when (< index (length rows))
              (nth index rows)))))
      (error () nil))))

(defun get-api-keys ()
  (let ((public-key (uiop:getenv "UNSANDBOX_PUBLIC_KEY"))
        (secret-key (uiop:getenv "UNSANDBOX_SECRET_KEY"))
        (api-key (uiop:getenv "UNSANDBOX_API_KEY"))
        (home (uiop:getenv "HOME")))
    (cond
      ;; --account N: load row N from accounts.csv, bypasses env vars
      ((not (null *account-index*))
       (let ((result (or (load-accounts-csv
                           (format nil "~a/.unsandbox/accounts.csv" home)
                           *account-index*)
                         (load-accounts-csv "./accounts.csv" *account-index*))))
         (or result
             (progn
               (format *error-output* "Error: account ~a not found in accounts.csv~%" *account-index*)
               (uiop:quit 1)))))
      ;; env vars
      ((and public-key secret-key) (list public-key secret-key))
      (api-key (list api-key nil))
      ;; accounts.csv fallback (row 0 or UNSANDBOX_ACCOUNT env var)
      (t
       (let* ((acc-env (uiop:getenv "UNSANDBOX_ACCOUNT"))
              (row-idx (if acc-env
                           (handler-case (parse-integer acc-env) (error () 0))
                           0))
              (result (or (load-accounts-csv
                            (format nil "~a/.unsandbox/accounts.csv" home)
                            row-idx)
                          (load-accounts-csv "./accounts.csv" row-idx))))
         (or result
             (progn
               (format *error-output* "Error: UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY not set (or UNSANDBOX_API_KEY for backwards compat)~%")
               (uiop:quit 1))))))))

(defun get-api-key ()
  (first (get-api-keys)))

;; SHA-256 in pure Lisp (FIPS 180-4). Signing stays in-process, so no
;; subprocess ever sees the secret on its command line.
(defparameter +sha256-k+
  (coerce '(#x428a2f98 #x71374491 #xb5c0fbcf #xe9b5dba5 #x3956c25b #x59f111f1 #x923f82a4 #xab1c5ed5
            #xd807aa98 #x12835b01 #x243185be #x550c7dc3 #x72be5d74 #x80deb1fe #x9bdc06a7 #xc19bf174
            #xe49b69c1 #xefbe4786 #x0fc19dc6 #x240ca1cc #x2de92c6f #x4a7484aa #x5cb0a9dc #x76f988da
            #x983e5152 #xa831c66d #xb00327c8 #xbf597fc7 #xc6e00bf3 #xd5a79147 #x06ca6351 #x14292967
            #x27b70a85 #x2e1b2138 #x4d2c6dfc #x53380d13 #x650a7354 #x766a0abb #x81c2c92e #x92722c85
            #xa2bfe8a1 #xa81a664b #xc24b8b70 #xc76c51a3 #xd192e819 #xd6990624 #xf40e3585 #x106aa070
            #x19a4c116 #x1e376c08 #x2748774c #x34b0bcb5 #x391c0cb3 #x4ed8aa4a #x5b9cca4f #x682e6ff3
            #x748f82ee #x78a5636f #x84c87814 #x8cc70208 #x90befffa #xa4506ceb #xbef9a3f7 #xc67178f2)
          'vector))

(defun sha256-octets (msg)
  "SHA-256 of octet vector MSG; returns the 32-byte raw digest."
  (flet ((u32 (x) (logand x #xFFFFFFFF))
         (rotr (x n) (logand (logior (ash x (- n)) (ash x (- 32 n))) #xFFFFFFFF)))
    (let* ((len (length msg))
           (r (mod (+ len 9) 64))
           (total (+ len 9 (if (zerop r) 0 (- 64 r))))
           (buf (make-array total :element-type '(unsigned-byte 8) :initial-element 0))
           (h (vector #x6a09e667 #xbb67ae85 #x3c6ef372 #xa54ff53a
                      #x510e527f #x9b05688c #x1f83d9ab #x5be0cd19))
           (w (make-array 64 :initial-element 0))
           (bitlen (* len 8)))
      (replace buf msg)
      (setf (aref buf len) #x80)
      (dotimes (i 8)
        (setf (aref buf (- total 1 i)) (ldb (byte 8 (* 8 i)) bitlen)))
      (loop for off from 0 below total by 64 do
        (dotimes (i 16)
          (let ((p (+ off (* 4 i))))
            (setf (aref w i) (logior (ash (aref buf p) 24) (ash (aref buf (+ p 1)) 16)
                                     (ash (aref buf (+ p 2)) 8) (aref buf (+ p 3))))))
        (loop for i from 16 below 64 do
          (let* ((x (aref w (- i 15)))
                 (y (aref w (- i 2)))
                 (s0 (logxor (rotr x 7) (rotr x 18) (ash x -3)))
                 (s1 (logxor (rotr y 17) (rotr y 19) (ash y -10))))
            (setf (aref w i) (u32 (+ (aref w (- i 16)) s0 (aref w (- i 7)) s1)))))
        (let ((a (aref h 0)) (b (aref h 1)) (c (aref h 2)) (d (aref h 3))
              (e (aref h 4)) (f (aref h 5)) (g (aref h 6)) (hh (aref h 7)))
          (dotimes (i 64)
            (let* ((s1 (logxor (rotr e 6) (rotr e 11) (rotr e 25)))
                   (ch (logxor (logand e f) (logand (u32 (lognot e)) g)))
                   (t1 (u32 (+ hh s1 ch (aref +sha256-k+ i) (aref w i))))
                   (s0 (logxor (rotr a 2) (rotr a 13) (rotr a 22)))
                   (maj (logxor (logand a b) (logand a c) (logand b c)))
                   (t2 (u32 (+ s0 maj))))
              (setf hh g g f f e e (u32 (+ d t1)) d c c b b a a (u32 (+ t1 t2)))))
          (loop for v in (list a b c d e f g hh)
                for i from 0
                do (setf (aref h i) (u32 (+ (aref h i) v))))))
      (let ((out (make-array 32 :element-type '(unsigned-byte 8))))
        (dotimes (i 8 out)
          (dotimes (j 4)
            (setf (aref out (+ (* 4 i) j)) (ldb (byte 8 (- 24 (* 8 j))) (aref h i)))))))))

(defun hmac-sha256 (secret message)
  "Compute HMAC-SHA256 (RFC 2104) in-process; returns lowercase hex."
  (let* ((k0 (sb-ext:string-to-octets secret :external-format :utf-8))
         (k1 (if (> (length k0) 64) (sha256-octets k0) k0))
         (key (make-array 64 :element-type '(unsigned-byte 8) :initial-element 0)))
    (replace key k1)
    (flet ((pad-key (pad)
             (map '(vector (unsigned-byte 8)) (lambda (b) (logxor b pad)) key)))
      (let* ((inner (sha256-octets
                     (concatenate '(vector (unsigned-byte 8)) (pad-key #x36)
                                  (sb-ext:string-to-octets message :external-format :utf-8))))
             (mac (sha256-octets (concatenate '(vector (unsigned-byte 8)) (pad-key #x5c) inner))))
        (format nil "~(~{~2,'0x~}~)" (coerce mac 'list))))))

(defun make-signature (secret-key timestamp method path body)
  (let ((message (format nil "~a:~a:~a:~a" timestamp method path body)))
    (hmac-sha256 secret-key message)))

(defun build-auth-headers (public-key secret-key method path body)
  (if secret-key
      (let* ((timestamp (write-to-string (- (get-universal-time) 2208988800)))
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
       (let ((result (curl-delete-with-sudo api-key (format nil "/services/~a" id))))
         (if (first result)
             (format t "~aService destroyed: ~a~a~%" *green* id *reset*)
             (progn
               (format *error-output* "~aError destroying service~a~%" *red* *reset*)
               (uiop:quit 1)))))
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
      ((string= action "unfreeze-on-demand")
       (if (or (null service-type) (string= service-type ""))
           (progn
             (format *error-output* "~aError: --unfreeze-on-demand requires true or false~a~%" *red* *reset*)
             (uiop:quit 1))
           (let* ((enabled (string= service-type "true"))
                  (json (format nil "{\"unfreeze_on_demand\":~a}" (if enabled "true" "false"))))
             (curl-patch api-key (format nil "/services/~a" id) json)
             (format t "~aService unfreeze_on_demand set to ~a: ~a~a~%" *green* (if enabled "true" "false") id *reset*))))
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
      (let* ((value-start (or (position-if-not (lambda (c) (member c '(#\Space #\Tab #\Newline #\Return)))
                                               json :start (+ start (length field-pattern)))
                              (length json)))
             (first-char (if (< value-start (length json)) (char json value-start) #\Space)))
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

(defun json-int-field (json field)
  "Integer value of FIELD in JSON, or nil."
  (let ((raw (parse-json-field json field)))
    (when raw (ignore-errors (parse-integer raw :junk-allowed t)))))

(defun validate-key (extend-flag)
  "un key: POST /keys/validate (empty body, signed); display per CLI_SPEC.md."
  (destructuring-bind (public-key secret-key) (get-api-keys)
    (when extend-flag
      (format t "~aOpening browser to extend key...~a~%" *blue* *reset*)
      (open-browser (format nil "~a/keys/extend?pk=~a" *portal-base* public-key))
      (return-from validate-key))
    (let* ((result (parse-response-with-status
                    (run-curl (append (list "curl" "-s" "-w" "\\n%{http_code}" "-X" "POST"
                                            (format nil "~a/keys/validate" *api-base*)
                                            "-H" "Content-Type: application/json")
                                      (build-auth-headers public-key secret-key "POST" "/keys/validate" "")))))
           (response (car result))
           (code (cdr result))
           (state nil)
           (reason nil))
      (cond
        ((= code 200)
         (setf state (or (parse-json-field response "status")
                         (if (equal (parse-json-field response "valid") "true") "active" "invalid"))
               reason (format nil "status ~a" state)))
        ((= code 401)
         (let ((err (or (parse-json-field response "error") "unauthorized")))
           (setf reason err
                 state (cond ((member err '(":expired" "expired") :test #'string=) "expired")
                             ((member err '(":suspended" "suspended") :test #'string=) "suspended")
                             (t "invalid")))))
        ((= code 0)
         (format *error-output* "~aError: request to ~a/keys/validate failed~a~%" *red* *api-base* *reset*)
         (uiop:quit 1))
        (t
         (format *error-output* "~aError: HTTP ~a ~a~a~%" *red* code
                 (or (parse-json-field response "error") (parse-json-field response "message") "") *reset*)
         (uiop:quit 4)))
      (cond
        ((string= state "active")
         (let* ((tier (json-int-field response "tier"))
                (status (parse-json-field response "status"))
                (vt (json-int-field response "valid_through"))
                (expires "N/A")
                (remaining "N/A"))
           (when vt
             (let* ((secs (floor vt 1000))
                    (left (- secs (- (get-universal-time) 2208988800))))
               (multiple-value-bind (sec min hour day mon year)
                   (decode-universal-time (+ secs 2208988800) 0)
                 (setf expires (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ" year mon day hour min sec)))
               (setf remaining
                     (if (<= left 0)
                         "expired"
                         (format nil "~ad ~ah ~am" (floor left 86400) (floor (mod left 86400) 3600)
                                 (floor (mod left 3600) 60))))))
           (format t "~aValid~a~%" *green* *reset*)
           (format t "Public Key: ~a~%" public-key)
           (format t "Tier: ~a~%" (or tier "N/A"))
           (format t "Status: ~a~%" (or status "N/A"))
           (format t "Expires: ~a~%" expires)
           (format t "Time Remaining: ~a~%" remaining)))
        ((string= state "expired")
         (format t "~aExpired~a~%" *red* *reset*)
         (format t "Public Key: ~a~%" public-key)
         (format t "~aTo renew: Visit ~a/keys/extend~a~%" *yellow* *portal-base* *reset*)
         (finish-output)
         (uiop:quit 3))
        ((string= state "suspended")
         (format t "~aSuspended~a~%" *red* *reset*)
         (format t "Public Key: ~a~%" public-key)
         (finish-output)
         (uiop:quit 3))
        (t
         (format t "~aInvalid~a~%" *red* *reset*)
         (format t "Public Key: ~a~%" public-key)
         (format t "Reason: ~a~%" reason)
         (finish-output)
         (uiop:quit 3))))))

(defun key-cmd (extend-flag)
  (validate-key extend-flag))

;; ---------------------------------------------------------------------------
;; un key --rotate [--otp CODE]  (see CLI_SPEC.md)
;; ---------------------------------------------------------------------------

(defun credentials-csv-path ()
  "The accounts.csv our credentials came from, or nil (env vars). Mirrors get-api-keys tiers."
  (let ((home (uiop:getenv "HOME")))
    (flet ((find-csv (idx)
             (loop for path in (list (format nil "~a/.unsandbox/accounts.csv" home) "./accounts.csv")
                   when (load-accounts-csv path idx) return path)))
      (cond
        ((not (null *account-index*)) (find-csv *account-index*))
        ((and (uiop:getenv "UNSANDBOX_PUBLIC_KEY") (uiop:getenv "UNSANDBOX_SECRET_KEY")) nil)
        ((uiop:getenv "UNSANDBOX_API_KEY") nil)
        (t (let ((acc-env (uiop:getenv "UNSANDBOX_ACCOUNT")))
             (find-csv (if acc-env (handler-case (parse-integer acc-env) (error () 0)) 0))))))))

(defun write-private-file (path content)
  "Write CONTENT to PATH with mode 0600 (umask 077 while creating)."
  (let ((old-umask (sb-posix:umask #o077)))
    (unwind-protect
         (with-open-file (stream path :direction :output :if-exists :supersede)
           (write-string content stream))
      (sb-posix:umask old-umask)))
  (sb-posix:chmod path #o600))

(defun rewrite-accounts-csv (path public-key new-secret)
  "Replace the secret on the line whose first field is PUBLIC-KEY. Every other line,
   field, comment and the order stay as they are. Temp file (0600) + rename."
  (let* ((lines (uiop:split-string (read-file path) :separator '(#\Newline)))
         (changed nil)
         (new-lines
           (mapcar (lambda (line)
                     (let ((trimmed (string-trim '(#\Space #\Tab #\Return) line)))
                       (if (or changed (zerop (length trimmed)) (char= (char trimmed 0) #\#))
                           line
                           (let ((fields (uiop:split-string line :separator '(#\,))))
                             (if (and (>= (length fields) 2)
                                      (string= (string-trim '(#\Space #\Tab) (first fields)) public-key))
                                 (progn (setf changed t)
                                        (setf (second fields) new-secret)
                                        (format nil "~{~a~^,~}" fields))
                                 line)))))
                   lines)))
    (when changed
      (let ((tmp (format nil "~a/.~a.~a.tmp" (directory-namestring (truename path))
                         (file-namestring path) (sb-posix:getpid))))
        (write-private-file tmp (format nil "~{~a~^~%~}" new-lines))
        (sb-posix:rename tmp (namestring (truename path)))))
    changed))

(defun stdin-tty-p ()
  (let ((r (ignore-errors (sb-unix:unix-isatty 0))))
    (and r (not (eql r 0)))))

(defun rotate-post (public-key secret-key otp challenge)
  "POST /keys/self/rotate-secret, body {}. Returns (body . http-code).
   OTP + challenge headers travel in a 0600 header file, never on a command line."
  (let* ((endpoint "/keys/self/rotate-secret")
         (body "{}")
         (auth-headers (build-auth-headers public-key secret-key "POST" endpoint body))
         (hdr-file (when otp (format nil "/tmp/un_lisp_hdr_~a_~a" (sb-posix:getpid) (random 999999)))))
    (unwind-protect
         (progn
           (when hdr-file
             (write-private-file hdr-file (format nil "X-Sudo-OTP: ~a~%X-Sudo-Challenge: ~a~%" otp (or challenge ""))))
           (parse-response-with-status
            (run-curl (append (list "curl" "-s" "-w" "\\n%{http_code}" "-X" "POST"
                                    (format nil "~a~a" *api-base* endpoint)
                                    "-H" "Content-Type: application/json")
                              auth-headers
                              (when hdr-file (list "-H" (format nil "@~a" hdr-file)))
                              (list "-d" body)))))
      (when (and hdr-file (probe-file hdr-file)) (delete-file hdr-file)))))

(defun rotate-fail (resp-body http-code)
  (let ((err (parse-json-field resp-body "error"))
        (msg (parse-json-field resp-body "message")))
    (cond
      ((and (= http-code 403) (equal err "email_required"))
       (format *error-output* "Error: this key has no email; add one or use POST /keys/recover~%")
       (uiop:quit 3))
      ((= http-code 401)
       (format *error-output* "Error: authentication failed~%")
       (uiop:quit 3))
      (t
       (format *error-output* "Error: ~a~%" (or msg err (format nil "HTTP ~a" http-code)))
       (uiop:quit 4)))))

(defun key-rotate-cmd (otp-arg)
  (destructuring-bind (public-key secret-key) (get-api-keys)
    (let* ((home (uiop:getenv "HOME"))
           (unsandbox-dir (format nil "~a/.unsandbox/" home))
           (digest (string-trim '(#\Space #\Tab #\Newline #\Return)
                                (uiop:run-program (list "sha256sum") :output :string
                                                  :input (make-string-input-stream public-key))))
           (challenge-file (format nil "~arotate-~a.challenge" unsandbox-dir (subseq digest 0 16)))
           (result nil))
      (if otp-arg
          (let ((challenge (and (probe-file challenge-file)
                                (string-trim '(#\Space #\Tab #\Newline #\Return) (read-file challenge-file)))))
            (unless challenge
              (format *error-output* "Error: no rotation in progress; run: un key --rotate~%")
              (uiop:quit 2))
            (setf result (rotate-post public-key secret-key
                                      (string-trim '(#\Space #\Tab #\Newline #\Return) otp-arg) challenge))
            (when (probe-file challenge-file) (delete-file challenge-file)))
          (progn
            (setf result (rotate-post public-key secret-key nil nil))
            (when (= (cdr result) 428)
              (let ((challenge (or (parse-json-field (car result) "challenge_id") "")))
                (if (stdin-tty-p)
                    (progn
                      (format *error-output* "One-time code emailed to the key owner.~%Code: ")
                      (force-output *error-output*)
                      (let ((otp (string-trim '(#\Space #\Tab #\Newline #\Return)
                                              (or (read-line *standard-input* nil) ""))))
                        (when (zerop (length otp))
                          (format *error-output* "Error: cancelled~%")
                          (uiop:quit 1))
                        (setf result (rotate-post public-key secret-key otp challenge))))
                    (progn
                      (ensure-directories-exist unsandbox-dir)
                      (sb-posix:chmod unsandbox-dir #o700)
                      (write-private-file challenge-file challenge)
                      (format t "One-time code emailed to the key owner. Finish with: un key --rotate --otp CODE~%")
                      (finish-output)
                      (uiop:quit 0)))))))
      (unless (= (cdr result) 200)
        (rotate-fail (car result) (cdr result)))
      (let ((new-secret (parse-json-field (car result) "secret"))
            (csv-path (credentials-csv-path)))
        (unless (and new-secret (> (length new-secret) 0))
          (format *error-output* "Error: API returned no secret~%")
          (uiop:quit 4))
        (if (and csv-path (rewrite-accounts-csv csv-path public-key new-secret))
            (format t "Secret rotated. Updated ~a (public key ~a).~%" csv-path public-key)
            (progn
              (format t "New secret (shown once, store it now): ~a~%" new-secret)
              (format t "Warning: UNSANDBOX_SECRET_KEY / -k still hold the old, now invalid, secret.~%")))))))

;; Image access management functions
(defun image-grant-access (id trusted-key)
  (let* ((api-key (get-api-key))
         (json (format nil "{\"trusted_api_key\":\"~a\"}" trusted-key)))
    (curl-post api-key (format nil "/images/~a/grant-access" id) json)
    (format t "~aAccess granted to: ~a~a~%" *green* trusted-key *reset*)))

(defun image-revoke-access (id trusted-key)
  (let* ((api-key (get-api-key))
         (json (format nil "{\"trusted_api_key\":\"~a\"}" trusted-key)))
    (curl-post api-key (format nil "/images/~a/revoke-access" id) json)
    (format t "~aAccess revoked from: ~a~a~%" *green* trusted-key *reset*)))

(defun image-list-trusted (id)
  (let ((api-key (get-api-key)))
    (format t "~a~%" (curl-get api-key (format nil "/images/~a/trusted" id)))))

(defun image-transfer (id to-key)
  (let* ((api-key (get-api-key))
         (json (format nil "{\"to_api_key\":\"~a\"}" to-key)))
    (curl-post api-key (format nil "/images/~a/transfer" id) json)
    (format t "~aImage transferred to: ~a~a~%" *green* to-key *reset*)))

;; Snapshot functions
(defun snapshot-list ()
  (let ((api-key (get-api-key)))
    (format t "~a~%" (curl-get api-key "/snapshots"))))

(defun snapshot-info (id)
  (let ((api-key (get-api-key)))
    (format t "~a~%" (curl-get api-key (format nil "/snapshots/~a" id)))))

(defun snapshot-session (session-id name hot)
  (let* ((api-key (get-api-key))
         (name-json (if name (format nil ",\"name\":\"~a\"" (escape-json name)) ""))
         (hot-json (if hot ",\"hot\":true" ""))
         (json (format nil "{\"session_id\":\"~a\"~a~a}" session-id name-json hot-json)))
    (format t "~aSnapshot created~a~%" *green* *reset*)
    (format t "~a~%" (curl-post api-key "/snapshots" json))))

(defun snapshot-service (service-id name hot)
  (let* ((api-key (get-api-key))
         (name-json (if name (format nil ",\"name\":\"~a\"" (escape-json name)) ""))
         (hot-json (if hot ",\"hot\":true" ""))
         (json (format nil "{\"service_id\":\"~a\"~a~a}" service-id name-json hot-json)))
    (format t "~aSnapshot created~a~%" *green* *reset*)
    (format t "~a~%" (curl-post api-key "/snapshots" json))))

(defun snapshot-restore (id)
  (let ((api-key (get-api-key)))
    (curl-post api-key (format nil "/snapshots/~a/restore" id) "{}")
    (format t "~aSnapshot restored: ~a~a~%" *green* id *reset*)))

(defun snapshot-delete (id)
  (let* ((api-key (get-api-key))
         (result (curl-delete-with-sudo api-key (format nil "/snapshots/~a" id))))
    (if (first result)
        (format t "~aSnapshot deleted: ~a~a~%" *green* id *reset*)
        (progn
          (format *error-output* "~aError deleting snapshot~a~%" *red* *reset*)
          (uiop:quit 1)))))

(defun snapshot-lock (id)
  (let ((api-key (get-api-key)))
    (curl-post api-key (format nil "/snapshots/~a/lock" id) "{}")
    (format t "~aSnapshot locked: ~a~a~%" *green* id *reset*)))

(defun snapshot-unlock (id)
  (let* ((api-key (get-api-key))
         (result (curl-post-with-sudo api-key (format nil "/snapshots/~a/unlock" id) "{}")))
    (if (first result)
        (format t "~aSnapshot unlocked: ~a~a~%" *green* id *reset*)
        (progn
          (format *error-output* "~aError unlocking snapshot~a~%" *red* *reset*)
          (uiop:quit 1)))))

(defun snapshot-clone (id clone-type name ports shell)
  (let* ((api-key (get-api-key))
         (type-json (format nil "\"clone_type\":\"~a\"" clone-type))
         (name-json (if name (format nil ",\"name\":\"~a\"" (escape-json name)) ""))
         (ports-json (if ports (format nil ",\"ports\":[~a]" ports) ""))
         (shell-json (if shell (format nil ",\"shell\":\"~a\"" shell) ""))
         (json (format nil "{~a~a~a~a}" type-json name-json ports-json shell-json)))
    (format t "~aSnapshot cloned~a~%" *green* *reset*)
    (format t "~a~%" (curl-post api-key (format nil "/snapshots/~a/clone" id) json))))

(defun snapshot-cmd (action id name ports shell hot)
  (cond
    ((string= action "list") (snapshot-list))
    ((string= action "info") (snapshot-info id))
    ((string= action "session") (snapshot-session id name hot))
    ((string= action "service") (snapshot-service id name hot))
    ((string= action "restore") (snapshot-restore id))
    ((string= action "delete") (snapshot-delete id))
    ((string= action "lock") (snapshot-lock id))
    ((string= action "unlock") (snapshot-unlock id))
    ((string= action "clone") (snapshot-clone id "session" name ports shell))
    (t (format t "~aError: Unknown snapshot action~a~%" *red* *reset*)
       (uiop:quit 1))))

;; Session additional functions
(defun session-info (id)
  (let ((api-key (get-api-key)))
    (format t "~a~%" (curl-get api-key (format nil "/sessions/~a" id)))))

(defun session-boost (id vcpu)
  (let* ((api-key (get-api-key))
         (json (format nil "{\"vcpu\":~a}" vcpu)))
    (curl-patch api-key (format nil "/sessions/~a" id) json)
    (format t "~aSession boosted to ~a vCPU~a~%" *green* vcpu *reset*)))

(defun session-unboost (id)
  (let* ((api-key (get-api-key))
         (json "{\"vcpu\":1}"))
    (curl-patch api-key (format nil "/sessions/~a" id) json)
    (format t "~aSession unboosted to 1 vCPU~a~%" *green* *reset*)))

(defun session-execute (id command)
  (let* ((api-key (get-api-key))
         (json (format nil "{\"command\":\"~a\"}" (escape-json command)))
         (response (curl-post api-key (format nil "/sessions/~a/execute" id) json))
         (stdout-val (parse-json-field response "stdout")))
    (when stdout-val
      (format t "~a~a~a" *blue* stdout-val *reset*))))

;; Service additional functions
(defun service-lock (id)
  (let ((api-key (get-api-key)))
    (curl-post api-key (format nil "/services/~a/lock" id) "{}")
    (format t "~aService locked: ~a~a~%" *green* id *reset*)))

(defun service-unlock (id)
  (let* ((api-key (get-api-key))
         (result (curl-post-with-sudo api-key (format nil "/services/~a/unlock" id) "{}")))
    (if (first result)
        (format t "~aService unlocked: ~a~a~%" *green* id *reset*)
        (progn
          (format *error-output* "~aError unlocking service~a~%" *red* *reset*)
          (uiop:quit 1)))))

(defun service-redeploy (id bootstrap)
  (let* ((api-key (get-api-key))
         (json (if bootstrap
                   (format nil "{\"bootstrap\":\"~a\"}" (escape-json bootstrap))
                   "{}")))
    (curl-post api-key (format nil "/services/~a/redeploy" id) json)
    (format t "~aService redeploying: ~a~a~%" *green* id *reset*)))

;; PaaS logs: GET {portal}/logs/{source}[/stream], HMAC over the path only.
;; Partner keys with log access; anything else answers 401/403.
(defun unix-now () (- (get-universal-time) 2208988800))

(defun url-encode (str)
  (with-output-to-string (out)
    (loop for b across (sb-ext:string-to-octets str :external-format :utf-8)
          for c = (code-char b)
          do (if (and (< b 128) (or (alphanumericp c) (find c "-_.~")))
                 (write-char c out)
                 (format out "%~2,'0X" b)))))

(defun logs-query (base params)
  "PARAMS: list of (key . value); empty/nil values are skipped."
  (let ((url base) (sep "?"))
    (dolist (p params url)
      (when (and (cdr p) (plusp (length (cdr p))))
        (setf url (format nil "~a~a~a=~a" url sep (car p) (url-encode (cdr p))) sep "&")))))

;; Minimal JSON reader: objects -> (:obj (key . val) ...), arrays -> vector,
;; strings, numbers, :true/:false/:null.
(defun json-parse (str)
  (let ((pos 0) (len (length str)))
    (labels ((peek () (if (< pos len) (char str pos) #\Nul))
             (skip-ws () (loop while (and (< pos len) (member (peek) '(#\Space #\Tab #\Newline #\Return))) do (incf pos)))
             (pstring ()
               (incf pos)
               (with-output-to-string (out)
                 (loop while (< pos len)
                       do (let ((c (char str pos)))
                            (incf pos)
                            (cond ((char= c #\") (return))
                                  ((char= c #\\)
                                   (let ((e (peek)))
                                     (incf pos)
                                     (case e
                                       (#\n (write-char #\Newline out))
                                       (#\t (write-char #\Tab out))
                                       (#\r (write-char #\Return out))
                                       (#\b (write-char #\Backspace out))
                                       (#\f (write-char #\Page out))
                                       (#\u (let ((n (ignore-errors (parse-integer str :start pos :end (min len (+ pos 4)) :radix 16))))
                                              (incf pos 4)
                                              (write-char (code-char (or n 63)) out)))
                                       (t (write-char e out)))))
                                  (t (write-char c out)))))))
             (pvalue ()
               (skip-ws)
               (let ((c (peek)))
                 (cond
                   ((char= c #\{)
                    (incf pos)
                    (let ((acc nil))
                      (loop (skip-ws)
                            (case (peek)
                              (#\} (incf pos) (return (cons :obj (nreverse acc))))
                              (#\, (incf pos))
                              (#\" (let ((k (pstring))) (skip-ws) (incf pos) (push (cons k (pvalue)) acc)))
                              (t (setf pos len) (return (cons :obj (nreverse acc))))))))
                   ((char= c #\[)
                    (incf pos)
                    (let ((acc nil))
                      (loop (skip-ws)
                            (cond ((char= (peek) #\]) (incf pos) (return (coerce (nreverse acc) 'vector)))
                                  ((char= (peek) #\,) (incf pos))
                                  ((>= pos len) (return (coerce (nreverse acc) 'vector)))
                                  (t (push (pvalue) acc))))))
                   ((char= c #\") (pstring))
                   (t (let* ((end (or (position-if (lambda (ch) (find ch ",}] 
")) str :start pos) len))
                             (tok (subseq str pos end)))
                        (setf pos end)
                        (cond ((string= tok "true") :true) ((string= tok "false") :false)
                              ((string= tok "null") :null)
                              (t (or (ignore-errors (parse-integer tok)) tok)))))))))
      (ignore-errors (pvalue)))))

(defun json-get (obj key)
  (and (consp obj) (eq (car obj) :obj) (cdr (assoc key (cdr obj) :test #'string=))))

(defun log-cursor-path ()
  (format nil "~a/.unsandbox/paas_log_cursor" (or (uiop:getenv "HOME") ".")))

(defun read-log-cursor ()
  "Seconds since our last successful fetch as \"Ns\" (min 5), or nil."
  (ignore-errors
    (with-open-file (in (log-cursor-path) :if-does-not-exist nil)
      (when in
        (let ((ts (ignore-errors (parse-integer (string-trim '(#\Space #\Newline #\Return) (or (read-line in nil) ""))))))
          (when (and ts (plusp ts)) (format nil "~as" (max 5 (- (unix-now) ts)))))))))

(defun write-log-cursor ()
  (ignore-errors
    (let ((dir (format nil "~a/.unsandbox/" (or (uiop:getenv "HOME") "."))))
      (unless (probe-file dir) (sb-posix:mkdir dir #o700))
      (with-open-file (out (log-cursor-path) :direction :output :if-exists :supersede)
        (format out "~a~%" (unix-now))))))

(defun logs-curl-args (public-key secret-key path url stream-p)
  "argv for curl (no shell); the secret only signs, it never reaches argv."
  (append (list "curl" "-s" "-A" "un-cli/2.0")
          (when stream-p (list "-N" "--http1.1" "-H" "Accept: text/event-stream"))
          (list "-w" (format nil "~%__UN_HTTP__%{http_code}~%"))
          (build-auth-headers public-key secret-key "GET" path "")
          (list url)))

(defun logs-print-line (source line)
  (if (and (stringp source) (plusp (length source)))
      (format t "~12a ~a~%" source line)
      (format t "~a~%" line))
  (finish-output))

(defun logs-error-text (body)
  (let ((e (json-get (json-parse body) "error")))
    (if (stringp e) (format nil " ~a ~a" (code-char 8212) e) "")))

(defun paas-logs-fetch (public-key secret-key source lines since grep-pattern level json-output)
  "Batch fetch; prints lines (or raw JSON). Returns 0 or 1."
  (let* ((path (format nil "/logs/~a" source))
         (url (logs-query (format nil "~a~a" *portal-base* path)
                          (list (cons "lines" (princ-to-string lines)) (cons "since" since)
                                (cons "grep" grep-pattern) (cons "level" level))))
         (out (run-curl (logs-curl-args public-key secret-key path url nil)))
         (mark (search (format nil "~%__UN_HTTP__") out))
         (body (if mark (subseq out 0 mark) out))
         (code (or (and mark (ignore-errors (parse-integer (string-trim '(#\Space #\Newline #\Return) (subseq out (+ mark 12)))))) 0)))
    (cond
      ((= code 0) (format *error-output* "Error: Network error fetching logs~%") 1)
      ((/= code 200) (format *error-output* "Error: HTTP ~a~a~%" code (logs-error-text body)) 1)
      (t
       (write-log-cursor)
       (if json-output
           (progn (format t "~a~%" body) 0)
           (let* ((j (json-parse body)) (ls (json-get j "lines")) (all (json-get j "sources")))
             (if (not (vectorp ls))
                 (progn (format *error-output* "Error: Invalid response format~%") 1)
                 (progn
                   (loop for e across ls
                         do (cond ((stringp e) (logs-print-line nil e))
                                  ((stringp (json-get e "line"))
                                   (logs-print-line (and all (json-get e "source")) (json-get e "line")))))
                   0))))))))

(defun paas-logs-stream (public-key secret-key source grep-pattern level)
  "Follow via SSE; reconnects with backoff, re-signing each time. Returns exit code."
  (let ((path (format nil "/logs/~a/stream" source))
        (delay 2) (last-recv 0) (reconnect nil))
    (format *error-output* "Streaming ~a logs... (Ctrl+C to stop)~%" source)
    (loop
      (let* ((since (when (and reconnect (plusp last-recv))
                      (format nil "~as" (max 10 (+ (- (unix-now) last-recv) 5)))))
             (url (logs-query (format nil "~a~a" *portal-base* path)
                              (list (cons "grep" grep-pattern) (cons "level" level) (cons "since" since))))
             (started (unix-now))
             (proc (uiop:launch-program (logs-curl-args public-key secret-key path url t)
                                        :output :stream :error-output nil :external-format :utf-8))
             (in (uiop:process-info-output proc))
             (code 0))
        (loop for line = (read-line in nil) while line
              do (cond
                   ((and (>= (length line) 11) (string= "__UN_HTTP__" line :end2 11))
                    (setf code (or (ignore-errors (parse-integer (string-trim '(#\Space #\Return) (subseq line 11)))) 0)))
                   ((and (>= (length line) 6) (string= "data: " line :end2 6))
                    (let* ((payload (subseq line 6)) (j (json-parse payload)) (l (json-get j "line")))
                      (cond ((string= payload ":keepalive"))
                            ((stringp l) (logs-print-line (json-get j "source") l) (setf last-recv (unix-now)))
                            ((json-get j "event"))
                            (t (logs-print-line nil payload) (setf last-recv (unix-now))))))))
        (uiop:wait-process proc)
        (cond
          ((member code '(401 403))
           (format *error-output* "Error: HTTP ~a (auth failed)~%" code) (return 1))
          ((or (= code 0) (= code 200) (>= code 500))
           (when (> (- (unix-now) started) 30) (setf delay 2))
           (sleep delay)
           (setf delay (min 30 (* delay 2)) reconnect t))
          (t (format *error-output* "Error: HTTP ~a~%" code) (return 1)))))))

(defun paas-logs-help ()
  (format *error-output* "~{~a~%~}"
          '("Usage: un.lisp paas logs [options]" ""
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

(defun paas-cmd (args)
  (cond
    ((null args)
     (format *error-output* "Usage: un.lisp paas <command> [options]~%~%Commands:~%  logs    Fetch or stream production logs~%")
     (uiop:quit 1))
    ((string/= (first args) "logs")
     (format *error-output* "Error: Unknown paas command '~a'~%" (first args))
     (uiop:quit 1))
    (t
     (let ((source "all") (lines 100) since grep-pattern level follow json-output pk sk
           (a (rest args)))
       (loop while a
             do (let ((opt (first a)) (val (second a)))
                  (cond
                    ((member opt '("-h" "--help") :test #'string=) (paas-logs-help) (uiop:quit 0))
                    ((string= opt "--all") (setf source "all"))
                    ((string= opt "--api") (setf source "api"))
                    ((string= opt "--portal") (setf source "portal"))
                    ((member opt '("--follow" "-f") :test #'string=) (setf follow t))
                    ((string= opt "--json") (setf json-output t))
                    ((null val))
                    ((string= opt "--pool") (setf source (format nil "pool/~a" val)) (pop a))
                    ((string= opt "--source") (setf source val) (pop a))
                    ((member opt '("--lines" "-n") :test #'string=)
                     (setf lines (max 1 (min 10000 (or (ignore-errors (parse-integer val)) 1)))) (pop a))
                    ((string= opt "--since") (setf since val) (pop a))
                    ((string= opt "--grep") (setf grep-pattern val) (pop a))
                    ((member opt '("--level" "-l") :test #'string=) (setf level val) (pop a))
                    ((string= opt "-p") (setf pk val) (pop a))
                    ((string= opt "-k") (setf sk val) (pop a))))
                (pop a))
       (destructuring-bind (public-key secret-key) (if (and pk sk) (list pk sk) (get-api-keys))
         (uiop:quit (if follow
                        (paas-logs-stream public-key secret-key source grep-pattern level)
                        (paas-logs-fetch public-key secret-key source lines
                                         (or since (read-log-cursor) "5m")
                                         grep-pattern level json-output))))))))

;; Library form kept for callers: prints the raw JSON response.
(defun logs-fetch (source lines since grep-pattern &optional level)
  (destructuring-bind (public-key secret-key) (get-api-keys)
    (paas-logs-fetch public-key secret-key (or source "all") (or lines 100)
                     (or since "5m") grep-pattern level t)))

;; Utility functions
(defun health-check ()
  (let* ((cmd (format nil "curl -s ~a/health" *api-base*))
         (result (uiop:run-program cmd :output :string)))
    (format t "~a~%" result)
    (search "ok" result)))

(defun sdk-version ()
  "4.2.0")

(defun image-cmd (action id name ports source-type visibility-mode)
  (let ((api-key (get-api-key)))
    (cond
      ((string= action "list")
       (format t "~a~%" (curl-get api-key "/images")))
      ((string= action "info")
       (format t "~a~%" (curl-get api-key (format nil "/images/~a" id))))
      ((string= action "delete")
       (let ((result (curl-delete-with-sudo api-key (format nil "/images/~a" id))))
         (if (first result)
             (format t "~aImage deleted: ~a~a~%" *green* id *reset*)
             (progn
               (format *error-output* "~aError deleting image~a~%" *red* *reset*)
               (uiop:quit 1)))))
      ((string= action "lock")
       (curl-post api-key (format nil "/images/~a/lock" id) "{}")
       (format t "~aImage locked: ~a~a~%" *green* id *reset*))
      ((string= action "unlock")
       (let ((result (curl-post-with-sudo api-key (format nil "/images/~a/unlock" id) "{}")))
         (if (first result)
             (format t "~aImage unlocked: ~a~a~%" *green* id *reset*)
             (progn
               (format *error-output* "~aError unlocking image~a~%" *red* *reset*)
               (uiop:quit 1)))))
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

(defun strip-account-flag (args)
  "Remove --account N from args, set *account-index* as side-effect. Returns filtered args."
  (let ((result nil)
        (i 0)
        (vec (coerce args 'vector)))
    (loop while (< i (length vec)) do
      (cond
        ((string= (aref vec i) "--account")
         (when (< (+ i 1) (length vec))
           (setf *account-index*
                 (handler-case (parse-integer (aref vec (+ i 1))) (error () nil))))
         (incf i 2))
        (t
         (push (aref vec i) result)
         (incf i))))
    (nreverse result)))

(defun main ()
  (let* ((raw-args (uiop:command-line-arguments))
         (args (strip-account-flag raw-args)))
    (if (null args)
        (progn
          (format t "Usage: un.lisp [options] <source_file>~%")
          (format t "       un.lisp session [options]~%")
          (format t "       un.lisp service [options]~%")
          (format t "       un.lisp image [options]~%")
          (format t "       un.lisp languages [--json]~%")
          (format t "       un.lisp paas logs [options]~%")
          (format t "       un.lisp key [--extend] | key --rotate [--otp CODE]~%")
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
          ((string= (first args) "paas")
           (paas-cmd (rest args)))
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
             ((and (> (length args) 3) (string= (second args) "--unfreeze-on-demand"))
              ;; --unfreeze-on-demand ID true|false: id is third, value is fourth
              (let ((id (third args))
                    (value (fourth args)))
                (service-cmd "unfreeze-on-demand" id nil nil nil nil value nil nil nil)))
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
           (let ((extend-flag (member "--extend" (cdr args) :test #'string=))
                 (rotate-flag (member "--rotate" (cdr args) :test #'string=))
                 (otp-tail (member "--otp" (cdr args) :test #'string=)))
             (when (and otp-tail (null (second otp-tail)))
               (format *error-output* "Error: --otp requires CODE~%")
               (uiop:quit 2))
             (if (or rotate-flag otp-tail)
                 (key-rotate-cmd (second otp-tail))
                 (key-cmd extend-flag))))
          ((string= (first args) "snapshot")
           (cond
             ((and (> (length args) 1) (string= (second args) "--list"))
              (snapshot-cmd "list" nil nil nil nil nil))
             ((and (> (length args) 2) (string= (second args) "--info"))
              (snapshot-cmd "info" (third args) nil nil nil nil))
             ((and (> (length args) 2) (string= (second args) "--session"))
              (snapshot-cmd "session" (third args) nil nil nil nil))
             ((and (> (length args) 2) (string= (second args) "--service"))
              (snapshot-cmd "service" (third args) nil nil nil nil))
             ((and (> (length args) 2) (string= (second args) "--restore"))
              (snapshot-cmd "restore" (third args) nil nil nil nil))
             ((and (> (length args) 2) (string= (second args) "--delete"))
              (snapshot-cmd "delete" (third args) nil nil nil nil))
             ((and (> (length args) 2) (string= (second args) "--lock"))
              (snapshot-cmd "lock" (third args) nil nil nil nil))
             ((and (> (length args) 2) (string= (second args) "--unlock"))
              (snapshot-cmd "unlock" (third args) nil nil nil nil))
             ((and (> (length args) 2) (string= (second args) "--clone"))
              (snapshot-cmd "clone" (third args) nil nil nil nil))
             (t
               (snapshot-cmd "list" nil nil nil nil nil))))
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
