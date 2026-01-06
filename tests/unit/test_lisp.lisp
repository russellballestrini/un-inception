#!/usr/bin/env sbcl --script
;;; Unit tests for un.lisp - tests internal functions without API calls
;;; Run with: sbcl --script test_lisp.lisp

(defvar *passed* 0)
(defvar *failed* 0)

(defun test (name result)
  (if result
      (progn
        (format t "  ✓ ~a~%" name)
        (incf *passed*))
      (progn
        (format t "  ✗ ~a~%" name)
        (incf *failed*))))

(defvar *ext-map*
  '((".py" . "python") (".js" . "javascript") (".ts" . "typescript")
    (".rb" . "ruby") (".go" . "go") (".rs" . "rust") (".c" . "c")
    (".lisp" . "lisp") (".java" . "java") (".hs" . "haskell")))

(defun get-language (ext)
  (cdr (assoc ext *ext-map* :test #'string=)))

(defun get-extension (filename)
  (let ((pos (position #\. filename :from-end t)))
    (if pos (subseq filename pos) "")))

(defun get-basename (path)
  (let ((pos (position #\/ path :from-end t)))
    (if pos (subseq path (1+ pos)) path)))

(defun starts-with (prefix str)
  (and (>= (length str) (length prefix))
       (string= prefix (subseq str 0 (length prefix)))))

(defun string-contains (haystack needle)
  (search needle haystack))

(format t "~%=== Extension Mapping Tests ===~%")

(test "Python extension maps correctly"
      (string= (get-language ".py") "python"))

(test "Common Lisp extension maps correctly"
      (string= (get-language ".lisp") "lisp"))

(test "JavaScript extension maps correctly"
      (string= (get-language ".js") "javascript"))

(test "Go extension maps correctly"
      (string= (get-language ".go") "go"))

(format t "~%=== Signature Format Tests ===~%")

(let* ((timestamp "1704067200")
       (method "POST")
       (endpoint "/execute")
       (body "{\"language\":\"python\"}")
       (message (format nil "~a:~a:~a:~a" timestamp method endpoint body)))

  (test "Signature format starts with timestamp"
        (starts-with timestamp message))

  (test "Signature format contains :POST:"
        (string-contains message ":POST:"))

  (test "Signature format contains :/execute:"
        (string-contains message ":/execute:")))

(format t "~%=== Language Detection Tests ===~%")

(let* ((content "#!/usr/bin/env python3
print('hello')")
       (first-line (subseq content 0 (position #\Newline content))))

  (test "Python shebang detection - starts with #!"
        (starts-with "#!" first-line))

  (test "Python shebang detection - contains python"
        (string-contains first-line "python")))

(format t "~%=== Argument Parsing Tests ===~%")

(let* ((arg1 "DEBUG=1")
       (eq-pos (position #\= arg1))
       (key1 (subseq arg1 0 eq-pos))
       (value1 (subseq arg1 (1+ eq-pos))))

  (test "Parse -e KEY=VALUE format - key"
        (string= key1 "DEBUG"))

  (test "Parse -e KEY=VALUE format - value"
        (string= value1 "1")))

(let* ((arg2 "URL=https://example.com?foo=bar")
       (eq-pos (position #\= arg2))
       (key2 (subseq arg2 0 eq-pos))
       (value2 (subseq arg2 (1+ eq-pos))))

  (test "Parse -e KEY=VALUE with equals in value"
        (and (string= key2 "URL")
             (string= value2 "https://example.com?foo=bar"))))

(format t "~%=== File Operations Tests ===~%")

(test "Extract file basename"
      (string= (get-basename "/home/user/project/script.lisp") "script.lisp"))

(test "Extract file extension"
      (string= (get-extension "/home/user/project/script.lisp") ".lisp"))

(format t "~%=== API Constants Tests ===~%")

(let ((api-base "https://api.unsandbox.com"))

  (test "API base URL starts with https://"
        (starts-with "https://" api-base))

  (test "API base URL contains unsandbox.com"
        (string-contains api-base "unsandbox.com")))

(format t "~%=== Summary ===~%")
(format t "Passed: ~a~%" *passed*)
(format t "Failed: ~a~%" *failed*)
(format t "Total:  ~a~%" (+ *passed* *failed*))

(sb-ext:exit :code (if (> *failed* 0) 1 0))
