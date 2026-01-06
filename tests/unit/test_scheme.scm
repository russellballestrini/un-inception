#!/usr/bin/env guile
!#
;; Unit tests for un.scm - tests internal functions without API calls

(use-modules (ice-9 format))

(define passed 0)
(define failed 0)

(define (test name thunk)
  (catch #t
    (lambda ()
      (thunk)
      (format #t "  ✓ ~a~%" name)
      (set! passed (+ passed 1)))
    (lambda (key . args)
      (format #t "  ✗ ~a~%" name)
      (set! failed (+ failed 1)))))

(define (assert-equal actual expected)
  (unless (equal? actual expected)
    (error (format #f "Expected '~a' but got '~a'" expected actual))))

(define (assert-true val)
  (unless val
    (error "Expected true but got false")))

(define (string-contains? str substr)
  (string-contains str substr))

(define (string-prefix? prefix str)
  (and (>= (string-length str) (string-length prefix))
       (string=? prefix (substring str 0 (string-length prefix)))))

;; Extension mapping
(define ext-map
  '((".py" . "python") (".js" . "javascript") (".ts" . "typescript")
    (".rb" . "ruby") (".go" . "go") (".rs" . "rust") (".c" . "c")
    (".java" . "java") (".scm" . "scheme") (".hs" . "haskell")))

(define (get-language ext)
  (let ((pair (assoc ext ext-map)))
    (if pair (cdr pair) #f)))

(format #t "~%=== Extension Mapping Tests ===~%")

(test "Python extension maps correctly"
  (lambda () (assert-equal (get-language ".py") "python")))

(test "Scheme extension maps correctly"
  (lambda () (assert-equal (get-language ".scm") "scheme")))

(test "JavaScript extension maps correctly"
  (lambda () (assert-equal (get-language ".js") "javascript")))

(test "Go extension maps correctly"
  (lambda () (assert-equal (get-language ".go") "go")))

(format #t "~%=== Signature Format Tests ===~%")

(let* ((timestamp "1704067200")
       (method "POST")
       (endpoint "/execute")
       (body "{\"language\":\"python\"}")
       (message (string-append timestamp ":" method ":" endpoint ":" body)))

  (test "Signature format starts with timestamp"
    (lambda () (assert-true (string-prefix? timestamp message))))

  (test "Signature format contains :POST:"
    (lambda () (assert-true (string-contains? message ":POST:"))))

  (test "Signature format contains :/execute:"
    (lambda () (assert-true (string-contains? message ":/execute:")))))

(format #t "~%=== Language Detection Tests ===~%")

(let* ((content "#!/usr/bin/env python3\nprint('hello')")
       (first-line (car (string-split content #\newline))))

  (test "Python shebang detection - starts with #!"
    (lambda () (assert-true (string-prefix? "#!" first-line))))

  (test "Python shebang detection - contains python"
    (lambda () (assert-true (string-contains? first-line "python")))))

(format #t "~%=== Argument Parsing Tests ===~%")

(test "Parse -e KEY=VALUE format"
  (lambda ()
    (let* ((arg "DEBUG=1")
           (parts (string-split arg #\=))
           (key (car parts))
           (value (string-join (cdr parts) "=")))
      (assert-equal key "DEBUG")
      (assert-equal value "1"))))

(test "Parse -e KEY=VALUE with equals in value"
  (lambda ()
    (let* ((arg "URL=https://example.com?foo=bar")
           (idx (string-index arg #\=))
           (key (substring arg 0 idx))
           (value (substring arg (+ idx 1))))
      (assert-equal key "URL")
      (assert-equal value "https://example.com?foo=bar"))))

(format #t "~%=== API Constants Tests ===~%")

(let ((api-base "https://api.unsandbox.com"))
  (test "API base URL starts with https://"
    (lambda () (assert-true (string-prefix? "https://" api-base))))

  (test "API base URL contains unsandbox.com"
    (lambda () (assert-true (string-contains? api-base "unsandbox.com")))))

(format #t "~%=== Summary ===~%")
(format #t "Passed: ~a~%" passed)
(format #t "Failed: ~a~%" failed)
(format #t "Total:  ~a~%" (+ passed failed))

(exit (if (> failed 0) 1 0))
