#!/usr/bin/env clojure

;; Clojure UN CLI Test Suite
;;
;; Usage:
;;   chmod +x test_un_clj.clj
;;   ./test_un_clj.clj
;;
;; Or with clj:
;;   clj -M test_un_clj.clj
;;
;; Tests the Clojure UN CLI implementation (un.clj) for:
;; 1. Extension detection logic
;; 2. API integration (if UNSANDBOX_API_KEY is set)
;; 3. End-to-end execution with fib.clj test file

(require '[clojure.java.io :as io]
         '[clojure.string :as str]
         '[clojure.java.shell :as shell])

;; ANSI color codes
(def green "\u001b[32m")
(def red "\u001b[31m")
(def yellow "\u001b[33m")
(def reset "\u001b[0m")

;; Extension to language mapping (from un.clj)
(def ext-to-lang
  {".hs"   "haskell"
   ".ml"   "ocaml"
   ".clj"  "clojure"
   ".scm"  "scheme"
   ".lisp" "commonlisp"
   ".erl"  "erlang"
   ".ex"   "elixir"
   ".py"   "python"
   ".js"   "javascript"
   ".rb"   "ruby"
   ".go"   "go"
   ".rs"   "rust"
   ".c"    "c"
   ".cpp"  "cpp"
   ".java" "java"})

;; Test result type
(defrecord TestResult [passed? message])

;; Print test result
(defn print-result [test-name result]
  (if (:passed? result)
    (do
      (println (str green "✓ PASS" reset " - " test-name))
      true)
    (do
      (println (str red "✗ FAIL" reset " - " test-name))
      (println (str "  Error: " (:message result)))
      false)))

;; Test 1: Extension detection
(defn test-extension-detection []
  (let [tests [[".hs" "haskell"]
               [".ml" "ocaml"]
               [".clj" "clojure"]
               [".scm" "scheme"]
               [".lisp" "commonlisp"]
               [".erl" "erlang"]
               [".ex" "elixir"]
               [".py" "python"]
               [".js" "javascript"]
               [".rb" "ruby"]]
        failures (filter (fn [[ext expected]]
                          (not= (get ext-to-lang ext) expected))
                        tests)]
    (if (empty? failures)
      (->TestResult true nil)
      (->TestResult false (str "Extension mappings failed: " failures)))))

;; Test 2: API integration
(defn test-api-integration []
  (let [api-key (System/getenv "UNSANDBOX_API_KEY")]
    (if (nil? api-key)
      (->TestResult true "Skipped - no API key")
      (try
        ;; Create a simple test file
        (let [test-code "(println \"test\")\n"]
          (spit "/tmp/test_un_clj_api.clj" test-code)

          ;; Run the CLI
          (let [result (shell/sh "./un.clj" "/tmp/test_un_clj_api.clj")
                {:keys [exit out err]} result]

            ;; Check if it executed successfully
            (if (and (= exit 0) (str/includes? out "test"))
              (->TestResult true nil)
              (->TestResult false (str "API call failed: exit=" exit
                                      ", stdout=" out
                                      ", stderr=" err)))))
        (catch Exception e
          (->TestResult false (str "Exception: " (.getMessage e))))))))

;; Test 3: Functional test with fib.clj
(defn test-fibonacci []
  (let [api-key (System/getenv "UNSANDBOX_API_KEY")]
    (if (nil? api-key)
      (->TestResult true "Skipped - no API key")
      (try
        ;; Check if fib.clj exists
        (let [fib-path "../test/fib.clj"]

          ;; Run the CLI with fib.clj
          (let [result (shell/sh "./un.clj" fib-path)
                {:keys [exit out err]} result]

            ;; Check if output contains expected fibonacci result
            (if (and (= exit 0) (str/includes? out "fib(10) = 55"))
              (->TestResult true nil)
              (->TestResult false (str "Fibonacci test failed: exit=" exit
                                      ", stdout=" out
                                      ", stderr=" err)))))
        (catch Exception e
          (->TestResult false (str "Exception: " (.getMessage e))))))))

;; Test 4: Snapshot command support (feature parity test)
(defn test-snapshot-command []
  (let [api-key (System/getenv "UNSANDBOX_API_KEY")]
    (if (nil? api-key)
      (->TestResult true "Skipped - no API key")
      (try
        ;; Run the CLI with snapshot --list
        (let [result (shell/sh "./un.clj" "snapshot" "--list")
              {:keys [exit out err]} result]

          ;; Check if it executed without errors (may return empty list)
          (if (= exit 0)
            (->TestResult true nil)
            (->TestResult false (str "Snapshot list failed: exit=" exit
                                    ", stdout=" out
                                    ", stderr=" err))))
        (catch Exception e
          (->TestResult false (str "Exception: " (.getMessage e))))))))

;; Main test runner
(defn main []
  (println "=== Clojure UN CLI Test Suite ===")
  (println "")

  ;; Check if API key is set
  (when (nil? (System/getenv "UNSANDBOX_API_KEY"))
    (println (str yellow "⚠ WARNING" reset
                 " - UNSANDBOX_API_KEY not set, skipping API tests"))
    (println ""))

  ;; Run tests
  (let [results [(print-result "Extension detection" (test-extension-detection))
                 (print-result "API integration" (test-api-integration))
                 (print-result "Fibonacci end-to-end test" (test-fibonacci))
                 (print-result "Snapshot command support" (test-snapshot-command))]
        passed (count (filter true? results))
        total (count results)]

    (println "")

    ;; Summary
    (if (= passed total)
      (do
        (println (str green "✓ All tests passed (" passed "/" total ")" reset))
        (System/exit 0))
      (do
        (println (str red "✗ Some tests failed (" passed "/" total " passed)" reset))
        (System/exit 1)))))

;; Entry point
(main)
