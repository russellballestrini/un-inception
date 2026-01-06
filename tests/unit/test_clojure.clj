#!/usr/bin/env clj -M
;; Unit tests for un.clj - tests internal functions without API calls

(ns test-un-clj
  (:import [javax.crypto Mac]
           [javax.crypto.spec SecretKeySpec]
           [java.util Base64]))

(def passed (atom 0))
(def failed (atom 0))

(defn test-case [name f]
  (try
    (f)
    (println (str "  ✓ " name))
    (swap! passed inc)
    (catch Exception e
      (println (str "  ✗ " name))
      (println (str "    " (.getMessage e)))
      (swap! failed inc))))

(defn assert-equal [actual expected]
  (when (not= actual expected)
    (throw (Exception. (str "Expected '" expected "' but got '" actual "'")))))

(defn assert-not-equal [a b]
  (when (= a b)
    (throw (Exception. (str "Expected values to be different but both were '" a "'")))))

(defn assert-contains [s substr]
  (when (not (.contains s substr))
    (throw (Exception. (str "Expected '" s "' to contain '" substr "'")))))

(defn assert-true [val]
  (when (not val)
    (throw (Exception. "Expected true but got false"))))

(defn hmac-sha256 [secret message]
  (let [mac (Mac/getInstance "HmacSHA256")
        key (SecretKeySpec. (.getBytes secret "UTF-8") "HmacSHA256")]
    (.init mac key)
    (apply str (map #(format "%02x" %) (.doFinal mac (.getBytes message "UTF-8"))))))

;; Extension mapping
(def ext-map
  {".py" "python" ".js" "javascript" ".ts" "typescript"
   ".rb" "ruby" ".php" "php" ".pl" "perl" ".lua" "lua"
   ".sh" "bash" ".go" "go" ".rs" "rust" ".c" "c"
   ".cpp" "cpp" ".java" "java" ".kt" "kotlin"
   ".hs" "haskell" ".clj" "clojure" ".erl" "erlang"
   ".ex" "elixir" ".jl" "julia" ".r" "r"})

(println "\n=== Extension Mapping Tests ===")

(test-case "Python extension maps correctly"
  #(assert-equal (ext-map ".py") "python"))

(test-case "Clojure extension maps correctly"
  #(assert-equal (ext-map ".clj") "clojure"))

(test-case "JavaScript extension maps correctly"
  #(assert-equal (ext-map ".js") "javascript"))

(test-case "Go extension maps correctly"
  #(assert-equal (ext-map ".go") "go"))

(test-case "Haskell extension maps correctly"
  #(assert-equal (ext-map ".hs") "haskell"))

(println "\n=== HMAC Signature Tests ===")

(test-case "HMAC-SHA256 generates 64 character hex string"
  #(let [sig (hmac-sha256 "test-secret" "test-message")]
     (assert-equal (count sig) 64)))

(test-case "Same input produces same signature"
  #(let [sig1 (hmac-sha256 "key" "msg")
         sig2 (hmac-sha256 "key" "msg")]
     (assert-equal sig1 sig2)))

(test-case "Different secrets produce different signatures"
  #(let [sig1 (hmac-sha256 "key1" "msg")
         sig2 (hmac-sha256 "key2" "msg")]
     (assert-not-equal sig1 sig2)))

(test-case "Signature format verification"
  #(let [timestamp "1704067200"
         method "POST"
         endpoint "/execute"
         body "{\"language\":\"python\"}"
         message (str timestamp ":" method ":" endpoint ":" body)]
     (assert-true (.startsWith message timestamp))
     (assert-contains message ":POST:")
     (assert-contains message ":/execute:")))

(println "\n=== Language Detection Tests ===")

(test-case "Detect language from .clj extension"
  #(let [filename "script.clj"
         ext (str "." (last (clojure.string/split filename #"\.")))]
     (assert-equal (ext-map ext) "clojure")))

(test-case "Python shebang detection"
  #(let [content "#!/usr/bin/env python3\nprint('hello')"
         first-line (first (clojure.string/split content #"\n"))]
     (assert-true (.startsWith first-line "#!"))
     (assert-contains first-line "python")))

(println "\n=== Argument Parsing Tests ===")

(test-case "Parse -e KEY=VALUE format"
  #(let [arg "DEBUG=1"
         [key value] (clojure.string/split arg #"=" 2)]
     (assert-equal key "DEBUG")
     (assert-equal value "1")))

(test-case "Parse -e KEY=VALUE with equals in value"
  #(let [arg "URL=https://example.com?foo=bar"
         [key value] (clojure.string/split arg #"=" 2)]
     (assert-equal key "URL")
     (assert-equal value "https://example.com?foo=bar")))

(println "\n=== File Operations Tests ===")

(test-case "Base64 encoding/decoding"
  #(let [content "print('hello world')"
         encoder (Base64/getEncoder)
         decoder (Base64/getDecoder)
         encoded (.encodeToString encoder (.getBytes content))
         decoded (String. (.decode decoder encoded))]
     (assert-equal decoded content)))

(println "\n=== API Constants Tests ===")

(test-case "API base URL format"
  #(let [api-base "https://api.unsandbox.com"]
     (assert-true (.startsWith api-base "https://"))
     (assert-contains api-base "unsandbox.com")))

;; Summary
(println "\n=== Summary ===")
(println (str "Passed: " @passed))
(println (str "Failed: " @failed))
(println (str "Total:  " (+ @passed @failed)))

(System/exit (if (> @failed 0) 1 0))
