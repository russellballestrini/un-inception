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


#!/usr/bin/env bb

;; Clojure UN CLI - Unsandbox CLI Client
;;
;; Full-featured CLI matching un.py capabilities:
;; - Execute code with env vars, input files, artifacts
;; - Interactive sessions with shell/REPL support
;; - Persistent services with domains and ports
;;
;; Usage:
;;   chmod +x un.clj
;;   export UNSANDBOX_API_KEY="your_key_here"
;;   ./un.clj [options] <source_file>
;;   ./un.clj session [options]
;;   ./un.clj service [options]
;;
;; Uses curl for HTTP (no external dependencies)

(require '[clojure.java.io :as io]
         '[clojure.string :as str]
         '[clojure.java.shell :refer [sh]])

(def blue "\u001b[34m")
(def red "\u001b[31m")
(def green "\u001b[32m")
(def yellow "\u001b[33m")
(def reset "\u001b[0m")

(def portal-base
  ;; UNSANDBOX_PORTAL_URL: portal for /logs/{source} and /keys/extend
  (str/replace (let [u (System/getenv "UNSANDBOX_PORTAL_URL")] (if (str/blank? u) "https://unsandbox.com" u)) #"/+$" ""))

;; API base URL: UNSANDBOX_API_URL overrides (staging / local), no trailing slash
(def api-base
  (str/replace (or (System/getenv "UNSANDBOX_API_URL") "https://api.unsandbox.com") #"/+$" ""))

(def languages-cache-ttl 3600) ;; 1 hour in seconds

(def ext-map
  {".hs" "haskell" ".ml" "ocaml" ".clj" "clojure" ".scm" "scheme"
   ".lisp" "commonlisp" ".erl" "erlang" ".ex" "elixir" ".exs" "elixir"
   ".py" "python" ".js" "javascript" ".ts" "typescript" ".rb" "ruby"
   ".go" "go" ".rs" "rust" ".c" "c" ".cpp" "cpp" ".cc" "cpp"
   ".cxx" "cpp" ".java" "java" ".kt" "kotlin" ".cs" "csharp"
   ".fs" "fsharp" ".jl" "julia" ".r" "r" ".cr" "crystal"
   ".d" "d" ".nim" "nim" ".zig" "zig" ".v" "v" ".dart" "dart"
   ".groovy" "groovy" ".scala" "scala" ".sh" "bash" ".pl" "perl"
   ".lua" "lua" ".php" "php"})

(defn get-extension [filename]
  (let [dot-pos (str/last-index-of filename ".")]
    (if dot-pos (subs filename dot-pos) "")))

(defn escape-json [s]
  (-> s
      (str/replace "\\" "\\\\")
      (str/replace "\"" "\\\"")
      (str/replace "\n" "\\n")
      (str/replace "\r" "\\r")
      (str/replace "\t" "\\t")))

(defn unescape-json [s]
  (-> s
      (str/replace "\\n" "\n")
      (str/replace "\\t" "\t")
      (str/replace "\\\"" "\"")
      (str/replace "\\\\" "\\")))

(defn read-and-base64 [filepath]
  (let [content (slurp filepath)
        bytes (.getBytes content "UTF-8")]
    (.encodeToString (java.util.Base64/getEncoder) bytes)))

(defn build-input-files-json [files]
  (if (empty? files)
    ""
    (let [file-jsons (map (fn [f]
                            (let [basename (-> (io/file f) .getName)
                                  b64 (read-and-base64 f)]
                              (str "{\"filename\":\"" (escape-json basename) "\",\"content\":\"" b64 "\"}")))
                          files)]
      (str ",\"input_files\":[" (str/join "," file-jsons) "]"))))

(defn extract-field [field json-str]
  (let [pattern-str (re-pattern (str "\"" field "\"\\s*:\\s*\"([^\"]*)\""))
        pattern-num (re-pattern (str "\"" field "\"\\s*:\\s*(\\d+)"))]
    (or (second (re-find pattern-str json-str))
        (second (re-find pattern-num json-str)))))

(def ^:dynamic *account-index* nil)

;; --os OS: target OS for execute/session/service, nil = API default (linux)
(def ^:dynamic *target-os* nil)

(defn normalize-os
  "Lowercase and validate a target OS. Returns \"linux\" or \"freebsd\", nil on invalid."
  [v]
  (let [s (str/lower-case (or v ""))]
    (when (#{"linux" "freebsd"} s) s)))

(defn os-json
  "JSON fragment adding \"os\" to a request body only when --os was given."
  []
  (if *target-os* (str ",\"os\":\"" *target-os* "\"") ""))

(defn load-accounts-csv [path index]
  (when (.exists (io/file path))
    (try
      (let [lines (str/split-lines (slurp path))
            rows (->> lines
                      (filter #(and (> (count %) 0)
                                    (not (str/starts-with? % "#"))))
                      ;; public_key,secret_key[,label...] -- extra fields are labels
                      (map #(str/split % #"," -1))
                      (filter #(>= (count %) 2)))]
        (when (< index (count rows))
          (let [row (nth rows index)]
            [(str/trim (first row)) (str/trim (second row))])))
      (catch Exception _ nil))))

(defn get-api-keys []
  (let [public-key (System/getenv "UNSANDBOX_PUBLIC_KEY")
        secret-key (System/getenv "UNSANDBOX_SECRET_KEY")
        api-key (System/getenv "UNSANDBOX_API_KEY")
        home (System/getenv "HOME")
        account-idx (or *account-index* 0)]
    (cond
      ;; --account N: load row N from accounts.csv, bypasses env vars
      (some? *account-index*)
      (or (load-accounts-csv (str home "/.unsandbox/accounts.csv") account-idx)
          (load-accounts-csv "./accounts.csv" account-idx)
          (do
            (binding [*out* *err*]
              (println (str "Error: account " account-idx " not found in accounts.csv")))
            (System/exit 1)))
      ;; env vars
      (and public-key secret-key) [public-key secret-key]
      api-key [api-key nil]
      ;; accounts.csv fallback (row 0 or UNSANDBOX_ACCOUNT)
      :else
      (let [row-idx (if-let [acc (System/getenv "UNSANDBOX_ACCOUNT")]
                      (try (Integer/parseInt acc) (catch Exception _ 0))
                      0)]
        (or (load-accounts-csv (str home "/.unsandbox/accounts.csv") row-idx)
            (load-accounts-csv "./accounts.csv" row-idx)
            (do
              (binding [*out* *err*]
                (println "Error: UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY not set (or UNSANDBOX_API_KEY for backwards compat)"))
              (System/exit 1)))))))

(defn get-api-key []
  (first (get-api-keys)))

(defn hmac-sha256 [secret message]
  (let [mac (javax.crypto.Mac/getInstance "HmacSHA256")
        secret-key (javax.crypto.spec.SecretKeySpec. (.getBytes secret "UTF-8") "HmacSHA256")]
    (.init mac secret-key)
    (let [bytes (.doFinal mac (.getBytes message "UTF-8"))]
      (apply str (map #(format "%02x" %) bytes)))))

(defn make-signature [secret-key timestamp method path body]
  ;; Server verifies HMAC against our path without its query string
  (let [message (str timestamp ":" method ":" (first (str/split path #"\?" 2)) ":" body)]
    (hmac-sha256 secret-key message)))

(defn check-clock-drift-error [response]
  (let [has-timestamp (or (str/includes? response "timestamp")
                          (str/includes? response "\"timestamp\""))
        has-401 (str/includes? response "401")
        has-expired (str/includes? response "expired")
        has-invalid (str/includes? response "invalid")]
    (when (and has-timestamp (or has-401 has-expired has-invalid))
      (binding [*out* *err*]
        (println (str red "Error: Request timestamp expired (must be within 5 minutes of server time)" reset))
        (println (str yellow "Your computer's clock may have drifted." reset))
        (println "Check your system time and sync with NTP if needed:")
        (println "  Linux:   sudo ntpdate -s time.nist.gov")
        (println "  macOS:   sudo sntp -sS time.apple.com")
        (println "  Windows: w32tm /resync"))
      (System/exit 1))))

(defn build-auth-headers [public-key secret-key method path body]
  (if secret-key
    (let [timestamp (str (quot (System/currentTimeMillis) 1000))
          signature (make-signature secret-key timestamp method path body)]
      ["-H" (str "Authorization: Bearer " public-key)
       "-H" (str "X-Timestamp: " timestamp)
       "-H" (str "X-Signature: " signature)])
    ["-H" (str "Authorization: Bearer " public-key)]))

(defn curl-post [api-key endpoint json-data]
  (let [tmp-file (str "/tmp/un_clj_" (rand-int 999999) ".json")
        [public-key secret-key] (get-api-keys)
        auth-headers (build-auth-headers public-key secret-key "POST" endpoint json-data)]
    (spit tmp-file json-data)
    (let [args (concat ["curl" "-s" "-X" "POST"
                        (str api-base endpoint)
                        "-H" "Content-Type: application/json"]
                       auth-headers
                       ["--data-binary" (str "@" tmp-file)])
          {:keys [out]} (apply sh args)]
      (io/delete-file tmp-file true)
      (check-clock-drift-error out)
      out)))

(defn curl-get [api-key endpoint]
  (let [[public-key secret-key] (get-api-keys)
        auth-headers (build-auth-headers public-key secret-key "GET" endpoint "")
        args (concat ["curl" "-s"
                      (str api-base endpoint)]
                     auth-headers)
        result (:out (apply sh args))]
    (check-clock-drift-error result)
    result))

(defn curl-delete [api-key endpoint]
  (let [[public-key secret-key] (get-api-keys)
        auth-headers (build-auth-headers public-key secret-key "DELETE" endpoint "")
        args (concat ["curl" "-s" "-w" "\n%{http_code}" "-X" "DELETE"
                      (str api-base endpoint)]
                     auth-headers)
        result (:out (apply sh args))]
    (check-clock-drift-error result)
    result))

(defn extract-http-code [response]
  "Extract HTTP code from response with status code appended"
  (let [lines (str/split response #"\n")
        last-line (last lines)]
    (try
      (Integer/parseInt (str/trim last-line))
      (catch Exception _ 0))))

(defn extract-body [response]
  "Extract body from response (everything except last line which is status code)"
  (let [lines (str/split response #"\n")]
    (str/join "\n" (butlast lines))))

(defn handle-sudo-challenge
  "Handle 428 sudo OTP challenge - prompts user for OTP and retries the request"
  [response-data public-key secret-key method endpoint body]
  (let [challenge-id (extract-field "challenge_id" response-data)]
    (binding [*out* *err*]
      (println (str yellow "Confirmation required. Check your email for a one-time code." reset)))
    (print "Enter OTP: ")
    (flush)
    (let [otp (str/trim (or (read-line) ""))]
      (when (empty? otp)
        (binding [*out* *err*]
          (println "Error: Operation cancelled"))
        (System/exit 1))
      ;; Retry the request with sudo headers
      (let [auth-headers (build-auth-headers public-key secret-key method endpoint (or body ""))
            sudo-headers ["-H" (str "X-Sudo-OTP: " otp)
                          "-H" (str "X-Sudo-Challenge: " (or challenge-id ""))]
            content-type-headers (if body ["-H" "Content-Type: application/json"] [])
            body-args (if body ["-d" body] [])
            method-args (cond
                          (= method "DELETE") ["-X" "DELETE"]
                          (= method "POST") ["-X" "POST"]
                          :else ["-X" method])
            args (concat ["curl" "-s"]
                         method-args
                         [(str api-base endpoint)]
                         auth-headers
                         sudo-headers
                         content-type-headers
                         body-args)
            {:keys [out]} (apply sh args)
            http-code (extract-http-code out)]
        (if (and (>= http-code 200) (< http-code 300))
          {:success true :response (extract-body out)}
          (do
            (binding [*out* *err*]
              (println (str red "Error: HTTP " http-code reset))
              (println (extract-body out)))
            {:success false}))))))

(defn curl-delete-with-sudo [api-key endpoint]
  "DELETE request that handles 428 sudo OTP challenge"
  (let [[public-key secret-key] (get-api-keys)
        auth-headers (build-auth-headers public-key secret-key "DELETE" endpoint "")
        args (concat ["curl" "-s" "-w" "\n%{http_code}" "-X" "DELETE"
                      (str api-base endpoint)]
                     auth-headers)
        result (:out (apply sh args))
        http-code (extract-http-code result)
        body (extract-body result)]
    (check-clock-drift-error body)
    (if (= http-code 428)
      (handle-sudo-challenge body public-key secret-key "DELETE" endpoint nil)
      {:success (and (>= http-code 200) (< http-code 300)) :response body :http-code http-code})))

(defn curl-post-with-sudo [api-key endpoint json-data]
  "POST request that handles 428 sudo OTP challenge"
  (let [tmp-file (str "/tmp/un_clj_" (rand-int 999999) ".json")
        [public-key secret-key] (get-api-keys)
        auth-headers (build-auth-headers public-key secret-key "POST" endpoint json-data)]
    (spit tmp-file json-data)
    (let [args (concat ["curl" "-s" "-w" "\n%{http_code}" "-X" "POST"
                        (str api-base endpoint)
                        "-H" "Content-Type: application/json"]
                       auth-headers
                       ["--data-binary" (str "@" tmp-file)])
          {:keys [out]} (apply sh args)]
      (io/delete-file tmp-file true)
      (let [http-code (extract-http-code out)
            body (extract-body out)]
        (check-clock-drift-error body)
        (if (= http-code 428)
          (handle-sudo-challenge body public-key secret-key "POST" endpoint json-data)
          {:success (and (>= http-code 200) (< http-code 300)) :response body :http-code http-code})))))

(defn curl-put-text [endpoint body]
  (let [tmp-file (str "/tmp/un_clj_" (rand-int 999999) ".txt")
        [public-key secret-key] (get-api-keys)
        auth-headers (build-auth-headers public-key secret-key "PUT" endpoint body)]
    (spit tmp-file body)
    (let [args (concat ["curl" "-s" "-o" "/dev/null" "-w" "%{http_code}" "-X" "PUT"
                        (str api-base endpoint)
                        "-H" "Content-Type: text/plain"]
                       auth-headers
                       ["--data-binary" (str "@" tmp-file)])
          {:keys [out]} (apply sh args)]
      (io/delete-file tmp-file true)
      (let [status (Integer/parseInt (str/trim out))]
        (and (>= status 200) (< status 300))))))

(defn curl-patch [api-key endpoint json-data]
  (let [tmp-file (str "/tmp/un_clj_" (rand-int 999999) ".json")
        [public-key secret-key] (get-api-keys)
        auth-headers (build-auth-headers public-key secret-key "PATCH" endpoint json-data)]
    (spit tmp-file json-data)
    (let [args (concat ["curl" "-s" "-X" "PATCH"
                        (str api-base endpoint)
                        "-H" "Content-Type: application/json"]
                       auth-headers
                       ["--data-binary" (str "@" tmp-file)])
          {:keys [out]} (apply sh args)]
      (io/delete-file tmp-file true)
      (check-clock-drift-error out)
      out)))

(defn set-unfreeze-on-demand [service-id enabled]
  (let [api-key (get-api-key)
        json (str "{\"unfreeze_on_demand\":" (if enabled "true" "false") "}")]
    (curl-patch api-key (str "/services/" service-id) json)
    (println (str green "Service unfreeze_on_demand set to " (if enabled "true" "false") reset))))

(def max-env-content-size 65536)

(defn read-env-file [path]
  (if (.exists (io/file path))
    (slurp path)
    (do
      (binding [*out* *err*]
        (println (str red "Error: Env file not found: " path reset)))
      (System/exit 1))))

(defn build-env-content [envs env-file]
  (let [file-lines (if env-file
                     (->> (str/split (read-env-file env-file) #"\n")
                          (map str/trim)
                          (filter #(and (> (count %) 0) (not (.startsWith % "#")))))
                     [])]
    (str/join "\n" (concat envs file-lines))))

(defn service-env-status [service-id]
  (let [api-key (get-api-key)]
    (curl-get api-key (str "/services/" service-id "/env"))))

(defn service-env-set [service-id env-content]
  (if (> (count env-content) max-env-content-size)
    (do
      (binding [*out* *err*]
        (println (str red "Error: Env content exceeds maximum size of 64KB" reset)))
      false)
    (curl-put-text (str "/services/" service-id "/env") env-content)))

(defn service-env-export [service-id]
  (let [api-key (get-api-key)]
    (curl-post api-key (str "/services/" service-id "/env/export") "{}")))

(defn service-env-delete [service-id]
  (let [api-key (get-api-key)]
    (try
      (curl-delete api-key (str "/services/" service-id "/env"))
      true
      (catch Exception _ false))))

(defn service-env-command [action target envs env-file]
  (case action
    "status" (if target
               (let [response (service-env-status target)
                     has-vault (= (extract-field "has_vault" response) "true")]
                 (if has-vault
                   (do
                     (println (str green "Vault: configured" reset))
                     (when-let [env-count (extract-field "env_count" response)]
                       (println (str "Variables: " env-count)))
                     (when-let [updated-at (extract-field "updated_at" response)]
                       (println (str "Updated: " updated-at))))
                   (println (str yellow "Vault: not configured" reset))))
               (do
                 (binding [*out* *err*]
                   (println (str red "Error: service env status requires service ID" reset)))
                 (System/exit 1)))
    "set" (if target
            (if (and (empty? envs) (nil? env-file))
              (do
                (binding [*out* *err*]
                  (println (str red "Error: service env set requires -e or --env-file" reset)))
                (System/exit 1))
              (let [env-content (build-env-content envs env-file)]
                (if (service-env-set target env-content)
                  (println (str green "Vault updated for service " target reset))
                  (do
                    (binding [*out* *err*]
                      (println (str red "Error: Failed to update vault" reset)))
                    (System/exit 1)))))
            (do
              (binding [*out* *err*]
                (println (str red "Error: service env set requires service ID" reset)))
              (System/exit 1)))
    "export" (if target
               (let [response (service-env-export target)
                     content (extract-field "content" response)]
                 (when content (print (unescape-json content))))
               (do
                 (binding [*out* *err*]
                   (println (str red "Error: service env export requires service ID" reset)))
                 (System/exit 1)))
    "delete" (if target
               (if (service-env-delete target)
                 (println (str green "Vault deleted for service " target reset))
                 (do
                   (binding [*out* *err*]
                     (println (str red "Error: Failed to delete vault" reset)))
                   (System/exit 1)))
               (do
                 (binding [*out* *err*]
                   (println (str red "Error: service env delete requires service ID" reset)))
                 (System/exit 1)))
    (do
      (binding [*out* *err*]
        (println (str red "Error: Unknown env action: " action reset))
        (println "Usage: un.clj service env <status|set|export|delete> <service_id>"))
      (System/exit 1))))

(defn execute-command [file env-vars artifacts out-dir network vcpu]
  (let [api-key (get-api-key)
        ext (get-extension file)
        language (get ext-map ext)]
    (when-not language
      (binding [*out* *err*]
        (println (str "Error: Unknown extension: " ext)))
      (System/exit 1))
    (let [code (slurp file)
          env-json (if (empty? env-vars) ""
                     (str ",\"env\":{"
                          (str/join "," (map (fn [[k v]]
                                               (str "\"" k "\":\"" (escape-json v) "\""))
                                             env-vars))
                          "}"))
          artifacts-json (if artifacts ",\"return_artifacts\":true" "")
          network-json (if network (str ",\"network\":\"" network "\"") "")
          vcpu-json (if vcpu (str ",\"vcpu\":" vcpu) "")
          json (str "{\"language\":\"" language "\",\"code\":\"" (escape-json code) "\""
                    env-json artifacts-json network-json vcpu-json (os-json) "}")]
      (let [response (curl-post api-key "/execute" json)
            stdout-val (extract-field "stdout" response)
            stderr-val (extract-field "stderr" response)
            exit-code (or (some-> (extract-field "exit_code" response) Integer/parseInt) 0)]
        (when stdout-val
          (print (str blue (unescape-json stdout-val) reset))
          (flush))
        (when stderr-val
          (binding [*out* *err*]
            (print (str red (unescape-json stderr-val) reset))
            (flush)))
        (System/exit exit-code)))))

(defn session-command [action sid shell network vcpu input-files]
  (let [api-key (get-api-key)]
    (case action
      :list (println (curl-get api-key "/sessions"))
      :kill (do
              (curl-delete api-key (str "/sessions/" sid))
              (println (str green "Session terminated: " sid reset)))
      :create (let [sh (or shell "bash")
                    network-json (if network (str ",\"network\":\"" network "\"") "")
                    vcpu-json (if vcpu (str ",\"vcpu\":" vcpu) "")
                    input-files-json (build-input-files-json input-files)
                    json (str "{\"shell\":\"" sh "\"" network-json vcpu-json (os-json) input-files-json "}")]
                (println (str yellow "Session created (WebSocket required)" reset))
                (println (curl-post api-key "/sessions" json))))))

(defn service-command [action sid name ports bootstrap bootstrap-file service-type network vcpu input-files envs env-file unfreeze-on-demand]
  (let [api-key (get-api-key)]
    (case action
      :env (service-env-command sid name envs env-file)
      :list (println (curl-get api-key "/services"))
      :info (println (curl-get api-key (str "/services/" sid)))
      :logs (println (curl-get api-key (str "/services/" sid "/logs")))
      :sleep (do
               (curl-post api-key (str "/services/" sid "/freeze") "{}")
               (println (str green "Service frozen: " sid reset)))
      :wake (do
              (curl-post api-key (str "/services/" sid "/unfreeze") "{}")
              (println (str green "Service unfreezing: " sid reset)))
      :destroy (let [result (curl-delete-with-sudo api-key (str "/services/" sid))]
                 (if (:success result)
                   (println (str green "Service destroyed: " sid reset))
                   (do
                     (binding [*out* *err*]
                       (println (str red "Error destroying service" reset)))
                     (System/exit 1))))
      :resize (when sid
                (if (or (nil? vcpu) (< vcpu 1) (> vcpu 8))
                  (do
                    (binding [*out* *err*]
                      (println (str red "Error: --resize requires -v N (1-8)" reset)))
                    (System/exit 1))
                  (let [json (str "{\"vcpu\":" vcpu "}")
                        _ (curl-patch api-key (str "/services/" sid) json)
                        ram (* vcpu 2)]
                    (println (str green "Service resized to " vcpu " vCPU, " ram " GB RAM" reset)))))
      :set-unfreeze-on-demand (when sid
                                (set-unfreeze-on-demand sid unfreeze-on-demand))
      :execute (when (and sid bootstrap)
                 (let [json (str "{\"command\":\"" (escape-json bootstrap) "\"}")
                       response (curl-post api-key (str "/services/" sid "/execute") json)
                       stdout-val (extract-field "stdout" response)]
                   (when stdout-val
                     (print (str blue (unescape-json stdout-val) reset))
                     (flush))))
      :dump-bootstrap (when sid
                        (binding [*out* *err*]
                          (println (str "Fetching bootstrap script from " sid "...")))
                        (let [json "{\"command\":\"cat /tmp/bootstrap.sh\"}"
                              response (curl-post api-key (str "/services/" sid "/execute") json)
                              stdout-val (extract-field "stdout" response)]
                          (if stdout-val
                            (let [script (unescape-json stdout-val)]
                              (if service-type
                                (do
                                  (spit service-type script)
                                  (sh "chmod" "755" service-type)
                                  (println (str "Bootstrap saved to " service-type)))
                                (print script)))
                            (do
                              (binding [*out* *err*]
                                (println (str red "Error: Failed to fetch bootstrap (service not running or no bootstrap file)" reset)))
                              (System/exit 1)))))
      :create (when name
                (let [ports-json (if ports (str ",\"ports\":[" ports "]") "")
                      bootstrap-json (if bootstrap (str ",\"bootstrap\":\"" (escape-json bootstrap) "\"") "")
                      bootstrap-content-json (if bootstrap-file
                                               (str ",\"bootstrap_content\":\"" (escape-json (slurp bootstrap-file)) "\"")
                                               "")
                      service-type-json (if service-type (str ",\"service_type\":\"" service-type "\"") "")
                      network-json (if network (str ",\"network\":\"" network "\"") "")
                      vcpu-json (if vcpu (str ",\"vcpu\":" vcpu) "")
                      unfreeze-on-demand-json (if (some? unfreeze-on-demand) (str ",\"unfreeze_on_demand\":" (if unfreeze-on-demand "true" "false")) "")
                      input-files-json (build-input-files-json input-files)
                      json (str "{\"name\":\"" name "\"" ports-json bootstrap-json bootstrap-content-json service-type-json network-json vcpu-json (os-json) unfreeze-on-demand-json input-files-json "}")
                      response (curl-post api-key "/services" json)
                      service-id (extract-field "id" response)]
                  (println (str green "Service created" reset))
                  (println response)
                  ;; Auto-set vault if env vars were provided
                  (when (and service-id (or (seq envs) env-file))
                    (let [env-content (build-env-content envs env-file)]
                      (when (> (count env-content) 0)
                        (if (service-env-set service-id env-content)
                          (println (str green "Vault configured with environment variables" reset))
                          (println (str yellow "Warning: Failed to set vault" reset)))))))))))

(defn- signed-post-http
  "Signed POST via java.net (no subprocess); returns [http-code body] for any status."
  [public-key secret-key endpoint body]
  (let [timestamp (str (quot (System/currentTimeMillis) 1000))
        conn (doto ^java.net.HttpURLConnection (.openConnection (java.net.URL. (str api-base endpoint)))
               (.setRequestMethod "POST")
               (.setConnectTimeout 30000)
               (.setReadTimeout 30000)
               (.setRequestProperty "Authorization" (str "Bearer " public-key))
               (.setRequestProperty "Content-Type" "application/json")
               (.setDoOutput true))]
    (when secret-key
      (.setRequestProperty conn "X-Timestamp" timestamp)
      (.setRequestProperty conn "X-Signature" (make-signature secret-key timestamp "POST" endpoint body)))
    (with-open [os (.getOutputStream conn)] (.write os (.getBytes ^String body "UTF-8")))
    (let [code (.getResponseCode conn)
          stream (if (< 199 code 300) (.getInputStream conn) (.getErrorStream conn))]
      [code (if stream (slurp stream :encoding "UTF-8") "")])))

(defn- fmt-expiry
  "valid_through (ms since epoch, string) -> [ISO-8601 UTC, \"<d>d <h>h <m>m\"]"
  [vt]
  (if-let [ms (when vt (try (Long/parseLong vt) (catch Exception _ nil)))]
    (let [secs (quot ms 1000)
          left (- secs (quot (System/currentTimeMillis) 1000))]
      [(.format (.withZone (java.time.format.DateTimeFormatter/ofPattern "yyyy-MM-dd'T'HH:mm:ss'Z'")
                           java.time.ZoneOffset/UTC)
                (java.time.Instant/ofEpochSecond secs))
       (if (<= left 0) "expired"
           (str (quot left 86400) "d " (quot (mod left 86400) 3600) "h " (quot (mod left 3600) 60) "m"))])
    ["N/A" "N/A"]))

(defn validate-key [api-key extend?]
  (let [[public-key secret-key] (get-api-keys)]
    (if extend?
      (let [url (str portal-base "/keys/extend?pk=" public-key)]
        (println (str blue "Opening browser to extend key..." reset))
        (sh "xdg-open" url))
      (let [[code response] (try (signed-post-http public-key secret-key "/keys/validate" "")
                                 (catch java.io.IOException e
                                   (binding [*out* *err*] (println (str red "Error: " (.getMessage e) reset)))
                                   (System/exit 1)))
            [state reason]
            (cond
              (= code 200) (let [st (or (extract-field "status" response)
                                        (if (re-find #"\"valid\"\s*:\s*true" response) "active" "invalid"))]
                             [st (str "status " st)])
              (= code 401) (let [r (or (extract-field "error" response) "unauthorized")]
                             [(cond (#{":expired" "expired"} r) "expired"
                                    (#{":suspended" "suspended"} r) "suspended"
                                    :else "invalid") r])
              :else (do (binding [*out* *err*]
                          (println (str red "Error: HTTP " code
                                        (when-let [m (or (extract-field "error" response) (extract-field "message" response))] (str " " m))
                                        reset)))
                        (System/exit 4)))]
        (case state
          "active"
          (let [[expires remaining] (fmt-expiry (extract-field "valid_through" response))]
            (println (str green "Valid" reset))
            (println (str "Public Key: " public-key))
            (println (str "Tier: " (or (extract-field "tier" response) "N/A")))
            (println (str "Status: " (or (extract-field "status" response) "N/A")))
            (println (str "Expires: " expires))
            (println (str "Time Remaining: " remaining)))
          "expired"
          (do (println (str red "Expired" reset))
              (println (str "Public Key: " public-key))
              (println (str yellow "To renew: Visit " portal-base "/keys/extend" reset))
              (System/exit 3))
          "suspended"
          (do (println (str red "Suspended" reset))
              (println (str "Public Key: " public-key))
              (System/exit 3))
          (do (println (str red "Invalid" reset))
              (println (str "Public Key: " public-key))
              (println (str "Reason: " reason))
              (System/exit 3)))))))

(defn key-command [extend?]
  (let [api-key (get-api-key)]
    (validate-key api-key extend?)))

;; ---------------------------------------------------------------------------
;; un key --rotate [--otp CODE]: rotate this key's secret (see CLI_SPEC.md)
;; ---------------------------------------------------------------------------

(def ^:private owner-only-file
  (java.nio.file.attribute.PosixFilePermissions/fromString "rw-------"))

(defn- sha256-hex [s]
  (let [md (java.security.MessageDigest/getInstance "SHA-256")]
    (apply str (map #(format "%02x" %) (.digest md (.getBytes s "UTF-8"))))))

(defn- stdin-tty? []
  (when-let [console (System/console)]
    ;; Java 22+ returns a Console even when redirected; ask isTerminal() there
    (try
      (boolean (clojure.lang.Reflector/invokeInstanceMethod console "isTerminal" (object-array 0)))
      (catch Exception _ true))))

(defn- write-owner-only [^java.nio.file.Path path ^String content]
  "Create (or truncate) path with mode 0600, then write content."
  (when-not (java.nio.file.Files/exists path (make-array java.nio.file.LinkOption 0))
    (java.nio.file.Files/createFile path
      (into-array java.nio.file.attribute.FileAttribute
                  [(java.nio.file.attribute.PosixFilePermissions/asFileAttribute owner-only-file)])))
  (java.nio.file.Files/setPosixFilePermissions path owner-only-file)
  (spit (.toFile path) content))

(defn- credentials-csv-path []
  "The accounts.csv the credentials came from, or nil (env vars). Mirrors get-api-keys."
  (let [env-creds? (or (and (System/getenv "UNSANDBOX_PUBLIC_KEY") (System/getenv "UNSANDBOX_SECRET_KEY"))
                       (System/getenv "UNSANDBOX_API_KEY"))
        idx (cond
              (some? *account-index*) *account-index*
              env-creds? nil
              :else (if-let [acc (System/getenv "UNSANDBOX_ACCOUNT")]
                      (try (Integer/parseInt acc) (catch Exception _ 0))
                      0))]
    (when (some? idx)
      (first (filter #(load-accounts-csv % idx)
                     [(str (System/getenv "HOME") "/.unsandbox/accounts.csv") "./accounts.csv"])))))

(defn- rewrite-accounts-csv [csv-path public-key new-secret]
  "Replace the secret on the line whose first field equals public-key. Every other
  line, field, comment and the order stay as they are. Temp file (0600) + rename."
  (let [path (.toAbsolutePath (java.nio.file.Paths/get csv-path (make-array String 0)))
        lines (str/split (slurp (.toFile path)) #"\n" -1)
        idx (first (keep-indexed
                     (fn [i line]
                       (let [t (str/trim line)
                             fields (str/split line #"," -1)]
                         (when (and (seq t) (not (str/starts-with? t "#"))
                                    (>= (count fields) 2)
                                    (= (str/trim (first fields)) public-key))
                           i)))
                     lines))]
    (when idx
      (let [fields (str/split (nth lines idx) #"," -1)
            updated (assoc (vec lines) idx (str/join "," (assoc (vec fields) 1 new-secret)))
            tmp (java.nio.file.Files/createTempFile (.getParent path) ".accounts.csv." ".tmp"
                  (into-array java.nio.file.attribute.FileAttribute
                              [(java.nio.file.attribute.PosixFilePermissions/asFileAttribute owner-only-file)]))]
        (try
          (spit (.toFile tmp) (str/join "\n" updated))
          (java.nio.file.Files/setPosixFilePermissions tmp owner-only-file)
          (java.nio.file.Files/move tmp path
            (into-array java.nio.file.CopyOption
                        [java.nio.file.StandardCopyOption/ATOMIC_MOVE
                         java.nio.file.StandardCopyOption/REPLACE_EXISTING]))
          true
          (finally
            (java.nio.file.Files/deleteIfExists tmp)))))))

(defn- rotate-post [public-key secret-key otp challenge]
  "POST /keys/self/rotate-secret; returns [http-code body]. Sudo headers travel
  through a 0600 header file, never the command line."
  (let [endpoint "/keys/self/rotate-secret"
        body "{}"
        auth-headers (build-auth-headers public-key secret-key "POST" endpoint body)
        hdr-file (when otp
                   (let [f (java.nio.file.Files/createTempFile "un_clj_sudo" ".hdr"
                             (into-array java.nio.file.attribute.FileAttribute
                                         [(java.nio.file.attribute.PosixFilePermissions/asFileAttribute owner-only-file)]))]
                     (spit (.toFile f) (str "X-Sudo-OTP: " otp "\nX-Sudo-Challenge: " (or challenge "") "\n"))
                     f))]
    (try
      (let [args (concat ["curl" "-s" "-w" "\n%{http_code}" "-X" "POST"
                          (str api-base endpoint)
                          "-H" "Content-Type: application/json"]
                         auth-headers
                         (if hdr-file ["-H" (str "@" hdr-file)] [])
                         ["--data-binary" body])
            {:keys [out]} (apply sh args)]
        [(extract-http-code out) (extract-body out)])
      (finally
        (when hdr-file (java.nio.file.Files/deleteIfExists hdr-file))))))

(defn- rotate-fail [http-code body]
  (binding [*out* *err*]
    (cond
      (and (= http-code 403) (= (extract-field "error" body) "email_required"))
      (do (println "Error: this key has no email; add one or use POST /keys/recover")
          (System/exit 3))
      (= http-code 401)
      (do (println "Error: authentication failed")
          (System/exit 3))
      :else
      (do (println (str "Error: " (or (extract-field "message" body)
                                      (extract-field "error" body)
                                      (str "HTTP " http-code))))
          (System/exit 4)))))

(defn key-rotate-command [otp]
  (let [[public-key secret-key] (get-api-keys)
        unsandbox-dir (java.nio.file.Paths/get (str (System/getenv "HOME") "/.unsandbox") (make-array String 0))
        challenge-file (.resolve unsandbox-dir
                                 (str "rotate-" (subs (sha256-hex public-key) 0 16) ".challenge"))
        [http-code body]
        (if otp
          (let [challenge (try (str/trim (slurp (.toFile challenge-file)))
                               (catch Exception _
                                 (binding [*out* *err*]
                                   (println "Error: no rotation in progress; run: un key --rotate"))
                                 (System/exit 2)))
                result (rotate-post public-key secret-key (str/trim otp) challenge)]
            (java.nio.file.Files/deleteIfExists challenge-file)
            result)
          (let [[code resp :as result] (rotate-post public-key secret-key nil nil)]
            (if (not= code 428)
              result
              (let [challenge (or (extract-field "challenge_id" resp) "")]
                (if (stdin-tty?)
                  (do
                    (binding [*out* *err*]
                      (println "One-time code emailed to the key owner.")
                      (print "Code: ")
                      (flush))
                    (let [code-in (str/trim (or (read-line) ""))]
                      (when (empty? code-in)
                        (binding [*out* *err*] (println "Error: cancelled"))
                        (System/exit 1))
                      (rotate-post public-key secret-key code-in challenge)))
                  (do
                    (when-not (java.nio.file.Files/isDirectory unsandbox-dir (make-array java.nio.file.LinkOption 0))
                      (java.nio.file.Files/createDirectories unsandbox-dir
                        (into-array java.nio.file.attribute.FileAttribute
                                    [(java.nio.file.attribute.PosixFilePermissions/asFileAttribute
                                       (java.nio.file.attribute.PosixFilePermissions/fromString "rwx------"))])))
                    (write-owner-only challenge-file challenge)
                    (println "One-time code emailed to the key owner. Finish with: un key --rotate --otp CODE")
                    (System/exit 0)))))))]
    (when (not= http-code 200)
      (rotate-fail http-code body))
    (let [new-secret (extract-field "secret" body)]
      (when-not new-secret
        (binding [*out* *err*] (println "Error: API returned no secret"))
        (System/exit 4))
      (let [csv-path (credentials-csv-path)]
        (if (and csv-path (rewrite-accounts-csv csv-path public-key new-secret))
          (println (str "Secret rotated. Updated " csv-path " (public key " public-key ")."))
          (do
            (println (str "New secret (shown once, store it now): " new-secret))
            (println "Warning: UNSANDBOX_SECRET_KEY / -k still hold the old, now invalid, secret.")))))))

(defn get-languages-cache-path []
  (let [home (System/getenv "HOME")]
    (str home "/.unsandbox/languages.json")))

(defn load-languages-cache []
  (let [cache-path (get-languages-cache-path)]
    (when (.exists (io/file cache-path))
      (try
        (let [content (slurp cache-path)
              timestamp (extract-field "timestamp" content)]
          (when timestamp
            (let [cache-time (Long/parseLong timestamp)
                  now (quot (System/currentTimeMillis) 1000)]
              (when (< (- now cache-time) languages-cache-ttl)
                content))))
        (catch Exception _ nil)))))

(defn save-languages-cache [languages-json]
  (let [cache-path (get-languages-cache-path)
        cache-dir (.getParent (io/file cache-path))]
    (.mkdirs (io/file cache-dir))
    (let [timestamp (quot (System/currentTimeMillis) 1000)
          cache-content (str "{\"languages\":" languages-json ",\"timestamp\":" timestamp "}")]
      (spit cache-path cache-content))))

(defn languages-command [json-output?]
  (let [api-key (get-api-key)
        ;; Try cache first
        cached (load-languages-cache)
        response (if cached
                   cached
                   (let [resp (curl-get api-key "/languages")]
                     ;; Extract and cache the languages array
                     (when-let [match (re-find #"\"languages\":\s*\[([^\]]*)\]" resp)]
                       (save-languages-cache (str "[" (second match) "]")))
                     resp))]
    (if json-output?
      ;; Extract language names and output as JSON array
      (let [langs (re-seq #"\"name\":\"([^\"]+)\"" response)
            names (map second langs)]
        (println (str "[" (str/join "," (map #(str "\"" % "\"") names)) "]")))
      ;; Output one language per line
      (let [langs (re-seq #"\"name\":\"([^\"]+)\"" response)
            names (map second langs)]
        (doseq [name names]
          (println name))))))

;; Image command functions
(defn image-list []
  (let [api-key (get-api-key)]
    (println (curl-get api-key "/images"))))

(defn image-info [id]
  (let [api-key (get-api-key)]
    (println (curl-get api-key (str "/images/" id)))))

(defn image-delete [id]
  (let [api-key (get-api-key)
        result (curl-delete-with-sudo api-key (str "/images/" id))]
    (if (:success result)
      (println (str green "Image deleted: " id reset))
      (do
        (binding [*out* *err*]
          (println (str red "Error deleting image" reset)))
        (System/exit 1)))))

(defn image-lock [id]
  (let [api-key (get-api-key)]
    (curl-post api-key (str "/images/" id "/lock") "{}")
    (println (str green "Image locked: " id reset))))

(defn image-unlock [id]
  (let [api-key (get-api-key)
        result (curl-post-with-sudo api-key (str "/images/" id "/unlock") "{}")]
    (if (:success result)
      (println (str green "Image unlocked: " id reset))
      (do
        (binding [*out* *err*]
          (println (str red "Error unlocking image" reset)))
        (System/exit 1)))))

(defn image-publish [source-id source-type name]
  (let [api-key (get-api-key)
        json (str "{\"source_type\":\"" source-type "\",\"source_id\":\"" source-id "\""
                  (if name (str ",\"name\":\"" (escape-json name) "\"") "")
                  "}")]
    (println (str green "Image published" reset))
    (println (curl-post api-key "/images/publish" json))))

(defn image-visibility [id visibility]
  (let [api-key (get-api-key)
        json (str "{\"visibility\":\"" visibility "\"}")]
    (curl-post api-key (str "/images/" id "/visibility") json)
    (println (str green "Image visibility set to: " visibility reset))))

(defn image-spawn [id name ports]
  (let [api-key (get-api-key)
        json (str "{"
                  (if name (str "\"name\":\"" (escape-json name) "\"") "")
                  (if (and name ports) "," "")
                  (if ports (str "\"ports\":[" ports "]") "")
                  "}")]
    (println (str green "Service spawned from image" reset))
    (println (curl-post api-key (str "/images/" id "/spawn") json))))

(defn image-clone [id name]
  (let [api-key (get-api-key)
        json (str "{" (if name (str "\"name\":\"" (escape-json name) "\"") "") "}")]
    (println (str green "Image cloned" reset))
    (println (curl-post api-key (str "/images/" id "/clone") json))))

;; Image access management functions
(defn image-grant-access [id trusted-key]
  (let [api-key (get-api-key)
        json (str "{\"trusted_api_key\":\"" trusted-key "\"}")]
    (curl-post api-key (str "/images/" id "/grant-access") json)
    (println (str green "Access granted to: " trusted-key reset))))

(defn image-revoke-access [id trusted-key]
  (let [api-key (get-api-key)
        json (str "{\"trusted_api_key\":\"" trusted-key "\"}")]
    (curl-post api-key (str "/images/" id "/revoke-access") json)
    (println (str green "Access revoked from: " trusted-key reset))))

(defn image-list-trusted [id]
  (let [api-key (get-api-key)]
    (println (curl-get api-key (str "/images/" id "/trusted")))))

(defn image-transfer [id to-key]
  (let [api-key (get-api-key)
        json (str "{\"to_api_key\":\"" to-key "\"}")]
    (curl-post api-key (str "/images/" id "/transfer") json)
    (println (str green "Image transferred to: " to-key reset))))

;; Snapshot functions
(defn snapshot-list []
  (let [api-key (get-api-key)]
    (println (curl-get api-key "/snapshots"))))

(defn snapshot-info [id]
  (let [api-key (get-api-key)]
    (println (curl-get api-key (str "/snapshots/" id)))))

(defn snapshot-session [session-id name hot]
  (let [api-key (get-api-key)
        json (str "{\"session_id\":\"" session-id "\""
                  (if name (str ",\"name\":\"" (escape-json name) "\"") "")
                  (if hot ",\"hot\":true" "")
                  "}")]
    (println (str green "Snapshot created" reset))
    (println (curl-post api-key "/snapshots" json))))

(defn snapshot-service [service-id name hot]
  (let [api-key (get-api-key)
        json (str "{\"service_id\":\"" service-id "\""
                  (if name (str ",\"name\":\"" (escape-json name) "\"") "")
                  (if hot ",\"hot\":true" "")
                  "}")]
    (println (str green "Snapshot created" reset))
    (println (curl-post api-key "/snapshots" json))))

(defn snapshot-restore [id]
  (let [api-key (get-api-key)]
    (curl-post api-key (str "/snapshots/" id "/restore") "{}")
    (println (str green "Snapshot restored: " id reset))))

(defn snapshot-delete [id]
  (let [api-key (get-api-key)
        result (curl-delete-with-sudo api-key (str "/snapshots/" id))]
    (if (:success result)
      (println (str green "Snapshot deleted: " id reset))
      (do
        (binding [*out* *err*]
          (println (str red "Error deleting snapshot" reset)))
        (System/exit 1)))))

(defn snapshot-lock [id]
  (let [api-key (get-api-key)]
    (curl-post api-key (str "/snapshots/" id "/lock") "{}")
    (println (str green "Snapshot locked: " id reset))))

(defn snapshot-unlock [id]
  (let [api-key (get-api-key)
        result (curl-post-with-sudo api-key (str "/snapshots/" id "/unlock") "{}")]
    (if (:success result)
      (println (str green "Snapshot unlocked: " id reset))
      (do
        (binding [*out* *err*]
          (println (str red "Error unlocking snapshot" reset)))
        (System/exit 1)))))

(defn snapshot-clone [id clone-type name ports shell]
  (let [api-key (get-api-key)
        json (str "{\"clone_type\":\"" clone-type "\""
                  (if name (str ",\"name\":\"" (escape-json name) "\"") "")
                  (if ports (str ",\"ports\":[" ports "]") "")
                  (if shell (str ",\"shell\":\"" shell "\"") "")
                  "}")]
    (println (str green "Snapshot cloned" reset))
    (println (curl-post api-key (str "/snapshots/" id "/clone") json))))

(defn snapshot-command [action id name ports shell hot]
  (case action
    :list (snapshot-list)
    :info (snapshot-info id)
    :session (snapshot-session id name hot)
    :service (snapshot-service id name hot)
    :restore (snapshot-restore id)
    :delete (snapshot-delete id)
    :lock (snapshot-lock id)
    :unlock (snapshot-unlock id)
    :clone (snapshot-clone id "session" name ports shell)))

;; Session additional functions
(defn session-info [id]
  (let [api-key (get-api-key)]
    (println (curl-get api-key (str "/sessions/" id)))))

(defn session-boost [id vcpu]
  (let [api-key (get-api-key)
        json (str "{\"vcpu\":" vcpu "}")]
    (curl-patch api-key (str "/sessions/" id) json)
    (println (str green "Session boosted to " vcpu " vCPU" reset))))

(defn session-unboost [id]
  (let [api-key (get-api-key)
        json "{\"vcpu\":1}"]
    (curl-patch api-key (str "/sessions/" id) json)
    (println (str green "Session unboosted to 1 vCPU" reset))))

(defn session-execute [id command]
  (let [api-key (get-api-key)
        json (str "{\"command\":\"" (escape-json command) "\"}")
        response (curl-post api-key (str "/sessions/" id "/execute") json)
        stdout-val (extract-field "stdout" response)]
    (when stdout-val
      (print (str blue (unescape-json stdout-val) reset))
      (flush))))

;; Service additional functions
(defn service-lock [id]
  (let [api-key (get-api-key)]
    (curl-post api-key (str "/services/" id "/lock") "{}")
    (println (str green "Service locked: " id reset))))

(defn service-unlock [id]
  (let [api-key (get-api-key)
        result (curl-post-with-sudo api-key (str "/services/" id "/unlock") "{}")]
    (if (:success result)
      (println (str green "Service unlocked: " id reset))
      (do
        (binding [*out* *err*]
          (println (str red "Error unlocking service" reset)))
        (System/exit 1)))))

(defn service-redeploy [id bootstrap]
  (let [api-key (get-api-key)
        json (if bootstrap
               (str "{\"bootstrap\":\"" (escape-json bootstrap) "\"}")
               "{}")]
    (curl-post api-key (str "/services/" id "/redeploy") json)
    (println (str green "Service redeploying: " id reset))))

;; PaaS logs: GET {portal}/logs/{source}[/stream], HMAC over the path only.
;; Partner keys with log access; anything else answers 401/403. In-process HTTP.
(defn- unix-now [] (quot (System/currentTimeMillis) 1000))

(defn- url-encode [^String v]
  (-> (java.net.URLEncoder/encode v "UTF-8") (str/replace "+" "%20") (str/replace "*" "%2A") (str/replace "%7E" "~")))

(defn- logs-query [base params]
  (let [ps (keep (fn [[k v]] (when-not (str/blank? v) (str k "=" (url-encode v)))) params)]
    (if (seq ps) (str base "?" (str/join "&" ps)) base)))

(defn- json-parse
  "Minimal JSON reader: maps with string keys, vectors, strings, numbers, true/false/nil."
  [^String s]
  (let [pos (volatile! 0) n (count s)
        peek (fn [] (if (< @pos n) (.charAt s @pos) \u0000))
        skip-ws (fn [] (while (and (< @pos n) (Character/isWhitespace (.charAt s @pos))) (vswap! pos inc)))
        pstr (fn []
               (vswap! pos inc)
               (let [sb (StringBuilder.)]
                 (loop []
                   (when (< @pos n)
                     (let [c (.charAt s @pos)]
                       (vswap! pos inc)
                       (cond
                         (= c \") nil
                         (= c \\) (let [e (peek)]
                                    (vswap! pos inc)
                                    (case e
                                      \n (.append sb \newline) \t (.append sb \tab) \r (.append sb \return)
                                      \b (.append sb \backspace) \f (.append sb \formfeed)
                                      \u (let [h (subs s @pos (min n (+ @pos 4)))]
                                           (vswap! pos + 4)
                                           (.append sb (char (try (Integer/parseInt h 16) (catch Exception _ 63)))))
                                      (.append sb e))
                                    (recur))
                         :else (do (.append sb c) (recur))))))
                 (str sb)))]
    (letfn [(pval []
              (skip-ws)
              (let [c (peek)]
                (cond
                  (= c \{) (do (vswap! pos inc)
                               (loop [m {}]
                                 (skip-ws)
                                 (case (peek)
                                   \} (do (vswap! pos inc) m)
                                   \, (do (vswap! pos inc) (recur m))
                                   \" (let [k (pstr)] (skip-ws) (vswap! pos inc) (recur (assoc m k (pval))))
                                   (do (vreset! pos n) m))))
                  (= c \[) (do (vswap! pos inc)
                               (loop [v []]
                                 (skip-ws)
                                 (cond
                                   (= (peek) \]) (do (vswap! pos inc) v)
                                   (= (peek) \,) (do (vswap! pos inc) (recur v))
                                   (>= @pos n) v
                                   :else (recur (conj v (pval))))))
                  (= c \") (pstr)
                  :else (let [start @pos]
                          (while (and (< @pos n) (not (#{\, \} \] \space \newline} (.charAt s @pos)))) (vswap! pos inc))
                          (let [tok (subs s start @pos)]
                            (case tok "true" true "false" false "null" nil
                                  (or (try (Long/parseLong tok) (catch Exception _ nil)) tok)))))))]
      (try (pval) (catch Exception _ nil)))))

(defn- log-cursor-file [] (io/file (str (System/getenv "HOME") "/.unsandbox/paas_log_cursor")))

(defn- read-log-cursor
  "Seconds since our last successful fetch as \"Ns\" (min 5), or nil."
  []
  (try
    (let [ts (Long/parseLong (str/trim (slurp (log-cursor-file))))]
      (when (pos? ts) (str (max 5 (- (unix-now) ts)) "s")))
    (catch Exception _ nil)))

(defn- write-log-cursor []
  (try
    (let [f (log-cursor-file) d (.getParentFile f)]
      (when-not (.exists d)
        (.mkdirs d)
        (.setReadable d false false) (.setReadable d true true)
        (.setWritable d false false) (.setWritable d true true)
        (.setExecutable d false false) (.setExecutable d true true))
      (spit f (str (unix-now) "\n")))
    (catch Exception _ nil)))

(defn- logs-open
  "Signed GET; the secret only signs in-process. Returns [code conn]."
  [public-key secret-key path url stream?]
  (let [timestamp (str (unix-now))
        conn (doto ^java.net.HttpURLConnection (.openConnection (java.net.URL. url))
               (.setRequestMethod "GET")
               (.setConnectTimeout 30000)
               (.setReadTimeout (if stream? 120000 300000))
               (.setRequestProperty "User-Agent" "un-cli/2.0")
               (.setRequestProperty "Authorization" (str "Bearer " public-key)))]
    (when stream? (.setRequestProperty conn "Accept" "text/event-stream"))
    (when secret-key
      (.setRequestProperty conn "X-Timestamp" timestamp)
      (.setRequestProperty conn "X-Signature" (make-signature secret-key timestamp "GET" path "")))
    [(.getResponseCode conn) conn]))

(defn- conn-body [^java.net.HttpURLConnection conn code]
  (let [st (if (< 199 code 300) (.getInputStream conn) (.getErrorStream conn))]
    (if st (slurp st :encoding "UTF-8") "")))

(defn- logs-print-line [source line]
  (if (str/blank? source)
    (println line)
    (println (format "%-12s %s" source line)))
  (flush))

(defn- logs-error-text [body]
  (let [e (get (json-parse body) "error")]
    (if (string? e) (str " — " e) "")))

(defn paas-logs-fetch
  "Batch fetch; prints lines (or raw JSON). Returns 0 or 1."
  [public-key secret-key source lines since grep-pattern level json-output]
  (let [path (str "/logs/" source)
        url (logs-query (str portal-base path) [["lines" (str lines)] ["since" since] ["grep" grep-pattern] ["level" level]])
        res (try (let [[code conn] (logs-open public-key secret-key path url false)] [code (conn-body conn code)])
                 (catch Exception e (binding [*out* *err*] (println (str "Error: Network error fetching logs: " (.getMessage e)))) nil))]
    (if-not res
      1
      (let [[code body] res]
        (if (not= code 200)
          (do (binding [*out* *err*] (println (str "Error: HTTP " code (logs-error-text body)))) 1)
          (do
            (write-log-cursor)
            (if json-output
              (do (println body) 0)
              (let [j (json-parse body) ls (get j "lines")]
                (if-not (vector? ls)
                  (do (binding [*out* *err*] (println "Error: Invalid response format")) 1)
                  (do (doseq [e ls]
                        (cond (string? e) (logs-print-line nil e)
                              (string? (get e "line")) (logs-print-line (when (get j "sources") (get e "source")) (get e "line"))))
                      0))))))))))

(defn paas-logs-stream
  "Follow via SSE; reconnects with backoff, re-signing each time. Returns exit code."
  [public-key secret-key source grep-pattern level]
  (let [path (str "/logs/" source "/stream")]
    (binding [*out* *err*] (println (str "Streaming " source " logs... (Ctrl+C to stop)")))
    (loop [delay 2 last-recv 0 reconnect? false]
      (let [since (when (and reconnect? (pos? last-recv)) (str (max 10 (+ (- (unix-now) last-recv) 5)) "s"))
            url (logs-query (str portal-base path) [["grep" grep-pattern] ["level" level] ["since" since]])
            started (unix-now)
            last (volatile! last-recv)
            code (try
                   (let [[code ^java.net.HttpURLConnection conn] (logs-open public-key secret-key path url true)]
                     (when (= code 200)
                       (try
                         (with-open [rdr (io/reader (.getInputStream conn) :encoding "UTF-8")]
                           (doseq [line (line-seq rdr)]
                             (when (str/starts-with? line "data: ")
                               (let [payload (subs line 6) j (json-parse payload) l (when (map? j) (get j "line"))]
                                 (cond (= payload ":keepalive") nil
                                       (string? l) (do (logs-print-line (get j "source") l) (vreset! last (unix-now)))
                                       (and (map? j) (contains? j "event")) nil
                                       :else (do (logs-print-line nil payload) (vreset! last (unix-now))))))))
                         (catch java.io.IOException _ nil)))
                     code)
                   (catch java.io.IOException _ 0))]
        (cond
          (#{401 403} code) (do (binding [*out* *err*] (println (str "Error: HTTP " code " (auth failed)"))) 1)
          (or (= code 0) (= code 200) (>= code 500))
          (let [delay (if (> (- (unix-now) started) 30) 2 delay)]
            (Thread/sleep (* 1000 delay))
            (recur (min 30 (* 2 delay)) @last true))
          :else (do (binding [*out* *err*] (println (str "Error: HTTP " code))) 1))))))

(def ^:private paas-logs-help-text
  ["Usage: un.clj paas logs [options]" ""
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
   "Requires a partner API key with log access enabled."])

(defn paas-command [args]
  (cond
    (empty? args)
    (do (binding [*out* *err*] (println "Usage: un.clj paas <command> [options]\n\nCommands:\n  logs    Fetch or stream production logs")) 1)
    (not= (first args) "logs")
    (do (binding [*out* *err*] (println (str "Error: Unknown paas command '" (first args) "'"))) 1)
    :else
    (loop [a (rest args) o {:source "all" :lines 100}]
      (if (empty? a)
        (let [[public-key secret-key] (if (and (:pk o) (:sk o)) [(:pk o) (:sk o)] (get-api-keys))]
          (if (:follow o)
            (paas-logs-stream public-key secret-key (:source o) (:grep o) (:level o))
            (paas-logs-fetch public-key secret-key (:source o) (:lines o)
                             (or (:since o) (read-log-cursor) "5m") (:grep o) (:level o) (:json o))))
        (let [[opt val] a has-val (some? val)]
          (cond
            (#{"-h" "--help"} opt) (do (binding [*out* *err*] (doseq [l paas-logs-help-text] (println l))) 0)
            (= opt "--all") (recur (rest a) (assoc o :source "all"))
            (= opt "--api") (recur (rest a) (assoc o :source "api"))
            (= opt "--portal") (recur (rest a) (assoc o :source "portal"))
            (#{"--follow" "-f"} opt) (recur (rest a) (assoc o :follow true))
            (= opt "--json") (recur (rest a) (assoc o :json true))
            (not has-val) (recur (rest a) o)
            (= opt "--pool") (recur (drop 2 a) (assoc o :source (str "pool/" val)))
            (= opt "--source") (recur (drop 2 a) (assoc o :source val))
            (#{"--lines" "-n"} opt) (recur (drop 2 a) (assoc o :lines (max 1 (min 10000 (try (Integer/parseInt val) (catch Exception _ 1))))))
            (= opt "--since") (recur (drop 2 a) (assoc o :since val))
            (= opt "--grep") (recur (drop 2 a) (assoc o :grep val))
            (#{"--level" "-l"} opt) (recur (drop 2 a) (assoc o :level val))
            (= opt "-p") (recur (drop 2 a) (assoc o :pk val))
            (= opt "-k") (recur (drop 2 a) (assoc o :sk val))
            :else (recur (rest a) o)))))))

;; Library form kept for callers: prints the raw JSON response.
(defn logs-fetch
  ([source lines since grep-pattern] (logs-fetch source lines since grep-pattern nil))
  ([source lines since grep-pattern level]
   (let [[public-key secret-key] (get-api-keys)]
     (paas-logs-fetch public-key secret-key (or source "all") (or lines 100) (or since "5m") grep-pattern level true))))

;; Utility functions
(defn health-check []
  (try
    (let [result (:out (sh "curl" "-s" (str api-base "/health")))]
      (println result)
      (str/includes? result "ok"))
    (catch Exception _ false)))

(defn version []
  "4.2.0")

(defn image-command [action id source-type visibility name ports]
  (case action
    :list (image-list)
    :info (image-info id)
    :delete (image-delete id)
    :lock (image-lock id)
    :unlock (image-unlock id)
    :publish (if source-type
               (image-publish id source-type name)
               (do
                 (binding [*out* *err*]
                   (println (str red "Error: --publish requires --source-type (service or snapshot)" reset)))
                 (System/exit 1)))
    :visibility (if visibility
                  (image-visibility id visibility)
                  (do
                    (binding [*out* *err*]
                      (println (str red "Error: --visibility requires visibility mode (private, unlisted, public)" reset)))
                    (System/exit 1)))
    :spawn (image-spawn id name ports)
    :clone (image-clone id name)))

(defn parse-args [args]
  (loop [args args
         file nil
         env-vars []
         artifacts false
         out-dir nil
         network nil
         vcpu nil
         session-action nil
         session-id nil
         session-shell nil
         session-input-files []
         service-action nil
         service-id nil
         service-name nil
         service-ports nil
         service-bootstrap nil
         service-bootstrap-file nil
         service-type nil
         service-input-files []
         service-envs []
         service-env-file nil
         service-unfreeze-on-demand nil
         key-extend false
         image-action nil
         image-id nil
         image-source-type nil
         image-visibility nil
         image-name nil
         image-ports nil
         mode :execute]
    (cond
      (empty? args)
      (case mode
        :session (session-command (or session-action :create) session-id session-shell network vcpu session-input-files)
        :service (service-command (or service-action :create) service-id service-name service-ports service-bootstrap service-bootstrap-file service-type network vcpu service-input-files service-envs service-env-file service-unfreeze-on-demand)
        :key (key-command key-extend)
        :languages (languages-command false)
        :image (image-command (or image-action :list) image-id image-source-type image-visibility image-name image-ports)
        :execute (if file
                   (execute-command file env-vars artifacts out-dir network vcpu)
                   (do (println "Usage: un.clj [options] <source_file>")
                       (println "  --os OS        Target OS: linux (default) or freebsd (execute, session, service)")
                       (println "       un.clj languages [--json]")
                       (println "       un.clj session [options]")
                       (println "       un.clj service [options]")
                       (println "       un.clj service env <action> <service_id>")
                       (println "       un.clj image [options]")
                       (println "       un.clj key [options]")
                       (println "       un.clj paas logs [options]")
                       (System/exit 1))))

      (= (first args) "session")
      (recur (rest args) file env-vars artifacts out-dir network vcpu session-action session-id session-shell session-input-files
             service-action service-id service-name service-ports service-bootstrap service-bootstrap-file service-type service-input-files service-envs service-env-file service-unfreeze-on-demand key-extend image-action image-id image-source-type image-visibility image-name image-ports :session)

      (= (first args) "service")
      (recur (rest args) file env-vars artifacts out-dir network vcpu session-action session-id session-shell session-input-files
             service-action service-id service-name service-ports service-bootstrap service-bootstrap-file service-type service-input-files service-envs service-env-file service-unfreeze-on-demand key-extend image-action image-id image-source-type image-visibility image-name image-ports :service)

      (= (first args) "key")
      (recur (rest args) file env-vars artifacts out-dir network vcpu session-action session-id session-shell session-input-files
             service-action service-id service-name service-ports service-bootstrap service-bootstrap-file service-type service-input-files service-envs service-env-file service-unfreeze-on-demand key-extend image-action image-id image-source-type image-visibility image-name image-ports :key)

      (= (first args) "languages")
      (recur (rest args) file env-vars artifacts out-dir network vcpu session-action session-id session-shell session-input-files
             service-action service-id service-name service-ports service-bootstrap service-bootstrap-file service-type service-input-files service-envs service-env-file service-unfreeze-on-demand key-extend image-action image-id image-source-type image-visibility image-name image-ports :languages)

      (= (first args) "image")
      (recur (rest args) file env-vars artifacts out-dir network vcpu session-action session-id session-shell session-input-files
             service-action service-id service-name service-ports service-bootstrap service-bootstrap-file service-type service-input-files service-envs service-env-file service-unfreeze-on-demand key-extend image-action image-id image-source-type image-visibility image-name image-ports :image)

      (= (first args) "snapshot")
      (let [rest-args (rest args)]
        (cond
          (empty? rest-args) (do (snapshot-list) (System/exit 0))
          (= (first rest-args) "--list") (do (snapshot-list) (System/exit 0))
          (= (first rest-args) "-l") (do (snapshot-list) (System/exit 0))
          (= (first rest-args) "--info") (do (snapshot-info (second rest-args)) (System/exit 0))
          (= (first rest-args) "--session") (do (snapshot-session (second rest-args) nil false) (System/exit 0))
          (= (first rest-args) "--service") (do (snapshot-service (second rest-args) nil false) (System/exit 0))
          (= (first rest-args) "--restore") (do (snapshot-restore (second rest-args)) (System/exit 0))
          (= (first rest-args) "--delete") (do (snapshot-delete (second rest-args)) (System/exit 0))
          (= (first rest-args) "--lock") (do (snapshot-lock (second rest-args)) (System/exit 0))
          (= (first rest-args) "--unlock") (do (snapshot-unlock (second rest-args)) (System/exit 0))
          (= (first rest-args) "--clone") (do (snapshot-clone (second rest-args) "session" nil nil nil) (System/exit 0))
          :else (do (println "Error: Unknown snapshot action") (System/exit 1))))

      ;; Image options
      (and (= mode :image) (or (= (first args) "--list") (= (first args) "-l")))
      (recur (rest args) file env-vars artifacts out-dir network vcpu session-action session-id session-shell session-input-files
             service-action service-id service-name service-ports service-bootstrap service-bootstrap-file service-type service-input-files service-envs service-env-file service-unfreeze-on-demand key-extend :list image-id image-source-type image-visibility image-name image-ports mode)

      (and (= mode :image) (= (first args) "--info"))
      (recur (rest (rest args)) file env-vars artifacts out-dir network vcpu session-action session-id session-shell session-input-files
             service-action service-id service-name service-ports service-bootstrap service-bootstrap-file service-type service-input-files service-envs service-env-file service-unfreeze-on-demand key-extend :info (second args) image-source-type image-visibility image-name image-ports mode)

      (and (= mode :image) (= (first args) "--delete"))
      (recur (rest (rest args)) file env-vars artifacts out-dir network vcpu session-action session-id session-shell session-input-files
             service-action service-id service-name service-ports service-bootstrap service-bootstrap-file service-type service-input-files service-envs service-env-file service-unfreeze-on-demand key-extend :delete (second args) image-source-type image-visibility image-name image-ports mode)

      (and (= mode :image) (= (first args) "--lock"))
      (recur (rest (rest args)) file env-vars artifacts out-dir network vcpu session-action session-id session-shell session-input-files
             service-action service-id service-name service-ports service-bootstrap service-bootstrap-file service-type service-input-files service-envs service-env-file service-unfreeze-on-demand key-extend :lock (second args) image-source-type image-visibility image-name image-ports mode)

      (and (= mode :image) (= (first args) "--unlock"))
      (recur (rest (rest args)) file env-vars artifacts out-dir network vcpu session-action session-id session-shell session-input-files
             service-action service-id service-name service-ports service-bootstrap service-bootstrap-file service-type service-input-files service-envs service-env-file service-unfreeze-on-demand key-extend :unlock (second args) image-source-type image-visibility image-name image-ports mode)

      (and (= mode :image) (= (first args) "--publish"))
      (recur (rest (rest args)) file env-vars artifacts out-dir network vcpu session-action session-id session-shell session-input-files
             service-action service-id service-name service-ports service-bootstrap service-bootstrap-file service-type service-input-files service-envs service-env-file service-unfreeze-on-demand key-extend :publish (second args) image-source-type image-visibility image-name image-ports mode)

      (and (= mode :image) (= (first args) "--source-type"))
      (recur (rest (rest args)) file env-vars artifacts out-dir network vcpu session-action session-id session-shell session-input-files
             service-action service-id service-name service-ports service-bootstrap service-bootstrap-file service-type service-input-files service-envs service-env-file service-unfreeze-on-demand key-extend image-action image-id (second args) image-visibility image-name image-ports mode)

      (and (= mode :image) (= (first args) "--visibility"))
      (recur (rest (rest (rest args))) file env-vars artifacts out-dir network vcpu session-action session-id session-shell session-input-files
             service-action service-id service-name service-ports service-bootstrap service-bootstrap-file service-type service-input-files service-envs service-env-file service-unfreeze-on-demand key-extend :visibility (second args) image-source-type (nth args 2) image-name image-ports mode)

      (and (= mode :image) (= (first args) "--spawn"))
      (recur (rest (rest args)) file env-vars artifacts out-dir network vcpu session-action session-id session-shell session-input-files
             service-action service-id service-name service-ports service-bootstrap service-bootstrap-file service-type service-input-files service-envs service-env-file service-unfreeze-on-demand key-extend :spawn (second args) image-source-type image-visibility image-name image-ports mode)

      (and (= mode :image) (= (first args) "--clone"))
      (recur (rest (rest args)) file env-vars artifacts out-dir network vcpu session-action session-id session-shell session-input-files
             service-action service-id service-name service-ports service-bootstrap service-bootstrap-file service-type service-input-files service-envs service-env-file service-unfreeze-on-demand key-extend :clone (second args) image-source-type image-visibility image-name image-ports mode)

      (and (= mode :image) (= (first args) "--name"))
      (recur (rest (rest args)) file env-vars artifacts out-dir network vcpu session-action session-id session-shell session-input-files
             service-action service-id service-name service-ports service-bootstrap service-bootstrap-file service-type service-input-files service-envs service-env-file service-unfreeze-on-demand key-extend image-action image-id image-source-type image-visibility (second args) image-ports mode)

      (and (= mode :image) (= (first args) "--ports"))
      (recur (rest (rest args)) file env-vars artifacts out-dir network vcpu session-action session-id session-shell session-input-files
             service-action service-id service-name service-ports service-bootstrap service-bootstrap-file service-type service-input-files service-envs service-env-file service-unfreeze-on-demand key-extend image-action image-id image-source-type image-visibility image-name (second args) mode)

      ;; Key options
      (and (= mode :key) (= (first args) "--extend"))
      (recur (rest args) file env-vars artifacts out-dir network vcpu session-action session-id session-shell session-input-files
             service-action service-id service-name service-ports service-bootstrap service-bootstrap-file service-type service-input-files service-envs service-env-file service-unfreeze-on-demand true image-action image-id image-source-type image-visibility image-name image-ports mode)

      ;; Languages options
      (and (= mode :languages) (= (first args) "--json"))
      (do
        (languages-command true)
        (System/exit 0))

      ;; Session options
      (and (= mode :session) (= (first args) "--list"))
      (recur (rest args) file env-vars artifacts out-dir network vcpu :list session-id session-shell session-input-files
             service-action service-id service-name service-ports service-bootstrap service-bootstrap-file service-type service-input-files service-envs service-env-file service-unfreeze-on-demand key-extend image-action image-id image-source-type image-visibility image-name image-ports mode)

      (and (= mode :session) (= (first args) "--kill"))
      (recur (rest (rest args)) file env-vars artifacts out-dir network vcpu :kill (second args) session-shell session-input-files
             service-action service-id service-name service-ports service-bootstrap service-bootstrap-file service-type service-input-files service-envs service-env-file service-unfreeze-on-demand key-extend image-action image-id image-source-type image-visibility image-name image-ports mode)

      (and (= mode :session) (or (= (first args) "--shell") (= (first args) "-s")))
      (recur (rest (rest args)) file env-vars artifacts out-dir network vcpu session-action session-id (second args) session-input-files
             service-action service-id service-name service-ports service-bootstrap service-bootstrap-file service-type service-input-files service-envs service-env-file service-unfreeze-on-demand key-extend image-action image-id image-source-type image-visibility image-name image-ports mode)

      (and (= mode :session) (= (first args) "-f"))
      (recur (rest (rest args)) file env-vars artifacts out-dir network vcpu session-action session-id session-shell (conj session-input-files (second args))
             service-action service-id service-name service-ports service-bootstrap service-bootstrap-file service-type service-input-files service-envs service-env-file service-unfreeze-on-demand key-extend image-action image-id image-source-type image-visibility image-name image-ports mode)

      ;; Service options
      (and (= mode :service) (= (first args) "--list"))
      (recur (rest args) file env-vars artifacts out-dir network vcpu session-action session-id session-shell session-input-files
             :list service-id service-name service-ports service-bootstrap service-bootstrap-file service-type service-input-files service-envs service-env-file service-unfreeze-on-demand key-extend image-action image-id image-source-type image-visibility image-name image-ports mode)

      (and (= mode :service) (= (first args) "--info"))
      (recur (rest (rest args)) file env-vars artifacts out-dir network vcpu session-action session-id session-shell session-input-files
             :info (second args) service-name service-ports service-bootstrap service-bootstrap-file service-type service-input-files service-envs service-env-file service-unfreeze-on-demand key-extend image-action image-id image-source-type image-visibility image-name image-ports mode)

      (and (= mode :service) (= (first args) "--logs"))
      (recur (rest (rest args)) file env-vars artifacts out-dir network vcpu session-action session-id session-shell session-input-files
             :logs (second args) service-name service-ports service-bootstrap service-bootstrap-file service-type service-input-files service-envs service-env-file service-unfreeze-on-demand key-extend image-action image-id image-source-type image-visibility image-name image-ports mode)

      (and (= mode :service) (= (first args) "--freeze"))
      (recur (rest (rest args)) file env-vars artifacts out-dir network vcpu session-action session-id session-shell session-input-files
             :sleep (second args) service-name service-ports service-bootstrap service-bootstrap-file service-type service-input-files service-envs service-env-file service-unfreeze-on-demand key-extend image-action image-id image-source-type image-visibility image-name image-ports mode)

      (and (= mode :service) (= (first args) "--unfreeze"))
      (recur (rest (rest args)) file env-vars artifacts out-dir network vcpu session-action session-id session-shell session-input-files
             :wake (second args) service-name service-ports service-bootstrap service-bootstrap-file service-type service-input-files service-envs service-env-file service-unfreeze-on-demand key-extend image-action image-id image-source-type image-visibility image-name image-ports mode)

      (and (= mode :service) (= (first args) "--destroy"))
      (recur (rest (rest args)) file env-vars artifacts out-dir network vcpu session-action session-id session-shell session-input-files
             :destroy (second args) service-name service-ports service-bootstrap service-bootstrap-file service-type service-input-files service-envs service-env-file service-unfreeze-on-demand key-extend image-action image-id image-source-type image-visibility image-name image-ports mode)

      (and (= mode :service) (= (first args) "--resize"))
      (recur (rest (rest args)) file env-vars artifacts out-dir network vcpu session-action session-id session-shell session-input-files
             :resize (second args) service-name service-ports service-bootstrap service-bootstrap-file service-type service-input-files service-envs service-env-file service-unfreeze-on-demand key-extend image-action image-id image-source-type image-visibility image-name image-ports mode)

      (and (= mode :service) (= (first args) "--set-unfreeze-on-demand"))
      (recur (rest (rest (rest args))) file env-vars artifacts out-dir network vcpu session-action session-id session-shell session-input-files
             :set-unfreeze-on-demand (second args) service-name service-ports service-bootstrap service-bootstrap-file service-type service-input-files service-envs service-env-file (= (nth args 2) "true") key-extend image-action image-id image-source-type image-visibility image-name image-ports mode)

      (and (= mode :service) (= (first args) "--execute"))
      (recur (rest (rest (rest args))) file env-vars artifacts out-dir network vcpu session-action session-id session-shell session-input-files
             :execute (second args) service-name service-ports (nth args 2) service-bootstrap-file service-type service-input-files service-envs service-env-file service-unfreeze-on-demand key-extend image-action image-id image-source-type image-visibility image-name image-ports mode)

      (and (= mode :service) (= (first args) "--dump-bootstrap") (>= (count args) 3) (not (.startsWith (nth args 2) "-")))
      (recur (rest (rest (rest args))) file env-vars artifacts out-dir network vcpu session-action session-id session-shell session-input-files
             :dump-bootstrap (second args) service-name service-ports service-bootstrap (nth args 2) service-type service-input-files service-envs service-env-file service-unfreeze-on-demand key-extend image-action image-id image-source-type image-visibility image-name image-ports mode)

      (and (= mode :service) (= (first args) "--dump-bootstrap"))
      (recur (rest (rest args)) file env-vars artifacts out-dir network vcpu session-action session-id session-shell session-input-files
             :dump-bootstrap (second args) service-name service-ports service-bootstrap service-bootstrap-file service-type service-input-files service-envs service-env-file service-unfreeze-on-demand key-extend image-action image-id image-source-type image-visibility image-name image-ports mode)

      (and (= mode :service) (= (first args) "--name"))
      (recur (rest (rest args)) file env-vars artifacts out-dir network vcpu session-action session-id session-shell session-input-files
             :create service-id (second args) service-ports service-bootstrap service-bootstrap-file service-type service-input-files service-envs service-env-file service-unfreeze-on-demand key-extend image-action image-id image-source-type image-visibility image-name image-ports mode)

      (and (= mode :service) (= (first args) "--ports"))
      (recur (rest (rest args)) file env-vars artifacts out-dir network vcpu session-action session-id session-shell session-input-files
             service-action service-id service-name (second args) service-bootstrap service-bootstrap-file service-type service-input-files service-envs service-env-file service-unfreeze-on-demand key-extend image-action image-id image-source-type image-visibility image-name image-ports mode)

      (and (= mode :service) (= (first args) "--bootstrap"))
      (recur (rest (rest args)) file env-vars artifacts out-dir network vcpu session-action session-id session-shell session-input-files
             service-action service-id service-name service-ports (second args) service-bootstrap-file service-type service-input-files service-envs service-env-file service-unfreeze-on-demand key-extend image-action image-id image-source-type image-visibility image-name image-ports mode)

      (and (= mode :service) (= (first args) "--bootstrap-file"))
      (recur (rest (rest args)) file env-vars artifacts out-dir network vcpu session-action session-id session-shell session-input-files
             service-action service-id service-name service-ports service-bootstrap (second args) service-type service-input-files service-envs service-env-file service-unfreeze-on-demand key-extend image-action image-id image-source-type image-visibility image-name image-ports mode)

      (and (= mode :service) (= (first args) "--type"))
      (recur (rest (rest args)) file env-vars artifacts out-dir network vcpu session-action session-id session-shell session-input-files
             service-action service-id service-name service-ports service-bootstrap service-bootstrap-file (second args) service-input-files service-envs service-env-file service-unfreeze-on-demand key-extend image-action image-id image-source-type image-visibility image-name image-ports mode)

      (and (= mode :service) (= (first args) "-f"))
      (recur (rest (rest args)) file env-vars artifacts out-dir network vcpu session-action session-id session-shell session-input-files
             service-action service-id service-name service-ports service-bootstrap service-bootstrap-file service-type (conj service-input-files (second args)) service-envs service-env-file service-unfreeze-on-demand key-extend image-action image-id image-source-type image-visibility image-name image-ports mode)

      (and (= mode :service) (= (first args) "env") (>= (count args) 2))
      (let [env-action (second args)
            env-target (when (and (>= (count args) 3) (not (.startsWith (nth args 2) "-"))) (nth args 2))
            rest-args (if env-target (drop 3 args) (drop 2 args))]
        (recur rest-args file env-vars artifacts out-dir network vcpu session-action session-id session-shell session-input-files
               :env env-action env-target service-ports service-bootstrap service-bootstrap-file service-type service-input-files service-envs service-env-file service-unfreeze-on-demand key-extend image-action image-id image-source-type image-visibility image-name image-ports mode))

      (and (= mode :service) (= (first args) "-e"))
      (recur (rest (rest args)) file env-vars artifacts out-dir network vcpu session-action session-id session-shell session-input-files
             service-action service-id service-name service-ports service-bootstrap service-bootstrap-file service-type service-input-files (conj service-envs (second args)) service-env-file service-unfreeze-on-demand key-extend image-action image-id image-source-type image-visibility image-name image-ports mode)

      (and (= mode :service) (= (first args) "--env-file"))
      (recur (rest (rest args)) file env-vars artifacts out-dir network vcpu session-action session-id session-shell session-input-files
             service-action service-id service-name service-ports service-bootstrap service-bootstrap-file service-type service-input-files service-envs (second args) service-unfreeze-on-demand key-extend image-action image-id image-source-type image-visibility image-name image-ports mode)

      (and (= mode :service) (= (first args) "--unfreeze-on-demand"))
      (recur (rest (rest args)) file env-vars artifacts out-dir network vcpu session-action session-id session-shell session-input-files
             service-action service-id service-name service-ports service-bootstrap service-bootstrap-file service-type service-input-files service-envs service-env-file (= (second args) "true") key-extend image-action image-id image-source-type image-visibility image-name image-ports mode)

      ;; Execute options
      (= (first args) "-e")
      (let [[k v] (str/split (second args) #"=" 2)]
        (recur (rest (rest args)) file (conj env-vars [k v]) artifacts out-dir network vcpu
               session-action session-id session-shell session-input-files service-action service-id service-name service-ports service-bootstrap service-bootstrap-file service-type service-input-files service-envs service-env-file service-unfreeze-on-demand key-extend image-action image-id image-source-type image-visibility image-name image-ports mode))

      (= (first args) "-a")
      (recur (rest args) file env-vars true out-dir network vcpu session-action session-id session-shell session-input-files
             service-action service-id service-name service-ports service-bootstrap service-bootstrap-file service-type service-input-files service-envs service-env-file service-unfreeze-on-demand key-extend image-action image-id image-source-type image-visibility image-name image-ports mode)

      (= (first args) "-o")
      (recur (rest (rest args)) file env-vars artifacts (second args) network vcpu session-action session-id session-shell session-input-files
             service-action service-id service-name service-ports service-bootstrap service-bootstrap-file service-type service-input-files service-envs service-env-file service-unfreeze-on-demand key-extend image-action image-id image-source-type image-visibility image-name image-ports mode)

      (= (first args) "-n")
      (recur (rest (rest args)) file env-vars artifacts out-dir (second args) vcpu session-action session-id session-shell session-input-files
             service-action service-id service-name service-ports service-bootstrap service-bootstrap-file service-type service-input-files service-envs service-env-file service-unfreeze-on-demand key-extend image-action image-id image-source-type image-visibility image-name image-ports mode)

      (= (first args) "-v")
      (recur (rest (rest args)) file env-vars artifacts out-dir network (Integer/parseInt (second args)) session-action session-id session-shell session-input-files
             service-action service-id service-name service-ports service-bootstrap service-bootstrap-file service-type service-input-files service-envs service-env-file service-unfreeze-on-demand key-extend image-action image-id image-source-type image-visibility image-name image-ports mode)

      ;; Source file
      (and (= mode :execute) (not (.startsWith (first args) "-")) (nil? file))
      (recur (rest args) (first args) env-vars artifacts out-dir network vcpu session-action session-id session-shell session-input-files
             service-action service-id service-name service-ports service-bootstrap service-bootstrap-file service-type service-input-files service-envs service-env-file service-unfreeze-on-demand key-extend image-action image-id image-source-type image-visibility image-name image-ports mode)

      ;; Unknown option check
      (and (= mode :session) (.startsWith (first args) "-"))
      (do
        (println (str red "Unknown option: " (first args) reset) *err*)
        (println "Usage: un.clj session [options]")
        (println "Options: --list, --kill ID, --shell SHELL, -s SHELL, -f FILE, -n NETWORK, -v VCPU")
        (System/exit 1))

      :else
      (recur (rest args) file env-vars artifacts out-dir network vcpu session-action session-id session-shell session-input-files
             service-action service-id service-name service-ports service-bootstrap service-bootstrap-file service-type service-input-files service-envs service-env-file service-unfreeze-on-demand key-extend image-action image-id image-source-type image-visibility image-name image-ports mode))))

(let [raw-args *command-line-args*
      account-val (second (drop-while #(not= % "--account") raw-args))
      account-idx (when account-val
                    (try (Integer/parseInt account-val) (catch Exception _ nil)))
      ;; --os OS / --os=OS: global, validated before any credential or HTTP use
      os-raw (loop [in raw-args found nil]
               (cond
                 (empty? in) found
                 (= (first in) "--os") (recur (drop 2 in) (if (next in) (second in) ::missing))
                 (.startsWith ^String (first in) "--os=") (recur (rest in) (subs (first in) 5))
                 :else (recur (rest in) found)))
      target-os (cond
                  (nil? os-raw) nil
                  (= os-raw ::missing)
                  (do (binding [*out* *err*] (println "Error: --os requires a value: linux or freebsd"))
                      (System/exit 2))
                  :else
                  (or (normalize-os os-raw)
                      (do (binding [*out* *err*]
                            (println (str "Error: invalid --os '" os-raw "' (expected linux or freebsd)")))
                          (System/exit 2))))
      filtered-args (loop [in raw-args out []]
                      (cond
                        (empty? in) out
                        (= (first in) "--account") (recur (drop 2 in) out)
                        (= (first in) "--os") (recur (drop 2 in) out)
                        (.startsWith ^String (first in) "--os=") (recur (rest in) out)
                        :else (recur (rest in) (conj out (first in)))))]
  (binding [*account-index* account-idx
            *target-os* target-os]
    (cond
      (= (first filtered-args) "paas")
      (let [rc (paas-command (rest filtered-args))]
        (shutdown-agents)
        (System/exit rc))
      (and (= (first filtered-args) "key") (some #{"--rotate"} filtered-args))
      (do
        (key-rotate-command (second (drop-while #(not= % "--otp") filtered-args)))
        (shutdown-agents))
      :else (parse-args filtered-args))))
