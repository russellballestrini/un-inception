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

(def portal-base "https://unsandbox.com")

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
  (let [pattern-str (re-pattern (str "\"" field "\":\"([^\"]*)\""))
        pattern-num (re-pattern (str "\"" field "\":(\\d+)"))]
    (or (second (re-find pattern-str json-str))
        (second (re-find pattern-num json-str)))))

(defn get-api-keys []
  (let [public-key (System/getenv "UNSANDBOX_PUBLIC_KEY")
        secret-key (System/getenv "UNSANDBOX_SECRET_KEY")
        api-key (System/getenv "UNSANDBOX_API_KEY")]
    (cond
      (and public-key secret-key) [public-key secret-key]
      api-key [api-key nil]
      :else (do
              (binding [*out* *err*]
                (println "Error: UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY not set (or UNSANDBOX_API_KEY for backwards compat)"))
              (System/exit 1)))))

(defn get-api-key []
  (first (get-api-keys)))

(defn hmac-sha256 [secret message]
  (let [mac (javax.crypto.Mac/getInstance "HmacSHA256")
        secret-key (javax.crypto.spec.SecretKeySpec. (.getBytes secret "UTF-8") "HmacSHA256")]
    (.init mac secret-key)
    (let [bytes (.doFinal mac (.getBytes message "UTF-8"))]
      (apply str (map #(format "%02x" %) bytes)))))

(defn make-signature [secret-key timestamp method path body]
  (let [message (str timestamp ":" method ":" path ":" body)]
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
                        (str "https://api.unsandbox.com" endpoint)
                        "-H" "Content-Type: application/json"]
                       auth-headers
                       ["-d" (str "@" tmp-file)])
          {:keys [out]} (apply sh args)]
      (io/delete-file tmp-file true)
      (check-clock-drift-error out)
      out)))

(defn curl-get [api-key endpoint]
  (let [[public-key secret-key] (get-api-keys)
        auth-headers (build-auth-headers public-key secret-key "GET" endpoint "")
        args (concat ["curl" "-s"
                      (str "https://api.unsandbox.com" endpoint)]
                     auth-headers)
        result (:out (apply sh args))]
    (check-clock-drift-error result)
    result))

(defn curl-delete [api-key endpoint]
  (let [[public-key secret-key] (get-api-keys)
        auth-headers (build-auth-headers public-key secret-key "DELETE" endpoint "")
        args (concat ["curl" "-s" "-X" "DELETE"
                      (str "https://api.unsandbox.com" endpoint)]
                     auth-headers)
        result (:out (apply sh args))]
    (check-clock-drift-error result)
    result))

(defn curl-put-text [endpoint body]
  (let [tmp-file (str "/tmp/un_clj_" (rand-int 999999) ".txt")
        [public-key secret-key] (get-api-keys)
        auth-headers (build-auth-headers public-key secret-key "PUT" endpoint body)]
    (spit tmp-file body)
    (let [args (concat ["curl" "-s" "-o" "/dev/null" "-w" "%{http_code}" "-X" "PUT"
                        (str "https://api.unsandbox.com" endpoint)
                        "-H" "Content-Type: text/plain"]
                       auth-headers
                       ["-d" (str "@" tmp-file)])
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
                        (str "https://api.unsandbox.com" endpoint)
                        "-H" "Content-Type: application/json"]
                       auth-headers
                       ["-d" (str "@" tmp-file)])
          {:keys [out]} (apply sh args)]
      (io/delete-file tmp-file true)
      (check-clock-drift-error out)
      out)))

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

(defn curl-portal-post [api-key endpoint json-data]
  (let [tmp-file (str "/tmp/un_clj_portal_" (rand-int 999999) ".json")
        [public-key secret-key] (get-api-keys)
        auth-headers (build-auth-headers public-key secret-key "POST" endpoint json-data)]
    (spit tmp-file json-data)
    (let [args (concat ["curl" "-s" "-X" "POST"
                        (str portal-base endpoint)
                        "-H" "Content-Type: application/json"]
                       auth-headers
                       ["-d" (str "@" tmp-file)])
          {:keys [out]} (apply sh args)]
      (io/delete-file tmp-file true)
      (check-clock-drift-error out)
      out)))

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
                    env-json artifacts-json network-json vcpu-json "}")]
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
                    json (str "{\"shell\":\"" sh "\"" network-json vcpu-json input-files-json "}")]
                (println (str yellow "Session created (WebSocket required)" reset))
                (println (curl-post api-key "/sessions" json))))))

(defn service-command [action sid name ports bootstrap bootstrap-file service-type network vcpu input-files envs env-file]
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
      :destroy (do
                 (curl-delete api-key (str "/services/" sid))
                 (println (str green "Service destroyed: " sid reset)))
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
                      input-files-json (build-input-files-json input-files)
                      json (str "{\"name\":\"" name "\"" ports-json bootstrap-json bootstrap-content-json service-type-json network-json vcpu-json input-files-json "}")
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
                          (println (str yellow "Warning: Failed to set vault" reset))))))))))))

(defn validate-key [api-key extend?]
  (let [response (curl-portal-post api-key "/keys/validate" "{}")
        status (extract-field "status" response)
        public-key (extract-field "public_key" response)
        tier (extract-field "tier" response)
        valid-through (extract-field "valid_through_datetime" response)
        valid-for (extract-field "valid_for_human" response)
        rate-limit (extract-field "rate_per_minute" response)
        burst (extract-field "burst" response)
        concurrency (extract-field "concurrency" response)
        expired-at (extract-field "expired_at_datetime" response)]
    (cond
      (= status "valid")
      (do
        (println (str green "Valid" reset "\n"))
        (when public-key (println (str "Public Key:          " public-key)))
        (when tier (println (str "Tier:                " tier)))
        (println "Status:              valid")
        (when valid-through (println (str "Expires:             " valid-through)))
        (when valid-for (println (str "Time Remaining:      " valid-for)))
        (when rate-limit (println (str "Rate Limit:          " rate-limit "/min")))
        (when burst (println (str "Burst:               " burst)))
        (when concurrency (println (str "Concurrency:         " concurrency)))
        (when extend?
          (let [url (str portal-base "/keys/extend?pk=" public-key)]
            (println (str blue "Opening browser to extend key..." reset))
            (sh "xdg-open" url))))

      (= status "expired")
      (do
        (println (str red "Expired" reset "\n"))
        (when public-key (println (str "Public Key:          " public-key)))
        (when tier (println (str "Tier:                " tier)))
        (when expired-at (println (str "Expired:             " expired-at)))
        (println (str "\n" yellow "To renew:" reset " Visit " portal-base "/keys/extend"))
        (when extend?
          (let [url (str portal-base "/keys/extend?pk=" public-key)]
            (println (str blue "Opening browser..." reset))
            (sh "xdg-open" url))))

      :else
      (do
        (println (str red "Invalid" reset))
        (println response)))))

(defn key-command [extend?]
  (let [api-key (get-api-key)]
    (validate-key api-key extend?)))

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
         key-extend false
         mode :execute]
    (cond
      (empty? args)
      (case mode
        :session (session-command (or session-action :create) session-id session-shell network vcpu session-input-files)
        :service (service-command (or service-action :create) service-id service-name service-ports service-bootstrap service-bootstrap-file service-type network vcpu service-input-files service-envs service-env-file)
        :key (key-command key-extend)
        :execute (if file
                   (execute-command file env-vars artifacts out-dir network vcpu)
                   (do (println "Usage: un.clj [options] <source_file>")
                       (println "       un.clj session [options]")
                       (println "       un.clj service [options]")
                       (println "       un.clj service env <action> <service_id>")
                       (println "       un.clj key [options]")
                       (System/exit 1))))

      (= (first args) "session")
      (recur (rest args) file env-vars artifacts out-dir network vcpu session-action session-id session-shell session-input-files
             service-action service-id service-name service-ports service-bootstrap service-bootstrap-file service-type service-input-files service-envs service-env-file key-extend :session)

      (= (first args) "service")
      (recur (rest args) file env-vars artifacts out-dir network vcpu session-action session-id session-shell session-input-files
             service-action service-id service-name service-ports service-bootstrap service-bootstrap-file service-type service-input-files service-envs service-env-file key-extend :service)

      (= (first args) "key")
      (recur (rest args) file env-vars artifacts out-dir network vcpu session-action session-id session-shell session-input-files
             service-action service-id service-name service-ports service-bootstrap service-bootstrap-file service-type service-input-files service-envs service-env-file key-extend :key)

      ;; Key options
      (and (= mode :key) (= (first args) "--extend"))
      (recur (rest args) file env-vars artifacts out-dir network vcpu session-action session-id session-shell session-input-files
             service-action service-id service-name service-ports service-bootstrap service-bootstrap-file service-type service-input-files true mode)

      ;; Session options
      (and (= mode :session) (= (first args) "--list"))
      (recur (rest args) file env-vars artifacts out-dir network vcpu :list session-id session-shell session-input-files
             service-action service-id service-name service-ports service-bootstrap service-bootstrap-file service-type service-input-files service-envs service-env-file key-extend mode)

      (and (= mode :session) (= (first args) "--kill"))
      (recur (rest (rest args)) file env-vars artifacts out-dir network vcpu :kill (second args) session-shell session-input-files
             service-action service-id service-name service-ports service-bootstrap service-bootstrap-file service-type service-input-files service-envs service-env-file key-extend mode)

      (and (= mode :session) (or (= (first args) "--shell") (= (first args) "-s")))
      (recur (rest (rest args)) file env-vars artifacts out-dir network vcpu session-action session-id (second args) session-input-files
             service-action service-id service-name service-ports service-bootstrap service-bootstrap-file service-type service-input-files service-envs service-env-file key-extend mode)

      (and (= mode :session) (= (first args) "-f"))
      (recur (rest (rest args)) file env-vars artifacts out-dir network vcpu session-action session-id session-shell (conj session-input-files (second args))
             service-action service-id service-name service-ports service-bootstrap service-bootstrap-file service-type service-input-files service-envs service-env-file key-extend mode)

      ;; Service options
      (and (= mode :service) (= (first args) "--list"))
      (recur (rest args) file env-vars artifacts out-dir network vcpu session-action session-id session-shell session-input-files
             :list service-id service-name service-ports service-bootstrap service-bootstrap-file service-type service-input-files service-envs service-env-file key-extend mode)

      (and (= mode :service) (= (first args) "--info"))
      (recur (rest (rest args)) file env-vars artifacts out-dir network vcpu session-action session-id session-shell session-input-files
             :info (second args) service-name service-ports service-bootstrap service-bootstrap-file service-type service-input-files service-envs service-env-file key-extend mode)

      (and (= mode :service) (= (first args) "--logs"))
      (recur (rest (rest args)) file env-vars artifacts out-dir network vcpu session-action session-id session-shell session-input-files
             :logs (second args) service-name service-ports service-bootstrap service-bootstrap-file service-type service-input-files service-envs service-env-file key-extend mode)

      (and (= mode :service) (= (first args) "--freeze"))
      (recur (rest (rest args)) file env-vars artifacts out-dir network vcpu session-action session-id session-shell session-input-files
             :sleep (second args) service-name service-ports service-bootstrap service-bootstrap-file service-type service-input-files service-envs service-env-file key-extend mode)

      (and (= mode :service) (= (first args) "--unfreeze"))
      (recur (rest (rest args)) file env-vars artifacts out-dir network vcpu session-action session-id session-shell session-input-files
             :wake (second args) service-name service-ports service-bootstrap service-bootstrap-file service-type service-input-files service-envs service-env-file key-extend mode)

      (and (= mode :service) (= (first args) "--destroy"))
      (recur (rest (rest args)) file env-vars artifacts out-dir network vcpu session-action session-id session-shell session-input-files
             :destroy (second args) service-name service-ports service-bootstrap service-bootstrap-file service-type service-input-files service-envs service-env-file key-extend mode)

      (and (= mode :service) (= (first args) "--resize"))
      (recur (rest (rest args)) file env-vars artifacts out-dir network vcpu session-action session-id session-shell session-input-files
             :resize (second args) service-name service-ports service-bootstrap service-bootstrap-file service-type service-input-files service-envs service-env-file key-extend mode)

      (and (= mode :service) (= (first args) "--execute"))
      (recur (rest (rest (rest args))) file env-vars artifacts out-dir network vcpu session-action session-id session-shell session-input-files
             :execute (second args) service-name service-ports (nth args 2) service-bootstrap-file service-type service-input-files service-envs service-env-file key-extend mode)

      (and (= mode :service) (= (first args) "--dump-bootstrap") (>= (count args) 3) (not (.startsWith (nth args 2) "-")))
      (recur (rest (rest (rest args))) file env-vars artifacts out-dir network vcpu session-action session-id session-shell session-input-files
             :dump-bootstrap (second args) service-name service-ports service-bootstrap (nth args 2) service-type service-input-files service-envs service-env-file key-extend mode)

      (and (= mode :service) (= (first args) "--dump-bootstrap"))
      (recur (rest (rest args)) file env-vars artifacts out-dir network vcpu session-action session-id session-shell session-input-files
             :dump-bootstrap (second args) service-name service-ports service-bootstrap service-bootstrap-file service-type service-input-files service-envs service-env-file key-extend mode)

      (and (= mode :service) (= (first args) "--name"))
      (recur (rest (rest args)) file env-vars artifacts out-dir network vcpu session-action session-id session-shell session-input-files
             :create service-id (second args) service-ports service-bootstrap service-bootstrap-file service-type service-input-files service-envs service-env-file key-extend mode)

      (and (= mode :service) (= (first args) "--ports"))
      (recur (rest (rest args)) file env-vars artifacts out-dir network vcpu session-action session-id session-shell session-input-files
             service-action service-id service-name (second args) service-bootstrap service-bootstrap-file service-type service-input-files service-envs service-env-file key-extend mode)

      (and (= mode :service) (= (first args) "--bootstrap"))
      (recur (rest (rest args)) file env-vars artifacts out-dir network vcpu session-action session-id session-shell session-input-files
             service-action service-id service-name service-ports (second args) service-bootstrap-file service-type service-input-files service-envs service-env-file key-extend mode)

      (and (= mode :service) (= (first args) "--bootstrap-file"))
      (recur (rest (rest args)) file env-vars artifacts out-dir network vcpu session-action session-id session-shell session-input-files
             service-action service-id service-name service-ports service-bootstrap (second args) service-type service-input-files service-envs service-env-file key-extend mode)

      (and (= mode :service) (= (first args) "--type"))
      (recur (rest (rest args)) file env-vars artifacts out-dir network vcpu session-action session-id session-shell session-input-files
             service-action service-id service-name service-ports service-bootstrap service-bootstrap-file (second args) service-input-files service-envs service-env-file key-extend mode)

      (and (= mode :service) (= (first args) "-f"))
      (recur (rest (rest args)) file env-vars artifacts out-dir network vcpu session-action session-id session-shell session-input-files
             service-action service-id service-name service-ports service-bootstrap service-bootstrap-file service-type (conj service-input-files (second args)) service-envs service-env-file key-extend mode)

      (and (= mode :service) (= (first args) "env") (>= (count args) 2))
      (let [env-action (second args)
            env-target (when (and (>= (count args) 3) (not (.startsWith (nth args 2) "-"))) (nth args 2))
            rest-args (if env-target (drop 3 args) (drop 2 args))]
        (recur rest-args file env-vars artifacts out-dir network vcpu session-action session-id session-shell session-input-files
               :env env-action env-target service-ports service-bootstrap service-bootstrap-file service-type service-input-files service-envs service-env-file key-extend mode))

      (and (= mode :service) (= (first args) "-e"))
      (recur (rest (rest args)) file env-vars artifacts out-dir network vcpu session-action session-id session-shell session-input-files
             service-action service-id service-name service-ports service-bootstrap service-bootstrap-file service-type service-input-files (conj service-envs (second args)) service-env-file key-extend mode)

      (and (= mode :service) (= (first args) "--env-file"))
      (recur (rest (rest args)) file env-vars artifacts out-dir network vcpu session-action session-id session-shell session-input-files
             service-action service-id service-name service-ports service-bootstrap service-bootstrap-file service-type service-input-files service-envs (second args) key-extend mode)

      ;; Execute options
      (= (first args) "-e")
      (let [[k v] (str/split (second args) #"=" 2)]
        (recur (rest (rest args)) file (conj env-vars [k v]) artifacts out-dir network vcpu
               session-action session-id session-shell session-input-files service-action service-id service-name service-ports service-bootstrap service-bootstrap-file service-type service-input-files service-envs service-env-file key-extend mode))

      (= (first args) "-a")
      (recur (rest args) file env-vars true out-dir network vcpu session-action session-id session-shell session-input-files
             service-action service-id service-name service-ports service-bootstrap service-bootstrap-file service-type service-input-files service-envs service-env-file key-extend mode)

      (= (first args) "-o")
      (recur (rest (rest args)) file env-vars artifacts (second args) network vcpu session-action session-id session-shell session-input-files
             service-action service-id service-name service-ports service-bootstrap service-bootstrap-file service-type service-input-files service-envs service-env-file key-extend mode)

      (= (first args) "-n")
      (recur (rest (rest args)) file env-vars artifacts out-dir (second args) vcpu session-action session-id session-shell session-input-files
             service-action service-id service-name service-ports service-bootstrap service-bootstrap-file service-type service-input-files service-envs service-env-file key-extend mode)

      (= (first args) "-v")
      (recur (rest (rest args)) file env-vars artifacts out-dir network (Integer/parseInt (second args)) session-action session-id session-shell session-input-files
             service-action service-id service-name service-ports service-bootstrap service-bootstrap-file service-type service-input-files service-envs service-env-file key-extend mode)

      ;; Source file
      (and (= mode :execute) (not (.startsWith (first args) "-")) (nil? file))
      (recur (rest args) (first args) env-vars artifacts out-dir network vcpu session-action session-id session-shell session-input-files
             service-action service-id service-name service-ports service-bootstrap service-bootstrap-file service-type service-input-files service-envs service-env-file key-extend mode)

      ;; Unknown option check
      (and (= mode :session) (.startsWith (first args) "-"))
      (do
        (println (str red "Unknown option: " (first args) reset) *err*)
        (println "Usage: un.clj session [options]")
        (println "Options: --list, --kill ID, --shell SHELL, -s SHELL, -f FILE, -n NETWORK, -v VCPU")
        (System/exit 1))

      :else
      (recur (rest args) file env-vars artifacts out-dir network vcpu session-action session-id session-shell session-input-files
             service-action service-id service-name service-ports service-bootstrap service-bootstrap-file service-type service-input-files service-envs service-env-file key-extend mode))))

(parse-args *command-line-args*)
