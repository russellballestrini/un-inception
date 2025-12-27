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

(defn extract-field [field json-str]
  (let [pattern-str (re-pattern (str "\"" field "\":\"([^\"]*)\""))
        pattern-num (re-pattern (str "\"" field "\":(\\d+)"))]
    (or (second (re-find pattern-str json-str))
        (second (re-find pattern-num json-str)))))

(defn get-api-key []
  (or (System/getenv "UNSANDBOX_API_KEY")
      (do (binding [*out* *err*]
            (println "Error: UNSANDBOX_API_KEY not set"))
          (System/exit 1))))

(defn curl-post [api-key endpoint json-data]
  (let [tmp-file (str "/tmp/un_clj_" (rand-int 999999) ".json")]
    (spit tmp-file json-data)
    (let [{:keys [out]} (sh "curl" "-s" "-X" "POST"
                            (str "https://api.unsandbox.com" endpoint)
                            "-H" "Content-Type: application/json"
                            "-H" (str "Authorization: Bearer " api-key)
                            "-d" (str "@" tmp-file))]
      (io/delete-file tmp-file true)
      out)))

(defn curl-get [api-key endpoint]
  (:out (sh "curl" "-s"
            (str "https://api.unsandbox.com" endpoint)
            "-H" (str "Authorization: Bearer " api-key))))

(defn curl-delete [api-key endpoint]
  (:out (sh "curl" "-s" "-X" "DELETE"
            (str "https://api.unsandbox.com" endpoint)
            "-H" (str "Authorization: Bearer " api-key))))

(defn curl-portal-post [api-key endpoint json-data]
  (let [tmp-file (str "/tmp/un_clj_portal_" (rand-int 999999) ".json")]
    (spit tmp-file json-data)
    (let [{:keys [out]} (sh "curl" "-s" "-X" "POST"
                            (str portal-base endpoint)
                            "-H" "Content-Type: application/json"
                            "-H" (str "Authorization: Bearer " api-key)
                            "-d" (str "@" tmp-file))]
      (io/delete-file tmp-file true)
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

(defn session-command [action sid shell network vcpu]
  (let [api-key (get-api-key)]
    (case action
      :list (println (curl-get api-key "/sessions"))
      :kill (do
              (curl-delete api-key (str "/sessions/" sid))
              (println (str green "Session terminated: " sid reset)))
      :create (let [sh (or shell "bash")
                    network-json (if network (str ",\"network\":\"" network "\"") "")
                    vcpu-json (if vcpu (str ",\"vcpu\":" vcpu) "")
                    json (str "{\"shell\":\"" sh "\"" network-json vcpu-json "}")]
                (println (str yellow "Session created (WebSocket required)" reset))
                (println (curl-post api-key "/sessions" json))))))

(defn service-command [action sid name ports bootstrap service-type network vcpu]
  (let [api-key (get-api-key)]
    (case action
      :list (println (curl-get api-key "/services"))
      :info (println (curl-get api-key (str "/services/" sid)))
      :logs (println (curl-get api-key (str "/services/" sid "/logs")))
      :sleep (do
               (curl-post api-key (str "/services/" sid "/sleep") "{}")
               (println (str green "Service sleeping: " sid reset)))
      :wake (do
              (curl-post api-key (str "/services/" sid "/wake") "{}")
              (println (str green "Service waking: " sid reset)))
      :destroy (do
                 (curl-delete api-key (str "/services/" sid))
                 (println (str green "Service destroyed: " sid reset)))
      :create (when name
                (let [ports-json (if ports (str ",\"ports\":[" ports "]") "")
                      bootstrap-json (if bootstrap (str ",\"bootstrap\":\"" (escape-json bootstrap) "\"") "")
                      service-type-json (if service-type (str ",\"service_type\":\"" service-type "\"") "")
                      network-json (if network (str ",\"network\":\"" network "\"") "")
                      vcpu-json (if vcpu (str ",\"vcpu\":" vcpu) "")
                      json (str "{\"name\":\"" name "\"" ports-json bootstrap-json service-type-json network-json vcpu-json "}")]
                  (println (str green "Service created" reset))
                  (println (curl-post api-key "/services" json)))))))

(defn validate-key [api-key extend?]
  (let [response (curl-portal-post api-key "/keys/validate" "{}")
        status (extract-field "status" response)
        public-key (extract-field "public_key" response)
        tier (extract-field "tier" response)
        expires-at (extract-field "expires_at" response)]
    (cond
      (= status "valid")
      (do
        (println (str green "Valid" reset))
        (when public-key (println (str "Public Key: " public-key)))
        (when tier (println (str "Tier: " tier)))
        (when expires-at (println (str "Expires: " expires-at)))
        (when extend?
          (let [url (str portal-base "/keys/extend?pk=" public-key)]
            (sh "xdg-open" url))))

      (= status "expired")
      (do
        (println (str red "Expired" reset))
        (when public-key (println (str "Public Key: " public-key)))
        (when tier (println (str "Tier: " tier)))
        (when expires-at (println (str "Expired: " expires-at)))
        (println (str yellow "To renew: Visit " portal-base "/keys/extend" reset))
        (when extend?
          (let [url (str portal-base "/keys/extend?pk=" public-key)]
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
         service-action nil
         service-id nil
         service-name nil
         service-ports nil
         service-bootstrap nil
         service-type nil
         key-extend false
         mode :execute]
    (cond
      (empty? args)
      (case mode
        :session (session-command (or session-action :create) session-id session-shell network vcpu)
        :service (service-command (or service-action :create) service-id service-name service-ports service-bootstrap service-type network vcpu)
        :key (key-command key-extend)
        :execute (if file
                   (execute-command file env-vars artifacts out-dir network vcpu)
                   (do (println "Usage: un.clj [options] <source_file>")
                       (println "       un.clj session [options]")
                       (println "       un.clj service [options]")
                       (println "       un.clj key [options]")
                       (System/exit 1))))

      (= (first args) "session")
      (recur (rest args) file env-vars artifacts out-dir network vcpu session-action session-id session-shell
             service-action service-id service-name service-ports service-bootstrap service-type key-extend :session)

      (= (first args) "service")
      (recur (rest args) file env-vars artifacts out-dir network vcpu session-action session-id session-shell
             service-action service-id service-name service-ports service-bootstrap service-type key-extend :service)

      (= (first args) "key")
      (recur (rest args) file env-vars artifacts out-dir network vcpu session-action session-id session-shell
             service-action service-id service-name service-ports service-bootstrap service-type key-extend :key)

      ;; Key options
      (and (= mode :key) (= (first args) "--extend"))
      (recur (rest args) file env-vars artifacts out-dir network vcpu session-action session-id session-shell
             service-action service-id service-name service-ports service-bootstrap service-type true mode)

      ;; Session options
      (and (= mode :session) (= (first args) "--list"))
      (recur (rest args) file env-vars artifacts out-dir network vcpu :list session-id session-shell
             service-action service-id service-name service-ports service-bootstrap service-type key-extend mode)

      (and (= mode :session) (= (first args) "--kill"))
      (recur (rest (rest args)) file env-vars artifacts out-dir network vcpu :kill (second args) session-shell
             service-action service-id service-name service-ports service-bootstrap service-type key-extend mode)

      (and (= mode :session) (or (= (first args) "--shell") (= (first args) "-s")))
      (recur (rest (rest args)) file env-vars artifacts out-dir network vcpu session-action session-id (second args)
             service-action service-id service-name service-ports service-bootstrap service-type key-extend mode)

      ;; Service options
      (and (= mode :service) (= (first args) "--list"))
      (recur (rest args) file env-vars artifacts out-dir network vcpu session-action session-id session-shell
             :list service-id service-name service-ports service-bootstrap service-type key-extend mode)

      (and (= mode :service) (= (first args) "--info"))
      (recur (rest (rest args)) file env-vars artifacts out-dir network vcpu session-action session-id session-shell
             :info (second args) service-name service-ports service-bootstrap service-type key-extend mode)

      (and (= mode :service) (= (first args) "--logs"))
      (recur (rest (rest args)) file env-vars artifacts out-dir network vcpu session-action session-id session-shell
             :logs (second args) service-name service-ports service-bootstrap service-type key-extend mode)

      (and (= mode :service) (= (first args) "--sleep"))
      (recur (rest (rest args)) file env-vars artifacts out-dir network vcpu session-action session-id session-shell
             :sleep (second args) service-name service-ports service-bootstrap service-type key-extend mode)

      (and (= mode :service) (= (first args) "--wake"))
      (recur (rest (rest args)) file env-vars artifacts out-dir network vcpu session-action session-id session-shell
             :wake (second args) service-name service-ports service-bootstrap service-type key-extend mode)

      (and (= mode :service) (= (first args) "--destroy"))
      (recur (rest (rest args)) file env-vars artifacts out-dir network vcpu session-action session-id session-shell
             :destroy (second args) service-name service-ports service-bootstrap service-type key-extend mode)

      (and (= mode :service) (= (first args) "--name"))
      (recur (rest (rest args)) file env-vars artifacts out-dir network vcpu session-action session-id session-shell
             :create service-id (second args) service-ports service-bootstrap service-type key-extend mode)

      (and (= mode :service) (= (first args) "--ports"))
      (recur (rest (rest args)) file env-vars artifacts out-dir network vcpu session-action session-id session-shell
             service-action service-id service-name (second args) service-bootstrap service-type key-extend mode)

      (and (= mode :service) (= (first args) "--bootstrap"))
      (recur (rest (rest args)) file env-vars artifacts out-dir network vcpu session-action session-id session-shell
             service-action service-id service-name service-ports (second args) service-type key-extend mode)

      (and (= mode :service) (= (first args) "--type"))
      (recur (rest (rest args)) file env-vars artifacts out-dir network vcpu session-action session-id session-shell
             service-action service-id service-name service-ports service-bootstrap (second args) key-extend mode)

      ;; Execute options
      (= (first args) "-e")
      (let [[k v] (str/split (second args) #"=" 2)]
        (recur (rest (rest args)) file (conj env-vars [k v]) artifacts out-dir network vcpu
               session-action session-id session-shell service-action service-id service-name service-ports service-bootstrap service-type key-extend mode))

      (= (first args) "-a")
      (recur (rest args) file env-vars true out-dir network vcpu session-action session-id session-shell
             service-action service-id service-name service-ports service-bootstrap service-type key-extend mode)

      (= (first args) "-o")
      (recur (rest (rest args)) file env-vars artifacts (second args) network vcpu session-action session-id session-shell
             service-action service-id service-name service-ports service-bootstrap service-type key-extend mode)

      (= (first args) "-n")
      (recur (rest (rest args)) file env-vars artifacts out-dir (second args) vcpu session-action session-id session-shell
             service-action service-id service-name service-ports service-bootstrap service-type key-extend mode)

      (= (first args) "-v")
      (recur (rest (rest args)) file env-vars artifacts out-dir network (Integer/parseInt (second args)) session-action session-id session-shell
             service-action service-id service-name service-ports service-bootstrap service-type key-extend mode)

      ;; Source file
      (and (= mode :execute) (not (.startsWith (first args) "-")) (nil? file))
      (recur (rest args) (first args) env-vars artifacts out-dir network vcpu session-action session-id session-shell
             service-action service-id service-name service-ports service-bootstrap service-type key-extend mode)

      :else
      (recur (rest args) file env-vars artifacts out-dir network vcpu session-action session-id session-shell
             service-action service-id service-name service-ports service-bootstrap service-type key-extend mode))))

(parse-args *command-line-args*)
