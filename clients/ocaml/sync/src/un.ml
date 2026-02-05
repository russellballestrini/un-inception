(* PUBLIC DOMAIN - NO LICENSE, NO WARRANTY
 *
 * This is free public domain software for the public good of a permacomputer hosted
 * at permacomputer.com - an always-on computer by the people, for the people. One
 * which is durable, easy to repair, and distributed like tap water for machine
 * learning intelligence.
 *
 * The permacomputer is community-owned infrastructure optimized around four values:
 *
 *   TRUTH    - First principles, math & science, open source code freely distributed
 *   FREEDOM  - Voluntary partnerships, freedom from tyranny & corporate control
 *   HARMONY  - Minimal waste, self-renewing systems with diverse thriving connections
 *   LOVE     - Be yourself without hurting others, cooperation through natural law
 *
 * This software contributes to that vision by enabling code execution across 42+
 * programming languages through a unified interface, accessible to all. Code is
 * seeds to sprout on any abandoned technology.
 *
 * Learn more: https://www.permacomputer.com
 *
 * Anyone is free to copy, modify, publish, use, compile, sell, or distribute this
 * software, either in source code form or as a compiled binary, for any purpose,
 * commercial or non-commercial, and by any means.
 *
 * NO WARRANTY. THE SOFTWARE IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY KIND.
 *
 * That said, our permacomputer's digital membrane stratum continuously runs unit,
 * integration, and functional tests on all of it's own software - with our
 * permacomputer monitoring itself, repairing itself, with minimal human in the
 * loop guidance. Our agents do their best.
 *
 * Copyright 2025 TimeHexOn & foxhop & russell@unturf
 * https://www.timehexon.com
 * https://www.foxhop.net
 * https://www.unturf.com/software
 *)

(** {1 unsandbox OCaml SDK}

    Secure code execution in sandboxed containers.

    {2 Library Usage}
    {[
      (* Simple execution *)
      let result = Un.execute "python" "print('Hello World')" () in
      print_endline result.stdout

      (* Using Client for stored credentials *)
      let client = Un.Client.create ~public_key:"unsb-pk-..." ~secret_key:"unsb-sk-..." () in
      let result = Un.Client.execute client "python" code in
      print_endline result.stdout

      (* Async execution *)
      let job = Un.execute_async "python" long_code () in
      let result = Un.wait job.job_id () in
      print_endline result.stdout
    ]}

    {2 CLI Usage}
    {[
      chmod +x un.ml
      ./un.ml script.py
      ./un.ml session --shell python3
      ./un.ml service --name web --ports 80
    ]}

    {2 Authentication}
    Credentials are loaded in priority order:
    + Function arguments (public_key, secret_key)
    + Environment variables (UNSANDBOX_PUBLIC_KEY, UNSANDBOX_SECRET_KEY)
    + Config file (~/.unsandbox/accounts.csv)
*)

#!/usr/bin/env ocaml

(* ============================================================================
   Configuration
   ============================================================================ *)

(** API base URL *)
let api_base = "https://api.unsandbox.com"

(** Portal base URL *)
let portal_base = "https://unsandbox.com"

(** Default execution timeout in seconds *)
let default_timeout = 300

(** Default TTL for code execution *)
let default_ttl = 60

(** Languages cache TTL in seconds (1 hour) *)
let languages_cache_ttl = 3600

(* ANSI colors *)
let blue = "\x1b[34m"
let red = "\x1b[31m"
let green = "\x1b[32m"
let yellow = "\x1b[33m"
let reset = "\x1b[0m"

(* ============================================================================
   Types
   ============================================================================ *)

(** Execution options for API calls *)
type exec_options = {
  env: (string * string) list;       (** Environment variables *)
  input_files: string list;          (** Input file paths *)
  network_mode: string;              (** "zerotrust" or "semitrusted" *)
  ttl: int;                          (** Execution timeout in seconds *)
  vcpu: int;                         (** vCPU count (1-8) *)
  return_artifacts: bool;            (** Return compiled artifacts *)
}

(** Default execution options *)
let default_exec_options = {
  env = [];
  input_files = [];
  network_mode = "zerotrust";
  ttl = default_ttl;
  vcpu = 1;
  return_artifacts = false;
}

(** Execution result *)
type exec_result = {
  success: bool;
  stdout: string;
  stderr: string;
  exit_code: int;
  job_id: string option;
}

(** Job status *)
type job_status = {
  job_id: string;
  status: string;  (** "pending", "running", "completed", "failed", "timeout", "cancelled" *)
  result: exec_result option;
}

(** Language info *)
type language_info = {
  name: string;
  version: string;
  aliases: string list;
}

(* ============================================================================
   Utility Functions
   ============================================================================ *)

(** Extension to language mapping *)
let ext_to_lang ext =
  match ext with
  | ".hs" -> Some "haskell" | ".ml" -> Some "ocaml" | ".clj" -> Some "clojure"
  | ".scm" -> Some "scheme" | ".lisp" -> Some "commonlisp" | ".erl" -> Some "erlang"
  | ".ex" -> Some "elixir" | ".exs" -> Some "elixir" | ".py" -> Some "python"
  | ".js" -> Some "javascript" | ".ts" -> Some "typescript" | ".rb" -> Some "ruby"
  | ".go" -> Some "go" | ".rs" -> Some "rust" | ".c" -> Some "c"
  | ".cpp" -> Some "cpp" | ".cc" -> Some "cpp" | ".cxx" -> Some "cpp"
  | ".java" -> Some "java" | ".kt" -> Some "kotlin" | ".cs" -> Some "csharp"
  | ".fs" -> Some "fsharp" | ".jl" -> Some "julia" | ".r" -> Some "r"
  | ".cr" -> Some "crystal" | ".d" -> Some "d" | ".nim" -> Some "nim"
  | ".zig" -> Some "zig" | ".v" -> Some "v" | ".dart" -> Some "dart"
  | ".groovy" -> Some "groovy" | ".scala" -> Some "scala"
  | ".sh" -> Some "bash" | ".pl" -> Some "perl" | ".lua" -> Some "lua"
  | ".php" -> Some "php"
  | _ -> None

(** Read file contents *)
let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

(** Base64 encode a file using shell command *)
let base64_encode_file filename =
  let cmd = Printf.sprintf "base64 -w0 %s" (Filename.quote filename) in
  let ic = Unix.open_process_in cmd in
  let result = try input_line ic with End_of_file -> "" in
  let _ = Unix.close_process_in ic in
  String.trim result

(** Build input_files JSON from list of filenames *)
let build_input_files_json files =
  if files = [] then ""
  else
    let entries = List.map (fun f ->
      let basename = Filename.basename f in
      let content = base64_encode_file f in
      Printf.sprintf "{\"filename\":\"%s\",\"content\":\"%s\"}" basename content
    ) files in
    ",\"input_files\":[" ^ (String.concat "," entries) ^ "]"

(** Get file extension *)
let get_extension filename =
  try
    let dot_pos = String.rindex filename '.' in
    String.sub filename dot_pos (String.length filename - dot_pos)
  with Not_found -> ""

(** Get languages cache file path *)
let get_languages_cache_path () =
  let home = try Sys.getenv "HOME" with Not_found -> "." in
  Filename.concat home ".unsandbox/languages.json"

(** Load languages from cache if valid *)
let load_languages_cache () =
  let cache_path = get_languages_cache_path () in
  if Sys.file_exists cache_path then
    try
      let content = read_file cache_path in
      let timestamp = extract_json_int content "timestamp" in
      match timestamp with
      | Some ts ->
        let now = int_of_float (Unix.time ()) in
        if now - ts < languages_cache_ttl then
          Some content
        else
          None
      | None -> None
    with _ -> None
  else
    None

(** Save languages to cache *)
let save_languages_cache languages_json =
  let cache_path = get_languages_cache_path () in
  let cache_dir = Filename.dirname cache_path in
  (try Unix.mkdir cache_dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let timestamp = int_of_float (Unix.time ()) in
  let cache_content = Printf.sprintf "{\"languages\":%s,\"timestamp\":%d}" languages_json timestamp in
  let oc = open_out cache_path in
  output_string oc cache_content;
  close_out oc

(** Escape JSON string *)
let escape_json s =
  let buf = Buffer.create (String.length s) in
  String.iter (fun c ->
    match c with
    | '\\' -> Buffer.add_string buf "\\\\"
    | '"' -> Buffer.add_string buf "\\\""
    | '\n' -> Buffer.add_string buf "\\n"
    | '\r' -> Buffer.add_string buf "\\r"
    | '\t' -> Buffer.add_string buf "\\t"
    | _ -> Buffer.add_char buf c
  ) s;
  Buffer.contents buf

(** Unescape JSON string *)
let unescape_json s =
  let s = Str.global_replace (Str.regexp "\\\\n") "\n" s in
  let s = Str.global_replace (Str.regexp "\\\\t") "\t" s in
  let s = Str.global_replace (Str.regexp "\\\\\"") "\"" s in
  let s = Str.global_replace (Str.regexp "\\\\\\\\") "\\" s in
  s

(** Extract JSON value - simple regex-based parser *)
let extract_json_value json_str key =
  let pattern = "\"" ^ key ^ "\"\\s*:\\s*\"\\([^\"]*\\)\"" in
  let regex = Str.regexp pattern in
  try
    let _ = Str.search_forward regex json_str 0 in
    Some (Str.matched_group 1 json_str)
  with Not_found -> None

(** Extract JSON integer value *)
let extract_json_int json_str key =
  let pattern = "\"" ^ key ^ "\"\\s*:\\s*\\([0-9]+\\)" in
  let regex = Str.regexp pattern in
  try
    let _ = Str.search_forward regex json_str 0 in
    Some (int_of_string (Str.matched_group 1 json_str))
  with Not_found -> None

(* ============================================================================
   Credentials Management
   ============================================================================ *)

(** Get credentials from config file ~/.unsandbox/accounts.csv *)
let get_credentials_from_file ?(account_index=0) () =
  let home = try Sys.getenv "HOME" with Not_found -> "." in
  let accounts_path = Filename.concat home ".unsandbox/accounts.csv" in
  if Sys.file_exists accounts_path then
    try
      let content = read_file accounts_path in
      let lines = String.split_on_char '\n' content in
      let valid_accounts = List.filter_map (fun line ->
        let line = String.trim line in
        if String.length line = 0 || line.[0] = '#' then None
        else
          try
            let comma_pos = String.index line ',' in
            let pk = String.sub line 0 comma_pos in
            let sk = String.sub line (comma_pos + 1) (String.length line - comma_pos - 1) in
            if String.length pk > 8 && String.sub pk 0 8 = "unsb-pk-" &&
               String.length sk > 8 && String.sub sk 0 8 = "unsb-sk-" then
              Some (pk, sk)
            else None
          with Not_found -> None
      ) lines in
      if account_index < List.length valid_accounts then
        Some (List.nth valid_accounts account_index)
      else None
    with _ -> None
  else None

(**
   Get API credentials in priority order:
   1. Function arguments
   2. Environment variables
   3. ~/.unsandbox/accounts.csv

   @param public_key Optional public key override
   @param secret_key Optional secret key override
   @param account_index Account index in config file (default 0)
   @return (public_key, secret_key) tuple
   @raise Failure if no credentials found
*)
let get_credentials ?public_key ?secret_key ?(account_index=0) () =
  (* Priority 1: Function arguments *)
  match (public_key, secret_key) with
  | (Some pk, Some sk) -> (pk, sk)
  | _ ->
    (* Priority 2: Environment variables *)
    let env_pk = try Some (Sys.getenv "UNSANDBOX_PUBLIC_KEY") with Not_found -> None in
    let env_sk = try Some (Sys.getenv "UNSANDBOX_SECRET_KEY") with Not_found -> None in
    match (env_pk, env_sk) with
    | (Some pk, Some sk) -> (pk, sk)
    | _ ->
      (* Priority 3: Config file *)
      match get_credentials_from_file ~account_index () with
      | Some (pk, sk) -> (pk, sk)
      | None ->
        failwith "No credentials found. Set UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY, \
                  or create ~/.unsandbox/accounts.csv, or pass credentials to function."

(* Legacy function for backward compatibility *)
let get_api_keys () =
  try
    get_credentials ()
  with Failure _ ->
    (* Fall back to old API key for backwards compat *)
    let api_key = try Some (Sys.getenv "UNSANDBOX_API_KEY") with Not_found -> None in
    match api_key with
    | Some ak -> (ak, ak)  (* Use same key for both in legacy mode *)
    | None ->
      Printf.fprintf stderr "Error: UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY not set\n";
      exit 1

let get_api_key () =
  let (public_key, _) = get_api_keys () in
  public_key

(* ============================================================================
   HMAC Authentication
   ============================================================================ *)

(** HMAC-SHA256 using openssl command *)
let hmac_sha256 secret message =
  let cmd = Printf.sprintf "echo -n '%s' | openssl dgst -sha256 -hmac '%s' | awk '{print $2}'"
    (Str.global_replace (Str.regexp "'") "'\\''" message)
    (Str.global_replace (Str.regexp "'") "'\\''" secret) in
  let ic = Unix.open_process_in cmd in
  let result = input_line ic in
  let _ = Unix.close_process_in ic in
  String.trim result

(**
   Generate HMAC-SHA256 signature for API request.

   Signature = HMAC-SHA256(secret_key, "timestamp:METHOD:path:body")
*)
let make_signature secret_key timestamp method_ path body =
  let message = Printf.sprintf "%s:%s:%s:%s" timestamp method_ path body in
  hmac_sha256 secret_key message

(** Build authentication headers for HTTP request *)
let build_auth_headers public_key secret_key method_ path body =
  let timestamp = string_of_int (int_of_float (Unix.time ())) in
  let signature = make_signature secret_key timestamp method_ path body in
  Printf.sprintf " -H 'Authorization: Bearer %s' -H 'X-Timestamp: %s' -H 'X-Signature: %s'"
    public_key timestamp signature

(** Check for clock drift errors in API response *)
let check_clock_drift response =
  let response_lower = String.lowercase_ascii response in
  let contains_substring s sub =
    try
      let _ = Str.search_forward (Str.regexp_string sub) s 0 in
      true
    with Not_found -> false
  in
  let has_timestamp = contains_substring response_lower "timestamp" in
  let has_401 = contains_substring response_lower "401" in
  let has_expired = contains_substring response_lower "expired" in
  let has_invalid = contains_substring response_lower "invalid" in
  let has_error = has_401 || has_expired || has_invalid in

  if has_timestamp && has_error then begin
    Printf.fprintf stderr "%sError: Request timestamp expired (must be within 5 minutes of server time)%s\n" red reset;
    Printf.fprintf stderr "%sYour computer's clock may have drifted.\n" yellow;
    Printf.fprintf stderr "Check your system time and sync with NTP if needed:\n";
    Printf.fprintf stderr "  Linux:   sudo ntpdate -s time.nist.gov\n";
    Printf.fprintf stderr "  macOS:   sudo sntp -sS time.apple.com\n";
    Printf.fprintf stderr "  Windows: w32tm /resync%s\n" reset;
    exit 1
  end

(* ============================================================================
   HTTP Client
   ============================================================================ *)

(** Make authenticated POST request to API *)
let api_post ?public_key ?secret_key endpoint json =
  let (pk, sk) = get_credentials ?public_key ?secret_key () in
  let auth_headers = build_auth_headers pk sk "POST" endpoint json in
  let tmp_file = Printf.sprintf "/tmp/un_ocaml_%d.json" (Random.int 999999) in
  let oc = open_out tmp_file in
  output_string oc json;
  close_out oc;
  let cmd = Printf.sprintf "curl -s -X POST %s%s -H 'Content-Type: application/json'%s -d @%s"
    api_base endpoint auth_headers tmp_file in
  let ic = Unix.open_process_in cmd in
  let rec read_all acc =
    try let line = input_line ic in read_all (acc ^ line ^ "\n")
    with End_of_file -> acc
  in
  let output = read_all "" in
  let _ = Unix.close_process_in ic in
  Sys.remove tmp_file;
  check_clock_drift output;
  output

(** Make authenticated GET request to API *)
let api_get ?public_key ?secret_key endpoint =
  let (pk, sk) = get_credentials ?public_key ?secret_key () in
  let auth_headers = build_auth_headers pk sk "GET" endpoint "" in
  let cmd = Printf.sprintf "curl -s %s%s%s" api_base endpoint auth_headers in
  let ic = Unix.open_process_in cmd in
  let rec read_all acc =
    try let line = input_line ic in read_all (acc ^ line ^ "\n")
    with End_of_file -> acc
  in
  let output = read_all "" in
  let _ = Unix.close_process_in ic in
  check_clock_drift output;
  output

(** Make authenticated DELETE request to API *)
let api_delete ?public_key ?secret_key endpoint =
  let (pk, sk) = get_credentials ?public_key ?secret_key () in
  let auth_headers = build_auth_headers pk sk "DELETE" endpoint "" in
  let cmd = Printf.sprintf "curl -s -X DELETE %s%s%s" api_base endpoint auth_headers in
  let ic = Unix.open_process_in cmd in
  let rec read_all acc =
    try let line = input_line ic in read_all (acc ^ line ^ "\n")
    with End_of_file -> acc
  in
  let output = read_all "" in
  let _ = Unix.close_process_in ic in
  check_clock_drift output;
  output

(** Result type for sudo challenge operations *)
type sudo_result = SudoSuccess of string | SudoError of string | SudoCancelled

(** Handle 428 sudo OTP challenge - prompts user for OTP and retries the request *)
let handle_sudo_challenge response method_ endpoint body =
  let challenge_id = extract_json_value response "challenge_id" in

  Printf.fprintf stderr "%sConfirmation required. Check your email for a one-time code.%s\n" yellow reset;
  Printf.fprintf stderr "Enter OTP: ";
  flush stderr;

  let otp_raw = try input_line stdin with End_of_file -> "" in
  let otp = String.trim otp_raw in

  if otp = "" then begin
    Printf.fprintf stderr "%sError: Operation cancelled%s\n" red reset;
    SudoCancelled
  end else begin
    (* Retry the request with sudo headers *)
    let (pk, sk) = get_credentials () in
    let auth_headers = build_auth_headers pk sk method_ endpoint body in

    let sudo_headers = Printf.sprintf " -H 'X-Sudo-OTP: %s'" otp in
    let challenge_header = match challenge_id with
      | Some cid -> Printf.sprintf " -H 'X-Sudo-Challenge: %s'" cid
      | None -> ""
    in

    let cmd = match method_ with
      | "DELETE" ->
        Printf.sprintf "curl -s -X DELETE %s%s%s%s%s" api_base endpoint auth_headers sudo_headers challenge_header
      | "POST" ->
        let tmp_file = Printf.sprintf "/tmp/un_ocaml_%d.json" (Random.int 999999) in
        let oc = open_out tmp_file in
        output_string oc body;
        close_out oc;
        let result = Printf.sprintf "curl -s -X POST %s%s -H 'Content-Type: application/json'%s%s%s -d @%s"
          api_base endpoint auth_headers sudo_headers challenge_header tmp_file in
        result
      | _ ->
        Printf.sprintf "curl -s %s%s%s%s%s" api_base endpoint auth_headers sudo_headers challenge_header
    in

    let ic = Unix.open_process_in cmd in
    let rec read_all acc =
      try let line = input_line ic in read_all (acc ^ line ^ "\n")
      with End_of_file -> acc
    in
    let retry_output = read_all "" in
    let _ = Unix.close_process_in ic in

    (* Clean up temp file for POST *)
    (if method_ = "POST" then
      try Sys.remove (Printf.sprintf "/tmp/un_ocaml_%d.json" (Random.int 999999)) with _ -> ());

    let contains_error s =
      try let _ = Str.search_forward (Str.regexp_string "\"error\"") s 0 in true
      with Not_found -> false
    in

    if not (contains_error retry_output) then
      SudoSuccess retry_output
    else
      SudoError retry_output
  end

(** Make authenticated DELETE request with 428 handling *)
let api_delete_with_sudo ?public_key ?secret_key endpoint =
  let (pk, sk) = get_credentials ?public_key ?secret_key () in
  let auth_headers = build_auth_headers pk sk "DELETE" endpoint "" in
  let cmd = Printf.sprintf "curl -s -w '\\n%%{http_code}' -X DELETE %s%s%s" api_base endpoint auth_headers in
  let ic = Unix.open_process_in cmd in
  let rec read_all acc =
    try let line = input_line ic in read_all (acc ^ line ^ "\n")
    with End_of_file -> acc
  in
  let output = read_all "" in
  let _ = Unix.close_process_in ic in

  (* Split response and status code *)
  let lines = String.split_on_char '\n' output in
  let lines_filtered = List.filter (fun s -> String.trim s <> "") lines in
  let (body_lines, status_lines) =
    let n = List.length lines_filtered in
    if n > 0 then
      (List.filteri (fun i _ -> i < n - 1) lines_filtered,
       [List.nth lines_filtered (n - 1)])
    else ([], [])
  in
  let body = String.concat "\n" body_lines in
  let http_code = match status_lines with
    | [s] -> (try int_of_string (String.trim s) with _ -> 200)
    | _ -> 200
  in

  check_clock_drift body;

  if http_code = 428 then
    handle_sudo_challenge body "DELETE" endpoint ""
  else
    SudoSuccess body

(** Make authenticated POST request with 428 handling *)
let api_post_with_sudo ?public_key ?secret_key endpoint json =
  let (pk, sk) = get_credentials ?public_key ?secret_key () in
  let auth_headers = build_auth_headers pk sk "POST" endpoint json in
  let tmp_file = Printf.sprintf "/tmp/un_ocaml_%d.json" (Random.int 999999) in
  let oc = open_out tmp_file in
  output_string oc json;
  close_out oc;
  let cmd = Printf.sprintf "curl -s -w '\\n%%{http_code}' -X POST %s%s -H 'Content-Type: application/json'%s -d @%s"
    api_base endpoint auth_headers tmp_file in
  let ic = Unix.open_process_in cmd in
  let rec read_all acc =
    try let line = input_line ic in read_all (acc ^ line ^ "\n")
    with End_of_file -> acc
  in
  let output = read_all "" in
  let _ = Unix.close_process_in ic in
  Sys.remove tmp_file;

  (* Split response and status code *)
  let lines = String.split_on_char '\n' output in
  let lines_filtered = List.filter (fun s -> String.trim s <> "") lines in
  let (body_lines, status_lines) =
    let n = List.length lines_filtered in
    if n > 0 then
      (List.filteri (fun i _ -> i < n - 1) lines_filtered,
       [List.nth lines_filtered (n - 1)])
    else ([], [])
  in
  let body = String.concat "\n" body_lines in
  let http_code = match status_lines with
    | [s] -> (try int_of_string (String.trim s) with _ -> 200)
    | _ -> 200
  in

  check_clock_drift body;

  if http_code = 428 then
    handle_sudo_challenge body "POST" endpoint json
  else
    SudoSuccess body

(** Make authenticated POST request to portal *)
let portal_post ?public_key ?secret_key endpoint json =
  let (pk, sk) = get_credentials ?public_key ?secret_key () in
  let auth_headers = build_auth_headers pk sk "POST" endpoint json in
  let tmp_file = Printf.sprintf "/tmp/un_ocaml_%d.json" (Random.int 999999) in
  let oc = open_out tmp_file in
  output_string oc json;
  close_out oc;
  let cmd = Printf.sprintf "curl -s -X POST %s%s -H 'Content-Type: application/json'%s -d @%s"
    portal_base endpoint auth_headers tmp_file in
  let ic = Unix.open_process_in cmd in
  let rec read_all acc =
    try let line = input_line ic in read_all (acc ^ line ^ "\n")
    with End_of_file -> acc
  in
  let output = read_all "" in
  let _ = Unix.close_process_in ic in
  Sys.remove tmp_file;
  check_clock_drift output;
  output

(* ============================================================================
   Library API - Core Execution Functions
   ============================================================================ *)

(**
   Execute code synchronously and return results.

   @param language Programming language (python, javascript, go, rust, etc.)
   @param code Source code to execute
   @param opts Execution options (optional)
   @param public_key API public key (optional if env vars set)
   @param secret_key API secret key (optional if env vars set)
   @return Execution result

   @example
   {[
     let result = execute "python" "print('Hello')" () in
     print_endline result.stdout
   ]}
*)
let execute ?public_key ?secret_key ?(opts=default_exec_options) language code =
  let env_json = if opts.env = [] then ""
    else ",\"env\":{" ^ (String.concat "," (List.map (fun (k, v) ->
      Printf.sprintf "\"%s\":\"%s\"" k (escape_json v)) opts.env)) ^ "}"
  in
  let input_files_json = build_input_files_json opts.input_files in
  let artifacts_json = if opts.return_artifacts then ",\"return_artifacts\":true" else "" in
  let network_json = Printf.sprintf ",\"network\":\"%s\"" opts.network_mode in
  let vcpu_json = Printf.sprintf ",\"vcpu\":%d" opts.vcpu in
  let ttl_json = Printf.sprintf ",\"ttl\":%d" opts.ttl in

  let json = Printf.sprintf "{\"language\":\"%s\",\"code\":\"%s\"%s%s%s%s%s%s}"
    language (escape_json code) env_json input_files_json artifacts_json network_json vcpu_json ttl_json in

  let response = api_post ?public_key ?secret_key "/execute" json in

  let stdout_val = match extract_json_value response "stdout" with Some s -> unescape_json s | None -> "" in
  let stderr_val = match extract_json_value response "stderr" with Some s -> unescape_json s | None -> "" in
  let exit_code = match extract_json_int response "exit_code" with Some i -> i | None -> 0 in
  let job_id = extract_json_value response "job_id" in

  { success = (exit_code = 0); stdout = stdout_val; stderr = stderr_val; exit_code; job_id }

(**
   Execute code asynchronously. Returns immediately with job_id for polling.

   @param language Programming language
   @param code Source code to execute
   @param opts Execution options (optional)
   @param public_key API public key (optional)
   @param secret_key API secret key (optional)
   @return Job status with job_id

   @example
   {[
     let job = execute_async "python" long_code () in
     let result = wait job.job_id () in
     print_endline result.stdout
   ]}
*)
let execute_async ?public_key ?secret_key ?(opts=default_exec_options) language code =
  let env_json = if opts.env = [] then ""
    else ",\"env\":{" ^ (String.concat "," (List.map (fun (k, v) ->
      Printf.sprintf "\"%s\":\"%s\"" k (escape_json v)) opts.env)) ^ "}"
  in
  let input_files_json = build_input_files_json opts.input_files in
  let artifacts_json = if opts.return_artifacts then ",\"return_artifacts\":true" else "" in
  let network_json = Printf.sprintf ",\"network\":\"%s\"" opts.network_mode in
  let vcpu_json = Printf.sprintf ",\"vcpu\":%d" opts.vcpu in
  let ttl_json = Printf.sprintf ",\"ttl\":%d" opts.ttl in

  let json = Printf.sprintf "{\"language\":\"%s\",\"code\":\"%s\"%s%s%s%s%s%s}"
    language (escape_json code) env_json input_files_json artifacts_json network_json vcpu_json ttl_json in

  let response = api_post ?public_key ?secret_key "/execute/async" json in

  let job_id = match extract_json_value response "job_id" with Some s -> s | None -> "" in
  let status = match extract_json_value response "status" with Some s -> s | None -> "pending" in

  { job_id; status; result = None }

(**
   Execute code with automatic language detection from shebang.

   @param code Source code with shebang (e.g., #!/usr/bin/env python3)
   @param opts Execution options (optional)
   @param public_key API public key (optional)
   @param secret_key API secret key (optional)
   @return Execution result
*)
let run ?public_key ?secret_key ?(opts=default_exec_options) code =
  let endpoint = Printf.sprintf "/run?ttl=%d&network_mode=%s" opts.ttl opts.network_mode in
  let (pk, sk) = get_credentials ?public_key ?secret_key () in
  let auth_headers = build_auth_headers pk sk "POST" endpoint code in
  let tmp_file = Printf.sprintf "/tmp/un_ocaml_%d.txt" (Random.int 999999) in
  let oc = open_out tmp_file in
  output_string oc code;
  close_out oc;
  let cmd = Printf.sprintf "curl -s -X POST %s%s -H 'Content-Type: text/plain'%s --data-binary @%s"
    api_base endpoint auth_headers tmp_file in
  let ic = Unix.open_process_in cmd in
  let rec read_all acc =
    try let line = input_line ic in read_all (acc ^ line ^ "\n")
    with End_of_file -> acc
  in
  let response = read_all "" in
  let _ = Unix.close_process_in ic in
  Sys.remove tmp_file;
  check_clock_drift response;

  let stdout_val = match extract_json_value response "stdout" with Some s -> unescape_json s | None -> "" in
  let stderr_val = match extract_json_value response "stderr" with Some s -> unescape_json s | None -> "" in
  let exit_code = match extract_json_int response "exit_code" with Some i -> i | None -> 0 in
  let job_id = extract_json_value response "job_id" in

  { success = (exit_code = 0); stdout = stdout_val; stderr = stderr_val; exit_code; job_id }

(**
   Execute code asynchronously with automatic language detection.

   @param code Source code with shebang
   @param opts Execution options (optional)
   @param public_key API public key (optional)
   @param secret_key API secret key (optional)
   @return Job status with job_id
*)
let run_async ?public_key ?secret_key ?(opts=default_exec_options) code =
  let endpoint = Printf.sprintf "/run/async?ttl=%d&network_mode=%s" opts.ttl opts.network_mode in
  let (pk, sk) = get_credentials ?public_key ?secret_key () in
  let auth_headers = build_auth_headers pk sk "POST" endpoint code in
  let tmp_file = Printf.sprintf "/tmp/un_ocaml_%d.txt" (Random.int 999999) in
  let oc = open_out tmp_file in
  output_string oc code;
  close_out oc;
  let cmd = Printf.sprintf "curl -s -X POST %s%s -H 'Content-Type: text/plain'%s --data-binary @%s"
    api_base endpoint auth_headers tmp_file in
  let ic = Unix.open_process_in cmd in
  let rec read_all acc =
    try let line = input_line ic in read_all (acc ^ line ^ "\n")
    with End_of_file -> acc
  in
  let response = read_all "" in
  let _ = Unix.close_process_in ic in
  Sys.remove tmp_file;
  check_clock_drift response;

  let job_id = match extract_json_value response "job_id" with Some s -> s | None -> "" in
  let status = match extract_json_value response "status" with Some s -> s | None -> "pending" in

  { job_id; status; result = None }

(* ============================================================================
   Library API - Job Management
   ============================================================================ *)

(** Polling delays (ms) - exponential backoff *)
let poll_delays = [|300; 450; 700; 900; 650; 1600; 2000|]

(**
   Get job status and results.

   @param job_id Job ID from execute_async or run_async
   @param public_key API public key (optional)
   @param secret_key API secret key (optional)
   @return Job status
*)
let get_job ?public_key ?secret_key job_id =
  let response = api_get ?public_key ?secret_key (Printf.sprintf "/jobs/%s" job_id) in

  let status = match extract_json_value response "status" with Some s -> s | None -> "unknown" in
  let result = if status = "completed" || status = "failed" then
    let stdout_val = match extract_json_value response "stdout" with Some s -> unescape_json s | None -> "" in
    let stderr_val = match extract_json_value response "stderr" with Some s -> unescape_json s | None -> "" in
    let exit_code = match extract_json_int response "exit_code" with Some i -> i | None -> 0 in
    Some { success = (exit_code = 0); stdout = stdout_val; stderr = stderr_val; exit_code; job_id = Some job_id }
  else None in

  { job_id; status; result }

(**
   Wait for job completion with exponential backoff polling.

   @param job_id Job ID from execute_async or run_async
   @param max_polls Maximum number of poll attempts (default 100)
   @param public_key API public key (optional)
   @param secret_key API secret key (optional)
   @return Final execution result
   @raise Failure if max polls exceeded or job failed
*)
let wait ?public_key ?secret_key ?(max_polls=100) job_id =
  let terminal_states = ["completed"; "failed"; "timeout"; "cancelled"] in

  let rec poll i =
    if i >= max_polls then
      failwith (Printf.sprintf "Max polls (%d) exceeded for job %s" max_polls job_id)
    else begin
      let delay_idx = min i (Array.length poll_delays - 1) in
      Unix.sleepf (float_of_int poll_delays.(delay_idx) /. 1000.0);

      let job = get_job ?public_key ?secret_key job_id in
      if List.mem job.status terminal_states then
        match job.result with
        | Some result -> result
        | None -> { success = false; stdout = ""; stderr = ""; exit_code = 1; job_id = Some job_id }
      else
        poll (i + 1)
    end
  in
  poll 0

(**
   Cancel a running job.

   @param job_id Job ID to cancel
   @param public_key API public key (optional)
   @param secret_key API secret key (optional)
   @return Partial result with output collected before cancellation
*)
let cancel_job ?public_key ?secret_key job_id =
  let response = api_delete ?public_key ?secret_key (Printf.sprintf "/jobs/%s" job_id) in

  let stdout_val = match extract_json_value response "stdout" with Some s -> unescape_json s | None -> "" in
  let stderr_val = match extract_json_value response "stderr" with Some s -> unescape_json s | None -> "" in
  let exit_code = match extract_json_int response "exit_code" with Some i -> i | None -> 137 in

  { success = false; stdout = stdout_val; stderr = stderr_val; exit_code; job_id = Some job_id }

(**
   List all active jobs for this API key.

   @param public_key API public key (optional)
   @param secret_key API secret key (optional)
   @return List of job status records
*)
let list_jobs ?public_key ?secret_key () =
  let response = api_get ?public_key ?secret_key "/jobs" in
  (* Return raw response for now - proper parsing would require JSON library *)
  response

(* ============================================================================
   Library API - Image Generation
   ============================================================================ *)

(**
   Generate images from text prompt.

   @param prompt Text description of the image to generate
   @param model Model to use (optional)
   @param size Image size (default "1024x1024")
   @param quality "standard" or "hd" (default "standard")
   @param n Number of images to generate (default 1)
   @param public_key API public key (optional)
   @param secret_key API secret key (optional)
   @return JSON response with images
*)
let image ?public_key ?secret_key ?(model="") ?(size="1024x1024") ?(quality="standard") ?(n=1) prompt =
  let model_json = if model = "" then "" else Printf.sprintf ",\"model\":\"%s\"" model in
  let json = Printf.sprintf "{\"prompt\":\"%s\",\"size\":\"%s\",\"quality\":\"%s\",\"n\":%d%s}"
    (escape_json prompt) size quality n model_json in

  api_post ?public_key ?secret_key "/image" json

(* ============================================================================
   Library API - Languages
   ============================================================================ *)

(**
   Get list of supported programming languages.
   Uses cached data if available and not expired (1 hour TTL).

   @param public_key API public key (optional)
   @param secret_key API secret key (optional)
   @return JSON response with languages list
*)
let languages ?public_key ?secret_key () =
  match load_languages_cache () with
  | Some cached -> cached
  | None ->
    let response = api_get ?public_key ?secret_key "/languages" in
    (* Extract and cache the languages array *)
    let langs_pattern = "\"languages\":\\s*\\[\\([^]]*\\)\\]" in
    let langs_regex = Str.regexp langs_pattern in
    (try
      let _ = Str.search_forward langs_regex response 0 in
      let langs_str = Str.matched_group 1 response in
      let languages_json = "[" ^ langs_str ^ "]" in
      save_languages_cache languages_json
    with Not_found -> ());
    response

(* ============================================================================
   Client Module
   ============================================================================ *)

(**
   Client module with stored credentials for convenient API access.

   @example
   {[
     let client = Client.create ~public_key:"unsb-pk-..." ~secret_key:"unsb-sk-..." () in
     let result = Client.execute client "python" "print('Hello')" in
     print_endline result.stdout
   ]}
*)
module Client = struct
  (** Client type with stored credentials *)
  type t = {
    public_key: string;
    secret_key: string;
  }

  (**
     Create a new client with credentials.

     @param public_key API public key (optional - uses env/config if not provided)
     @param secret_key API secret key (optional - uses env/config if not provided)
     @param account_index Account index in config file (default 0)
     @return Client instance
  *)
  let create ?public_key ?secret_key ?(account_index=0) () =
    let (pk, sk) = get_credentials ?public_key ?secret_key ~account_index () in
    { public_key = pk; secret_key = sk }

  (** Execute code synchronously *)
  let execute client ?opts language code =
    execute ~public_key:client.public_key ~secret_key:client.secret_key ?opts language code

  (** Execute code asynchronously *)
  let execute_async client ?opts language code =
    execute_async ~public_key:client.public_key ~secret_key:client.secret_key ?opts language code

  (** Execute with auto-detect language *)
  let run client ?opts code =
    run ~public_key:client.public_key ~secret_key:client.secret_key ?opts code

  (** Execute async with auto-detect language *)
  let run_async client ?opts code =
    run_async ~public_key:client.public_key ~secret_key:client.secret_key ?opts code

  (** Get job status *)
  let get_job client job_id =
    get_job ~public_key:client.public_key ~secret_key:client.secret_key job_id

  (** Wait for job completion *)
  let wait client ?max_polls job_id =
    wait ~public_key:client.public_key ~secret_key:client.secret_key ?max_polls job_id

  (** Cancel a job *)
  let cancel_job client job_id =
    cancel_job ~public_key:client.public_key ~secret_key:client.secret_key job_id

  (** List active jobs *)
  let list_jobs client =
    list_jobs ~public_key:client.public_key ~secret_key:client.secret_key ()

  (** Generate image *)
  let image client ?model ?size ?quality ?n prompt =
    image ~public_key:client.public_key ~secret_key:client.secret_key ?model ?size ?quality ?n prompt

  (** Get supported languages *)
  let languages client =
    languages ~public_key:client.public_key ~secret_key:client.secret_key ()
end

(* ============================================================================
   CLI - Legacy curl-based functions for CLI
   ============================================================================ *)

let curl_post api_key endpoint json =
  let (public_key, secret_key) = get_api_keys () in
  api_post ~public_key ~secret_key endpoint json

let curl_get api_key endpoint =
  let (public_key, secret_key) = get_api_keys () in
  api_get ~public_key ~secret_key endpoint

let curl_delete api_key endpoint =
  let (public_key, secret_key) = get_api_keys () in
  api_delete ~public_key ~secret_key endpoint

let portal_curl_post api_key endpoint json =
  let (public_key, secret_key) = get_api_keys () in
  portal_post ~public_key ~secret_key endpoint json

let curl_put_text endpoint body =
  let (public_key, secret_key) = get_api_keys () in
  let auth_headers = build_auth_headers public_key secret_key "PUT" endpoint body in
  let tmp_file = Printf.sprintf "/tmp/un_ocaml_%d.txt" (Random.int 999999) in
  let oc = open_out tmp_file in
  output_string oc body;
  close_out oc;
  let cmd = Printf.sprintf "curl -s -o /dev/null -w '%%{http_code}' -X PUT %s%s -H 'Content-Type: text/plain'%s -d @%s"
    api_base endpoint auth_headers tmp_file in
  let ic = Unix.open_process_in cmd in
  let status = try input_line ic with End_of_file -> "0" in
  let _ = Unix.close_process_in ic in
  Sys.remove tmp_file;
  let code = int_of_string (String.trim status) in
  code >= 200 && code < 300

let max_env_content_size = 65536

let read_env_file path =
  if not (Sys.file_exists path) then begin
    Printf.fprintf stderr "%sError: Env file not found: %s%s\n" red path reset;
    exit 1
  end;
  read_file path

let build_env_content envs env_file =
  let lines = ref envs in
  (match env_file with
   | Some path ->
     let content = read_env_file path in
     let file_lines = String.split_on_char '\n' content in
     List.iter (fun line ->
       let trimmed = String.trim line in
       if String.length trimmed > 0 && trimmed.[0] <> '#' then
         lines := trimmed :: !lines
     ) file_lines
   | None -> ());
  String.concat "\n" (List.rev !lines)

let service_env_status service_id =
  let api_key = get_api_key () in
  curl_get api_key (Printf.sprintf "/services/%s/env" service_id)

let service_env_set service_id env_content =
  if String.length env_content > max_env_content_size then begin
    Printf.fprintf stderr "%sError: Env content exceeds maximum size of 64KB%s\n" red reset;
    false
  end else
    curl_put_text (Printf.sprintf "/services/%s/env" service_id) env_content

let service_env_export service_id =
  let api_key = get_api_key () in
  let (public_key, secret_key) = get_api_keys () in
  let endpoint = Printf.sprintf "/services/%s/env/export" service_id in
  let auth_headers = build_auth_headers public_key secret_key "POST" endpoint "{}" in
  let tmp_file = Printf.sprintf "/tmp/un_ocaml_%d.json" (Random.int 999999) in
  let oc = open_out tmp_file in
  output_string oc "{}";
  close_out oc;
  let cmd = Printf.sprintf "curl -s -X POST %s%s -H 'Content-Type: application/json'%s -d @%s"
    api_base endpoint auth_headers tmp_file in
  let ic = Unix.open_process_in cmd in
  let rec read_all acc =
    try let line = input_line ic in read_all (acc ^ line ^ "\n")
    with End_of_file -> acc
  in
  let response = read_all "" in
  let _ = Unix.close_process_in ic in
  Sys.remove tmp_file;
  response

let service_env_delete service_id =
  let api_key = get_api_key () in
  try
    let _ = curl_delete api_key (Printf.sprintf "/services/%s/env" service_id) in
    true
  with _ -> false

let service_env_command action target envs env_file =
  match action with
  | "status" ->
    (match target with
     | Some sid ->
       let response = service_env_status sid in
       let has_vault = match extract_json_value response "has_vault" with
         | Some "true" -> true
         | _ -> false
       in
       if has_vault then begin
         Printf.printf "%sVault: configured%s\n" green reset;
         (match extract_json_value response "env_count" with
          | Some c -> Printf.printf "Variables: %s\n" c
          | None -> ());
         (match extract_json_value response "updated_at" with
          | Some u -> Printf.printf "Updated: %s\n" u
          | None -> ())
       end else
         Printf.printf "%sVault: not configured%s\n" yellow reset
     | None ->
       Printf.fprintf stderr "%sError: service env status requires service ID%s\n" red reset;
       exit 1)
  | "set" ->
    (match target with
     | Some sid ->
       if envs = [] && env_file = None then begin
         Printf.fprintf stderr "%sError: service env set requires -e or --env-file%s\n" red reset;
         exit 1
       end;
       let env_content = build_env_content envs env_file in
       if service_env_set sid env_content then
         Printf.printf "%sVault updated for service %s%s\n" green sid reset
       else begin
         Printf.fprintf stderr "%sError: Failed to update vault%s\n" red reset;
         exit 1
       end
     | None ->
       Printf.fprintf stderr "%sError: service env set requires service ID%s\n" red reset;
       exit 1)
  | "export" ->
    (match target with
     | Some sid ->
       let response = service_env_export sid in
       (match extract_json_value response "content" with
        | Some content -> Printf.printf "%s" (unescape_json content)
        | None -> ())
     | None ->
       Printf.fprintf stderr "%sError: service env export requires service ID%s\n" red reset;
       exit 1)
  | "delete" ->
    (match target with
     | Some sid ->
       if service_env_delete sid then
         Printf.printf "%sVault deleted for service %s%s\n" green sid reset
       else begin
         Printf.fprintf stderr "%sError: Failed to delete vault%s\n" red reset;
         exit 1
       end
     | None ->
       Printf.fprintf stderr "%sError: service env delete requires service ID%s\n" red reset;
       exit 1)
  | _ ->
    Printf.fprintf stderr "%sError: Unknown env action: %s%s\n" red action reset;
    Printf.fprintf stderr "Usage: un.ml service env <status|set|export|delete> <service_id>\n";
    exit 1

(* Open browser *)
let open_browser url =
  Printf.printf "%sOpening browser: %s%s\n" blue url reset;
  let _ = match Sys.os_type with
    | "Unix" | "Cygwin" ->
      (try Sys.command (Printf.sprintf "xdg-open '%s' 2>/dev/null" url)
       with _ ->
         try Sys.command (Printf.sprintf "open '%s' 2>/dev/null" url)
         with _ -> 1)
    | "Win32" ->
      Sys.command (Printf.sprintf "start '%s'" url)
    | _ -> 1
  in ()

(* Parse JSON response for stdout/stderr/exit_code *)
let extract_field field json =
  try
    let pattern = "\"" ^ field ^ "\":\"\\([^\"]*\\)\"" in
    let regex = Str.regexp pattern in
    let _ = Str.search_forward regex json 0 in
    Some (Str.matched_group 1 json)
  with Not_found ->
    try
      let pattern = "\"" ^ field ^ "\":\\([0-9]+\\)" in
      let regex = Str.regexp pattern in
      let _ = Str.search_forward regex json 0 in
      Some (Str.matched_group 1 json)
    with Not_found -> None

(* Execute command *)
let execute_command file env_vars artifacts out_dir network vcpu =
  let api_key = get_api_key () in
  let ext = get_extension file in
  let language = match ext_to_lang ext with
    | Some lang -> lang
    | None ->
      Printf.fprintf stderr "Error: Unknown extension: %s\n" ext;
      exit 1
  in
  let code = read_file file in
  let env_json = if env_vars = [] then ""
    else ",\"env\":{" ^ (String.concat "," (List.map (fun (k, v) ->
      Printf.sprintf "\"%s\":\"%s\"" k (escape_json v)) env_vars)) ^ "}"
  in
  let artifacts_json = if artifacts then ",\"return_artifacts\":true" else "" in
  let network_json = match network with Some n -> Printf.sprintf ",\"network\":\"%s\"" n | None -> "" in
  let vcpu_json = match vcpu with Some v -> Printf.sprintf ",\"vcpu\":%d" v | None -> "" in
  let json = Printf.sprintf "{\"language\":\"%s\",\"code\":\"%s\"%s%s%s%s}"
    language (escape_json code) env_json artifacts_json network_json vcpu_json in

  let tmp_file = Printf.sprintf "/tmp/un_ocaml_%d.json" (Random.int 999999) in
  let oc = open_out tmp_file in
  output_string oc json;
  close_out oc;

  let (public_key, secret_key) = get_api_keys () in
  let auth_headers = build_auth_headers public_key secret_key "POST" "/execute" json in
  let cmd = Printf.sprintf "curl -s -X POST %s/execute -H 'Content-Type: application/json'%s -d @%s"
    api_base auth_headers tmp_file in
  let ic = Unix.open_process_in cmd in
  let rec read_all acc =
    try let line = input_line ic in read_all (acc ^ line ^ "\n")
    with End_of_file -> acc
  in
  let response = read_all "" in
  let _ = Unix.close_process_in ic in
  Sys.remove tmp_file;

  (* Parse response *)
  (match extract_field "stdout" response with
   | Some s -> Printf.printf "%s%s%s" blue (unescape_json s) reset
   | None -> ());
  (match extract_field "stderr" response with
   | Some s -> Printf.fprintf stderr "%s%s%s" red (unescape_json s) reset
   | None -> ());
  let exit_code = match extract_field "exit_code" response with
    | Some s -> int_of_string s
    | None -> 0
  in
  exit exit_code

(* Display key info *)
let display_key_info response extend =
  let status = extract_json_value response "status" in
  let public_key = extract_json_value response "public_key" in
  let tier = extract_json_value response "tier" in
  let valid_through = extract_json_value response "valid_through_datetime" in
  let valid_for = extract_json_value response "valid_for_human" in
  let rate_limit = extract_json_value response "rate_per_minute" in
  let burst = extract_json_value response "burst" in
  let concurrency = extract_json_value response "concurrency" in
  let expired_at = extract_json_value response "expired_at_datetime" in

  match status with
  | Some "valid" ->
    Printf.printf "%sValid%s\n\n" green reset;
    (match public_key with Some pk -> Printf.printf "Public Key:          %s\n" pk | None -> ());
    (match tier with Some t -> Printf.printf "Tier:                %s\n" t | None -> ());
    Printf.printf "Status:              valid\n";
    (match valid_through with Some exp -> Printf.printf "Expires:             %s\n" exp | None -> ());
    (match valid_for with Some vf -> Printf.printf "Time Remaining:      %s\n" vf | None -> ());
    (match rate_limit with Some r -> Printf.printf "Rate Limit:          %s/min\n" r | None -> ());
    (match burst with Some b -> Printf.printf "Burst:               %s\n" b | None -> ());
    (match concurrency with Some c -> Printf.printf "Concurrency:         %s\n" c | None -> ());
    if extend then
      (match public_key with
       | Some pk -> open_browser (Printf.sprintf "%s/keys/extend?pk=%s" portal_base pk)
       | None -> ())
  | Some "expired" ->
    Printf.printf "%sExpired%s\n\n" red reset;
    (match public_key with Some pk -> Printf.printf "Public Key:          %s\n" pk | None -> ());
    (match tier with Some t -> Printf.printf "Tier:                %s\n" t | None -> ());
    (match expired_at with Some exp -> Printf.printf "Expired:             %s\n" exp | None -> ());
    Printf.printf "\n%sTo renew:%s Visit %s/keys/extend\n" yellow reset portal_base;
    if extend then
      (match public_key with
       | Some pk -> open_browser (Printf.sprintf "%s/keys/extend?pk=%s" portal_base pk)
       | None -> ())
  | Some "invalid" ->
    Printf.printf "%sInvalid%s\n" red reset
  | Some s ->
    Printf.printf "%sUnknown status: %s%s\n" red s reset;
    Printf.printf "%s\n" response
  | None ->
    Printf.printf "%sError: Could not parse response%s\n" red reset;
    Printf.printf "%s\n" response

(* Validate key command *)
let validate_key api_key extend =
  let json = "{}" in
  let response = portal_curl_post api_key "/keys/validate" json in
  display_key_info response extend

(* Key command *)
let key_command extend =
  let api_key = get_api_key () in
  validate_key api_key extend

(* Session command *)
let session_command action shell network vcpu input_files =
  let api_key = get_api_key () in
  match action with
  | "list" ->
    let response = curl_get api_key "/sessions" in
    Printf.printf "%s\n" response
  | "kill" ->
    (match shell with
     | Some sid ->
       let _ = curl_delete api_key ("/sessions/" ^ sid) in
       Printf.printf "%sSession terminated: %s%s\n" green sid reset
     | None ->
       Printf.fprintf stderr "Error: --kill requires session ID\n";
       exit 1)
  | "create" ->
    let sh = match shell with Some s -> s | None -> "bash" in
    let network_json = match network with Some n -> Printf.sprintf ",\"network\":\"%s\"" n | None -> "" in
    let vcpu_json = match vcpu with Some v -> Printf.sprintf ",\"vcpu\":%d" v | None -> "" in
    let input_files_json = build_input_files_json input_files in
    let json = Printf.sprintf "{\"shell\":\"%s\"%s%s%s}" sh network_json vcpu_json input_files_json in
    let response = curl_post api_key "/sessions" json in
    Printf.printf "%sSession created (WebSocket required)%s\n" yellow reset;
    Printf.printf "%s\n" response
  | _ -> ()

(* Set unfreeze_on_demand for a service *)
let set_service_unfreeze_on_demand service_id enabled =
  let (public_key, secret_key) = get_api_keys () in
  let enabled_str = if enabled then "true" else "false" in
  let json = Printf.sprintf "{\"unfreeze_on_demand\":%s}" enabled_str in
  let endpoint = Printf.sprintf "/services/%s" service_id in
  let auth_headers = build_auth_headers public_key secret_key "PATCH" endpoint json in
  let tmp_file = Printf.sprintf "/tmp/un_ocaml_%d.json" (Random.int 999999) in
  let oc = open_out tmp_file in
  output_string oc json;
  close_out oc;
  let cmd = Printf.sprintf "curl -s -X PATCH %s%s -H 'Content-Type: application/json'%s -d @%s"
    api_base endpoint auth_headers tmp_file in
  let _ = Sys.command cmd in
  Sys.remove tmp_file;
  true

(* Service command *)
let service_command action name ports bootstrap bootstrap_file service_type network vcpu unfreeze_on_demand input_files envs env_file =
  let api_key = get_api_key () in
  match action with
  | "env" ->
    service_env_command (match name with Some n -> n | None -> "") (match ports with Some p -> Some p | None -> None) envs env_file
  | "env_cmd" ->
    (match (name, ports) with
     | (Some act, target) -> service_env_command act target envs env_file
     | _ ->
       Printf.fprintf stderr "Error: service env requires action\n";
       exit 1)
  | "list" ->
    let response = curl_get api_key "/services" in
    Printf.printf "%s\n" response
  | "info" ->
    (match name with
     | Some sid ->
       let response = curl_get api_key ("/services/" ^ sid) in
       Printf.printf "%s\n" response
     | None ->
       Printf.fprintf stderr "Error: --info requires service ID\n";
       exit 1)
  | "logs" ->
    (match name with
     | Some sid ->
       let response = curl_get api_key ("/services/" ^ sid ^ "/logs") in
       Printf.printf "%s\n" response
     | None ->
       Printf.fprintf stderr "Error: --logs requires service ID\n";
       exit 1)
  | "sleep" ->
    (match name with
     | Some sid ->
       let _ = curl_post api_key ("/services/" ^ sid ^ "/freeze") "{}" in
       Printf.printf "%sService frozen: %s%s\n" green sid reset
     | None ->
       Printf.fprintf stderr "Error: --freeze requires service ID\n";
       exit 1)
  | "wake" ->
    (match name with
     | Some sid ->
       let _ = curl_post api_key ("/services/" ^ sid ^ "/unfreeze") "{}" in
       Printf.printf "%sService unfreezing: %s%s\n" green sid reset
     | None ->
       Printf.fprintf stderr "Error: --unfreeze requires service ID\n";
       exit 1)
  | "destroy" ->
    (match name with
     | Some sid ->
       (match api_delete_with_sudo ("/services/" ^ sid) with
        | SudoSuccess _ -> Printf.printf "%sService destroyed: %s%s\n" green sid reset
        | SudoCancelled -> exit 1
        | SudoError msg ->
          Printf.fprintf stderr "%sError: %s%s\n" red msg reset;
          exit 1)
     | None ->
       Printf.fprintf stderr "Error: --destroy requires service ID\n";
       exit 1)
  | "resize" ->
    (match (name, vcpu) with
     | (Some sid, Some v) ->
       if v < 1 || v > 8 then begin
         Printf.fprintf stderr "%sError: vCPU must be between 1 and 8%s\n" red reset;
         exit 1
       end;
       let json = Printf.sprintf "{\"vcpu\":%d}" v in
       let endpoint = Printf.sprintf "/services/%s" sid in
       let (public_key, secret_key) = get_api_keys () in
       let auth_headers = build_auth_headers public_key secret_key "PATCH" endpoint json in
       let tmp_file = Printf.sprintf "/tmp/un_ocaml_%d.json" (Random.int 999999) in
       let oc = open_out tmp_file in
       output_string oc json;
       close_out oc;
       let cmd = Printf.sprintf "curl -s -X PATCH %s%s -H 'Content-Type: application/json'%s -d @%s"
         api_base endpoint auth_headers tmp_file in
       let _ = Sys.command cmd in
       Sys.remove tmp_file;
       let ram = v * 2 in
       Printf.printf "%sService resized to %d vCPU, %d GB RAM%s\n" green v ram reset
     | (Some _, None) ->
       Printf.fprintf stderr "%sError: --resize requires --vcpu or -v%s\n" red reset;
       exit 1
     | (None, _) ->
       Printf.fprintf stderr "Error: --resize requires service ID\n";
       exit 1)
  | "set_unfreeze_on_demand" ->
    (match (name, ports) with
     | (Some sid, Some enabled_str) ->
       let enabled = enabled_str = "true" || enabled_str = "1" in
       let _ = set_service_unfreeze_on_demand sid enabled in
       let status = if enabled then "enabled" else "disabled" in
       Printf.printf "%sUnfreeze-on-demand %s for service: %s%s\n" green status sid reset
     | _ ->
       Printf.fprintf stderr "Error: --set-unfreeze-on-demand requires service ID and enabled (true/false)\n";
       exit 1)
  | "execute" ->
    (match name with
     | Some sid ->
       (match bootstrap with
        | Some cmd ->
          let json = Printf.sprintf "{\"command\":\"%s\"}" (escape_json cmd) in
          let response = curl_post api_key ("/services/" ^ sid ^ "/execute") json in
          (match extract_field "stdout" response with
           | Some s -> Printf.printf "%s%s%s" blue (unescape_json s) reset
           | None -> ())
        | None ->
          Printf.fprintf stderr "Error: --command required with --execute\n";
          exit 1)
     | None ->
       Printf.fprintf stderr "Error: --execute requires service ID\n";
       exit 1)
  | "dump_bootstrap" ->
    (match name with
     | Some sid ->
       Printf.fprintf stderr "Fetching bootstrap script from %s...\n" sid;
       let json = "{\"command\":\"cat /tmp/bootstrap.sh\"}" in
       let response = curl_post api_key ("/services/" ^ sid ^ "/execute") json in
       (match extract_field "stdout" response with
        | Some s ->
          let script = unescape_json s in
          (match service_type with
           | Some file ->
             let oc = open_out file in
             output_string oc script;
             close_out oc;
             Unix.chmod file 0o755;
             Printf.printf "Bootstrap saved to %s\n" file
           | None ->
             Printf.printf "%s" script)
        | None ->
          Printf.fprintf stderr "%sError: Failed to fetch bootstrap (service not running or no bootstrap file)%s\n" red reset;
          exit 1)
     | None ->
       Printf.fprintf stderr "Error: --dump-bootstrap requires service ID\n";
       exit 1)
  | "create" ->
    (match name with
     | Some n ->
       let ports_json = match ports with Some p -> Printf.sprintf ",\"ports\":[%s]" p | None -> "" in
       let bootstrap_json = match bootstrap with Some b -> Printf.sprintf ",\"bootstrap\":\"%s\"" (escape_json b) | None -> "" in
       let bootstrap_content_json = match bootstrap_file with
         | Some f ->
           let content = read_file f in
           Printf.sprintf ",\"bootstrap_content\":\"%s\"" (escape_json content)
         | None -> ""
       in
       let service_type_json = match service_type with Some t -> Printf.sprintf ",\"service_type\":\"%s\"" t | None -> "" in
       let network_json = match network with Some net -> Printf.sprintf ",\"network\":\"%s\"" net | None -> "" in
       let vcpu_json = match vcpu with Some v -> Printf.sprintf ",\"vcpu\":%d" v | None -> "" in
       let unfreeze_on_demand_json = if unfreeze_on_demand then ",\"unfreeze_on_demand\":true" else "" in
       let input_files_json = build_input_files_json input_files in
       let json = Printf.sprintf "{\"name\":\"%s\"%s%s%s%s%s%s%s%s}" n ports_json bootstrap_json bootstrap_content_json service_type_json network_json vcpu_json unfreeze_on_demand_json input_files_json in
       let response = curl_post api_key "/services" json in
       Printf.printf "%sService created%s\n" green reset;
       Printf.printf "%s\n" response;
       (* Auto-set vault if env vars were provided *)
       (match extract_json_value response "id" with
        | Some service_id when envs <> [] || env_file <> None ->
          let env_content = build_env_content envs env_file in
          if String.length env_content > 0 then
            if service_env_set service_id env_content then
              Printf.printf "%sVault configured with environment variables%s\n" green reset
            else
              Printf.printf "%sWarning: Failed to set vault%s\n" yellow reset
        | _ -> ())
     | None ->
       Printf.fprintf stderr "Error: --name required to create service\n";
       exit 1)
  | _ -> ()

(* Parse -f flags from argument list *)
let rec parse_input_files acc = function
  | [] -> List.rev acc
  | "-f" :: file :: rest ->
    if Sys.file_exists file then
      parse_input_files (file :: acc) rest
    else begin
      Printf.fprintf stderr "Error: File not found: %s\n" file;
      exit 1
    end
  | _ :: rest -> parse_input_files acc rest

(* ============================================================================
   CLI Entry Point
   ============================================================================ *)

(* Languages command *)
let languages_command json_output =
  let response = languages () in
  if json_output then begin
    (* Extract languages array and output as JSON *)
    let pattern = "\"languages\":\\s*\\[\\([^]]*\\)\\]" in
    let regex = Str.regexp pattern in
    try
      let _ = Str.search_forward regex response 0 in
      let langs_str = Str.matched_group 1 response in
      Printf.printf "[%s]\n" langs_str
    with Not_found ->
      Printf.printf "[]\n"
  end else begin
    (* Extract each language and print one per line *)
    let pattern = "\"\\([a-zA-Z0-9_+-]+\\)\"" in
    let regex = Str.regexp pattern in
    (* Find the languages array first *)
    let langs_pattern = "\"languages\":\\s*\\[\\([^]]*\\)\\]" in
    let langs_regex = Str.regexp langs_pattern in
    try
      let _ = Str.search_forward langs_regex response 0 in
      let langs_str = Str.matched_group 1 response in
      let rec extract_langs pos =
        try
          let _ = Str.search_forward regex langs_str pos in
          let lang = Str.matched_group 1 langs_str in
          Printf.printf "%s\n" lang;
          extract_langs (Str.match_end ())
        with Not_found -> ()
      in
      extract_langs 0
    with Not_found -> ()
  end

(* Image command *)
let image_command args =
  let api_key = get_api_key () in
  let rec parse_args list_mode info_id delete_id lock_id unlock_id publish_id source_type visibility_id visibility_mode spawn_id clone_id name ports = function
    | [] ->
      if list_mode then begin
        let response = curl_get api_key "/images" in
        Printf.printf "%s\n" response
      end
      else if info_id <> "" then begin
        let response = curl_get api_key (Printf.sprintf "/images/%s" info_id) in
        Printf.printf "%s\n" response
      end
      else if delete_id <> "" then begin
        (match api_delete_with_sudo (Printf.sprintf "/images/%s" delete_id) with
         | SudoSuccess _ -> Printf.printf "%sImage deleted: %s%s\n" green delete_id reset
         | SudoCancelled -> exit 1
         | SudoError msg ->
           Printf.fprintf stderr "%sError: %s%s\n" red msg reset;
           exit 1)
      end
      else if lock_id <> "" then begin
        let _ = curl_post api_key (Printf.sprintf "/images/%s/lock" lock_id) "{}" in
        Printf.printf "%sImage locked: %s%s\n" green lock_id reset
      end
      else if unlock_id <> "" then begin
        (match api_post_with_sudo (Printf.sprintf "/images/%s/unlock" unlock_id) "{}" with
         | SudoSuccess _ -> Printf.printf "%sImage unlocked: %s%s\n" green unlock_id reset
         | SudoCancelled -> exit 1
         | SudoError msg ->
           Printf.fprintf stderr "%sError: %s%s\n" red msg reset;
           exit 1)
      end
      else if publish_id <> "" then begin
        if source_type = "" then begin
          Printf.fprintf stderr "%sError: --publish requires --source-type (service or snapshot)%s\n" red reset;
          exit 1
        end;
        let name_json = if name <> "" then Printf.sprintf ",\"name\":\"%s\"" name else "" in
        let json = Printf.sprintf "{\"source_type\":\"%s\",\"source_id\":\"%s\"%s}" source_type publish_id name_json in
        let response = curl_post api_key "/images/publish" json in
        Printf.printf "%sImage published%s\n" green reset;
        Printf.printf "%s\n" response
      end
      else if visibility_id <> "" && visibility_mode <> "" then begin
        let json = Printf.sprintf "{\"visibility\":\"%s\"}" visibility_mode in
        let _ = curl_post api_key (Printf.sprintf "/images/%s/visibility" visibility_id) json in
        Printf.printf "%sImage visibility set to %s: %s%s\n" green visibility_mode visibility_id reset
      end
      else if spawn_id <> "" then begin
        let name_json = if name <> "" then Printf.sprintf "\"name\":\"%s\"" name else "" in
        let ports_json = if ports <> "" then Printf.sprintf "%s\"ports\":[%s]" (if name <> "" then "," else "") ports else "" in
        let json = Printf.sprintf "{%s%s}" name_json ports_json in
        let response = curl_post api_key (Printf.sprintf "/images/%s/spawn" spawn_id) json in
        Printf.printf "%sService spawned from image%s\n" green reset;
        Printf.printf "%s\n" response
      end
      else if clone_id <> "" then begin
        let name_json = if name <> "" then Printf.sprintf "{\"name\":\"%s\"}" name else "{}" in
        let response = curl_post api_key (Printf.sprintf "/images/%s/clone" clone_id) name_json in
        Printf.printf "%sImage cloned%s\n" green reset;
        Printf.printf "%s\n" response
      end
      else begin
        Printf.fprintf stderr "%sError: Use --list, --info, --delete, --lock, --unlock, --publish, --visibility, --spawn, or --clone%s\n" red reset;
        exit 1
      end
    | "--list" :: rest -> parse_args true info_id delete_id lock_id unlock_id publish_id source_type visibility_id visibility_mode spawn_id clone_id name ports rest
    | "-l" :: rest -> parse_args true info_id delete_id lock_id unlock_id publish_id source_type visibility_id visibility_mode spawn_id clone_id name ports rest
    | "--info" :: id :: rest -> parse_args list_mode id delete_id lock_id unlock_id publish_id source_type visibility_id visibility_mode spawn_id clone_id name ports rest
    | "--delete" :: id :: rest -> parse_args list_mode info_id id lock_id unlock_id publish_id source_type visibility_id visibility_mode spawn_id clone_id name ports rest
    | "--lock" :: id :: rest -> parse_args list_mode info_id delete_id id unlock_id publish_id source_type visibility_id visibility_mode spawn_id clone_id name ports rest
    | "--unlock" :: id :: rest -> parse_args list_mode info_id delete_id lock_id id publish_id source_type visibility_id visibility_mode spawn_id clone_id name ports rest
    | "--publish" :: id :: rest -> parse_args list_mode info_id delete_id lock_id unlock_id id source_type visibility_id visibility_mode spawn_id clone_id name ports rest
    | "--source-type" :: t :: rest -> parse_args list_mode info_id delete_id lock_id unlock_id publish_id t visibility_id visibility_mode spawn_id clone_id name ports rest
    | "--visibility" :: id :: mode :: rest -> parse_args list_mode info_id delete_id lock_id unlock_id publish_id source_type id mode spawn_id clone_id name ports rest
    | "--spawn" :: id :: rest -> parse_args list_mode info_id delete_id lock_id unlock_id publish_id source_type visibility_id visibility_mode id clone_id name ports rest
    | "--clone" :: id :: rest -> parse_args list_mode info_id delete_id lock_id unlock_id publish_id source_type visibility_id visibility_mode spawn_id id name ports rest
    | "--name" :: n :: rest -> parse_args list_mode info_id delete_id lock_id unlock_id publish_id source_type visibility_id visibility_mode spawn_id clone_id n ports rest
    | "--ports" :: p :: rest -> parse_args list_mode info_id delete_id lock_id unlock_id publish_id source_type visibility_id visibility_mode spawn_id clone_id name p rest
    | _ :: rest -> parse_args list_mode info_id delete_id lock_id unlock_id publish_id source_type visibility_id visibility_mode spawn_id clone_id name ports rest
  in
  parse_args false "" "" "" "" "" "" "" "" "" "" "" "" args

let () =
  Random.self_init ();
  let args = Array.to_list Sys.argv in
  match List.tl args with
  | [] ->
    Printf.printf "Usage: un.ml [options] <source_file>\n";
    Printf.printf "       un.ml session [options]\n";
    Printf.printf "       un.ml service [options]\n";
    Printf.printf "       un.ml image [options]\n";
    Printf.printf "       un.ml service env <action> <service_id>\n";
    Printf.printf "       un.ml languages [--json]\n";
    Printf.printf "       un.ml key [--extend]\n\n";
    Printf.printf "Service options: --name, --ports, --bootstrap, --bootstrap-file, -e KEY=VALUE, --env-file FILE\n";
    Printf.printf "Service env commands: status, set, export, delete\n";
    Printf.printf "Image options: --list, --info ID, --delete ID, --lock ID, --unlock ID,\n";
    Printf.printf "               --publish ID --source-type TYPE, --visibility ID MODE,\n";
    Printf.printf "               --spawn ID, --clone ID, --name NAME, --ports PORTS\n";
    Printf.printf "Languages options: --json (output as JSON array)\n";
    exit 1
  | "languages" :: rest ->
    let json_output = List.mem "--json" rest in
    languages_command json_output
  | "image" :: rest ->
    image_command rest
  | "key" :: rest ->
    let extend = List.mem "--extend" rest in
    key_command extend
  | "session" :: rest ->
    let input_files = parse_input_files [] rest in
    let rec parse_session action shell network vcpu = function
      | [] -> session_command action shell network vcpu input_files
      | "--list" :: rest -> parse_session "list" shell network vcpu rest
      | "--kill" :: id :: rest -> parse_session "kill" (Some id) network vcpu rest
      | "--shell" :: sh :: rest | "-s" :: sh :: rest -> parse_session action (Some sh) network vcpu rest
      | "-n" :: net :: rest -> parse_session action shell (Some net) vcpu rest
      | "-v" :: v :: rest -> parse_session action shell network (Some (int_of_string v)) rest
      | "-f" :: _ :: rest -> parse_session action shell network vcpu rest (* skip -f, already parsed *)
      | arg :: rest ->
        if String.length arg > 0 && arg.[0] = '-' then begin
          Printf.fprintf stderr "Unknown option: %s\n" arg;
          Printf.fprintf stderr "Usage: un.ml session [options]\n";
          exit 1
        end else
          parse_session action shell network vcpu rest
    in
    parse_session "create" None None None rest
  | "service" :: rest ->
    let input_files = parse_input_files [] rest in
    let rec parse_envs acc = function
      | [] -> List.rev acc
      | "-e" :: kv :: rest -> parse_envs (kv :: acc) rest
      | _ :: rest -> parse_envs acc rest
    in
    let envs = parse_envs [] rest in
    let rec parse_service action name ports bootstrap bootstrap_file service_type network vcpu unfreeze_on_demand env_file = function
      | [] -> service_command action name ports bootstrap bootstrap_file service_type network vcpu unfreeze_on_demand input_files envs env_file
      | "env" :: env_action :: target :: rest when not (String.length target > 0 && target.[0] = '-') ->
        parse_service "env_cmd" (Some env_action) (Some target) bootstrap bootstrap_file service_type network vcpu unfreeze_on_demand env_file rest
      | "env" :: env_action :: rest ->
        parse_service "env_cmd" (Some env_action) None bootstrap bootstrap_file service_type network vcpu unfreeze_on_demand env_file rest
      | "--list" :: rest -> parse_service "list" name ports bootstrap bootstrap_file service_type network vcpu unfreeze_on_demand env_file rest
      | "--info" :: id :: rest -> parse_service "info" (Some id) ports bootstrap bootstrap_file service_type network vcpu unfreeze_on_demand env_file rest
      | "--logs" :: id :: rest -> parse_service "logs" (Some id) ports bootstrap bootstrap_file service_type network vcpu unfreeze_on_demand env_file rest
      | "--freeze" :: id :: rest -> parse_service "sleep" (Some id) ports bootstrap bootstrap_file service_type network vcpu unfreeze_on_demand env_file rest
      | "--unfreeze" :: id :: rest -> parse_service "wake" (Some id) ports bootstrap bootstrap_file service_type network vcpu unfreeze_on_demand env_file rest
      | "--destroy" :: id :: rest -> parse_service "destroy" (Some id) ports bootstrap bootstrap_file service_type network vcpu unfreeze_on_demand env_file rest
      | "--resize" :: id :: rest -> parse_service "resize" (Some id) ports bootstrap bootstrap_file service_type network vcpu unfreeze_on_demand env_file rest
      | "--vcpu" :: v :: rest -> parse_service action name ports bootstrap bootstrap_file service_type network (Some (int_of_string v)) unfreeze_on_demand env_file rest
      | "--execute" :: id :: "--command" :: cmd :: rest -> parse_service "execute" (Some id) ports (Some cmd) bootstrap_file service_type network vcpu unfreeze_on_demand env_file rest
      | "--dump-bootstrap" :: id :: file :: rest -> parse_service "dump_bootstrap" (Some id) ports bootstrap (Some file) service_type network vcpu unfreeze_on_demand env_file rest
      | "--dump-bootstrap" :: id :: rest -> parse_service "dump_bootstrap" (Some id) ports bootstrap bootstrap_file service_type network vcpu unfreeze_on_demand env_file rest
      | "--name" :: n :: rest -> parse_service "create" (Some n) ports bootstrap bootstrap_file service_type network vcpu unfreeze_on_demand env_file rest
      | "--ports" :: p :: rest -> parse_service action name (Some p) bootstrap bootstrap_file service_type network vcpu unfreeze_on_demand env_file rest
      | "--bootstrap" :: b :: rest -> parse_service action name ports (Some b) bootstrap_file service_type network vcpu unfreeze_on_demand env_file rest
      | "--bootstrap-file" :: f :: rest -> parse_service action name ports bootstrap (Some f) service_type network vcpu unfreeze_on_demand env_file rest
      | "--type" :: t :: rest -> parse_service action name ports bootstrap bootstrap_file (Some t) network vcpu unfreeze_on_demand env_file rest
      | "-n" :: net :: rest -> parse_service action name ports bootstrap bootstrap_file service_type (Some net) vcpu unfreeze_on_demand env_file rest
      | "-v" :: v :: rest -> parse_service action name ports bootstrap bootstrap_file service_type network (Some (int_of_string v)) unfreeze_on_demand env_file rest
      | "--env-file" :: f :: rest -> parse_service action name ports bootstrap bootstrap_file service_type network vcpu unfreeze_on_demand (Some f) rest
      | "--unfreeze-on-demand" :: rest -> parse_service action name ports bootstrap bootstrap_file service_type network vcpu true env_file rest
      | "--set-unfreeze-on-demand" :: id :: enabled :: rest -> parse_service "set_unfreeze_on_demand" (Some id) (Some enabled) bootstrap bootstrap_file service_type network vcpu unfreeze_on_demand env_file rest
      | "-e" :: _ :: rest -> parse_service action name ports bootstrap bootstrap_file service_type network vcpu unfreeze_on_demand env_file rest (* skip -e, already parsed *)
      | "-f" :: _ :: rest -> parse_service action name ports bootstrap bootstrap_file service_type network vcpu unfreeze_on_demand env_file rest (* skip -f, already parsed *)
      | _ :: rest -> parse_service action name ports bootstrap bootstrap_file service_type network vcpu unfreeze_on_demand env_file rest
    in
    parse_service "create" None None None None None None None false None rest
  | args ->
    let rec parse_execute file env_vars artifacts out_dir network vcpu = function
      | [] -> execute_command file env_vars artifacts out_dir network vcpu
      | "-e" :: kv :: rest ->
        (try
          let eq_pos = String.index kv '=' in
          let k = String.sub kv 0 eq_pos in
          let v = String.sub kv (eq_pos + 1) (String.length kv - eq_pos - 1) in
          parse_execute file ((k, v) :: env_vars) artifacts out_dir network vcpu rest
        with Not_found -> parse_execute file env_vars artifacts out_dir network vcpu rest)
      | "-a" :: rest -> parse_execute file env_vars true out_dir network vcpu rest
      | "-o" :: dir :: rest -> parse_execute file env_vars artifacts (Some dir) network vcpu rest
      | "-n" :: net :: rest -> parse_execute file env_vars artifacts out_dir (Some net) vcpu rest
      | "-v" :: v :: rest -> parse_execute file env_vars artifacts out_dir network (Some (int_of_string v)) rest
      | arg :: rest ->
        if file = "" && not (String.get arg 0 = '-') then
          parse_execute arg env_vars artifacts out_dir network vcpu rest
        else
          parse_execute file env_vars artifacts out_dir network vcpu rest
    in
    parse_execute "" [] false None None None args
