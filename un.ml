-- PUBLIC DOMAIN - NO LICENSE, NO WARRANTY
--
-- This is free public domain software for the public good of a permacomputer hosted
-- at permacomputer.com - an always-on computer by the people, for the people. One
-- which is durable, easy to repair, and distributed like tap water for machine
-- learning intelligence.
--
-- The permacomputer is community-owned infrastructure optimized around four values:
--
--   TRUTH    - First principles, math & science, open source code freely distributed
--   FREEDOM  - Voluntary partnerships, freedom from tyranny & corporate control
--   HARMONY  - Minimal waste, self-renewing systems with diverse thriving connections
--   LOVE     - Be yourself without hurting others, cooperation through natural law
--
-- This software contributes to that vision by enabling code execution across 42+
-- programming languages through a unified interface, accessible to all. Code is
-- seeds to sprout on any abandoned technology.
--
-- Learn more: https://www.permacomputer.com
--
-- Anyone is free to copy, modify, publish, use, compile, sell, or distribute this
-- software, either in source code form or as a compiled binary, for any purpose,
-- commercial or non-commercial, and by any means.
--
-- NO WARRANTY. THE SOFTWARE IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY KIND.
--
-- That said, our permacomputer's digital membrane stratum continuously runs unit,
-- integration, and functional tests on all of it's own software - with our
-- permacomputer monitoring itself, repairing itself, with minimal human in the
-- loop guidance. Our agents do their best.
--
-- Copyright 2025 TimeHexOn & foxhop & russell@unturf
-- https://www.timehexon.com
-- https://www.foxhop.net
-- https://www.unturf.com/software


#!/usr/bin/env ocaml

(*
OCaml UN CLI - Unsandbox CLI Client

Full-featured CLI matching un.py capabilities:
- Execute code with env vars, input files, artifacts
- Interactive sessions with shell/REPL support
- Persistent services with domains and ports

Usage:
  chmod +x un.ml
  export UNSANDBOX_API_KEY="your_key_here"
  ./un.ml [options] <source_file>
  ./un.ml session [options]
  ./un.ml service [options]

Uses curl for HTTP (no external dependencies)
*)

(* ANSI colors *)
let blue = "\x1b[34m"
let red = "\x1b[31m"
let green = "\x1b[32m"
let yellow = "\x1b[33m"
let reset = "\x1b[0m"

(* Portal base URL *)
let portal_base = "https://unsandbox.com"

(* Extension to language mapping *)
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

(* Read file contents *)
let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

(* Base64 encode a file using shell command *)
let base64_encode_file filename =
  let cmd = Printf.sprintf "base64 -w0 %s" (Filename.quote filename) in
  let ic = Unix.open_process_in cmd in
  let result = try input_line ic with End_of_file -> "" in
  let _ = Unix.close_process_in ic in
  String.trim result

(* Build input_files JSON from list of filenames *)
let build_input_files_json files =
  if files = [] then ""
  else
    let entries = List.map (fun f ->
      let basename = Filename.basename f in
      let content = base64_encode_file f in
      Printf.sprintf "{\"filename\":\"%s\",\"content\":\"%s\"}" basename content
    ) files in
    ",\"input_files\":[" ^ (String.concat "," entries) ^ "]"

(* Get file extension *)
let get_extension filename =
  try
    let dot_pos = String.rindex filename '.' in
    String.sub filename dot_pos (String.length filename - dot_pos)
  with Not_found -> ""

(* Escape JSON string *)
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

(* Check for clock drift errors *)
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

(* Execute curl command *)
let curl_post api_key endpoint json =
  let (public_key, secret_key) = get_api_keys () in
  let auth_headers = build_auth_headers public_key secret_key "POST" endpoint json in
  let tmp_file = Printf.sprintf "/tmp/un_ocaml_%d.json" (Random.int 999999) in
  let oc = open_out tmp_file in
  output_string oc json;
  close_out oc;
  let cmd = Printf.sprintf "curl -s -X POST https://api.unsandbox.com%s -H 'Content-Type: application/json'%s -d @%s"
    endpoint auth_headers tmp_file in
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

let portal_curl_post api_key endpoint json =
  let tmp_file = Printf.sprintf "/tmp/un_ocaml_%d.json" (Random.int 999999) in
  let oc = open_out tmp_file in
  output_string oc json;
  close_out oc;
  let (public_key, secret_key) = get_api_keys () in
  let auth_headers = build_auth_headers public_key secret_key "POST" endpoint json in
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

let curl_get api_key endpoint =
  let (public_key, secret_key) = get_api_keys () in
  let auth_headers = build_auth_headers public_key secret_key "GET" endpoint "" in
  let cmd = Printf.sprintf "curl -s https://api.unsandbox.com%s%s"
    endpoint auth_headers in
  let ic = Unix.open_process_in cmd in
  let rec read_all acc =
    try
      let line = input_line ic in
      read_all (acc ^ line ^ "\n")
    with End_of_file -> acc
  in
  let output = read_all "" in
  let _ = Unix.close_process_in ic in
  check_clock_drift output;
  output

let curl_delete api_key endpoint =
  let (public_key, secret_key) = get_api_keys () in
  let auth_headers = build_auth_headers public_key secret_key "DELETE" endpoint "" in
  let cmd = Printf.sprintf "curl -s -X DELETE https://api.unsandbox.com%s%s"
    endpoint auth_headers in
  let ic = Unix.open_process_in cmd in
  let rec read_all acc =
    try
      let line = input_line ic in
      read_all (acc ^ line ^ "\n")
    with End_of_file -> acc
  in
  let output = read_all "" in
  let _ = Unix.close_process_in ic in
  check_clock_drift output;
  output

let curl_put_text endpoint body =
  let (public_key, secret_key) = get_api_keys () in
  let auth_headers = build_auth_headers public_key secret_key "PUT" endpoint body in
  let tmp_file = Printf.sprintf "/tmp/un_ocaml_%d.txt" (Random.int 999999) in
  let oc = open_out tmp_file in
  output_string oc body;
  close_out oc;
  let cmd = Printf.sprintf "curl -s -o /dev/null -w '%%{http_code}' -X PUT https://api.unsandbox.com%s -H 'Content-Type: text/plain'%s -d @%s"
    endpoint auth_headers tmp_file in
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
  let cmd = Printf.sprintf "curl -s -X POST https://api.unsandbox.com%s -H 'Content-Type: application/json'%s -d @%s"
    endpoint auth_headers tmp_file in
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

(* Extract JSON value - simple regex-based parser *)
let extract_json_value json_str key =
  let pattern = "\"" ^ key ^ "\"\\s*:\\s*\"\\([^\"]*\\)\"" in
  let regex = Str.regexp pattern in
  try
    let _ = Str.search_forward regex json_str 0 in
    Some (Str.matched_group 1 json_str)
  with Not_found -> None

(* Open browser *)
let open_browser url =
  Printf.printf "%sOpening browser: %s%s\n" blue url reset;
  let _ = match Sys.os_type with
    | "Unix" | "Cygwin" ->
      (* Try xdg-open for Linux *)
      (try Sys.command (Printf.sprintf "xdg-open '%s' 2>/dev/null" url)
       with _ ->
         (* Fallback to open for macOS *)
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

let unescape_json s =
  let s = Str.global_replace (Str.regexp "\\\\n") "\n" s in
  let s = Str.global_replace (Str.regexp "\\\\t") "\t" s in
  let s = Str.global_replace (Str.regexp "\\\\\"") "\"" s in
  let s = Str.global_replace (Str.regexp "\\\\\\\\") "\\" s in
  s

(* Get API keys *)
let get_api_keys () =
  let public_key = try Some (Sys.getenv "UNSANDBOX_PUBLIC_KEY") with Not_found -> None in
  let secret_key = try Some (Sys.getenv "UNSANDBOX_SECRET_KEY") with Not_found -> None in
  let api_key = try Some (Sys.getenv "UNSANDBOX_API_KEY") with Not_found -> None in
  match (public_key, secret_key, api_key) with
  | (Some pk, Some sk, _) -> (pk, Some sk)
  | (_, _, Some ak) -> (ak, None)
  | _ ->
    Printf.fprintf stderr "Error: UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY not set (or UNSANDBOX_API_KEY for backwards compat)\n";
    exit 1

let get_api_key () =
  let (public_key, _) = get_api_keys () in
  public_key

(* HMAC-SHA256 using openssl command *)
let hmac_sha256 secret message =
  let cmd = Printf.sprintf "echo -n '%s' | openssl dgst -sha256 -hmac '%s' | awk '{print $2}'"
    (Str.global_replace (Str.regexp "'") "'\\''" message)
    (Str.global_replace (Str.regexp "'") "'\\''" secret) in
  let ic = Unix.open_process_in cmd in
  let result = input_line ic in
  let _ = Unix.close_process_in ic in
  String.trim result

let make_signature secret_key timestamp method_ path body =
  let message = Printf.sprintf "%s:%s:%s:%s" timestamp method_ path body in
  hmac_sha256 secret_key message

let build_auth_headers public_key secret_key method_ path body =
  match secret_key with
  | Some sk ->
    let timestamp = string_of_int (int_of_float (Unix.time ())) in
    let signature = make_signature sk timestamp method_ path body in
    Printf.sprintf " -H 'Authorization: Bearer %s' -H 'X-Timestamp: %s' -H 'X-Signature: %s'"
      public_key timestamp signature
  | None ->
    Printf.sprintf " -H 'Authorization: Bearer %s'" public_key

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

  let cmd = Printf.sprintf "curl -s -X POST https://api.unsandbox.com/execute -H 'Content-Type: application/json' -H 'Authorization: Bearer %s' -d @%s"
    api_key tmp_file in
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
       let response = curl_delete api_key ("/sessions/" ^ sid) in
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
    let tmp_file = Printf.sprintf "/tmp/un_ocaml_%d.json" (Random.int 999999) in
    let oc = open_out tmp_file in
    output_string oc json;
    close_out oc;
    let cmd = Printf.sprintf "curl -s -X POST https://api.unsandbox.com/sessions -H 'Content-Type: application/json' -H 'Authorization: Bearer %s' -d @%s"
      api_key tmp_file in
    let ic = Unix.open_process_in cmd in
    let rec read_all acc =
      try let line = input_line ic in read_all (acc ^ line ^ "\n")
      with End_of_file -> acc
    in
    let response = read_all "" in
    let _ = Unix.close_process_in ic in
    Sys.remove tmp_file;
    Printf.printf "%sSession created (WebSocket required)%s\n" yellow reset;
    Printf.printf "%s\n" response
  | _ -> ()

(* Service command *)
let service_command action name ports bootstrap bootstrap_file service_type network vcpu input_files envs env_file =
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
       let tmp_file = Printf.sprintf "/tmp/un_ocaml_%d.json" (Random.int 999999) in
       let oc = open_out tmp_file in
       output_string oc "{}";
       close_out oc;
       let cmd = Printf.sprintf "curl -s -X POST https://api.unsandbox.com/services/%s/sleep -H 'Content-Type: application/json' -H 'Authorization: Bearer %s' -d @%s"
         sid api_key tmp_file in
       let _ = Sys.command cmd in
       Sys.remove tmp_file;
       Printf.printf "%sService sleeping: %s%s\n" green sid reset
     | None ->
       Printf.fprintf stderr "Error: --freeze requires service ID\n";
       exit 1)
  | "wake" ->
    (match name with
     | Some sid ->
       let tmp_file = Printf.sprintf "/tmp/un_ocaml_%d.json" (Random.int 999999) in
       let oc = open_out tmp_file in
       output_string oc "{}";
       close_out oc;
       let cmd = Printf.sprintf "curl -s -X POST https://api.unsandbox.com/services/%s/wake -H 'Content-Type: application/json' -H 'Authorization: Bearer %s' -d @%s"
         sid api_key tmp_file in
       let _ = Sys.command cmd in
       Sys.remove tmp_file;
       Printf.printf "%sService waking: %s%s\n" green sid reset
     | None ->
       Printf.fprintf stderr "Error: --unfreeze requires service ID\n";
       exit 1)
  | "destroy" ->
    (match name with
     | Some sid ->
       let response = curl_delete api_key ("/services/" ^ sid) in
       Printf.printf "%sService destroyed: %s%s\n" green sid reset
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
       let cmd = Printf.sprintf "curl -s -X PATCH https://api.unsandbox.com%s -H 'Content-Type: application/json'%s -d @%s"
         endpoint auth_headers tmp_file in
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
  | "execute" ->
    (match name with
     | Some sid ->
       (match bootstrap with
        | Some cmd ->
          let json = Printf.sprintf "{\"command\":\"%s\"}" (escape_json cmd) in
          let tmp_file = Printf.sprintf "/tmp/un_ocaml_%d.json" (Random.int 999999) in
          let oc = open_out tmp_file in
          output_string oc json;
          close_out oc;
          let curl_cmd = Printf.sprintf "curl -s -X POST https://api.unsandbox.com/services/%s/execute -H 'Content-Type: application/json' -H 'Authorization: Bearer %s' -d @%s"
            sid api_key tmp_file in
          let ic = Unix.open_process_in curl_cmd in
          let rec read_all acc =
            try let line = input_line ic in read_all (acc ^ line ^ "\n")
            with End_of_file -> acc
          in
          let response = read_all "" in
          let _ = Unix.close_process_in ic in
          Sys.remove tmp_file;
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
       let tmp_file = Printf.sprintf "/tmp/un_ocaml_%d.json" (Random.int 999999) in
       let oc = open_out tmp_file in
       output_string oc json;
       close_out oc;
       let curl_cmd = Printf.sprintf "curl -s -X POST https://api.unsandbox.com/services/%s/execute -H 'Content-Type: application/json' -H 'Authorization: Bearer %s' -d @%s"
         sid api_key tmp_file in
       let ic = Unix.open_process_in curl_cmd in
       let rec read_all acc =
         try let line = input_line ic in read_all (acc ^ line ^ "\n")
         with End_of_file -> acc
       in
       let response = read_all "" in
       let _ = Unix.close_process_in ic in
       Sys.remove tmp_file;
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
       let input_files_json = build_input_files_json input_files in
       let json = Printf.sprintf "{\"name\":\"%s\"%s%s%s%s%s%s%s}" n ports_json bootstrap_json bootstrap_content_json service_type_json network_json vcpu_json input_files_json in
       let tmp_file = Printf.sprintf "/tmp/un_ocaml_%d.json" (Random.int 999999) in
       let oc = open_out tmp_file in
       output_string oc json;
       close_out oc;
       let cmd = Printf.sprintf "curl -s -X POST https://api.unsandbox.com/services -H 'Content-Type: application/json' -H 'Authorization: Bearer %s' -d @%s"
         api_key tmp_file in
       let ic = Unix.open_process_in cmd in
       let rec read_all acc =
         try let line = input_line ic in read_all (acc ^ line ^ "\n")
         with End_of_file -> acc
       in
       let response = read_all "" in
       let _ = Unix.close_process_in ic in
       Sys.remove tmp_file;
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

(* Parse arguments *)
let () =
  Random.self_init ();
  let args = Array.to_list Sys.argv in
  match List.tl args with
  | [] ->
    Printf.printf "Usage: un.ml [options] <source_file>\n";
    Printf.printf "       un.ml session [options]\n";
    Printf.printf "       un.ml service [options]\n";
    Printf.printf "       un.ml service env <action> <service_id>\n";
    Printf.printf "       un.ml key [--extend]\n\n";
    Printf.printf "Service options: --name, --ports, --bootstrap, --bootstrap-file, -e KEY=VALUE, --env-file FILE\n";
    Printf.printf "Service env commands: status, set, export, delete\n";
    exit 1
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
    let rec parse_service action name ports bootstrap bootstrap_file service_type network vcpu env_file = function
      | [] -> service_command action name ports bootstrap bootstrap_file service_type network vcpu input_files envs env_file
      | "env" :: env_action :: target :: rest when not (String.length target > 0 && target.[0] = '-') ->
        parse_service "env_cmd" (Some env_action) (Some target) bootstrap bootstrap_file service_type network vcpu env_file rest
      | "env" :: env_action :: rest ->
        parse_service "env_cmd" (Some env_action) None bootstrap bootstrap_file service_type network vcpu env_file rest
      | "--list" :: rest -> parse_service "list" name ports bootstrap bootstrap_file service_type network vcpu env_file rest
      | "--info" :: id :: rest -> parse_service "info" (Some id) ports bootstrap bootstrap_file service_type network vcpu env_file rest
      | "--logs" :: id :: rest -> parse_service "logs" (Some id) ports bootstrap bootstrap_file service_type network vcpu env_file rest
      | "--freeze" :: id :: rest -> parse_service "sleep" (Some id) ports bootstrap bootstrap_file service_type network vcpu env_file rest
      | "--unfreeze" :: id :: rest -> parse_service "wake" (Some id) ports bootstrap bootstrap_file service_type network vcpu env_file rest
      | "--destroy" :: id :: rest -> parse_service "destroy" (Some id) ports bootstrap bootstrap_file service_type network vcpu env_file rest
      | "--resize" :: id :: rest -> parse_service "resize" (Some id) ports bootstrap bootstrap_file service_type network vcpu env_file rest
      | "--vcpu" :: v :: rest -> parse_service action name ports bootstrap bootstrap_file service_type network (Some (int_of_string v)) env_file rest
      | "--execute" :: id :: "--command" :: cmd :: rest -> parse_service "execute" (Some id) ports (Some cmd) bootstrap_file service_type network vcpu env_file rest
      | "--dump-bootstrap" :: id :: file :: rest -> parse_service "dump_bootstrap" (Some id) ports bootstrap (Some file) service_type network vcpu env_file rest
      | "--dump-bootstrap" :: id :: rest -> parse_service "dump_bootstrap" (Some id) ports bootstrap bootstrap_file service_type network vcpu env_file rest
      | "--name" :: n :: rest -> parse_service "create" (Some n) ports bootstrap bootstrap_file service_type network vcpu env_file rest
      | "--ports" :: p :: rest -> parse_service action name (Some p) bootstrap bootstrap_file service_type network vcpu env_file rest
      | "--bootstrap" :: b :: rest -> parse_service action name ports (Some b) bootstrap_file service_type network vcpu env_file rest
      | "--bootstrap-file" :: f :: rest -> parse_service action name ports bootstrap (Some f) service_type network vcpu env_file rest
      | "--type" :: t :: rest -> parse_service action name ports bootstrap bootstrap_file (Some t) network vcpu env_file rest
      | "-n" :: net :: rest -> parse_service action name ports bootstrap bootstrap_file service_type (Some net) vcpu env_file rest
      | "-v" :: v :: rest -> parse_service action name ports bootstrap bootstrap_file service_type network (Some (int_of_string v)) env_file rest
      | "--env-file" :: f :: rest -> parse_service action name ports bootstrap bootstrap_file service_type network vcpu (Some f) rest
      | "-e" :: _ :: rest -> parse_service action name ports bootstrap bootstrap_file service_type network vcpu env_file rest (* skip -e, already parsed *)
      | "-f" :: _ :: rest -> parse_service action name ports bootstrap bootstrap_file service_type network vcpu env_file rest (* skip -f, already parsed *)
      | _ :: rest -> parse_service action name ports bootstrap bootstrap_file service_type network vcpu env_file rest
    in
    parse_service "create" None None None None None None None None rest
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
