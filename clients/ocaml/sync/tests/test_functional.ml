#!/usr/bin/env ocaml

(*
 * Functional Tests for Un OCaml SDK
 *
 * Run with: ocaml test_functional.ml
 * Requires: UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY environment variables
 *
 * These tests make real API calls to api.unsandbox.com
 *)

(* ANSI colors *)
let blue = "\x1b[34m"
let red = "\x1b[31m"
let green = "\x1b[32m"
let yellow = "\x1b[33m"
let reset = "\x1b[0m"

(* API constants *)
let api_base = "https://api.unsandbox.com"
let portal_base = "https://unsandbox.com"

(* HMAC-SHA256 using openssl command *)
let hmac_sha256 secret message =
  let cmd = Printf.sprintf "echo -n '%s' | openssl dgst -sha256 -hmac '%s' | awk '{print $2}'"
    (Str.global_replace (Str.regexp "'") "'\\''" message)
    (Str.global_replace (Str.regexp "'") "'\\''" secret) in
  let ic = Unix.open_process_in cmd in
  let result = input_line ic in
  let _ = Unix.close_process_in ic in
  String.trim result

(* Build auth headers *)
let build_auth_headers public_key secret_key method_ path body =
  let timestamp = string_of_int (int_of_float (Unix.time ())) in
  let message = Printf.sprintf "%s:%s:%s:%s" timestamp method_ path body in
  let signature = hmac_sha256 secret_key message in
  Printf.sprintf " -H 'Authorization: Bearer %s' -H 'X-Timestamp: %s' -H 'X-Signature: %s'"
    public_key timestamp signature

(* HTTP helpers *)
let curl_get public_key secret_key endpoint =
  let auth_headers = build_auth_headers public_key secret_key "GET" endpoint "" in
  let cmd = Printf.sprintf "curl -s %s%s%s" api_base endpoint auth_headers in
  let ic = Unix.open_process_in cmd in
  let rec read_all acc =
    try let line = input_line ic in read_all (acc ^ line ^ "\n")
    with End_of_file -> acc
  in
  let output = read_all "" in
  let _ = Unix.close_process_in ic in
  output

let curl_post public_key secret_key endpoint json =
  let auth_headers = build_auth_headers public_key secret_key "POST" endpoint json in
  let tmp_file = Printf.sprintf "/tmp/un_ocaml_test_%d.json" (Random.int 999999) in
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
  output

let curl_post_portal public_key secret_key endpoint json =
  let auth_headers = build_auth_headers public_key secret_key "POST" endpoint json in
  let tmp_file = Printf.sprintf "/tmp/un_ocaml_test_%d.json" (Random.int 999999) in
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
  output

(* String contains helper *)
let contains haystack needle =
  try
    let _ = Str.search_forward (Str.regexp_string needle) haystack 0 in
    true
  with Not_found -> false

(* Test tracking *)
let passed = ref 0
let failed = ref 0

let run_test name test_fn =
  Printf.printf "  Running %s... " name;
  flush stdout;
  try
    test_fn ();
    Printf.printf "%sPASS%s\n" green reset;
    incr passed
  with e ->
    Printf.printf "%sFAIL%s\n" red reset;
    Printf.printf "    %s\n" (Printexc.to_string e);
    incr failed

let assert_true condition message =
  if not condition then failwith message

(* ============================================================================
   Functional Tests
   ============================================================================ *)

let test_health_check () =
  let cmd = Printf.sprintf "curl -s -o /dev/null -w '%%{http_code}' %s/health" api_base in
  let ic = Unix.open_process_in cmd in
  let status = try input_line ic with End_of_file -> "0" in
  let _ = Unix.close_process_in ic in
  assert_true (String.trim status = "200") "health check should return 200"

let test_validate_keys public_key secret_key =
  let response = curl_post_portal public_key secret_key "/keys/validate" "{}" in
  assert_true (contains response "\"valid\"" || contains response "\"status\"") "should contain valid or status"

let test_execute_python public_key secret_key =
  let json = "{\"language\":\"python\",\"code\":\"print(6 * 7)\"}" in
  let response = curl_post public_key secret_key "/execute" json in
  assert_true (contains response "42") "stdout should contain 42"

let test_execute_with_error public_key secret_key =
  let json = "{\"language\":\"python\",\"code\":\"import sys; sys.exit(1)\"}" in
  let response = curl_post public_key secret_key "/execute" json in
  assert_true (contains response "\"exit_code\":1" || contains response "\"exit_code\": 1") "exit_code should be 1"

let test_session_list public_key secret_key =
  let response = curl_get public_key secret_key "/sessions" in
  let trimmed = String.trim response in
  assert_true (String.length trimmed > 0 && (trimmed.[0] = '[' || trimmed.[0] = '{')) "response should be JSON"

let test_service_list public_key secret_key =
  let response = curl_get public_key secret_key "/services" in
  let trimmed = String.trim response in
  assert_true (String.length trimmed > 0 && (trimmed.[0] = '[' || trimmed.[0] = '{')) "response should be JSON"

let test_snapshot_list public_key secret_key =
  let response = curl_get public_key secret_key "/snapshots" in
  let trimmed = String.trim response in
  assert_true (String.length trimmed > 0 && (trimmed.[0] = '[' || trimmed.[0] = '{')) "response should be JSON"

let test_image_list public_key secret_key =
  let response = curl_get public_key secret_key "/images" in
  let trimmed = String.trim response in
  assert_true (String.length trimmed > 0 && (trimmed.[0] = '[' || trimmed.[0] = '{')) "response should be JSON"

let () =
  Random.self_init ();
  Printf.printf "\n%s=== Un OCaml SDK Functional Tests ===%s\n\n" blue reset;

  (* Check for credentials *)
  let public_key = try Some (Sys.getenv "UNSANDBOX_PUBLIC_KEY") with Not_found -> None in
  let secret_key = try Some (Sys.getenv "UNSANDBOX_SECRET_KEY") with Not_found -> None in

  match (public_key, secret_key) with
  | (Some pk, Some sk) ->
    run_test "health_check" test_health_check;
    run_test "validate_keys" (fun () -> test_validate_keys pk sk);
    run_test "execute_python" (fun () -> test_execute_python pk sk);
    run_test "execute_with_error" (fun () -> test_execute_with_error pk sk);
    run_test "session_list" (fun () -> test_session_list pk sk);
    run_test "service_list" (fun () -> test_service_list pk sk);
    run_test "snapshot_list" (fun () -> test_snapshot_list pk sk);
    run_test "image_list" (fun () -> test_image_list pk sk);

    let total = !passed + !failed in
    Printf.printf "\n%sResults: %d/%d passed%s\n" blue !passed total reset;

    if !failed > 0 then begin
      Printf.printf "%s%d test(s) failed%s\n" red !failed reset;
      exit 1
    end else
      Printf.printf "%sAll functional tests passed!%s\n" green reset

  | _ ->
    Printf.printf "%sSKIP: UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY not set%s\n" yellow reset
