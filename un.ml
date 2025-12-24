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

(* Execute curl command *)
let curl_post api_key endpoint json =
  let cmd = Printf.sprintf "curl -s -X POST https://api.unsandbox.com%s -H 'Content-Type: application/json' -H 'Authorization: Bearer %s' -d '%s'"
    endpoint api_key json in
  let ic = Unix.open_process_in cmd in
  let output = read_file "/dev/stdin" in
  let _ = Unix.close_process_in ic in
  output

let curl_get api_key endpoint =
  let cmd = Printf.sprintf "curl -s https://api.unsandbox.com%s -H 'Authorization: Bearer %s'"
    endpoint api_key in
  let ic = Unix.open_process_in cmd in
  let rec read_all acc =
    try
      let line = input_line ic in
      read_all (acc ^ line ^ "\n")
    with End_of_file -> acc
  in
  let output = read_all "" in
  let _ = Unix.close_process_in ic in
  output

let curl_delete api_key endpoint =
  let cmd = Printf.sprintf "curl -s -X DELETE https://api.unsandbox.com%s -H 'Authorization: Bearer %s'"
    endpoint api_key in
  let ic = Unix.open_process_in cmd in
  let output = read_all "" where
    let rec read_all acc =
      try
        let line = input_line ic in
        read_all (acc ^ line ^ "\n")
      with End_of_file -> acc
    in read_all ""
  in
  let _ = Unix.close_process_in ic in
  output

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

(* Get API key *)
let get_api_key () =
  try Sys.getenv "UNSANDBOX_API_KEY"
  with Not_found ->
    Printf.fprintf stderr "Error: UNSANDBOX_API_KEY not set\n";
    exit 1

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

(* Session command *)
let session_command action shell network vcpu =
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
    let json = Printf.sprintf "{\"shell\":\"%s\"%s%s}" sh network_json vcpu_json in
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
let service_command action name ports bootstrap network vcpu =
  let api_key = get_api_key () in
  match action with
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
       Printf.fprintf stderr "Error: --sleep requires service ID\n";
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
       Printf.fprintf stderr "Error: --wake requires service ID\n";
       exit 1)
  | "destroy" ->
    (match name with
     | Some sid ->
       let response = curl_delete api_key ("/services/" ^ sid) in
       Printf.printf "%sService destroyed: %s%s\n" green sid reset
     | None ->
       Printf.fprintf stderr "Error: --destroy requires service ID\n";
       exit 1)
  | "create" ->
    (match name with
     | Some n ->
       let ports_json = match ports with Some p -> Printf.sprintf ",\"ports\":[%s]" p | None -> "" in
       let bootstrap_json = match bootstrap with Some b -> Printf.sprintf ",\"bootstrap\":\"%s\"" (escape_json b) | None -> "" in
       let network_json = match network with Some net -> Printf.sprintf ",\"network\":\"%s\"" net | None -> "" in
       let vcpu_json = match vcpu with Some v -> Printf.sprintf ",\"vcpu\":%d" v | None -> "" in
       let json = Printf.sprintf "{\"name\":\"%s\"%s%s%s%s}" n ports_json bootstrap_json network_json vcpu_json in
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
       Printf.printf "%s\n" response
     | None ->
       Printf.fprintf stderr "Error: --name required to create service\n";
       exit 1)
  | _ -> ()

(* Parse arguments *)
let () =
  Random.self_init ();
  let args = Array.to_list Sys.argv in
  match List.tl args with
  | [] ->
    Printf.printf "Usage: un.ml [options] <source_file>\n";
    Printf.printf "       un.ml session [options]\n";
    Printf.printf "       un.ml service [options]\n";
    exit 1
  | "session" :: rest ->
    let rec parse_session action shell network vcpu = function
      | [] -> session_command action shell network vcpu
      | "--list" :: rest -> parse_session "list" shell network vcpu rest
      | "--kill" :: id :: rest -> parse_session "kill" (Some id) network vcpu rest
      | "--shell" :: sh :: rest | "-s" :: sh :: rest -> parse_session action (Some sh) network vcpu rest
      | "-n" :: net :: rest -> parse_session action shell (Some net) vcpu rest
      | "-v" :: v :: rest -> parse_session action shell network (Some (int_of_string v)) rest
      | _ :: rest -> parse_session action shell network vcpu rest
    in
    parse_session "create" None None None rest
  | "service" :: rest ->
    let rec parse_service action name ports bootstrap network vcpu = function
      | [] -> service_command action name ports bootstrap network vcpu
      | "--list" :: rest -> parse_service "list" name ports bootstrap network vcpu rest
      | "--info" :: id :: rest -> parse_service "info" (Some id) ports bootstrap network vcpu rest
      | "--logs" :: id :: rest -> parse_service "logs" (Some id) ports bootstrap network vcpu rest
      | "--sleep" :: id :: rest -> parse_service "sleep" (Some id) ports bootstrap network vcpu rest
      | "--wake" :: id :: rest -> parse_service "wake" (Some id) ports bootstrap network vcpu rest
      | "--destroy" :: id :: rest -> parse_service "destroy" (Some id) ports bootstrap network vcpu rest
      | "--name" :: n :: rest -> parse_service "create" (Some n) ports bootstrap network vcpu rest
      | "--ports" :: p :: rest -> parse_service action name (Some p) bootstrap network vcpu rest
      | "--bootstrap" :: b :: rest -> parse_service action name ports (Some b) network vcpu rest
      | "-n" :: net :: rest -> parse_service action name ports bootstrap (Some net) vcpu rest
      | "-v" :: v :: rest -> parse_service action name ports bootstrap network (Some (int_of_string v)) rest
      | _ :: rest -> parse_service action name ports bootstrap network vcpu rest
    in
    parse_service "create" None None None None None rest
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
