#!/usr/bin/env ocaml

(*
 * Unit Tests for Un OCaml SDK Library Functions
 *
 * Run with: ocaml test_library.ml
 * No credentials required - tests pure library functions only.
 *)

(* ANSI colors *)
let blue = "\x1b[34m"
let red = "\x1b[31m"
let green = "\x1b[32m"
let yellow = "\x1b[33m"
let reset = "\x1b[0m"

(* Inline implementation for testing (matches un.ml) *)
let sdk_version = "4.2.0"

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

let get_extension filename =
  try
    let dot_pos = String.rindex filename '.' in
    String.sub filename dot_pos (String.length filename - dot_pos)
  with Not_found -> ""

let detect_language filename =
  let ext = get_extension filename in
  ext_to_lang ext

(* HMAC-SHA256 using openssl command *)
let hmac_sha256 secret message =
  let cmd = Printf.sprintf "echo -n '%s' | openssl dgst -sha256 -hmac '%s' | awk '{print $2}'"
    (Str.global_replace (Str.regexp "'") "'\\''" message)
    (Str.global_replace (Str.regexp "'") "'\\''" secret) in
  let ic = Unix.open_process_in cmd in
  let result = input_line ic in
  let _ = Unix.close_process_in ic in
  String.trim result

let hmac_sign secret message = hmac_sha256 secret message

(* Test tracking *)
let passed = ref 0
let failed = ref 0

let run_test name test_fn =
  try
    test_fn ();
    Printf.printf "%sPASS%s: %s\n" green reset name;
    incr passed
  with e ->
    Printf.printf "%sFAIL%s: %s - %s\n" red reset name (Printexc.to_string e);
    incr failed

let assert_true condition message =
  if not condition then failwith message

let assert_equal a b message =
  if a <> b then failwith (message ^ " (got: " ^ a ^ ")")

let assert_some opt message =
  match opt with
  | Some _ -> ()
  | None -> failwith message

let assert_none opt message =
  match opt with
  | None -> ()
  | Some _ -> failwith message

(* ============================================================================
   Unit Tests
   ============================================================================ *)

let test_version () =
  let version = sdk_version in
  assert_true (String.length version > 0) "version should be non-empty";
  (* Check semver format X.Y.Z *)
  let parts = String.split_on_char '.' version in
  assert_true (List.length parts = 3) "version should be semver format"

let test_detect_language () =
  (* Test common extensions *)
  assert_equal (match detect_language "script.py" with Some s -> s | None -> "") "python" "python";
  assert_equal (match detect_language "app.js" with Some s -> s | None -> "") "javascript" "javascript";
  assert_equal (match detect_language "main.go" with Some s -> s | None -> "") "go" "go";
  assert_equal (match detect_language "main.rs" with Some s -> s | None -> "") "rust" "rust";
  assert_equal (match detect_language "main.c" with Some s -> s | None -> "") "c" "c";
  assert_equal (match detect_language "main.cpp" with Some s -> s | None -> "") "cpp" "cpp";
  assert_equal (match detect_language "Main.java" with Some s -> s | None -> "") "java" "java";
  assert_equal (match detect_language "script.rb" with Some s -> s | None -> "") "ruby" "ruby";
  assert_equal (match detect_language "script.sh" with Some s -> s | None -> "") "bash" "bash";
  assert_equal (match detect_language "script.lua" with Some s -> s | None -> "") "lua" "lua";
  assert_equal (match detect_language "script.pl" with Some s -> s | None -> "") "perl" "perl";
  assert_equal (match detect_language "index.php" with Some s -> s | None -> "") "php" "php";
  assert_equal (match detect_language "main.hs" with Some s -> s | None -> "") "haskell" "haskell";
  assert_equal (match detect_language "main.ml" with Some s -> s | None -> "") "ocaml" "ocaml";
  assert_equal (match detect_language "main.ex" with Some s -> s | None -> "") "elixir" "elixir";
  assert_equal (match detect_language "main.erl" with Some s -> s | None -> "") "erlang" "erlang";

  (* Test with paths *)
  assert_equal (match detect_language "/path/to/script.py" with Some s -> s | None -> "") "python" "path/python";

  (* Test unknown extensions *)
  assert_none (detect_language "Makefile") "Makefile should be None";
  assert_none (detect_language "README") "README should be None";
  assert_none (detect_language "script.unknown") "unknown should be None"

let test_hmac_sign () =
  let signature = hmac_sign "my_secret" "test message" in
  assert_true (String.length signature = 64) "signature should be 64 hex characters";
  (* Should be lowercase hex *)
  let is_hex_char c = (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') in
  let all_hex = String.to_seq signature |> Seq.for_all is_hex_char in
  assert_true all_hex "signature should be lowercase hex"

let test_hmac_sign_deterministic () =
  let sig1 = hmac_sign "test_secret" "same message" in
  let sig2 = hmac_sign "test_secret" "same message" in
  assert_equal sig1 sig2 "same inputs should produce same signature"

let test_hmac_sign_different_secrets () =
  let sig1 = hmac_sign "secret1" "test message" in
  let sig2 = hmac_sign "secret2" "test message" in
  assert_true (sig1 <> sig2) "different secrets should produce different signatures"

let () =
  Printf.printf "\n%s=== Un OCaml SDK Library Tests ===%s\n\n" blue reset;

  run_test "version" test_version;
  run_test "detect_language" test_detect_language;
  run_test "hmac_sign" test_hmac_sign;
  run_test "hmac_sign_deterministic" test_hmac_sign_deterministic;
  run_test "hmac_sign_different_secrets" test_hmac_sign_different_secrets;

  let total = !passed + !failed in
  Printf.printf "\n%sResults: %d/%d passed%s\n" blue !passed total reset;

  if !failed > 0 then begin
    Printf.printf "%s%d test(s) failed%s\n" red !failed reset;
    exit 1
  end else
    Printf.printf "%sAll tests passed!%s\n" green reset
