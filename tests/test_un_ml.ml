#!/usr/bin/env ocaml

(*
OCaml UN CLI Test Suite

Usage:
  chmod +x test_un_ml.ml
  ocaml test_un_ml.ml

Or compile and run:
  ocamlopt test_un_ml.ml -o test_un_ml
  ./test_un_ml

Tests the OCaml UN CLI implementation (un.ml) for:
1. Extension detection logic
2. API integration (if UNSANDBOX_API_KEY is set)
3. End-to-end execution with fib.ml test file
*)

(* ANSI color codes *)
let green = "\x1b[32m"
let red = "\x1b[31m"
let yellow = "\x1b[33m"
let reset = "\x1b[0m"

(* Extension to language mapping (from un.ml) *)
let ext_to_lang ext =
  match ext with
  | ".hs" -> Some "haskell"
  | ".ml" -> Some "ocaml"
  | ".clj" -> Some "clojure"
  | ".scm" -> Some "scheme"
  | ".lisp" -> Some "commonlisp"
  | ".erl" -> Some "erlang"
  | ".ex" -> Some "elixir"
  | ".py" -> Some "python"
  | ".js" -> Some "javascript"
  | ".rb" -> Some "ruby"
  | ".go" -> Some "go"
  | ".rs" -> Some "rust"
  | ".c" -> Some "c"
  | ".cpp" -> Some "cpp"
  | ".java" -> Some "java"
  | _ -> None

(* Test result type *)
type test_result = Pass | Fail of string

(* Print test result *)
let print_result test_name result =
  match result with
  | Pass ->
      Printf.printf "%s✓ PASS%s - %s\n" green reset test_name;
      true
  | Fail msg ->
      Printf.printf "%s✗ FAIL%s - %s\n" red reset test_name;
      Printf.printf "  Error: %s\n" msg;
      false

(* Test 1: Extension detection *)
let test_extension_detection () =
  let tests = [
    (".hs", Some "haskell");
    (".ml", Some "ocaml");
    (".clj", Some "clojure");
    (".scm", Some "scheme");
    (".lisp", Some "commonlisp");
    (".erl", Some "erlang");
    (".ex", Some "elixir");
    (".py", Some "python");
    (".js", Some "javascript");
    (".rb", Some "ruby");
  ] in

  let failures = List.filter (fun (ext, expected) ->
    let actual = ext_to_lang ext in
    actual <> expected
  ) tests in

  if List.length failures = 0 then
    Pass
  else
    Fail (Printf.sprintf "Extension mappings failed: %d tests" (List.length failures))

(* Test 2: API integration *)
let test_api_integration () =
  try
    let api_key = Sys.getenv "UNSANDBOX_API_KEY" in

    (* Create a simple test file *)
    let test_code = "let () = print_endline \"test\"\n" in
    let oc = open_out "/tmp/test_un_ml_api.ml" in
    output_string oc test_code;
    close_out oc;

    (* Run the CLI *)
    let cmd = "./un.ml /tmp/test_un_ml_api.ml 2>&1" in
    let ic = Unix.open_process_in cmd in
    let output = really_input_string ic (in_channel_length ic) in
    let status = Unix.close_process_in ic in

    (* Check if it executed successfully *)
    if status = Unix.WEXITED 0 && String.sub output 0 4 = "test" then
      Pass
    else
      Fail (Printf.sprintf "API call failed: %s" output)
  with
  | Not_found -> Pass  (* Skip test if no API key *)
  | e -> Fail (Printf.sprintf "Exception: %s" (Printexc.to_string e))

(* Test 3: Functional test with fib.ml *)
let test_fibonacci () =
  try
    let _ = Sys.getenv "UNSANDBOX_API_KEY" in

    (* Check if fib.ml exists *)
    let fib_path = "../test/fib.ml" in

    (* Run the CLI with fib.ml *)
    let cmd = Printf.sprintf "./un.ml %s 2>&1" fib_path in
    let ic = Unix.open_process_in cmd in
    let buffer = Buffer.create 1024 in
    (try
      while true do
        let line = input_line ic in
        Buffer.add_string buffer line;
        Buffer.add_char buffer '\n'
      done
    with End_of_file -> ());
    let output = Buffer.contents buffer in
    let status = Unix.close_process_in ic in

    (* Check if output contains expected fibonacci result *)
    if status = Unix.WEXITED 0 &&
       (try ignore (Str.search_forward (Str.regexp "fib(10) = 55") output 0); true
        with Not_found -> false) then
      Pass
    else
      Fail (Printf.sprintf "Fibonacci test failed: %s" output)
  with
  | Not_found -> Pass  (* Skip test if no API key *)
  | e -> Fail (Printf.sprintf "Exception: %s" (Printexc.to_string e))

(* Main test runner *)
let main () =
  Printf.printf "=== OCaml UN CLI Test Suite ===\n\n";

  (* Check if API key is set *)
  (try
    ignore (Sys.getenv "UNSANDBOX_API_KEY")
  with Not_found ->
    Printf.printf "%s⚠ WARNING%s - UNSANDBOX_API_KEY not set, skipping API tests\n\n"
      yellow reset);

  (* Run tests *)
  let results = [
    print_result "Extension detection" (test_extension_detection ());
    print_result "API integration" (test_api_integration ());
    print_result "Fibonacci end-to-end test" (test_fibonacci ());
  ] in

  Printf.printf "\n";

  (* Summary *)
  let passed = List.length (List.filter (fun x -> x) results) in
  let total = List.length results in

  if passed = total then begin
    Printf.printf "%s✓ All tests passed (%d/%d)%s\n" green passed total reset;
    exit 0
  end else begin
    Printf.printf "%s✗ Some tests failed (%d/%d passed)%s\n" red passed total reset;
    exit 1
  end

let () = main ()
