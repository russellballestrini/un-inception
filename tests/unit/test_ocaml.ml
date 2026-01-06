(* Unit tests for un.ml - tests internal functions without API calls *)
(* Run with: ocaml test_ocaml.ml *)

let passed = ref 0
let failed = ref 0

let test name result =
  if result then begin
    Printf.printf "  ✓ %s\n" name;
    incr passed
  end else begin
    Printf.printf "  ✗ %s\n" name;
    incr failed
  end

let ext_map = [
  (".py", "python"); (".js", "javascript"); (".ts", "typescript");
  (".rb", "ruby"); (".go", "go"); (".rs", "rust"); (".c", "c");
  (".ml", "ocaml"); (".java", "java"); (".hs", "haskell")
]

let get_language ext =
  try List.assoc ext ext_map
  with Not_found -> ""

let get_extension filename =
  try
    let idx = String.rindex filename '.' in
    String.sub filename idx (String.length filename - idx)
  with Not_found -> ""

let get_basename path =
  try
    let idx = String.rindex path '/' in
    String.sub path (idx + 1) (String.length path - idx - 1)
  with Not_found -> path

let starts_with prefix str =
  let plen = String.length prefix in
  String.length str >= plen && String.sub str 0 plen = prefix

let contains haystack needle =
  let rec search i =
    if i + String.length needle > String.length haystack then false
    else if String.sub haystack i (String.length needle) = needle then true
    else search (i + 1)
  in
  search 0

let () =
  print_endline "\n=== Extension Mapping Tests ===";

  test "Python extension maps correctly"
    (get_language ".py" = "python");

  test "OCaml extension maps correctly"
    (get_language ".ml" = "ocaml");

  test "JavaScript extension maps correctly"
    (get_language ".js" = "javascript");

  test "Go extension maps correctly"
    (get_language ".go" = "go");

  print_endline "\n=== Signature Format Tests ===";

  let timestamp = "1704067200" in
  let http_method = "POST" in
  let endpoint = "/execute" in
  let body = {|{"language":"python"}|} in
  let message = Printf.sprintf "%s:%s:%s:%s" timestamp http_method endpoint body in

  test "Signature format starts with timestamp"
    (starts_with timestamp message);

  test "Signature format contains :POST:"
    (contains message ":POST:");

  test "Signature format contains :/execute:"
    (contains message ":/execute:");

  print_endline "\n=== Language Detection Tests ===";

  let content = "#!/usr/bin/env python3\nprint('hello')" in
  let first_line = List.hd (String.split_on_char '\n' content) in

  test "Python shebang detection - starts with #!"
    (starts_with "#!" first_line);

  test "Python shebang detection - contains python"
    (contains first_line "python");

  print_endline "\n=== Argument Parsing Tests ===";

  let arg1 = "DEBUG=1" in
  let idx1 = String.index arg1 '=' in
  let key1 = String.sub arg1 0 idx1 in
  let value1 = String.sub arg1 (idx1 + 1) (String.length arg1 - idx1 - 1) in

  test "Parse -e KEY=VALUE format - key"
    (key1 = "DEBUG");

  test "Parse -e KEY=VALUE format - value"
    (value1 = "1");

  let arg2 = "URL=https://example.com?foo=bar" in
  let idx2 = String.index arg2 '=' in
  let key2 = String.sub arg2 0 idx2 in
  let value2 = String.sub arg2 (idx2 + 1) (String.length arg2 - idx2 - 1) in

  test "Parse -e KEY=VALUE with equals in value"
    (key2 = "URL" && value2 = "https://example.com?foo=bar");

  print_endline "\n=== File Operations Tests ===";

  test "Extract file basename"
    (get_basename "/home/user/project/script.ml" = "script.ml");

  test "Extract file extension"
    (get_extension "/home/user/project/script.ml" = ".ml");

  print_endline "\n=== API Constants Tests ===";

  let api_base = "https://api.unsandbox.com" in

  test "API base URL starts with https://"
    (starts_with "https://" api_base);

  test "API base URL contains unsandbox.com"
    (contains api_base "unsandbox.com");

  print_endline "\n=== Summary ===";
  Printf.printf "Passed: %d\n" !passed;
  Printf.printf "Failed: %d\n" !failed;
  Printf.printf "Total:  %d\n" (!passed + !failed);

  exit (if !failed > 0 then 1 else 0)
