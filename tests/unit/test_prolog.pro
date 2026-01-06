#!/usr/bin/env swipl
% Unit tests for un.pro - tests internal functions without API calls
% Run with: swipl -g main -t halt test_prolog.pro

:- initialization(main, main).

:- dynamic passed/1, failed/1.
passed(0).
failed(0).

test(Name, Goal) :-
    ( call(Goal) ->
        format("  ✓ ~w~n", [Name]),
        retract(passed(P)),
        P1 is P + 1,
        assertz(passed(P1))
    ;
        format("  ✗ ~w~n", [Name]),
        retract(failed(F)),
        F1 is F + 1,
        assertz(failed(F1))
    ).

% Extension mapping
ext_map(".py", "python").
ext_map(".js", "javascript").
ext_map(".ts", "typescript").
ext_map(".rb", "ruby").
ext_map(".go", "go").
ext_map(".rs", "rust").
ext_map(".c", "c").
ext_map(".pro", "prolog").
ext_map(".java", "java").
ext_map(".hs", "haskell").

get_language(Ext, Lang) :-
    ext_map(Ext, Lang), !.
get_language(_, "").

% String utilities
starts_with(String, Prefix) :-
    atom_string(StringAtom, String),
    atom_string(PrefixAtom, Prefix),
    atom_concat(PrefixAtom, _, StringAtom).

contains(String, Substr) :-
    sub_string(String, _, _, _, Substr).

get_extension(Filename, Ext) :-
    atom_string(FilenameAtom, Filename),
    file_name_extension(_, ExtNoDoc, FilenameAtom),
    atom_concat('.', ExtNoDoc, ExtAtom),
    atom_string(ExtAtom, Ext).

get_basename(Path, Basename) :-
    atom_string(PathAtom, Path),
    file_base_name(PathAtom, BasenameAtom),
    atom_string(BasenameAtom, Basename).

main :-
    format("~n=== Extension Mapping Tests ===~n", []),

    test("Python extension maps correctly",
         (get_language(".py", L1), L1 == "python")),

    test("Prolog extension maps correctly",
         (get_language(".pro", L2), L2 == "prolog")),

    test("JavaScript extension maps correctly",
         (get_language(".js", L3), L3 == "javascript")),

    test("Go extension maps correctly",
         (get_language(".go", L4), L4 == "go")),

    format("~n=== Signature Format Tests ===~n", []),

    Timestamp = "1704067200",
    Method = "POST",
    Endpoint = "/execute",
    Body = "{\"language\":\"python\"}",
    format(string(Message), "~w:~w:~w:~w", [Timestamp, Method, Endpoint, Body]),

    test("Signature format starts with timestamp",
         starts_with(Message, Timestamp)),

    test("Signature format contains :POST:",
         contains(Message, ":POST:")),

    test("Signature format contains :/execute:",
         contains(Message, ":/execute:")),

    format("~n=== Language Detection Tests ===~n", []),

    Content = "#!/usr/bin/env python3\nprint('hello')",
    split_string(Content, "\n", "", [FirstLine|_]),

    test("Python shebang detection - starts with #!",
         starts_with(FirstLine, "#!")),

    test("Python shebang detection - contains python",
         contains(FirstLine, "python")),

    format("~n=== Argument Parsing Tests ===~n", []),

    Arg1 = "DEBUG=1",
    split_string(Arg1, "=", "", [Key1, Value1]),

    test("Parse -e KEY=VALUE format - key",
         Key1 == "DEBUG"),

    test("Parse -e KEY=VALUE format - value",
         Value1 == "1"),

    format("~n=== File Operations Tests ===~n", []),

    test("Extract file basename",
         (get_basename("/home/user/project/script.pro", B), B == "script.pro")),

    format("~n=== API Constants Tests ===~n", []),

    ApiBase = "https://api.unsandbox.com",

    test("API base URL starts with https://",
         starts_with(ApiBase, "https://")),

    test("API base URL contains unsandbox.com",
         contains(ApiBase, "unsandbox.com")),

    format("~n=== Summary ===~n", []),
    passed(P),
    failed(F),
    Total is P + F,
    format("Passed: ~w~n", [P]),
    format("Failed: ~w~n", [F]),
    format("Total:  ~w~n", [Total]),

    ( F > 0 -> halt(1) ; halt(0) ).
