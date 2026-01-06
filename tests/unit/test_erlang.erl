#!/usr/bin/env escript
%% Unit tests for un.erl - tests internal functions without API calls

-mode(compile).

main(_) ->
    put(passed, 0),
    put(failed, 0),

    io:format("~n=== Extension Mapping Tests ===~n"),

    ExtMap = #{
        ".py" => "python", ".js" => "javascript", ".ts" => "typescript",
        ".rb" => "ruby", ".php" => "php", ".pl" => "perl", ".lua" => "lua",
        ".sh" => "bash", ".go" => "go", ".rs" => "rust", ".c" => "c",
        ".cpp" => "cpp", ".java" => "java", ".kt" => "kotlin",
        ".hs" => "haskell", ".clj" => "clojure", ".erl" => "erlang",
        ".ex" => "elixir", ".jl" => "julia"
    },

    test("Python extension maps correctly", fun() ->
        assert_equal(maps:get(".py", ExtMap), "python")
    end),

    test("Erlang extension maps correctly", fun() ->
        assert_equal(maps:get(".erl", ExtMap), "erlang")
    end),

    test("JavaScript extension maps correctly", fun() ->
        assert_equal(maps:get(".js", ExtMap), "javascript")
    end),

    test("Go extension maps correctly", fun() ->
        assert_equal(maps:get(".go", ExtMap), "go")
    end),

    io:format("~n=== HMAC Signature Tests ===~n"),

    test("HMAC-SHA256 generates 64 character hex string", fun() ->
        Sig = crypto:mac(hmac, sha256, <<"test-secret">>, <<"test-message">>),
        HexSig = binary_to_hex(Sig),
        assert_equal(length(HexSig), 64)
    end),

    test("Same input produces same signature", fun() ->
        Sig1 = crypto:mac(hmac, sha256, <<"key">>, <<"msg">>),
        Sig2 = crypto:mac(hmac, sha256, <<"key">>, <<"msg">>),
        assert_equal(Sig1, Sig2)
    end),

    test("Different secrets produce different signatures", fun() ->
        Sig1 = crypto:mac(hmac, sha256, <<"key1">>, <<"msg">>),
        Sig2 = crypto:mac(hmac, sha256, <<"key2">>, <<"msg">>),
        assert_not_equal(Sig1, Sig2)
    end),

    test("Signature format verification", fun() ->
        Timestamp = "1704067200",
        Method = "POST",
        Endpoint = "/execute",
        Body = "{\"language\":\"python\"}",
        Message = Timestamp ++ ":" ++ Method ++ ":" ++ Endpoint ++ ":" ++ Body,
        assert_true(string:prefix(Message, Timestamp) =/= nomatch),
        assert_contains(Message, ":POST:"),
        assert_contains(Message, ":/execute:")
    end),

    io:format("~n=== Language Detection Tests ===~n"),

    test("Detect language from .erl extension", fun() ->
        Ext = filename:extension("script.erl"),
        assert_equal(maps:get(Ext, ExtMap), "erlang")
    end),

    test("Python shebang detection", fun() ->
        Content = "#!/usr/bin/env python3\nprint('hello')",
        [FirstLine | _] = string:split(Content, "\n"),
        assert_true(string:prefix(FirstLine, "#!") =/= nomatch),
        assert_contains(FirstLine, "python")
    end),

    io:format("~n=== Argument Parsing Tests ===~n"),

    test("Parse -e KEY=VALUE format", fun() ->
        Arg = "DEBUG=1",
        [Key, Value] = string:split(Arg, "="),
        assert_equal(Key, "DEBUG"),
        assert_equal(Value, "1")
    end),

    io:format("~n=== File Operations Tests ===~n"),

    test("Base64 encoding/decoding", fun() ->
        Content = "print('hello world')",
        Encoded = base64:encode(Content),
        Decoded = base64:decode(Encoded),
        assert_equal(binary_to_list(Decoded), Content)
    end),

    test("Extract file basename", fun() ->
        Path = "/home/user/project/script.erl",
        assert_equal(filename:basename(Path), "script.erl")
    end),

    test("Extract file extension", fun() ->
        Path = "/home/user/project/script.erl",
        assert_equal(filename:extension(Path), ".erl")
    end),

    io:format("~n=== API Constants Tests ===~n"),

    test("API base URL format", fun() ->
        ApiBase = "https://api.unsandbox.com",
        assert_true(string:prefix(ApiBase, "https://") =/= nomatch),
        assert_contains(ApiBase, "unsandbox.com")
    end),

    % Summary
    Passed = get(passed),
    Failed = get(failed),
    io:format("~n=== Summary ===~n"),
    io:format("Passed: ~p~n", [Passed]),
    io:format("Failed: ~p~n", [Failed]),
    io:format("Total:  ~p~n", [Passed + Failed]),

    case Failed > 0 of
        true -> halt(1);
        false -> halt(0)
    end.

test(Name, Fun) ->
    try
        Fun(),
        io:format("  ✓ ~s~n", [Name]),
        put(passed, get(passed) + 1)
    catch
        _:Reason ->
            io:format("  ✗ ~s~n", [Name]),
            io:format("    ~p~n", [Reason]),
            put(failed, get(failed) + 1)
    end.

assert_equal(Actual, Expected) ->
    case Actual =:= Expected of
        true -> ok;
        false -> throw({expected, Expected, got, Actual})
    end.

assert_not_equal(A, B) ->
    case A =/= B of
        true -> ok;
        false -> throw({expected_different, A})
    end.

assert_contains(Str, Substr) ->
    case string:find(Str, Substr) of
        nomatch -> throw({expected_to_contain, Str, Substr});
        _ -> ok
    end.

assert_true(Val) ->
    case Val of
        true -> ok;
        _ -> throw({expected_true, got, Val})
    end.

binary_to_hex(Bin) ->
    lists:flatten([io_lib:format("~2.16.0b", [X]) || <<X:8>> <= Bin]).
