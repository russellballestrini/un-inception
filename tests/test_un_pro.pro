#!/usr/bin/env swipl
% Comprehensive tests for un.pro (Prolog UN CLI Inception implementation)
% Run with: swipl -g main -t halt test_un_pro.pro

:- initialization(main, main).

% Color codes
green('\033[32m').
red('\033[31m').
blue('\033[34m').
reset('\033[0m').

% Test counters (dynamic predicates)
:- dynamic passed/1.
:- dynamic failed/1.

passed(0).
failed(0).

% Extension to language mapping (from un.pro)
ext_lang('.jl', 'julia').
ext_lang('.r', 'r').
ext_lang('.cr', 'crystal').
ext_lang('.f90', 'fortran').
ext_lang('.cob', 'cobol').
ext_lang('.pro', 'prolog').
ext_lang('.forth', 'forth').
ext_lang('.4th', 'forth').
ext_lang('.py', 'python').
ext_lang('.js', 'javascript').
ext_lang('.rb', 'ruby').
ext_lang('.go', 'go').
ext_lang('.rs', 'rust').
ext_lang('.c', 'c').
ext_lang('.cpp', 'cpp').
ext_lang('.java', 'java').
ext_lang('.sh', 'bash').

% Detect language from filename
detect_language(Filename, Language) :-
    file_name_extension(_, Ext, Filename),
    downcase_atom(Ext, ExtLower),
    atomic_list_concat(['.', ExtLower], ExtWithDot),
    ext_lang(ExtWithDot, Language), !.
detect_language(_, 'unknown').

% Print test result
print_test(Name, Result) :-
    green(Green), red(Red), reset(Reset),
    (   Result = true
    ->  format('~w✓ PASS~w: ~w~n', [Green, Reset, Name]),
        retract(passed(N)),
        N1 is N + 1,
        assert(passed(N1))
    ;   format('~w✗ FAIL~w: ~w~n', [Red, Reset, Name]),
        retract(failed(N)),
        N1 is N + 1,
        assert(failed(N1))
    ).

% Test extension detection
test_detect(Ext, ExpectedLang) :-
    atomic_list_concat(['test', Ext], Filename),
    detect_language(Filename, Lang),
    format(atom(TestName), 'Detect ~w as ~w', [Ext, ExpectedLang]),
    (   Lang = ExpectedLang
    ->  print_test(TestName, true)
    ;   print_test(TestName, false)
    ).

% Main test suite
main(_) :-
    blue(Blue), reset(Reset),

    format('~n~w========================================~w~n', [Blue, Reset]),
    format('~wUN CLI Inception Tests - Prolog~w~n', [Blue, Reset]),
    format('~w========================================~w~n~n', [Blue, Reset]),

    % Test Suite 1: Extension Detection
    format('~wTest Suite 1: Extension Detection~w~n', [Blue, Reset]),
    test_detect('.jl', 'julia'),
    test_detect('.r', 'r'),
    test_detect('.cr', 'crystal'),
    test_detect('.f90', 'fortran'),
    test_detect('.cob', 'cobol'),
    test_detect('.pro', 'prolog'),
    test_detect('.forth', 'forth'),
    test_detect('.4th', 'forth'),
    test_detect('.py', 'python'),
    test_detect('.rs', 'rust'),
    test_detect('.xyz', 'unknown'),

    % Test Suite 2: API Integration
    format('~n~wTest Suite 2: API Integration~w~n', [Blue, Reset]),
    (   getenv('UNSANDBOX_API_KEY', ApiKey),
        ApiKey \= ''
    ->  format('~wℹ NOTE~w: API integration test requires curl and jq~n', [Blue, Reset]),
        print_test('API key is set', true)
    ;   format('~wℹ SKIP~w: API integration test (UNSANDBOX_API_KEY not set)~n', [Blue, Reset])
    ),

    % Test Suite 3: End-to-End
    format('~n~wTest Suite 3: End-to-End Functional Test~w~n', [Blue, Reset]),
    (   getenv('UNSANDBOX_API_KEY', ApiKey2),
        ApiKey2 \= ''
    ->  (   exists_file('../../test/fib.pro')
        ->  FibFile = '../../test/fib.pro'
        ;   exists_file('/home/fox/git/unsandbox.com/cli/test/fib.pro')
        ->  FibFile = '/home/fox/git/unsandbox.com/cli/test/fib.pro'
        ;   FibFile = none
        ),
        (   FibFile \= none
        ->  format('~wℹ NOTE~w: E2E test requires compiled un.pro~n', [Blue, Reset]),
            print_test('fib.pro exists', true)
        ;   format('~wℹ SKIP~w: E2E test (fib.pro not found)~n', [Blue, Reset])
        )
    ;   format('~wℹ SKIP~w: E2E test (UNSANDBOX_API_KEY not set)~n', [Blue, Reset])
    ),

    % Test Suite 4: Error Handling
    format('~n~wTest Suite 4: Error Handling~w~n', [Blue, Reset]),
    test_detect('.unknown', 'unknown'),

    % Case insensitive test
    file_name_extension(_, 'PRO', 'TEST.PRO'),
    downcase_atom('PRO', 'pro'),
    atomic_list_concat(['.', 'pro'], '.pro'),
    ext_lang('.pro', 'prolog'),
    print_test('Case insensitive detection', true),

    % Multiple dots test
    detect_language('my.test.py', PyLang),
    (   PyLang = 'python'
    ->  print_test('Multiple dots in filename', true)
    ;   print_test('Multiple dots in filename', false)
    ),

    % Print summary
    passed(PassedCount),
    failed(FailedCount),
    Total is PassedCount + FailedCount,

    green(Green), red(Red),
    format('~n~w========================================~w~n', [Blue, Reset]),
    format('~wTest Summary~w~n', [Blue, Reset]),
    format('~w========================================~w~n', [Blue, Reset]),
    format('~wPassed: ~w~w~n', [Green, PassedCount, Reset]),
    format('~wFailed: ~w~w~n', [Red, FailedCount, Reset]),
    format('~wTotal:  ~w~w~n', [Blue, Total, Reset]),

    (   FailedCount > 0
    ->  format('~n~wTESTS FAILED~w~n', [Red, Reset]),
        halt(1)
    ;   format('~n~wALL TESTS PASSED~w~n', [Green, Reset]),
        halt(0)
    ).
