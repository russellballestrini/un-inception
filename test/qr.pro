% Prolog QR code via libqrencode C FFI
:- use_module(library(ctypes)).

:- use_foreign_library(foreign(qrencode)).

% Define the struct layout: { int version, int width, unsigned char *data }
qr_width(Text, Width) :-
    c_alloc(CText, char[]),
    c_store(CText, Text),
    c_load(CText, CTextPtr),
    load_foreign_library(libqrencode, Lib),
    c_function(Lib, 'QRcode_encodeString',
        [pointer, int, int, int, int], pointer, EncFn),
    c_call(EncFn, [CTextPtr, 0, 1, 2, 1], QRPtr),
    c_load(QRPtr[1], Width),
    c_function(Lib, 'QRcode_free', [pointer], void, FreeFn),
    c_call(FreeFn, [QRPtr], _).

main :-
    (   catch(qr_width("unsandbox-qr-ok", W), _, fail)
    ->  format('QR:unsandbox-qr-ok:ROWS:~w~n', [W])
    ;   % SWI-Prolog FFI may not be available; use process interface
        process_create(path(qrencode),
            ['-t', 'ASCII', '-m', '0', 'unsandbox-qr-ok'],
            [stdout(pipe(Out))]),
        read_string(Out, _, Str),
        split_string(Str, "\n", "", Lines0),
        exclude(=(""), Lines0, Lines),
        length(Lines, Rows),
        format('QR:unsandbox-qr-ok:ROWS:~w~n', [Rows])
    ),
    halt.

:- initialization(main).
