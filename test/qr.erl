-module(qr).
-export([main/1]).

main(_) ->
    QR = qrcode:encode(<<"unsandbox-qr-ok">>),
    Dim = element(4, QR),
    io:format("QR:unsandbox-qr-ok:ROWS:~p~n", [Dim]),
    halt().
