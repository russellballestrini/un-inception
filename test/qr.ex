# Use Erlang's :qrcode module (pre-cached via rebar3)
# Falls back to shell if module not available
try do
  {:ok, qr} = :qrcode.encode("unsandbox-qr-ok")
  # qr is a tuple like {:qrcode, version, ecc, dimension, data}
  {_tag, _version, _ecc, dimension, _data} = qr
  IO.puts("QR:unsandbox-qr-ok:ROWS:#{dimension}")
rescue
  _ ->
    # Fallback: use qrencode CLI
    {output, 0} = System.cmd("sh", ["-c", "qrencode -t ASCII -m 0 'unsandbox-qr-ok' | wc -l"])
    rows = String.trim(output)
    IO.puts("QR:unsandbox-qr-ok:ROWS:#{rows}")
end
