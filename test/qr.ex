qr = EQRCode.encode("unsandbox-qr-ok")
matrix = qr.matrix
rows = length(matrix)
IO.puts("QR:unsandbox-qr-ok:ROWS:#{rows}")
