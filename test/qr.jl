#!/usr/bin/env julia
using QRCoders
mat = qrcode("unsandbox-qr-ok")
rows = size(mat, 1)
println("QR:unsandbox-qr-ok:ROWS:$rows")
