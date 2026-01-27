#!/usr/bin/Rscript
library(qrcode)
qr <- qr_code("unsandbox-qr-ok")
rows <- nrow(qr)
cat(sprintf("QR:unsandbox-qr-ok:ROWS:%d\n", rows))
