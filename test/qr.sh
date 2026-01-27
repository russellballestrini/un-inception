#!/bin/bash
# Bash QR: qrencode is the native CLI for libqrencode
OUTPUT=$(qrencode -t UTF8 -m 0 "unsandbox-qr-ok")
ROWS=$(echo "$OUTPUT" | wc -l)
echo "QR:unsandbox-qr-ok:ROWS:$ROWS"
