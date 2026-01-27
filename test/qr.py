#!/usr/bin/env python3
import qrcode
q = qrcode.QRCode(border=0)
q.add_data("unsandbox-qr-ok")
q.make()
rows = len(q.get_matrix())
print(f"QR:unsandbox-qr-ok:ROWS:{rows}")
