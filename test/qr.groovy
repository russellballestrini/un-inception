#!/usr/bin/env groovy
import com.google.zxing.BarcodeFormat
import com.google.zxing.qrcode.QRCodeWriter

def w = new QRCodeWriter()
def m = w.encode("unsandbox-qr-ok", BarcodeFormat.QR_CODE, 0, 0)
println "QR:unsandbox-qr-ok:ROWS:${m.height}"
