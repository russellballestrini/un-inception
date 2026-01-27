(import '[com.google.zxing BarcodeFormat]
        '[com.google.zxing.qrcode QRCodeWriter])

(let [writer (QRCodeWriter.)
      matrix (.encode writer "unsandbox-qr-ok" BarcodeFormat/QR_CODE 0 0)
      rows (.getHeight matrix)]
  (println (str "QR:unsandbox-qr-ok:ROWS:" rows)))
