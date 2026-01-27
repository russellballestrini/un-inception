require "qr-code"

qr = QRCode.new("unsandbox-qr-ok")
rows = qr.modules.size
puts "QR:unsandbox-qr-ok:ROWS:#{rows}"
