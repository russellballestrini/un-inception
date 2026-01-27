require 'rqrcode'
qr = RQRCode::QRCode.new('unsandbox-qr-ok')
rows = qr.modules.length
puts "QR:unsandbox-qr-ok:ROWS:#{rows}"
