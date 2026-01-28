@[Link("qrencode")]
lib LibQRencode
  struct QRcode
    version : LibC::Int
    width : LibC::Int
    data : UInt8*
  end

  fun QRcode_encodeString(s : LibC::Char*, version : LibC::Int, level : LibC::Int, hint : LibC::Int, casesensitive : LibC::Int) : QRcode*
  fun QRcode_free(qr : QRcode*)
end

qr = LibQRencode.QRcode_encodeString("unsandbox-qr-ok", 0, 1, 2, 1)
if qr.null?
  puts "QR encode failed"
  exit 1
end
puts "QR:unsandbox-qr-ok:ROWS:#{qr.value.width}"
LibQRencode.QRcode_free(qr)
