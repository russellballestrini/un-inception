local qr = require("qrencode")
local ok, tab = qr.qrcode("unsandbox-qr-ok")
if ok then
    print(string.format("QR:unsandbox-qr-ok:ROWS:%d", #tab))
end
