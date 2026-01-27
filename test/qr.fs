open QRCoder

let gen = QRCodeGenerator()
let data = gen.CreateQrCode("unsandbox-qr-ok", QRCodeGenerator.ECCLevel.M)
let rows = data.ModuleMatrix.Count
printfn "QR:unsandbox-qr-ok:ROWS:%d" rows
