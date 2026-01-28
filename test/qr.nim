import dynlib

type
  QRcode {.pure.} = object
    version: cint
    width: cint
    data: pointer

type
  QRencodeString = proc(s: cstring, ver: cint, level: cint, hint: cint, cs: cint): ptr QRcode {.cdecl.}
  QRfree = proc(qr: ptr QRcode) {.cdecl.}

let lib = loadLib("libqrencode.so")
if lib == nil:
  quit("Failed to load libqrencode.so", 1)

let encode = cast[QRencodeString](lib.symAddr("QRcode_encodeString"))
let free_qr = cast[QRfree](lib.symAddr("QRcode_free"))

let qr = encode("unsandbox-qr-ok", 0, 1, 2, 1)
if qr != nil:
  echo "QR:unsandbox-qr-ok:ROWS:", qr.width
  free_qr(qr)
else:
  echo "QR encode failed"

unloadLib(lib)
