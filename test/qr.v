#flag -lqrencode

#include <qrencode.h>

@[typedef]
struct C.QRcode {
	version int
	width   int
	data    &u8
}

fn C.QRcode_encodeString(str &u8, version int, level int, hint int, cs int) &C.QRcode
fn C.QRcode_free(qr &C.QRcode)

fn main() {
	qr := C.QRcode_encodeString('unsandbox-qr-ok'.str, 0, 1, 2, 1)
	println('QR:unsandbox-qr-ok:ROWS:${qr.width}')
	C.QRcode_free(qr)
}
