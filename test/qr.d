import std.stdio;
import qr;

void main() {
    auto qrCode = QrCode("unsandbox-qr-ok");
    auto size = qrCode.size;
    writefln("QR:unsandbox-qr-ok:ROWS:%d", size);
}
