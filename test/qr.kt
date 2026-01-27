import com.google.zxing.BarcodeFormat
import com.google.zxing.qrcode.QRCodeWriter

fun main() {
    val w = QRCodeWriter()
    val m = w.encode("unsandbox-qr-ok", BarcodeFormat.QR_CODE, 0, 0)
    println("QR:unsandbox-qr-ok:ROWS:${m.height}")
}
