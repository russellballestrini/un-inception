import com.google.zxing.*;
import com.google.zxing.common.BitMatrix;
import com.google.zxing.qrcode.QRCodeWriter;

public class qr {
    public static void main(String[] args) throws Exception {
        QRCodeWriter w = new QRCodeWriter();
        BitMatrix m = w.encode("unsandbox-qr-ok", BarcodeFormat.QR_CODE, 0, 0);
        System.out.println("QR:unsandbox-qr-ok:ROWS:" + m.getHeight());
    }
}
