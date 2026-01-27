fn main() {
    let qr = qrcode::QrCode::new("unsandbox-qr-ok").unwrap();
    let w = qr.width();
    println!("QR:unsandbox-qr-ok:ROWS:{}", w);
}
