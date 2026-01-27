const QRCode = require('qrcode');
QRCode.toString('unsandbox-qr-ok', { type: 'utf8', margin: 0 }, function(err, str) {
    const rows = str.trim().split('\n').length;
    console.log('QR:unsandbox-qr-ok:ROWS:' + rows);
});
