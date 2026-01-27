<?php
require_once '/usr/share/php/phpqrcode/qrlib.php';

$matrix = QRcode::text("unsandbox-qr-ok", false, QR_ECLEVEL_M);
$rows = count($matrix);
echo "QR:unsandbox-qr-ok:ROWS:$rows\n";
