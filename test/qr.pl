use Imager::QRCode;
my $qr = Imager::QRCode->new(size => 1, margin => 0);
my $img = $qr->plot("unsandbox-qr-ok");
my $rows = $img->getheight();
print "QR:unsandbox-qr-ok:ROWS:$rows\n";
