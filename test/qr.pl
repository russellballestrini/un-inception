use Text::QRCode;
my $qr = Text::QRCode->new();
my $matrix = $qr->plot("unsandbox-qr-ok");
my $rows = scalar(@$matrix);
print "QR:unsandbox-qr-ok:ROWS:$rows\n";
