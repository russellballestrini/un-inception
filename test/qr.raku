#!/usr/bin/env raku
use Text::QRCode;

my $qr = Text::QRCode.new();
my @matrix = $qr.plot("unsandbox-qr-ok");
my $rows = @matrix.elems;
say "QR:unsandbox-qr-ok:ROWS:$rows";
