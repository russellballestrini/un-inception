BEGIN {
    # AWK QR: pipes to qrencode, the native CLI for libqrencode
    cmd = "qrencode -t UTF8 -m 0 'unsandbox-qr-ok'"
    rows = 0
    while ((cmd | getline line) > 0) {
        rows++
    }
    close(cmd)
    print "QR:unsandbox-qr-ok:ROWS:" rows
}
