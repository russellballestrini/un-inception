\c #include <dlfcn.h>
\c #include <stdio.h>
\c typedef struct { int version; int width; unsigned char *data; } QRcode;
\c static void *qrlib;
\c static QRcode* (*qr_enc)(const char*,int,int,int,int);
\c static void (*qr_fr)(QRcode*);
\c void qr_init(void) {
\c   qrlib = dlopen("libqrencode.so", 2);
\c   qr_enc = dlsym(qrlib, "QRcode_encodeString");
\c   qr_fr = dlsym(qrlib, "QRcode_free");
\c }
\c int qr_width(void) {
\c   QRcode *qr = qr_enc("unsandbox-qr-ok", 0, 1, 2, 1);
\c   int w = qr->width;
\c   qr_fr(qr);
\c   return w;
\c }

c-function qr-init qr_init -- void
c-function qr-width qr_width -- n

qr-init
." QR:unsandbox-qr-ok:ROWS:" qr-width 0 .r cr
bye
