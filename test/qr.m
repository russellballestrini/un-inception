#include <stdio.h>
#include <dlfcn.h>

typedef struct { int version; int width; unsigned char *data; } QRcode;
typedef QRcode* (*qr_encode_fn)(const char*, int, int, int, int);
typedef void (*qr_free_fn)(QRcode*);

int main(void) {
    void *lib = dlopen("libqrencode.so", RTLD_LAZY);
    if (!lib) { fprintf(stderr, "dlopen: %s\n", dlerror()); return 1; }

    qr_encode_fn encode = (qr_encode_fn)dlsym(lib, "QRcode_encodeString");
    qr_free_fn   qfree  = (qr_free_fn)dlsym(lib, "QRcode_free");

    QRcode *qr = encode("unsandbox-qr-ok", 0, 1, 2, 1);
    if (!qr) { fprintf(stderr, "encode failed\n"); return 1; }

    printf("QR:unsandbox-qr-ok:ROWS:%d\n", qr->width);
    qfree(qr);
    dlclose(lib);
    return 0;
}
