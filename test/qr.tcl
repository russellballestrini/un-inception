package require critcl

critcl::ccode {
    #include <dlfcn.h>
    typedef struct { int version; int width; unsigned char *data; } QRcode;
}

critcl::cproc qr_width {Tcl_Interp* interp} int {
    void *lib = dlopen("libqrencode.so", 2);
    if (!lib) return -1;
    QRcode* (*enc)(const char*,int,int,int,int) = dlsym(lib, "QRcode_encodeString");
    void (*fr)(QRcode*) = dlsym(lib, "QRcode_free");
    QRcode *qr = enc("unsandbox-qr-ok", 0, 1, 2, 1);
    int w = qr->width;
    fr(qr);
    dlclose(lib);
    return w;
}

set rows [qr_width]
puts "QR:unsandbox-qr-ok:ROWS:$rows"
