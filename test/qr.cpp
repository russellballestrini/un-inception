#include <iostream>
#include <dlfcn.h>

extern "C" {
    typedef struct { int version; int width; unsigned char *data; } QRcode;
}

int main() {
    void *lib = dlopen("libqrencode.so", RTLD_LAZY);
    if (!lib) { std::cerr << "dlopen: " << dlerror() << std::endl; return 1; }

    auto encode = reinterpret_cast<QRcode*(*)(const char*, int, int, int, int)>(
        dlsym(lib, "QRcode_encodeString"));
    auto qfree = reinterpret_cast<void(*)(QRcode*)>(
        dlsym(lib, "QRcode_free"));

    QRcode *qr = encode("unsandbox-qr-ok", 0, 1, 2, 1);
    if (!qr) { std::cerr << "encode failed" << std::endl; return 1; }

    std::cout << "QR:unsandbox-qr-ok:ROWS:" << qr->width << std::endl;
    qfree(qr);
    dlclose(lib);
    return 0;
}
