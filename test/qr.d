import core.sys.posix.dlfcn;
import std.stdio;
import std.string;

struct QRcode {
    int _version;
    int width;
    void* data;
}

void main() {
    auto lib = dlopen("libqrencode.so", RTLD_LAZY);
    if (lib is null) {
        stderr.writeln("dlopen: ", dlerror().fromStringz);
        return;
    }

    alias EncFunc = extern(C) QRcode* function(const char*, int, int, int, int);
    alias FreeFunc = extern(C) void function(QRcode*);

    auto encode = cast(EncFunc) dlsym(lib, "QRcode_encodeString");
    auto qfree = cast(FreeFunc) dlsym(lib, "QRcode_free");

    auto qr = encode("unsandbox-qr-ok", 0, 1, 2, 1);
    if (qr is null) {
        stderr.writeln("encode failed");
        return;
    }

    writefln("QR:unsandbox-qr-ok:ROWS:%d", qr.width);
    qfree(qr);
    dlclose(lib);
}
