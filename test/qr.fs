open System
open System.Runtime.InteropServices

[<DllImport("libqrencode.so", CallingConvention = CallingConvention.Cdecl)>]
extern nativeint QRcode_encodeString(string text, int version, int level, int hint, int caseSensitive)

[<DllImport("libqrencode.so", CallingConvention = CallingConvention.Cdecl)>]
extern void QRcode_free(nativeint qr)

[<Struct; StructLayout(LayoutKind.Sequential)>]
type QRcode =
    val version: int
    val width: int

let qr = QRcode_encodeString("unsandbox-qr-ok", 0, 1, 2, 1)
if qr <> IntPtr.Zero then
    let s = Marshal.PtrToStructure<QRcode>(qr)
    printfn "QR:unsandbox-qr-ok:ROWS:%d" s.width
    QRcode_free(qr)
else
    printfn "QR encode failed"
