using QRCoder;

class Program {
    static void Main() {
        var gen = new QRCodeGenerator();
        var data = gen.CreateQrCode("unsandbox-qr-ok", QRCodeGenerator.ECCLevel.M);
        var rows = data.ModuleMatrix.Count;
        Console.WriteLine($"QR:unsandbox-qr-ok:ROWS:{rows}");
    }
}
