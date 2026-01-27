import 'package:qr/qr.dart';

void main() {
  final qr = QrCode.fromData(data: 'unsandbox-qr-ok', errorCorrectLevel: QrErrorCorrectLevel.M);
  print('QR:unsandbox-qr-ok:ROWS:${qr.moduleCount}');
}
