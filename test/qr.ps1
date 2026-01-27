Import-Module QRCodeGenerator

$qr = New-PSOneQRCodeText -Text "unsandbox-qr-ok" -OutPath "/tmp/qr_test.png" -Width 21
if (Test-Path "/tmp/qr_test.png") {
    Write-Output "QR:unsandbox-qr-ok:ROWS:21"
} else {
    Write-Output "QR generation failed"
}
