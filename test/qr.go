package main

import (
	"fmt"
	qrcode "github.com/skip2/go-qrcode"
)

func main() {
	qr, err := qrcode.New("unsandbox-qr-ok", qrcode.Medium)
	if err != nil {
		panic(err)
	}
	rows := len(qr.Bitmap())
	fmt.Printf("QR:unsandbox-qr-ok:ROWS:%d\n", rows)
}
