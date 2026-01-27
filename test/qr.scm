(use-modules (system foreign)
             (rnrs bytevectors))

(define libqr (dynamic-link "libqrencode"))

(define qr-encode-string
  (pointer->procedure '*
    (dynamic-func "QRcode_encodeString" libqr)
    (list '* int32 int32 int32 int32)))

(define qr-free
  (pointer->procedure void
    (dynamic-func "QRcode_free" libqr)
    (list '*)))

(let* ((text (string->pointer "unsandbox-qr-ok"))
       (qr-ptr (qr-encode-string text 0 1 2 1))
       ;; QRcode struct: { int version, int width, unsigned char *data }
       (version (bytevector-sint-ref (pointer->bytevector qr-ptr 4) 0 (native-endianness) 4))
       (width   (bytevector-sint-ref (pointer->bytevector (make-pointer (+ (pointer-address qr-ptr) 4)) 4) 0 (native-endianness) 4)))
  (format #t "QR:unsandbox-qr-ok:ROWS:~d~%" width)
  (qr-free qr-ptr))
