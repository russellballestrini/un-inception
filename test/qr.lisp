(ql:quickload :cl-qrencode :silent t)
(let* ((matrix (cl-qrencode:matrix (cl-qrencode:encode-symbol "unsandbox-qr-ok")))
       (rows (array-dimension matrix 0)))
  (format t "QR:unsandbox-qr-ok:ROWS:~d~%" rows))
