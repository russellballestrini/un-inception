let () =
  let qr = Qrc.encode "unsandbox-qr-ok" in
  let rows = Array.length (Qrc.to_matrix qr) in
  Printf.printf "QR:unsandbox-qr-ok:ROWS:%d\n" rows
