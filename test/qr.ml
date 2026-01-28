(* OCaml single-file execution can't use ocamlfind packages *)
(* Use Unix.open_process_in with qrencode CLI *)
let () =
  let ic = Unix.open_process_in "qrencode -t ASCII -m 0 'unsandbox-qr-ok' | wc -l" in
  let rows = String.trim (input_line ic) in
  ignore (Unix.close_process_in ic);
  Printf.printf "QR:unsandbox-qr-ok:ROWS:%s\n" rows
