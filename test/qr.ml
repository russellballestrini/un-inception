#use "topfind";;
#require "ctypes";;
#require "ctypes.foreign";;

open Ctypes;;
open Foreign;;

type qrcode
let qrcode : qrcode structure typ = structure "QRcode";;
let _qr_version = field qrcode "version" int;;
let qr_width   = field qrcode "width"   int;;
let _qr_data   = field qrcode "data"    (ptr char);;
let () = seal qrcode;;

let encode_string =
  foreign "QRcode_encodeString"
    (string @-> int @-> int @-> int @-> int @-> returning (ptr qrcode));;

let qr_free =
  foreign "QRcode_free"
    (ptr qrcode @-> returning void);;

let () =
  let qr = encode_string "unsandbox-qr-ok" 0 1 2 1 in
  let w = getf !@qr qr_width in
  Printf.printf "QR:unsandbox-qr-ok:ROWS:%d\n" w;
  qr_free qr;;
