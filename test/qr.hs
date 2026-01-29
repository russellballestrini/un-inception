import Codec.QRCode
import Codec.QRCode.JuicyPixels

main :: IO ()
main = do
  let mqr = encodeText defaultQRCodeOptions Iso8859_1OrUtf8WithoutECI "unsandbox-qr-ok"
  case mqr of
    Nothing  -> putStrLn "QR encode failed"
    Just img -> do
      let matrix = toMatrix True False img :: [[Bool]]
      putStrLn $ "QR:unsandbox-qr-ok:ROWS:" ++ show (length matrix)
