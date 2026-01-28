import System.Posix.DynamicLinker
import Foreign
import Foreign.C.Types
import Foreign.C.String

data QRcode = QRcode CInt CInt (Ptr Word8)

instance Storable QRcode where
  sizeOf    _ = 16
  alignment _ = 8
  peek ptr = QRcode
    <$> peekByteOff ptr 0
    <*> peekByteOff ptr 4
    <*> peekByteOff ptr 8
  poke _ _ = return ()

type EncodeFunc = CString -> CInt -> CInt -> CInt -> CInt -> IO (Ptr QRcode)
type FreeFunc = Ptr QRcode -> IO ()

foreign import ccall "dynamic"
  mkEncode :: FunPtr EncodeFunc -> EncodeFunc

foreign import ccall "dynamic"
  mkFree :: FunPtr FreeFunc -> FreeFunc

main :: IO ()
main = do
  dl <- dlopen "libqrencode.so" [RTLD_LAZY]
  encFP <- dlsym dl "QRcode_encodeString"
  freeFP <- dlsym dl "QRcode_free"
  let encode = mkEncode encFP
  let qfree  = mkFree freeFP
  cs <- newCString "unsandbox-qr-ok"
  qr <- encode cs 0 1 2 1
  if qr == nullPtr
    then putStrLn "QR encode failed"
    else do
      QRcode _ w _ <- peek qr
      putStrLn $ "QR:unsandbox-qr-ok:ROWS:" ++ show w
      qfree qr
  free cs
