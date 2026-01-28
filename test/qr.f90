program qrcode_test
  use, intrinsic :: iso_c_binding
  implicit none

  ! Direct C FFI - requires linking with -lqrencode
  interface
    type(c_ptr) function QRcode_encodeString(s, ver, lvl, hint, cs) bind(C, name='QRcode_encodeString')
      import :: c_ptr, c_char, c_int
      character(kind=c_char), dimension(*), intent(in) :: s
      integer(c_int), value :: ver, lvl, hint, cs
    end function

    subroutine QRcode_free(qr) bind(C, name='QRcode_free')
      import :: c_ptr
      type(c_ptr), value :: qr
    end subroutine
  end interface

  type, bind(C) :: QRcode
    integer(c_int) :: version
    integer(c_int) :: width
    type(c_ptr)    :: data
  end type

  type(c_ptr) :: qr_ptr
  type(QRcode), pointer :: qr

  qr_ptr = QRcode_encodeString("unsandbox-qr-ok" // c_null_char, 0, 1, 2, 1)
  if (c_associated(qr_ptr)) then
    call c_f_pointer(qr_ptr, qr)
    write(*, '(A,I0)') 'QR:unsandbox-qr-ok:ROWS:', qr%width
    call QRcode_free(qr_ptr)
  else
    write(*, '(A)') 'QR encode failed'
  end if
end program qrcode_test
