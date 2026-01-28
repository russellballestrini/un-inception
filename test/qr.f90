program qrcode_test
  use, intrinsic :: iso_c_binding
  implicit none

  ! QRcode struct layout: version(int), width(int), data(ptr)
  type, bind(C) :: QRcode
    integer(c_int) :: version
    integer(c_int) :: width
    type(c_ptr)    :: data
  end type QRcode

  ! C function interfaces
  interface
    type(c_ptr) function dlopen(filename, flag) bind(C, name='dlopen')
      import :: c_ptr, c_char, c_int
      character(kind=c_char), dimension(*), intent(in) :: filename
      integer(c_int), value :: flag
    end function dlopen

    type(c_funptr) function dlsym(handle, symbol) bind(C, name='dlsym')
      import :: c_ptr, c_funptr, c_char
      type(c_ptr), value :: handle
      character(kind=c_char), dimension(*), intent(in) :: symbol
    end function dlsym

    integer(c_int) function f_dlclose(handle) bind(C, name='dlclose')
      import :: c_ptr, c_int
      type(c_ptr), value :: handle
    end function f_dlclose
  end interface

  abstract interface
    type(c_ptr) function encode_iface(s, ver, lvl, hint, cs) bind(C)
      import :: c_ptr, c_char, c_int
      character(kind=c_char), dimension(*), intent(in) :: s
      integer(c_int), value :: ver, lvl, hint, cs
    end function encode_iface
    subroutine free_iface(qr) bind(C)
      import :: c_ptr
      type(c_ptr), value :: qr
    end subroutine free_iface
  end interface

  type(c_ptr) :: lib, qr_ptr
  type(c_funptr) :: enc_fptr, free_fptr
  procedure(encode_iface), pointer :: qr_encode
  procedure(free_iface), pointer :: qr_free
  type(QRcode), pointer :: qr
  integer(c_int) :: rc

  lib = dlopen("libqrencode.so" // c_null_char, 2)
  enc_fptr = dlsym(lib, "QRcode_encodeString" // c_null_char)
  free_fptr = dlsym(lib, "QRcode_free" // c_null_char)
  call c_f_procpointer(enc_fptr, qr_encode)
  call c_f_procpointer(free_fptr, qr_free)

  qr_ptr = qr_encode("unsandbox-qr-ok" // c_null_char, 0, 1, 2, 1)
  call c_f_pointer(qr_ptr, qr)
  write(*, '(A,I0)') 'QR:unsandbox-qr-ok:ROWS:', qr%width

  call qr_free(qr_ptr)
  rc = f_dlclose(lib)
end program qrcode_test
