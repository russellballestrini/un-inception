program fibonacci
  implicit none
  integer :: i

  do i = 0, 10
    print '(A,I0,A,I0)', 'fib(', i, ') = ', fib(i)
  end do

contains
  recursive function fib(n) result(res)
    integer, intent(in) :: n
    integer :: res

    if (n <= 1) then
      res = n
    else
      res = fib(n-1) + fib(n-2)
    end if
  end function fib
end program fibonacci
