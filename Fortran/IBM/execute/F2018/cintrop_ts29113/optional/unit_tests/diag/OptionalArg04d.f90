! TS29113 C-Interop Optional arguments
! Check that optional arguments are allowed in bind(c) procedures
! Check langlvl checking for optional arguments in bind(c) procedures
function foo(a, b) bind(c)
  use iso_c_binding
  integer(c_int) :: a
  integer(c_int), optional :: b
  integer(c_int) foo

  if (present(b)) then
    foo = a + b
  else
    foo = a
  endif
end function 

program main
  use iso_c_binding
  implicit none

  interface
    function foo(a, b) bind(c)
      import c_int
      integer(c_int) :: a
      integer(c_int), optional :: b
      integer(c_int) foo
    end function
  end interface

  integer(c_int) :: a, b, res

  a = 1
  b = 2

  res = foo(a, b)
  if (res /= 3) then
    print *, res
    error stop 1
  endif

  res = foo(a)
  if (res /= a) then
    print *, res
    error stop 2
  endif
end program
