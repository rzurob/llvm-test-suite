! TS29113 C-Interop Optional arguments
! Check langlvl checking for optional arguments in bind(c) procedures
! appearing in interface blocks (with and without NAME=)
use, intrinsic :: iso_c_binding
implicit none

interface
  ! Implicit binding label
  function func1(a) bind(c)
    use iso_c_binding
    integer(c_int), optional :: a
    integer(c_int) func1
  end function

  ! Explicit binding label.  (Same name)
  function func2(a) bind(c, name='func2')
    use iso_c_binding
    integer(c_int), optional :: a
    integer(c_int) func
  end function

  ! Explicit binding label.  (Different name)
  function func3(a) bind(c, name='func4')
    use iso_c_binding
    integer(c_int), optional :: a
    integer(c_int) func
  end function
end interface

print *, func1(), func2(3_c_int), func3()
end
