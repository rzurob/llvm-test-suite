! F2008 generic extensions:  Procedure and data dummy arguments
! are distinguishable.
module m
  implicit none
  integer, parameter :: from_sub1 = 1
  integer, parameter :: from_sub2 = 2
  integer :: status = 0
contains
  subroutine sub1(x)
    implicit none
    procedure() x
    status = from_sub1
  end subroutine

  subroutine sub2(x)
    integer x
    status = from_sub2
  end subroutine
end module

use m
implicit none
interface foo
  procedure sub1
  procedure sub2
end interface

interface
  integer function bar(x, y, z)
    real x, y, z
  end function
end interface

integer y

call foo(y)   ! should go to sub2
if (status /= from_sub2) then
  print *, status
  error stop 1
end if

call foo(bar) ! should go to sub1
if (status /= from_sub1) then
  print *, status
  error stop 2
end if
end

integer function bar(x, y, z)
  real x, y, z
  bar = int(x + y + z)
end function
