! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/27/2006
!*
!*  DESCRIPTION                : Passing scalar characters as actual
!*                               arguments associated with array dummy
!*                               arguments
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
contains
  subroutine sub(x)
    character :: x(100)
    character(100) :: y
    if (len(x) /= 1) error stop 1
    if (size(x) /= 100) error stop 2
    do i = 1, size(x)
      y(i:i) = x(i)
    end do
    if (y /= repeat('0123456789', 10)) error stop 3
  end subroutine
end module

use m
character(100) :: c
c = repeat('0123456789', 10)
call sub(c)
end
