!#######################################################################
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

character(100) :: c
c = repeat('0123456789', 10)
call sub(c)
contains
  subroutine sub(x)
    character :: x(100)
    character(100) :: y
    if (len(x) /= 1) stop 1
    if (size(x) /= 100) stop 2
    do i = 1, size(x)
      y(i:i) = x(i)
    end do
    if (y /= repeat('0123456789', 10)) stop 3
  end subroutine
end
