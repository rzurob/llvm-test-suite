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
    character(5) :: x(*)
    character(100) :: y
    if (len(x) /= 5) error stop 1
    do i = 1, 20
      y((i-1)*len(x)+1:i*len(x)) = x(i)
    end do
    if (y /= repeat('0123456789', 10)) error stop 2
  end subroutine
end