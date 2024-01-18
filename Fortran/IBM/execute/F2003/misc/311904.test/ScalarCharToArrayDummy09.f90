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

interface
  subroutine sub(x)
    character :: x(*)
  end subroutine
end interface
character(100) :: c
c = repeat('0123456789', 10)
call sub(c)
end

subroutine sub(x)
  character :: x(*)
  character(100) :: y
  if (len(x) /= 1) error stop 1
  do i = 1, 100
    y(i:i) = x(i)
  end do
  if (y /= repeat('0123456789', 10)) error stop 2
end subroutine
