!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : ScalarCharToArrayDummy07
!*
!*  PROGRAMMER                 : Rob James
!*  DATE                       : 04/27/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
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
    character :: x(*)
    character(100) :: y
    if (len(x) /= 1) stop 1
    do i = 1, 100
      y(i:i) = x(i)
    end do
    if (y /= repeat('0123456789', 10)) stop 2
  end subroutine
end
