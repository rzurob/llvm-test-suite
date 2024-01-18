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
!*  TEST CASE TITLE            : ScalarCharToArrayDummy10
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

module m
contains
  subroutine sub(x)
    character(5) :: x(*)
    character(100) :: y
    if (len(x) /= 5) stop 1
    do i = 1, 20
      y((i-1)*len(x)+1:i*len(x)) = x(i)
    end do
    if (y /= repeat('0123456789', 10)) stop 2
  end subroutine
end module

use m
character(100) :: c
c = repeat('0123456789', 10)
call sub(c)
end
