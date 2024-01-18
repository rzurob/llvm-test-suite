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
!*  TEST CASE TITLE            : ReallocAssignArray16
!*
!*  PROGRAMMER                 : Rob James
!*  DATE                       : June 5, 2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*  DESCRIPTION                : Testing reallocation on assignment, 
!*                               using dummy arguments.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

integer, allocatable :: i(:)
call sub(i)
if (.not. allocated(i)) stop 1
if (any(shape(i) .ne. (/5/))) stop 2
if (lbound(i,1) /= 2) stop 3
if (ubound(i,1) /= 6) stop 4
if (any(i /= (/2,4,6,8,10/))) stop 5
contains
  subroutine sub(a)
    integer, allocatable :: a(:)
    integer b(2:6)
    b = (/(2*j,j=1,5)/)
    a = b
  end subroutine
end
