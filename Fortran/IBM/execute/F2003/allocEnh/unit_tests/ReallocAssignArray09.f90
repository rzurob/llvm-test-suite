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
!*  TEST CASE TITLE            : ReallocAssignArray09
!*
!*  PROGRAMMER                 : Rob James
!*  DATE                       : June 5, 2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*  DESCRIPTION                : Testing reallocation on assignment,
!*                               with components of derived types, and
!*                               an array expression on the right-hand
!*                               side of the assignment.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

type dt
  integer, allocatable :: i(:)
end type
integer :: i(2:4)
type(dt) :: x
x%i = abs(i)
if (.not. allocated(x%i)) stop 1
if (any(shape(x%i) .ne. (/3/))) stop 2
if (lbound(x%i,1) /= 1) stop 3
if (ubound(x%i,1) /= 3) stop 4
end
