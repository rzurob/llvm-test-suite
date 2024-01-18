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
!*  TEST CASE TITLE            : ReallocAssignArray06
!*
!*  PROGRAMMER                 : Rob James
!*  DATE                       : June 5, 2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*  DESCRIPTION                : Testing reallocation on assignment,
!*                               making sure a pointer is still 
!*                               associated if no reallocation should
!*                               be done.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

integer(8), allocatable, target :: a(:)
integer(8), pointer :: p(:)
allocate(a(3))
p => a
a = (/1,2,3/)
if (.not.associated(p,a)) stop 1
if (any(shape(a) /= (/3/))) stop 2
if (any(a /= (/1,2,3/))) stop 3
end
