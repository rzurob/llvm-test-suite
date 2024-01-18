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
!*  TEST CASE TITLE            : ReallocAssignArray07
!*
!*  PROGRAMMER                 : Rob James
!*  DATE                       : June 5, 2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*  DESCRIPTION                : Testing reallocation on assignment, 
!*                               and testing pointer association with
!*                               the left-hand side.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

real(4), allocatable, target :: a(:)
real(4), target :: b(5)
real(4), pointer :: p(:)
b = (/(real(i),i=1,5)/)
allocate(a(2))
p => a
a = b
if (associated(p,a)) stop 1
if (any(shape(a) /= (/5/))) stop 2
if (any(a /= (/1.0,2.0,3.0,4.0,5.0/))) stop 3
end
