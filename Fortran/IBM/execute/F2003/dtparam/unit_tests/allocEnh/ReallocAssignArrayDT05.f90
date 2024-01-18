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
!*  TEST CASE TITLE            : ReallocAssignArrayDT05
!*
!*  PROGRAMMER                 : Rob James
!*  DATE                       : June 5, 2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*  DESCRIPTION                : Testing reallocation on assignment,
!*                               with an array of derived types with
!*                               a deferred length type parameter on 
!*                               the left-hand side of the assignment.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

@process check
type t(x)
  integer, len :: x
  integer :: a
end type
type(t(3)), allocatable :: a(:)
type(t(3)) :: b(5)
allocate(a(4))
b = (/t(3)(1),t(3)(2),t(3)(3),t(3)(4),t(3)(5)/)
a = b(2:3)
if (any(shape(a) /= 2)) stop 1
if (a%x /= 3) stop 2
if (any(a%a /= (/2,3/))) stop 3
if (lbound(a,1) /= 1) stop 4
if (ubound(a,1) /= 2) stop 5
end
