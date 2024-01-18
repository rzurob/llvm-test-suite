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
!*  TEST CASE TITLE            : ReallocAssignArrayDT09
!*
!*  PROGRAMMER                 : Rob James
!*  DATE                       : June 7, 2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*  DESCRIPTION                : Testing reallocation on assignment,
!*                               with an array of derived types with
!*                               a deferred length type parameter on 
!*                               the left-hand side of the assignment.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
  type t
    integer :: a
  contains
    final finaltarray
  end type
  integer :: finalizecount = 0
contains
  subroutine finaltarray(obj)
    type(t) :: obj(:)
    finalizecount = finalizecount + 1
  end subroutine
end module

use m
type(t), allocatable :: a(:)
type(t) :: b(5)
b = (/t(1),t(2),t(3),t(4),t(5)/)
a = b(1:4)
if (any(shape(a) /= 4)) stop 1
if (any(a%a /= (/1,2,3,4/))) stop 2
if (lbound(a,1) /= 1) stop 3
if (ubound(a,1) /= 4) stop 4
if (finalizecount /= 1) stop 5
end
