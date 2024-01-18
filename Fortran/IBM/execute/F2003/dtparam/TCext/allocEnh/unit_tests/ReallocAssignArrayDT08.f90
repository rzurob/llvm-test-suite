! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/allocEnh/unit_tests/ReallocAssignArrayDT08.f
! opt variations: -ql

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : June 5, 2006
!*
!*  DESCRIPTION                : Testing reallocation on assignment,
!*                               with an array of derived types with
!*                               a deferred length type parameter on
!*                               the left-hand side of the assignment.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
  type t(k1)    ! (4)
    integer, kind :: k1
    integer(k1)   :: a
  contains
    final finaltarray
  end type
  integer :: finalizecount = 0
contains
  subroutine finaltarray(obj)
    type(t(4)) :: obj(:)
    finalizecount = finalizecount + 1
  end subroutine
end module

use m
type(t(4)), allocatable :: a(:)
type(t(4)) :: b(5)
allocate(t(4) :: a(4))
b = (/t(4)(1),t(4)(2),t(4)(3),t(4)(4),t(4)(5)/)
a = b(2:3)
if (any(shape(a) /= 2)) stop 1
if (any(a%a /= (/2,3/))) stop 2
if (lbound(a,1) /= 1) stop 3
if (ubound(a,1) /= 2) stop 4
if (finalizecount /= 2) stop 5
end
