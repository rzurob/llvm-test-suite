! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/allocEnh/unit_tests/ReallocAssignArrayDT09.f
! opt variations: -qnol -qnodeferredlp

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : June 7, 2006
!*
!*  DESCRIPTION                : Testing reallocation on assignment,
!*                               with an array of derived types with
!*                               a deferred length type parameter on
!*                               the left-hand side of the assignment.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
  type t(n1,k1)    ! (20,4)
    integer, kind :: k1
    integer, len  :: n1
    integer(k1)   :: a
  contains
    final finaltarray
  end type
  integer :: finalizecount = 0
contains
  subroutine finaltarray(obj)
    type(t(*,4)) :: obj(:)
    finalizecount = finalizecount + 1
  end subroutine
end module

use m
type(t(:,4)), allocatable :: a(:)
type(t(20,4)) :: b(5)
b = (/t(20,4)(1),t(20,4)(2),t(20,4)(3),t(20,4)(4),t(20,4)(5)/)
a = b(1:4)
if (any(shape(a) /= 4)) stop 1
if (any(a%a /= (/1,2,3,4/))) stop 2
if (lbound(a,1) /= 1) stop 3
if (ubound(a,1) /= 4) stop 4
if (finalizecount /= 1) stop 5
end
