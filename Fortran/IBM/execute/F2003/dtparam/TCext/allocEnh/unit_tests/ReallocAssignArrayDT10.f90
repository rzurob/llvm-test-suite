! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=none /tstdev/F2003/allocEnh/unit_tests/ReallocAssignArrayDT10.f
! opt variations: -qnol -qnodeferredlp -qreuse=base

! SCCS ID Information
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
    final finalt
  end type
  type, extends(t) :: t2(n2,k2)    ! (20,4,20,4)
    integer, kind :: k2
    integer, len  :: n2
    integer(k2)   :: b
  end type
  integer :: finalizecount = 0
contains
  subroutine finalt(obj)
    type(t(*,4)) :: obj
    finalizecount = finalizecount + 1
  end subroutine
end module

use m
type(t(:,4)), allocatable :: a(:)
type(t2(20,4,20,4)) :: b(5)
b = (/t2(20,4,20,4)(1,1),t2(20,4,20,4)(2,2),t2(20,4,20,4)(3,3),t2(20,4,20,4)(4,4),t2(20,4,20,4)(5,5)/)
a = b%t
if (any(shape(a) /= (/5/))) stop 1
if (any(a%a /= (/1,2,3,4,5/))) stop 2
if (lbound(a,1) /= 1) stop 3
if (ubound(a,1) /= 5) stop 4
if (finalizecount /= 10) stop 5
end
