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
  type t
    integer :: a
  contains
    final finalt
  end type
  type, extends(t) :: t2
    integer :: b
  end type
  integer :: finalizecount = 0
contains
  subroutine finalt(obj)
    type(t) :: obj
    finalizecount = finalizecount + 1
  end subroutine
end module

use m
type(t), allocatable :: a(:)
type(t2) :: b(5)
b = (/t2(1,1),t2(2,2),t2(3,3),t2(4,4),t2(5,5)/)
a = b%t
if (any(shape(a) /= (/5/))) error stop 1
if (any(a%a /= (/1,2,3,4,5/))) error stop 2
if (lbound(a,1) /= 1) error stop 3
if (ubound(a,1) /= 5) error stop 4
if (finalizecount /= 10) error stop 5
end