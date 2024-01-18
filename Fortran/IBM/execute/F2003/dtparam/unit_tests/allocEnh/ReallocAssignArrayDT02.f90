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

type t(x)
  integer, len :: x
  integer :: a
end type
type(t(:)), allocatable :: a(:)
type(t(3)) :: b(5)
b = (/t(3)(1),t(3)(2),t(3)(3),t(3)(4),t(3)(5)/)
a = b(2:3)
if (any(shape(a) /= 2)) error stop 1
if (a%x /= 3) error stop 2
if (any(a%a /= (/2,3/))) error stop 3
if (lbound(a,1) /= 1) error stop 4
if (ubound(a,1) /= 2) error stop 5
end
