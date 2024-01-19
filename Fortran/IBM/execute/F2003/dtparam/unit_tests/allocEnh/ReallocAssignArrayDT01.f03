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
end type
type(t(:)), allocatable :: a(:)
type(t(3)) :: b(5)
allocate(t(2) :: a(4))
a = b(1:2)
if (any(shape(a) /= 2)) error stop 1
if (a%x /= 3) error stop 2
end
