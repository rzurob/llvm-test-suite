!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : June 9, 2006
!*
!*  DESCRIPTION                : Testing reallocation on assignment,
!*                               with an array of derived types with
!*                               a deferred length type parameter on
!*                               the left-hand side of the assignment.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

@process check
type t
  integer :: a
end type
type(t), allocatable :: a(:)
type(t) b(5)
b = (/t(1),t(2),t(3),t(4),t(5)/)
allocate(a(3))
associate (x => a)
  a = b
end associate
if (.not. allocated(a)) stop 1
if (any(shape(a) /= shape(b))) stop 2
if (lbound(a,1) /= 1) stop 3
if (ubound(a,1) /= 5) stop 4
if (any(a%a /= (/1,2,3,4,5/))) stop 5
end
