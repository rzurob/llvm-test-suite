! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : June 5, 2006
!*
!*  DESCRIPTION                : Testing reallocation on assignment,
!*                               with components of derived types, and
!*                               an array expression on the right-hand
!*                               side of the assignment.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

type dt
  integer, allocatable :: i(:)
end type
integer :: i(2:4)
type(dt) :: x
x%i = abs(i)
if (.not. allocated(x%i)) error stop 1
if (any(shape(x%i) .ne. (/3/))) error stop 2
if (lbound(x%i,1) /= 1) error stop 3
if (ubound(x%i,1) /= 3) error stop 4
end
