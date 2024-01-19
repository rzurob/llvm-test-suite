! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : June 5, 2006
!*
!*  DESCRIPTION                : Testing reallocation on assignment,
!*                               with a pointer array on the right-
!*                               hand side of the assignment.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

type dt
  integer, allocatable :: i(:)
end type
type dt2
  integer, pointer :: p(:)
end type
type(dt2) :: y
type(dt) :: x
allocate(y%p(6:9))
y%p = (/4,3,2,1/)
x%i = y%p
if (.not. allocated(x%i)) error stop 1
if (any(shape(x%i) .ne. (/4/))) error stop 2
if (lbound(x%i,1) /= 6) error stop 3
if (ubound(x%i,1) /= 9) error stop 4
if (any(x%i /= (/4,3,2,1/))) error stop 5
end
