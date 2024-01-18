! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : June 5, 2006
!*
!*  DESCRIPTION                : Testing reallocation on assignment,
!*                               with components of derived types.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

type dt
  integer, allocatable :: i(:)
end type
type dt2
  integer :: i(2)
end type
type(dt) :: x
type(dt2) :: y
x%i = y%i
if (.not. allocated(x%i)) error stop 1
if (any(shape(x%i) .ne. (/2/))) error stop 2
end
