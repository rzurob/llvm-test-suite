! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : June 5, 2006
!*
!*  DESCRIPTION                : Testing reallocation on assignment,
!*                               making sure a pointer is still
!*                               associated if no reallocation should
!*                               be done.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

integer(8), allocatable, target :: a(:)
integer(8), pointer :: p(:)
allocate(a(3))
p => a
a = (/1,2,3/)
if (.not.associated(p,a)) error stop 1
if (any(shape(a) /= (/3/))) error stop 2
if (any(a /= (/1,2,3/))) error stop 3
end
