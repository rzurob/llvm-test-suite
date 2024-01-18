! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/28/2006
!*
!*  DESCRIPTION                : Use of MOVE_ALLOC with allocatable
!*                               scalars
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

type t
  integer, allocatable :: x
end type
type(t), target :: a(5), b(10)
integer, pointer :: p
allocate(a(3)%x)
p => a(3)%x
call move_alloc(a(3)%x,b(5)%x)
if (allocated(a(3)%x)) stop 1
if (.not.allocated(b(5)%x)) stop 2
if (.not.associated(p,b(5)%x)) stop 3
end
