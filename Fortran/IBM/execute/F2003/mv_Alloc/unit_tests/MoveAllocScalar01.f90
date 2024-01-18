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

integer, allocatable, target :: a, b
integer, pointer :: p
allocate(a)
p => a
call move_alloc(a,b)
if (allocated(a)) stop 1
if (.not.allocated(b)) stop 2
if (.not.associated(p,b)) stop 3
if (allocated(a)) deallocate(a)
if (allocated(b)) deallocate(b)
end
