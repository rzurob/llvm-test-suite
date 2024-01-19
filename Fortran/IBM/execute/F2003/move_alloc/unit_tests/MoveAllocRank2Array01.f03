! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/28/2006
!*
!*  DESCRIPTION                : Use of MOVE_ALLOC with allocatable
!*                               rank-2 arrays
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

integer, allocatable, target :: a(:,:), b(:,:)
integer, pointer :: p(:,:)
allocate(a(10,10))
p => a
call move_alloc(a,b)
if (allocated(a)) error stop 1
if (.not.allocated(b)) error stop 2
if (.not.associated(p,b)) error stop 3
if (allocated(a)) deallocate(a)
if (allocated(b)) deallocate(b)
end
